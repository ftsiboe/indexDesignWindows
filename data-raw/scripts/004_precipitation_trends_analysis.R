# ============================================================
# Precipitation Trend Workflow
# ============================================================
# Purpose:
#   Estimate county-specific linear trends in both the conditional
#   mean and conditional variance of historical precipitation.
#
# Main outputs:
#   1. Mean precipitation trend estimates by county
#   2. Variance trend estimates by county
#   3. Fisher combined evidence tests for mean/variance trends
#   4. Clean mean-variance trend classifications for mapping/summaries
#
# Modeling strategy:
#   For each rolling historical window, the workflow estimates:
#     Step 1: Initial panel mean model
#     Step 2: Residual-based log-variance model
#     Step 3: Inverse-variance pre-weighted mean model
#
# Important note:
#   The pre-weighted mean model is an FGLS-style robustness approach.
#   Instead of using plm's weights argument, the outcome and trend
#   variable are multiplied by sqrt(weight) before estimation. This
#   avoids plm's limitation where vcovDC/vcovHC are not implemented
#   for weighted panel regressions.
# ============================================================

# ============================================================
# Reset workspace
# ============================================================

rm(list = ls(all = TRUE))
gc()


# ============================================================
# Load required packages
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(tidyr)
  library(plm)
  library(lmtest)
  library(stringr)
  library(sf)
  library(terra)
})

devtools::document()

# ============================================================
# Read input data and define output directory
# ============================================================

study_environment <- readRDS("data/study_environment.rds")

output_directory <- "data-raw/fastscratch/precipitation_trend"

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# Build precipitation-by-grid-year panel
# ============================================================
# The CPC precipitation data are aggregated to the grid-year level.
# Each observation represents total precipitation for a PRF grid in a
# given commodity year.
# ============================================================

data <- as.data.table(readRDS("data/cpc_historic_precipitation.rds"))[
  ,
  .(
    precipitation     = sum(precipitation, na.rm = TRUE),
    precipitation_rma = sum(precipitation_rma, na.rm = TRUE)
  ),
  by = .(commodity_year, grid_id)
]


# ============================================================
# Attach county and state identifiers
# ============================================================
# Create a grid-to-county lookup table and merge it onto the
# precipitation panel. County FIPS is later used to estimate
# county-specific trend coefficients through:
#
#   trend:factor(county_fips)
# ============================================================

grid_county_lookup <- unique(
  as.data.table(readRDS("data/prf_counties.rds"))[
    ,
    .(grid_id, county_fips)
  ]
)

grid_county_lookup[, county_fips := as.character(county_fips)]

data <- grid_county_lookup[
  data,
  on = "grid_id",
  allow.cartesian = TRUE
]

data <- tidyr::separate(
  data,
  col    = "county_fips",
  into   = c("state_code", "county_code"),
  sep    = 2,
  remove = FALSE
)

data <- as.data.table(data)


# ============================================================
# Define panel structure
# ============================================================
# Each panel unit is a county-grid combination observed repeatedly
# over commodity years. This allows fixed-effects estimators to
# control for persistent county-grid differences.
# ============================================================

data[
  ,
  panelid := paste(county_fips, grid_id, sep = "_")
]

current_year <- max(data$commodity_year, na.rm = TRUE)

# Optional testing filter.
# Uncomment when testing on only a few states.
# data <- data[state_code %in% unique(data$state_code)[1:2]]

# ============================================================
# Define rolling history windows
# ============================================================
# Each value defines the number of most-recent years included in
# the estimation sample.
#
# Example:
#   i = 5  uses the most recent 5 years
#   i = 30 uses the most recent 30 years
#   i = 200 effectively uses the full available history if fewer
#           than 200 years are available
# ============================================================

history_windows <- c(seq(5, 60, 1), 200)

# For quick testing, uncomment:
# history_windows <- history_windows[1:3]


# ============================================================
# SLURM array support
# ============================================================
# If this script is submitted as a SLURM array job, each array task
# estimates one rolling history window.
# ============================================================

slurm_id <- suppressWarnings(
  as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
)

if (!is.na(slurm_id)) {
  history_windows <- history_windows[slurm_id]
}

# ============================================================
# Main estimation loop
# ============================================================

invisible(
  lapply(history_windows, function(i) {
    # i <- history_windows[1]; estimators <- c("within");estimator <- "within"

    # ------------------------------------------------------------
    # Output file for this history window
    # ------------------------------------------------------------
    output_file_county <- file.path(
      output_directory,
      paste0(
        "county_precipitation_trends_",
        stringr::str_pad(i, width = 3, pad = "0"),
        ".rds"
      )
    )

    # ------------------------------------------------------------
    # Select years included in the current rolling window
    # ------------------------------------------------------------
    yrs <- (current_year - i + 1):current_year

    dtw <- data[
      commodity_year %in% yrs
    ]

    if (nrow(dtw) == 0L) {
      stop("No data for window length i = ", i)
    }

    # ------------------------------------------------------------
    # Define linear trend within the selected history window
    # ------------------------------------------------------------
    # The first year in each rolling window is coded as zero.
    # This makes trend coefficients interpretable as the annual
    # change within that specific historical window.
    # ------------------------------------------------------------
    dtw[
      ,
      trend := commodity_year - min(commodity_year, na.rm = TRUE)
    ]

    # ------------------------------------------------------------
    # Remove non-finite values before model estimation
    # ------------------------------------------------------------
    dtw <- dtw[
      is.finite(precipitation) &
        is.finite(trend)
    ]

    # ------------------------------------------------------------
    # Estimate alternative panel specifications
    # ------------------------------------------------------------
    # These are sensitivity checks across pooled, fixed-effects,
    # random-effects, Hausman-Taylor, between, and first-difference
    # panel estimators.
    # ------------------------------------------------------------
    estimators <- c(
      "pooling",
      "within",
      "random",
      "ht",
      "between",
      "fd"
    )

    res <- data.table::rbindlist(
      lapply(
        estimators,
        function(estimator) {
          tryCatch(
            estimate_by_estimator(
              dtw       = dtw,
              estimator = estimator
            ),
            error = function(e) NULL
          )
        }
      ),
      fill = TRUE
    )

    # ------------------------------------------------------------
    # Add history window identifier
    # ------------------------------------------------------------
    res[, history_range := i]

    # ------------------------------------------------------------
    # Classify county trends by significance and direction
    # ------------------------------------------------------------
    res_classified <- classify_mean_variance_trends(
      res = res,
      sig_levels = c(0.01, 0.05, 0.10)
    )

    # ------------------------------------------------------------
    # Save classified results for this history window
    # ------------------------------------------------------------
    saveRDS(res_classified, output_file_county)

    message("Saved county linear results: ", output_file_county)

    invisible(TRUE)
  })
)


# ============================================================
# Optional asset aggregation helper
# ============================================================
# This helper combines all rolling-window county result files into
# one output file. Run manually after confirming the rolling-window
# files were created correctly.
# ============================================================

upload_assets <- function() {

  county_precipitation <- data.table::rbindlist(
    lapply(
      list.files(
        "data-raw/fastscratch/precipitation_trend",
        full.names = TRUE,
        pattern = "county_precipitation"
      ),
      function(i) {
        tryCatch(
          readRDS(i),
          error = function(e) NULL
        )
      }
    ),
    fill = TRUE
  )

  saveRDS(
    county_precipitation,
    "data-raw/output/county_precipitation_trend.rds"
  )

  invisible(county_precipitation)
}

# Run manually when ready:
# county_precipitation <- upload_assets()

