# ============================================================
# Precipitation Trend Workflow
# Linear county-level trend estimation only
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
})

# Use this only if you are actively developing the package.
# devtools::document()

# ============================================================
# Read input data
# ============================================================

study_environment <- readRDS("data/study_environment.rds")

output_directory <- "data-raw/fastscratch/precipitation_trend"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

prf_grid_weights <- as.data.table(readRDS("data/prf_grid_weights.rds"))
cpc_data         <- as.data.table(readRDS("data/cpc_historic_precipitation.rds"))

official_interval_names <- as.data.table(
  readRDS("data/official_interval_names.rds")
)

# Keep only the variables needed to define the PRF intervals
official_interval_names <- unique(
  official_interval_names[
    ,
    .(month_end_code, month_beg_code, interval_name)
  ]
)

# Convert beginning and ending month codes into one common month column.
# This allows the CPC monthly data to be matched to PRF interval definitions.
official_interval_names <- tidyr::gather(
  official_interval_names,
  key   = "name",
  value = "month",
  month_end_code,
  month_beg_code
)

official_interval_names <- as.data.table(official_interval_names)

# ============================================================
# Build precipitation-by-grid-by-interval panel
# ============================================================

# Join CPC precipitation data to official PRF interval definitions.
# The join uses common column names between cpc_data and official_interval_names.
data <- cpc_data[
  official_interval_names,
  on = intersect(names(cpc_data), names(official_interval_names)),
  allow.cartesian = TRUE,
  nomatch = 0
]

# Aggregate precipitation to the commodity-year, grid, and interval level.
data <- data[
  ,
  .(
    precipitation     = sum(precipitation, na.rm = TRUE),
    precipitation_rma = sum(precipitation_rma, na.rm = TRUE)
  ),
  by = .(commodity_year, grid_id, interval_name)
]

# ============================================================
# Attach county and state identifiers
# ============================================================

# Create a grid-to-county lookup table.
# This avoids relying on factor labels to map grid_id to county_fips.
grid_county_lookup <- unique(
  prf_grid_weights[
    ,
    .(grid_id, county_fips)
  ]
)

grid_county_lookup[, county_fips := as.character(county_fips)]

# Merge county FIPS onto the precipitation panel
data <- grid_county_lookup[
  data,
  on = "grid_id"
]

# Split county FIPS into state and county codes
data <- tidyr::separate(
  data,
  col    = "county_fips",
  into   = c("state_code", "county_code"),
  sep    = 2,
  remove = FALSE
)

data <- as.data.table(data)

# Define the panel unit.
# Each panel is a grid-by-interval combination observed over years.
data[
  ,
  panelid := paste(grid_id, interval_name, sep = "_")
]

# Latest commodity year in the dataset
current_year <- max(data$commodity_year, na.rm = TRUE)

# Optional testing filter.
# Remove this line when running the full national workflow.
# data <- data[state_code %in% unique(data$state_code)[1:2]]

# ============================================================
# Helper function: estimate one model for one state
# ============================================================

estimate_one_state <- function(dt, formula, model = "within") {

  # ------------------------------------------------------------
  # Guardrails
  # ------------------------------------------------------------
  # These prevent model estimation when the state-window sample
  # is too small for reliable panel estimation.
  # ------------------------------------------------------------

  if (nrow(dt) < 10L) return(NULL)
  if (length(unique(dt$commodity_year)) < 3L) return(NULL)
  if (length(unique(dt$panelid)) < 2L) return(NULL)

  # ------------------------------------------------------------
  # Estimate panel model
  # ------------------------------------------------------------
  # model options:
  #   "within"  = fixed effects
  #   "random"  = random effects
  #   "pooling" = pooled OLS
  # ------------------------------------------------------------

  obj <- plm::plm(
    formula = formula,
    data    = dt,
    index   = c("panelid", "commodity_year"),
    model   = model
  )

  # ------------------------------------------------------------
  # Double-cluster robust standard errors
  # ------------------------------------------------------------
  # vcovDC accounts for dependence across both panel and time.
  # ------------------------------------------------------------

  V  <- plm::vcovDC(obj)
  ct <- lmtest::coeftest(obj, vcov = V)

  # ------------------------------------------------------------
  # Return tidy coefficient table
  # ------------------------------------------------------------

  out <- data.table::data.table(
    term           = rownames(ct),
    estimate       = ct[, "Estimate"],
    standard_error = ct[, "Std. Error"],
    t_value        = ct[, "t value"],
    p_value        = ct[, "Pr(>|t|)"]
  )

  # Extract county FIPS from terms like:
  # trend:factor(county_fips)20001
  out[
    ,
    county_fips := gsub(
      "trend:factor[(]county_fips[)]",
      "",
      term
    )
  ]

  out[]
}

# ============================================================
# Helper function: estimate county trends for one estimator
# ============================================================

estimate_by_estimator <- function(dtw, estimator) {

  # Linear county-specific trend model.
  # This estimates one precipitation trend for each county FIPS.
  fml <- precipitation ~ trend:factor(county_fips)

  res <- dtw[
    ,
    {
      out_i <- tryCatch(
        estimate_one_state(
          dt      = .SD,
          formula = fml,
          model   = estimator
        ),
        error = function(e) NULL
      )

      # Return a consistent empty row if model estimation fails.
      if (is.null(out_i)) {
        data.table(
          term           = NA_character_,
          county_fips    = NA_character_,
          estimate       = NA_real_,
          standard_error = NA_real_,
          t_value        = NA_real_,
          p_value        = NA_real_
        )
      } else {
        out_i
      }
    },
    by = .(state_code)
  ]

  res[, model_estimator := estimator]

  res[]
}

# ============================================================
# Define history windows
# ============================================================

# Rolling windows to fit.
# 5:60 = 5-year through 60-year windows.
# 200 = long/full-history window if available.
history_windows <- c(seq(5, 60, 1), 200)

# For quick testing, uncomment:
# history_windows <- history_windows[1:3]

# ============================================================
# SLURM array support
# ============================================================

# If this script is submitted as a SLURM array job, each array task
# estimates one history window.
slurm_id <- suppressWarnings(
  as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
)

if (!is.na(slurm_id)) {

  if (slurm_id < 1L || slurm_id > length(history_windows)) {
    stop("SLURM_ARRAY_TASK_ID is outside the range of history_windows.")
  }

  history_windows <- history_windows[slurm_id]
}

# ============================================================
# Main estimation loop
# ============================================================

invisible(
  lapply(history_windows, function(i) {

    # i <- history_windows[1]
    # Output file for this history window
    output_file_county <- file.path(
      output_directory,
      paste0(
        "county_precipitation_trends_",
        stringr::str_pad(i, width = 3, pad = "0"),
        ".rds"
      )
    )

    # Select years included in the current rolling window
    yrs <- (current_year - i + 1):current_year

    dtw <- data[
      commodity_year %in% yrs
    ]

    if (nrow(dtw) == 0L) {
      stop("No data for window length i = ", i)
    }

    # Define linear trend within the selected history window.
    # The first year in each window is coded as zero.
    dtw[
      ,
      trend := commodity_year - min(commodity_year, na.rm = TRUE)
    ]

    # Estimate three panel specifications
    estimators <- c("within", "random", "ht", "between", "pooling", "fd")
    # estimators <- c("pooling")
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

    # Add history window identifier
    res[
      ,
      history_range := i
    ]

    # ========================================================
    # Classify county trends by statistical significance
    # ========================================================

    # 1% significance level
    res[
      ,
      trend_class01 := fifelse(
        is.na(p_value),
        NA_character_,
        fifelse(
          p_value > 0.01,
          "None",
          fifelse(
            estimate > 0,
            "Positive",
            fifelse(estimate < 0, "Negative", "None")
          )
        )
      )
    ]

    # 5% significance level
    res[
      ,
      trend_class05 := fifelse(
        is.na(p_value),
        NA_character_,
        fifelse(
          p_value > 0.05,
          "None",
          fifelse(
            estimate > 0,
            "Positive",
            fifelse(estimate < 0, "Negative", "None")
          )
        )
      )
    ]

    # 10% significance level
    res[
      ,
      trend_class10 := fifelse(
        is.na(p_value),
        NA_character_,
        fifelse(
          p_value > 0.10,
          "None",
          fifelse(
            estimate > 0,
            "Positive",
            fifelse(estimate < 0, "Negative", "None")
          )
        )
      )
    ]

    # Save results for this history window
    saveRDS(res, output_file_county)

    message("Saved county linear results: ", output_file_county)

    invisible(TRUE)
  })
)






# #data <- data[state_code %in% "20"]
#
# prf_polygon <- get_official_prf_polygon("data/official_RMA_RI_grid_01.zip")
# prf_polygon <- prf_polygon[prf_polygon$grid_id %in% data$grid_id, ]
# # sp::plot(prf_polygon)
#
# prf_sf <- st_as_sf(prf_polygon)
# # Queen neighbors
# nb_queen <- spdep::poly2nb(prf_sf,queen = TRUE,row.names = prf_sf$grid_id)
# # Convert neighbors to spatial weights
# listw_queen <- spdep::nb2listw(nb_queen,style = "W",zero.policy = TRUE)
#
# listw_queen
#
#
#
#
#
#
#
# object <- fit_plm
#
# vcov_cgm_2way_plm <- function(object) {
#   Vi <- plm::vcovHC(object, method = "arellano", type = "HC1", cluster = "group")
#   Vt <- plm::vcovHC(object, method = "arellano", type = "HC1", cluster = "time")
#
#   idx  <- as.data.frame(plm::index(object))
#   pool <- idx[[1]]
#   year <- idx[[2]]
#   both <- interaction(pool, year, drop = TRUE)
#
#   object_both <- object
#   attr(object_both, "index")[[1]] <- both
#   Vw <- plm::vcovHC(object_both, method = "arellano", type = "HC1", cluster = "group")
#
#   Vi + Vt - Vw
#
#   plm::vcovDC(object)
# }
#
# evcov <- vcov_cgm_2way_plm()
#
#
# summary(fit_plm, vcov = function(x) vcovDC(x))
#
# summary(fit_plm, vcov = function(x) vcovHC(x,cluster="group"))
#
#
#
# fit <- splm::spml(
#   formula       = precipitation ~ trend:factor(county_fips),
#   data          = data,
#   index         = c("panelid","commodity_year"),
#   listw         = listw_queen,
#   model         = "within",
#   effect        = "individual",
#   lag           = TRUE,
#   spatial.error = "b",
#   initval       = "estimate")
#
# summary(fit)
