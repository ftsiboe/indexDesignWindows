###############################################################################
# Precipitation trend estimation (rolling history windows) with:
#  (i) queen-neighborhood pooling (spatial dependence)
#  (ii) FE at (grid_id × interval_name)
#  (iii) two-way clustered SEs (Cameron–Gelbach–Miller) after plm "within"
#
# Outputs (per window h):
#   - Grid-level:   data-raw/releases/precipitation_trend/grid_precipitation_trends_XXX.rds
#   - County-level: data-raw/releases/precipitation_trend/county_precipitation_trends_XXX.rds
###############################################################################

rm(list = ls(all = TRUE))
gc()

suppressPackageStartupMessages({
  library(data.table)
  library(tidyr)
  library(plm)
  library(lmtest)
  library(terra)
  library(rfcipPRF)
})

devtools::document()

# -----------------------
# Load inputs
# -----------------------
study_environment <- readRDS("data/study_environment.rds")

output_directory <- "data-raw/fastscratch/precipitation_trend"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

prf_grid_weights <- as.data.table(readRDS("data/prf_grid_weights.rds"))
cpc_data         <- readRDS("data/cpc_historic_precipitation.rds")

# Ensure data.table for safe subsetting
cpc_data <- as.data.table(cpc_data)

# for testing: subset to KS (state_code == 20) grids
# cpc_data <- cpc_data[
#   grid_id %in% prf_grid_weights[state_code %in% 20, grid_id]
# ]

prf_polygon <- rfcipPRF::get_official_prf_polygon()
# prf_polygon <- prf_polygon[prf_polygon$grid_id %in% cpc_data$grid_id, ]  # for testing

official_interval_names <- as.data.table(readRDS("data/official_interval_names.rds"))

# Keep only needed columns and deduplicate
official_interval_names <- unique(
  official_interval_names[, .(month_end_code, month_beg_code, interval_name)]
)

# Long format: stack begin/end month codes into a single "month" column
official_interval_names <- tidyr::gather(
  official_interval_names,
  key   = "name",
  value = "month",
  month_end_code, month_beg_code
)

# -----------------------
# Join CPC precip to interval mapping
# -----------------------
common_keys <- intersect(names(cpc_data), names(official_interval_names))

data <- as.data.table(cpc_data)[
  official_interval_names,
  on = common_keys,
  allow.cartesian = TRUE,
  nomatch = 0
]

# -----------------------
# Aggregate to grid × interval × year
# -----------------------
data <- data[
  , .(
    precipitation     = sum(precipitation, na.rm = TRUE),
    precipitation_rma = sum(precipitation_rma, na.rm = TRUE)
  ),
  by = c("commodity_year", "grid_id", "interval_name")
]

# Linear trend (0,1,2,...) relative to first year
data[, trend := commodity_year - min(commodity_year, na.rm = TRUE)]
current_year <- max(data$commodity_year, na.rm = TRUE)

# Cross-sectional unit for pooled-neighborhood estimation: grid × interval
data[, cs_id := paste(grid_id, interval_name, sep = "__")]

# -----------------------
# Build queen-neighborhood lookup from PRF polygons
# -----------------------
n   <- nrow(prf_polygon)
gid <- prf_polygon$grid_id

# Get adjacency as FROM–TO pairs (row indices of polygons)
adj_pairs <- terra::adjacent(prf_polygon, type = "queen", pairs = TRUE)
adj_pairs <- as.data.frame(adj_pairs)

# terra versions differ: columns might be from/to OR i/j
if (all(c("from", "to") %in% names(adj_pairs))) {
  from_col <- "from"; to_col <- "to"
} else if (all(c("i", "j") %in% names(adj_pairs))) {
  from_col <- "i"; to_col <- "j"
} else {
  stop("Unexpected columns from terra::adjacent(): ", paste(names(adj_pairs), collapse = ", "))
}

# Build neighbor index list: nb_idx[[i]] = polygon row indices adjacent to polygon i
nb_idx <- vector("list", n)
tmp <- split(adj_pairs[[to_col]], adj_pairs[[from_col]])
nb_idx[as.integer(names(tmp))] <- lapply(tmp, unique)

# Convert polygon-row indices -> grid_id neighbors; include self
nb_gids <- lapply(seq_len(n), function(i) {
  nbr_rows <- nb_idx[[i]]
  unique(c(gid[i], gid[nbr_rows]))
})
names(nb_gids) <- as.character(gid)

# Optional sanity check
# message("Example neighborhood size (incl self): ", length(nb_gids[[1]]))

# -----------------------
# Helper: two-way clustered VCOV (CGM) for plm object
#   V = V_group + V_time - V_intersection
# -----------------------
vcov_cgm_2way_plm <- function(object) {
  Vi <- plm::vcovHC(object, method = "arellano", type = "HC1", cluster = "group")
  Vt <- plm::vcovHC(object, method = "arellano", type = "HC1", cluster = "time")

  idx  <- as.data.frame(plm::index(object))
  pool <- idx[[1]]
  year <- idx[[2]]
  both <- interaction(pool, year, drop = TRUE)

  object_both <- object
  attr(object_both, "index")[[1]] <- both
  Vw <- plm::vcovHC(object_both, method = "arellano", type = "HC1", cluster = "group")

  Vi + Vt - Vw
}

# -----------------------
# Helper: estimate trend for one focal grid using focal+queen neighbors
# -----------------------
estimate_one_grid <- function(dt, focal_grid_id, nb_gids) {
  gset <- nb_gids[[as.character(focal_grid_id)]]
  if (is.null(gset) || length(gset) == 0L) return(NULL)

  d <- dt[grid_id %in% gset]

  # Guardrails
  if (nrow(d) < 10L) return(NULL)
  if (length(unique(d$commodity_year)) < 3L) return(NULL)
  if (length(unique(d$cs_id)) < 2L) return(NULL)

  obj <- plm::plm(
    precipitation ~ trend,
    data  = d,
    index = c("cs_id", "commodity_year"),
    model = "within"
  )

  V  <- vcov_cgm_2way_plm(obj)
  ct <- lmtest::coeftest(obj, vcov = V)

  list(
    estimate       = ct["trend", "Estimate"],
    standard_error = ct["trend", "Std. Error"],
    t_value        = ct["trend", "t value"],
    p_value        = ct["trend", "Pr(>|t|)"]
  )
}

# -----------------------
# Fit models by focal grid_id, for each history window
# -----------------------

# Rolling windows to fit (years of history)
history_windows <- c(seq(5, 60, 1), 200)
# history_windows <- history_windows[1:3]  # for testing

# SLURM array support: if SLURM_ARRAY_TASK_ID is set, run only that window
slurm_id <- suppressWarnings(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))
if (!is.na(slurm_id)) {
  history_windows <- history_windows[slurm_id]
}

invisible(
  lapply(history_windows, function(i) {
    #tryCatch({

      output_file_county <- file.path(
        output_directory,
        paste0("county_precipitation_trends_", stringr::str_pad(i, width = 3, pad = "0"), ".rds")
      )
      output_file_grid <- file.path(
        output_directory,
        paste0("grid_precipitation_trends_", stringr::str_pad(i, width = 3, pad = "0"), ".rds")
      )

      yrs <- (current_year - i + 1):current_year
      dtw <- data[commodity_year %in% yrs]
      if (nrow(dtw) == 0L) stop("No data for window length i = ", i)

      # Grid-level estimation (each grid_id uses pooled sample: focal + queen neighbors)
      res <- dtw[
        ,
        {
          out_i <- tryCatch(
            estimate_one_grid(dtw, focal_grid_id = grid_id[1], nb_gids = nb_gids),
            error = function(e) NULL
          )
          if (is.null(out_i)) {
            list(
              estimate = NA_real_,
              standard_error = NA_real_,
              t_value = NA_real_,
              p_value = NA_real_
            )
          } else {
            out_i
          }
        },
        by = .(grid_id)
      ]
      res[, history_range := i]

      # Merge PRF grid weights (must include county id + weight)
      join_keys <- intersect(names(res), names(prf_grid_weights))
      res <- res[prf_grid_weights, on = join_keys, nomatch = 0]

      # Grid classification (optional)
      res[
        ,
        trend_class := fifelse(
          is.na(p_value), NA_character_,
          fifelse(p_value > 0.05, "None",
                  fifelse(estimate > 0, "Positive",
                          fifelse(estimate < 0, "Negative", "None")))
        )
      ]

      # Save grid results
      saveRDS(res, output_file_grid)
      message("Saved grid results: ", output_file_grid)

      # -----------------------
      # County aggregation (weighted averages by potential PRF acres)
      # -----------------------
      weight_col <- "potential_range_pasture"
      county_col <- "county_fips"

      if (!(weight_col %in% names(res))) stop("Missing weight column: ", weight_col)
      if (!(county_col %in% names(res))) stop("Missing county column: ", county_col)

      county_res <- res[
        !is.na(get(weight_col)) & get(weight_col) > 0,
        .(
          estimate_w = weighted.mean(estimate, w = get(weight_col), na.rm = TRUE),
          p_value_w  = weighted.mean(p_value,  w = get(weight_col), na.rm = TRUE),
          weight_sum = sum(get(weight_col), na.rm = TRUE),
          n_grids    = uniqueN(grid_id)
        ),
        by = c(county_col, "history_range")
      ]

      county_res[
        ,
        trend_class := fifelse(
          is.na(p_value_w), NA_character_,
          fifelse(p_value_w > 0.05, "None",
                  fifelse(estimate_w > 0, "Positive",
                          fifelse(estimate_w < 0, "Negative", "None")))
        )
      ]

      # Save county results
      saveRDS(county_res, output_file_county)
      message("Saved county results: ", output_file_county)

      invisible(TRUE)
    #}, error = function(e) {message("Window ", i, " failed: ", conditionMessage(e))})
  })
)

# -----------------------
# Upload assets
# -----------------------
function(){

  grid_precipitation <- data.table::rbindlist(
    lapply(
      list.files("data-raw/fastscratch/precipitation_trend",full.names = TRUE,pattern = "grid_precipitation"),
      function(i){
        tryCatch({
          readRDS(i)
        }, error = function(e){NULL})
      }),fill = TRUE)
  saveRDS(grid_precipitation,"data-raw/output/grid_precipitation_trend.rds")

  county_precipitation <- data.table::rbindlist(
    lapply(
      list.files("data-raw/fastscratch/precipitation_trend",full.names = TRUE,pattern = "county_precipitation"),
      function(i){
        tryCatch({
          readRDS(i)
        }, error = function(e){NULL})
      }),fill = TRUE)
  saveRDS(county_precipitation,"data-raw/output/county_precipitation_trend.rds")

  # if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)
  #
  # piggyback::pb_release_create(
  #   repo = "ftsiboe/indexDesignWindows",
  #   tag  = "precipitation_trend",
  #   name = "PRF Grid and County Precipitation Trend Estimates",
  #   body = paste(
  #     "This release contains grid-level and county-level precipitation trend estimates",
  #     "for PRF grids using a spatially pooled (queen contiguity) fixed-effects panel framework.",
  #     "",
  #     "Key features:",
  #     "- Grid-level trend estimates using pooled focal + neighboring grids.",
  #     "- Fixed effects at the grid × interval level.",
  #     "- Two-way clustered standard errors (grid×interval and year).",
  #     "- Rolling historical windows (5–60 years and full sample).",
  #     "- County-level aggregation using PRF acreage weights.",
  #     "- County classification: Positive, Negative, or No statistically significant trend (p ≤ 0.05).",
  #     "",
  #     "Outputs include:",
  #     "- grid_precipitation_trends_XXX.rds",
  #     "- county_precipitation_trends_XXX.rds",
  #     "",
  #     "These files support evaluation of alternative PRF index design windows and spatial exposure sensitivity.",
  #     sep = "\n"
  #   )
  # )
  #
  # piggyback::pb_upload(
  #   list.files(output_directory, full.names = TRUE, recursive = T),
  #   repo  = "ftsiboe/indexDesignWindows",
  #   tag   = "precipitation_trend",
  #   overwrite = TRUE
  # )
}




