

rm(list = ls(all = TRUE))
gc()

suppressPackageStartupMessages({
  library(data.table)
  library(tidyr)
  library(plm)
  library(lmtest)
})

devtools::document()

study_environment <- readRDS("data/study_environment.rds")

output_directory <- "data-raw/fastscratch/precipitation_trend"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

prf_grid_weights <- as.data.table(readRDS("data/prf_grid_weights.rds"))
cpc_data         <- readRDS("data/cpc_historic_precipitation.rds")

# Ensure data.table for safe subsetting
cpc_data <- as.data.table(cpc_data)

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
data <- as.data.table(cpc_data)[
  official_interval_names,
  on = intersect(names(cpc_data), names(official_interval_names)),
  allow.cartesian = TRUE,
  nomatch = 0
]

data <- data[
  , .(
    precipitation = sum(precipitation, na.rm = TRUE),
    precipitation_rma = sum(precipitation_rma, na.rm = TRUE)
  ),
  by = c("commodity_year", "grid_id", "interval_name")
]
data[,county_fips := as.character(factor(grid_id,prf_grid_weights$grid_id,prf_grid_weights$county_fips))]
data <- tidyr::separate(data,"county_fips",into=c("state_code","county_code"),sep=2,remove=FALSE)
data <- as.data.table(data)

data[, panelid := paste(grid_id, interval_name, sep = "_")]
data[, trend := commodity_year - min(commodity_year, na.rm = TRUE)]
current_year <- max(data$commodity_year, na.rm = TRUE)

# data <- data[state_code %in% unique(data$state_code)[1:3]]

# -----------------------
# Helper
# -----------------------
estimate_one_state <- function(dt, formula, model = "within") {

  # Guardrails
  if (nrow(dt) < 10L) return(NULL)
  if (length(unique(dt$commodity_year)) < 3L) return(NULL)
  if (length(unique(dt$panelid)) < 2L) return(NULL)

  obj <- plm::plm(
    formula = formula,
    data    = dt,
    index   = c("panelid", "commodity_year"),
    model   = model
  )

  V  <- plm::vcovDC(obj)
  ct <- lmtest::coeftest(obj, vcov = V)

  data.table::data.table(
    county_fips    = gsub("trend:factor[(]county_fips[)]","",rownames(ct)),
    estimate       = ct[, "Estimate"],
    standard_error = ct[, "Std. Error"],
    t_value        = ct[, "t value"],
    p_value        = ct[, "Pr(>|t|)"]
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

    # i <- history_windows[1]
    output_file_county <- file.path(
      output_directory,
      paste0("county_precipitation_trends_", stringr::str_pad(i, width = 3, pad = "0"), ".rds")
    )

    yrs <- (current_year - i + 1):current_year
    dtw <- data[commodity_year %in% yrs]
    if (nrow(dtw) == 0L) stop("No data for window length i = ", i)

    # Grid-level estimation (each grid_id uses pooled sample: focal + queen neighbors)

    #
    res <- data.table::rbindlist(
      lapply(
        c("within", "random", "ht", "between", "pooling", "fd"), # "within", "random",
        function(estimator){
          tryCatch({
            # estimator <- "pooling"
            res <- dtw[
              ,{out_i <- tryCatch(
                estimate_one_state(
                  dt      = .SD,
                  formula = precipitation ~ trend:factor(county_fips),
                  model   = estimator
                ),
                error = function(e) NULL
              )
              if(is.null(out_i)) {
                list(county_fips=NA_character_ ,estimate = NA_real_,standard_error = NA_real_,t_value = NA_real_,p_value = NA_real_)
              } else {out_i}},by = .(state_code)]

            res[, model_estimator := estimator]
            res
          }, error = function(e){NULL})
        }),fill = TRUE);gc()

    res[, history_range := i]

    # classification (optional)
    res[
      ,
      trend_class01 := fifelse(
        is.na(p_value), NA_character_,
        fifelse(p_value > 0.01, "None",
                fifelse(estimate > 0, "Positive",
                        fifelse(estimate < 0, "Negative", "None")))
      )
    ]

    res[
      ,
      trend_class05 := fifelse(
        is.na(p_value), NA_character_,
        fifelse(p_value > 0.05, "None",
                fifelse(estimate > 0, "Positive",
                        fifelse(estimate < 0, "Negative", "None")))
      )
    ]

    res[
      ,
      trend_class10 := fifelse(
        is.na(p_value), NA_character_,
        fifelse(p_value > 0.10, "None",
                fifelse(estimate > 0, "Positive",
                        fifelse(estimate < 0, "Negative", "None")))
      )
    ]

    saveRDS(res, output_file_county)
    message("Saved county results: ", output_file_county)

    invisible(TRUE)
    #}, error = function(e) {message("Window ", i, " failed: ", conditionMessage(e))})
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
