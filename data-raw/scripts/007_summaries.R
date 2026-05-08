#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE));library(data.table);gc()
devtools::document()
study_environment <- readRDS("data/study_environment.rds")
# output_directory <- "data-raw/releases/summary"
# if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

alternative_directory <- "data-raw/releases/alternative"

precipitation_trend <- readRDS("data-raw/output/county_precipitation_trend.rds")
precipitation_trend <- precipitation_trend[history_range %in% 200]
precipitation_trend <- precipitation_trend[model_estimator %in% "pooling"]
Keep.List<-c("Keep.List",ls())
#-------------------------------------------------------------------------------
# Statistical summary                                                        ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

xlist <- c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
           "pvalue_pearson_cor","pvalue_kendall_cor")

data_stats <- list.files(alternative_directory,full.names = TRUE, pattern = "statistical_analysis")

data_stats <- data.table::rbindlist(
  lapply(
    data_stats[!grepl("200",data_stats)],
    function(i) {
      tryCatch({
        # i <- data_stats[1]
        data <- as.data.frame(readRDS(i))

        data <- data[c(xlist,"index_history_range","state_code", "county_code","county_fips","coverage_level","y_level")]

        # data <- data[data$y_level %in% c("alternative_index_adj01" ,"alternative_base_rate_adj03" ,"alternative_payment_factor_adj01"),]

        data$variable <- ifelse(grepl("_index",data$y_level),"Index",NA)

        data$variable <- ifelse(grepl("_base_rate_",data$y_level),
                                paste0("Base rate\nat ",data$coverage_level,"%"),
                                data$variable)

        data$variable <- ifelse(grepl("_payment_factor_",data$y_level),
                                paste0("Payment\nrate at ",data$coverage_level,"%"),
                                data$variable)

        # data <- data[
        #   data$coverage_level %in% 90 |grepl("_index",data$y_level), names(data)
        # ]

        data <- data |>
          tidyr::gather(statistic, value, xlist)
        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data_stats$statistic_name <- factor(
  data_stats$statistic,
  levels = c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
             "pvalue_pearson_cor","pvalue_kendall_cor"),
  labels = c("Mean equality\nt-test",
             "F-test of equal\nvariances",
             "Kruskal-Wallis test\nof equal variances",
             "Kolmogorov-Smirnov\ntest",
             "Pearson\ncorrelation",
             "Kendall's\ntau"))

data_stats <- add_break_categories(
  data = data_stats,
  variable = "value",
  break_levels = c(-Inf,0.10,Inf),
  break_labels = c("Less than 0.10","Greater than 0.10"))

data_stats <- data_stats[!data_stats$value_cat %in% NA,]

variable_list <- unique(data_stats$variable)
variable_list <- variable_list[order(variable_list)]
data_stats$variable <- factor(
  data_stats$variable,
  levels =  c("Index",variable_list[! variable_list %in% "Index"]))

specifications <- c(
  "alternative_index"                = "Unadjusted",
  "alternative_index_adj01"          = "Adjusted for RMA Index Discretion" ,
  "alternative_base_rate_adj00"      = "Unadjusted",
  "alternative_base_rate_adj01"      = "Adjusted for RMA Rate Discretion",
  "alternative_base_rate_adj02"      = "Adjusted for RMA Index Discretion",
  "alternative_base_rate_adj03"      = "Adjusted for RMA Index and Rate Discretion",
  "alternative_payment_factor_adj00" = "Unadjusted",
  "alternative_payment_factor_adj01" = "Adjusted for RMA Index Discretion")

data_stats$specification <- factor(
  data_stats$y_level,
  levels = names(specifications),
  labels = specifications)

data_stats <- as.data.table(data_stats)

res_disaag <- data.table::rbindlist(
  lapply(
    names(precipitation_trend)[grepl("_class_",names(precipitation_trend))],
    function(nm) {
      tryCatch({

        # nm <- "mean_trend_class_01"

        df <- merge(
          x  = data_stats, y = precipitation_trend[!get(nm) %in% NA],
          by = "county_fips", all = FALSE)[
            ,disaggregate_level := get(nm)][
              statistic %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
              .(
                n = .N
              ),
              by = .(index_history_range, variable, statistic_name, value_cat, y_level, specification,disaggregate_level)
            ][
              , prop := n / sum(n),
              by = .(index_history_range, variable, statistic_name, y_level, specification,disaggregate_level)
            ][,disaggregate := nm]

        df

      }, error = function(e){NULL})
    }),fill = TRUE)

res_state <- data_stats[
  statistic %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
  .(
    n = .N
  ),
  by = .(index_history_range, variable, statistic_name, value_cat, y_level, specification,state_code)
][
  , prop := n / sum(n),
  by = .(index_history_range, variable, statistic_name, y_level, specification,state_code)
]

res_state[, disaggregate := "state_code"]
res_state[, disaggregate_level := state_code]
res_state[, state_code := NULL]

res_full <- data_stats[
  statistic %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
  .(
    n = .N
  ),
  by = .(index_history_range, variable, statistic_name, value_cat, y_level, specification)
][
  , prop := n / sum(n),
  by = .(index_history_range, variable, statistic_name, y_level, specification)
]
res_full[, disaggregate := "full"]
res_full[, disaggregate_level := "full"]

res_full <- rbind(res_full,res_disaag,res_state)

res_full[,history_years := as.numeric(gsub("[^0-9]", "", index_history_range))]


res_full[
  ,
  group_variable := do.call(paste, c(.SD, sep = "_xx_")),
  .SDcols = c("variable", "statistic_name", "y_level", "specification","disaggregate","disaggregate_level")]

tp_final <- estimate_turning_point_sd(
  data          = res_full[history_years >= 20],
  outcome       = "prop",
  x_var         = "history_years",
  group_var     = "group_variable",
  direction     = "min",
  sd_multiplier = 1
)

# tp_final <- estimate_final_turning_point(
#   data              = res_full[history_years >= 20],
#   outcome           = "prop",
#   group_var         = "group_variable",
#   x_var             = "history_years",
#   benchmark_x       = 60,
#   tolerance_grid    = seq(0.01, 0.15, by = 0.005),
#   consecutive_years = 1,
#   penalty_weight    = 0.01)

res_full <- merge(
  res_full,
  tp_final[, c("group_variable","turning_point"), with = FALSE],
  by = "group_variable", all.x = TRUE)[, group_variable:= NULL]

hist(res_full$turning_point)
saveRDS(as.data.table(res_full),file.path(alternative_directory,"summary_statistical.rds"))

#-------------------------------------------------------------------------------
# Actuarial summary                                                          ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
data_actuarial <- list.files(alternative_directory,full.names = TRUE, pattern = "insurance_analysis")

data_actuarial <- data.table::rbindlist(
  lapply(
    data_actuarial[!grepl("200",data_actuarial)],
    function(i) {
      tryCatch({
        # i <- list.files(alternative_directory,full.names = TRUE, pattern = "insurance_analysis")[2]
        data <- readRDS(i)
        data <- data[!baseline_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!alternative_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!baseline_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]
        data <- data[!alternative_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]

        data_disaag <- data.table::rbindlist(
          lapply(
            names(precipitation_trend)[grepl("_class_",names(precipitation_trend))],
            function(nm) {
              tryCatch({
                merge(
                  x  = data, y = precipitation_trend[!mean_trend_class_05 %in% NA],
                  by = "county_fips", all = FALSE)[
                    ,disaggregate_level := get(nm)][
                      , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
                      by = c("index_history_range","adjustment","disaggregate_level"),
                      .SDcols = c("alternative_indemnity_amount","alternative_total_premium_amount",
                                  "baseline_indemnity_amount","baseline_total_premium_amount" )][
                                    ,disaggregate := nm]
              }, error = function(e){NULL})
            }),fill = TRUE)

        data_state <- data[
          , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment","state_code"),
          .SDcols = c("alternative_indemnity_amount","alternative_total_premium_amount",
                      "baseline_indemnity_amount","baseline_total_premium_amount" )]
        data_state[, disaggregate := "state_code"]
        data_state[, disaggregate_level := state_code]
        data_state[, state_code := NULL]

        data <- data[
          , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment"),
          .SDcols = c("alternative_indemnity_amount","alternative_total_premium_amount",
                      "baseline_indemnity_amount","baseline_total_premium_amount" )]
        data[, disaggregate := "full"]
        data[, disaggregate_level := "full"]

        data <- rbind(data,data_disaag,data_state)

        data[,alternative_lr  := alternative_indemnity_amount/alternative_total_premium_amount]
        data[,baseline_lr     := baseline_indemnity_amount/baseline_total_premium_amount]
        data[,actuarial_index := (abs(alternative_lr-1) - abs(baseline_lr-1))*100]

        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data_actuarial[,history_years := as.numeric(gsub("[^0-9]", "", index_history_range))]

data_actuarial[
  ,adjustmentCat := factor(
    adjustment,
    levels = 0:3,
    labels = c(
      "Unadjusted",
      "Adjusted for RMA Rate Discretion",
      "Adjusted for RMA Index Discretion",
      "Adjusted for RMA Index and Rate Discretion") )
]

data_actuarial[
  ,
  group_variable := do.call(paste, c(.SD, sep = "_xx_")),
  .SDcols = c("adjustment","disaggregate","disaggregate_level")]

tp_final <- estimate_final_turning_point(
  data              = data_actuarial[history_years >= 20],,
  outcome           = "actuarial_index",
  group_var         = "group_variable",
  x_var             = "history_years",
  benchmark_x       = 60,
  tolerance_grid    = seq(0.01, 0.15, by = 0.005),
  consecutive_years = 1,
  penalty_weight    = 0.01)

data_actuarial <- merge(
  data_actuarial,
  tp_final[, c("group_variable","turning_point"), with = FALSE],
  by = "group_variable", all.x = TRUE)[, group_variable:= NULL]

saveRDS(as.data.table(data_actuarial),file.path(alternative_directory,"summary_actuarial.rds"))

#-------------------------------------------------------------------------------
# Economic summary                                                           ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()

data <- list.files(alternative_directory,full.names = TRUE, pattern = "insurance_analysis")

data <- data.table::rbindlist(
  lapply(
    data[!grepl("200",data)],
    function(i) {
      tryCatch({
        # i <- list.files(alternative_directory,full.names = TRUE, pattern = "insurance_analysis")[2]
        data <- readRDS(i)
        data <- data[!baseline_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!alternative_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!baseline_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]
        data <- data[!alternative_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]

        data_disaag <- data.table::rbindlist(
          lapply(
            names(precipitation_trend)[grepl("_class_",names(precipitation_trend))],
            function(nm) {
              tryCatch({
                merge(
                  x  = data, y = precipitation_trend[!mean_trend_class_05 %in% NA],
                  by = "county_fips", all = FALSE)[
                    ,disaggregate_level := get(nm)][
                      , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
                      by = c("index_history_range","adjustment","disaggregate_level"),
                      .SDcols = c( names(data)[grepl("ceded_|retained_|indifferent_",names(data))])][
                                    ,disaggregate := nm]
              }, error = function(e){NULL})
            }),fill = TRUE)

        data_state <- data[
          , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment","state_code"),
          .SDcols = c( names(data)[grepl("ceded_|retained_|indifferent_",names(data))])]
        data_state[, disaggregate := "state_code"]
        data_state[, disaggregate_level := state_code]
        data_state[, state_code := NULL]

        data <- data[
          , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment"),
          .SDcols = c( names(data)[grepl("ceded_|retained_|indifferent_",names(data))])]
        data[, disaggregate := "full"]
        data[, disaggregate_level := "full"]

        data <- rbind(data,data_disaag,data_state)

        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data[,history_years := as.numeric(gsub("[^0-9]", "", index_history_range))]

data[,economic_indemnity := (ceded_indemnity)/
       (retained_indemnity+indifferent_indemnity)]

data[,ceded_lr    := (ceded_indemnity)/(ceded_premium)]
data[,retained_lr := (indifferent_indemnity+retained_indemnity)/(indifferent_premium+retained_premium)]
data[,economic_lr := ceded_lr/retained_lr]

data[
  ,adjustmentCat := factor(
    adjustment,
    levels = 0:3,
    labels = c("Unadjusted",
               "Adjusted for RMA Rate Discretion",
               "Adjusted for RMA Index Discretion",
               "Adjusted for RMA Index and Rate Discretion") )
]

data[
  ,
  group_variable := do.call(paste, c(.SD, sep = "_xx_")),
  .SDcols = c("adjustment","disaggregate","disaggregate_level")]

tp_final <- estimate_final_turning_point(
  data              = data[history_years >= 20],
  outcome           = "economic_lr",
  group_var         = "group_variable",
  x_var             = "history_years",
  benchmark_x       = 60,
  tolerance_grid    = seq(0.01, 0.15, by = 0.005),
  consecutive_years = 1,
  penalty_weight    = 0.01)

data <- merge(
  data,
  tp_final[, c("group_variable","turning_point"), with = FALSE],
  by = "group_variable", all.x = TRUE)[, group_variable:= NULL]

saveRDS(as.data.table(data),file.path(alternative_directory,"summary_economic.rds"))

#-------------------------------------------------------------------------------

piggyback::pb_upload(
  list.files(alternative_directory,full.names = TRUE, pattern = "summary_"),
  repo  = "ftsiboe/indexDesignWindows",
  tag   = "alternative",
  overwrite = TRUE
)

