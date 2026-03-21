#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE));library(data.table);library(future.apply);library(rfcipPRF);gc()
devtools::document()
study_environment <- readRDS("data/study_environment.rds")
myline <- paste0(rep("---",5),collapse = "")
statistics_list <- c(
  "mean" = " a) Mean ",
  "q50"  = " b) Median ",
  "var"  = " c) Variance ",
  "cv"   = " d) Coefficient of variation ",
  "q10"  = " e) 10th percentile ",
  "q90"  = " f) 90th percentile "
)
output_directory <- "data-raw/releases/alternative"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
redesigns_directory  <- study_environment$wd$redesigns
prf_grid_weights <- readRDS("data/prf_grid_weights.rds")
xlist <- c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
           "pvalue_pearson_cor","pvalue_kendall_cor")


colour_palt <- c(
  "#B83E27",
  "#FFC425",
  "#9DD9F7",
  "#00583D"
)
Keep.List<-c("Keep.List",ls())
#-------------------------------------------------------------------------------
# Plot - statistical analysis - area                                         ####

data_stats <- list.files(output_directory,full.names = TRUE, pattern = "statistical_analysis")

data_stats <- data.table::rbindlist(
  lapply(
    data_stats[!grepl("200",data_stats)],
    function(i) {
      tryCatch({
        # i <- data_stats[1]
        data <- as.data.frame(readRDS(i))

        data <- data[c(xlist,"index_history_range","state_code", "county_code","county_fips","coverage_level","y_level")]

        # data <- data[data$y_level %in% c("alternative_index_adj01" ,"alternative_base_rate_adj03" ,"alternative_payment_factor_adj01"),]

        data$disaggregate <- ifelse(grepl("_index",data$y_level),"Index",NA)

        data$disaggregate <- ifelse(grepl("_base_rate_",data$y_level),
                                    paste0("Base rate\nat ",data$coverage_level,"%"),
                                    data$disaggregate)

        data$disaggregate <- ifelse(grepl("_payment_factor_",data$y_level),
                                    paste0("Payment\nrate at ",data$coverage_level,"%"),
                                    data$disaggregate)

        # data <- data[
        #   data$coverage_level %in% 90 |grepl("_index",data$y_level), names(data)
        # ]

        data <- data |>
          tidyr::gather(variable, value, xlist)
        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data_stats$variable_name <- factor(
  data_stats$variable,
  levels = c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
             "pvalue_pearson_cor","pvalue_kendall_cor"),
  labels = c("Mean equality\nt-test",
             "F-test of equal\nvariances",
             "Kruskal-Wallis test\nof equal variances",
             "Kolmogorov-Smirnov\ntest",
             "Pearson\ncorrelation",
             "Kendall's\ntau")
)

data_stats <- add_break_categories(
  data = data_stats,
  variable = "value",
  break_levels = c(-Inf,0.01,0.05,0.10,0.50,Inf),
  break_labels = c("Less than 0.01","0.01 to 0.05","0.05 to 0.10","0.10 to 0.50","Greater than 0.50"))

data_stats <- data_stats[!data_stats$value_cat %in% NA,]

disaggregate_list <- unique(data_stats$disaggregate)
disaggregate_list <- disaggregate_list[order(disaggregate_list)]
data_stats$disaggregate <- factor(
  data_stats$disaggregate,
 levels =  c("Index",disaggregate_list[! disaggregate_list %in% "Index"]))

specifications <- c(
  "alternative_index"                = "CPC-Raw-Index\n(CPC-RI)",
  "alternative_index_adj01"          = "CPC-Adjusted-Index\n(CPC-AI) ††" ,
  "alternative_base_rate_adj00"      = "Base Rate at 90%\nBased on CPC-RI",
  "alternative_base_rate_adj01"      = "Base Rate at 90%\nBased on CPC-RI and\nAdjusted for RMA Rate Discretion",
  "alternative_base_rate_adj02"      = "Base Rate at 90%\nBased on CPC-AI",
  "alternative_base_rate_adj03"      = "Base Rate at 90%\nBased on CPC-AI and\nAdjusted for RMA Rate Discretion ††",
  "alternative_payment_factor_adj00" = "Payment Factor at 90%\nBased on CPC-RI",
  "alternative_payment_factor_adj01" = "Payment Factor at 90%\nBased on CPC-AI ††")

data_stats$specification <- factor(
  data_stats$y_level,
  levels = names(specifications),
  labels = specifications)

data_stats <- as.data.table(data_stats)[
    variable %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
  .(
    n = .N
  ),
  by = .(index_history_range, disaggregate, variable_name, value_cat, y_level, specification)
][
  , prop := n / sum(n),
  by = .(index_history_range, disaggregate, variable_name, y_level, specification)
]

data_stats <- as.data.frame(data_stats)

data_stats$history_range <- as.numeric(gsub("[^0-9]","",data_stats$index_history_range))

fig_stats <- ggplot(
  data_stats[
    data_stats$y_level %in%
      c("alternative_index_adj01" ,
        "alternative_base_rate_adj03" ,"alternative_payment_factor_adj01"),],
  aes(
    x     = history_range,
    y     = prop,
    fill  = value_cat,
    group = value_cat
  )
) +
  geom_area(
    position = "fill",
    alpha = 0.95
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Proportion of counties with specified p-value range",
    x = "",
    y = "Proportion"
  ) +
  scale_fill_manual(
    values = c(
      "#00583D",
      "#A0BD78",
      "#BED73B",
      "#FFC425",
      "#B83E27"
    ),
    na.value = "white"
  ) +
  facet_grid(disaggregate  ~ variable_name) +
  ers_theme() +
  theme(
    plot.caption      = element_blank(),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.y       = element_text(size = 8),
    axis.text.x       = element_text(size = 8, color = "black"),
    legend.position   = "top",
    legend.key.size   = unit(0.5, "cm"),
    legend.background = element_blank(),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 10),
    strip.text        = element_text(size = 10),
    strip.background  = element_blank()
  )

ggsave(file.path("data-raw/output/figure","statistical_threshold_balance_proportion.png"),
       fig_stats, dpi = 600,width = 8, height =11)


#-------------------------------------------------------------------------------
# Plot - statistical analysis - point                                        ####

data_stats <- list.files(output_directory,full.names = TRUE, pattern = "statistical_analysis")

data_stats <- data.table::rbindlist(
  lapply(
    data_stats[!grepl("200",data_stats)],
    function(i) {
      tryCatch({
        # i <- data_stats[1]
        data <- as.data.frame(readRDS(i))

        data <- data[c(xlist,"index_history_range","state_code", "county_code","county_fips","coverage_level","y_level")]

        # data <- data[data$y_level %in% c("alternative_index_adj01" ,"alternative_base_rate_adj03" ,"alternative_payment_factor_adj01"),]

        data$disaggregate <- ifelse(grepl("_index",data$y_level),"Index",NA)

        data$disaggregate <- ifelse(grepl("_base_rate_",data$y_level),
                                    paste0("Base rate\nat ",data$coverage_level,"%"),
                                    data$disaggregate)

        data$disaggregate <- ifelse(grepl("_payment_factor_",data$y_level),
                                    paste0("Payment\nrate at ",data$coverage_level,"%"),
                                    data$disaggregate)

        # data <- data[
        #   data$coverage_level %in% 90 |grepl("_index",data$y_level), names(data)
        # ]

        data <- data |>
          tidyr::gather(variable, value, xlist)
        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data_stats$variable_name <- factor(
  data_stats$variable,
  levels = c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
             "pvalue_pearson_cor","pvalue_kendall_cor"),
  labels = c("Mean equality\nt-test",
             "F-test of equal\nvariances",
             "Kruskal-Wallis test\nof equal variances",
             "Kolmogorov-Smirnov\ntest",
             "Pearson\ncorrelation",
             "Kendall's\ntau")
)

data_stats <- add_break_categories(
  data = data_stats,
  variable = "value",
  break_levels = c(-Inf,0.10,Inf),
  break_labels = c("Less than 0.10","Greater than 0.10"))

data_stats <- data_stats[!data_stats$value_cat %in% NA,]

disaggregate_list <- unique(data_stats$disaggregate)
disaggregate_list <- disaggregate_list[order(disaggregate_list)]
data_stats$disaggregate <- factor(
  data_stats$disaggregate,
  levels =  c("Index",disaggregate_list[! disaggregate_list %in% "Index"]))

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

data_stats <- as.data.table(data_stats)[
  variable %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
  .(
    n = .N
  ),
  by = .(index_history_range, disaggregate, variable_name, value_cat, y_level, specification)
][
  , prop := n / sum(n),
  by = .(index_history_range, disaggregate, variable_name, y_level, specification)
]

data_stats <- as.data.frame(data_stats)

data_stats$history_range <- as.numeric(gsub("[^0-9]","",data_stats$index_history_range))

fig_stats <- ggplot(
  data_stats[data_stats$value_cat %in%c("Less than 0.10") &
               data_stats$history_range >= 10,],
  aes(
    x     = history_range,
    y     = prop*100,
    fill  = specification,
    colour = specification,
    shape = specification,
    group = specification
  )
) +
  geom_point(size=1) +
  geom_vline(xintercept = 40, linetype = "dashed", color="gray")+
  facet_grid(variable_name~disaggregate, scales = "free_y") +
  labs(
    x = "\nNumber of years of historical precipitation used in index design",
    y = "Percentage of counties with specified p-value less than 0.10\n"
  ) +
  scale_colour_manual(values = colour_palt) +
  scale_fill_manual(values = colour_palt) +
  ers_theme() +
  theme(
    plot.caption      = element_blank(),
    axis.title.y      = element_text(size = 8),
    axis.title.x      = element_text(size = 8),
    axis.text.y       = element_text(size = 6),
    axis.text.x       = element_text(size = 6, color = "black"),
    legend.position   = "top",
    legend.background = element_blank(),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 7),
    strip.text        = element_text(size = 7),
    strip.background  = element_blank()
  )

ggsave(file.path("data-raw/output/figure","threshold_statistical.png"),
       fig_stats, dpi = 600,width = 11, height =5.5)

#-------------------------------------------------------------------------------
# Plot - actuarial  analysis                                                 ####

data_actuarial <- list.files(output_directory,full.names = TRUE, pattern = "insurance_analysis")

data_actuarial <- data.table::rbindlist(
  lapply(
    data_actuarial[!grepl("200",data_actuarial)],
    function(i) {
      tryCatch({
        # i <- list.files(output_directory,full.names = TRUE, pattern = "insurance_analysis")[2]
        data <- readRDS(i)
        data <- data[!baseline_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!alternative_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!baseline_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]
        data <- data[!alternative_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]

        data <- data[
          , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment"),
          .SDcols = c("alternative_indemnity_amount","alternative_total_premium_amount",
                      "baseline_indemnity_amount","baseline_total_premium_amount" )]

        data[,alternative_lr  := alternative_indemnity_amount/alternative_total_premium_amount]
        data[,baseline_lr     := baseline_indemnity_amount/baseline_total_premium_amount]
        data[,actuarial_index := (abs(alternative_lr-1) - abs(baseline_lr-1))*100]

        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data_actuarial[,history_range := as.numeric(gsub("[^0-9]","",index_history_range))]
data_actuarial[
  ,adjustmentCat := factor(
    adjustment,
    levels = 0:3,
    labels = c("Unadjusted",
               "Adjusted for RMA Rate Discretion",
               "Adjusted for RMA Index Discretion",
               "Adjusted for RMA Index and Rate Discretion") )
]

fig_actuarial <- ggplot(
  data_actuarial[history_range >= 20],
  aes(
    x     = history_range,
    y     = actuarial_index,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) +
  geom_point(size=2) +
  #geom_vline(xintercept = 40, linetype = "dashed", color="gray")+
  labs(
    x = "\nNumber of years of historical precipitation used in index design",
    y = "Simulated difference in absolute deviation of loss ratio form 1 between full history\nand alternate history range (Measured in percentage points)\n"
  ) +
  scale_colour_manual(values = colour_palt) +
  scale_fill_manual(values = colour_palt) +
  ers_theme() +
  theme(
    plot.caption      = element_blank(),
    axis.title.y      = element_text(size = 11),
    axis.title.x      = element_text(size = 11),
    axis.text.y       = element_text(size = 9),
    axis.text.x       = element_text(size = 9, color = "black"),
    legend.position=c(0.75,0.80),
    legend.background = element_blank(),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 9),
    strip.text        = element_text(size = 9),
    strip.background  = element_blank()
  )

ggsave(file.path("data-raw/output/figure","threshold_actuarial.png"),
       fig_actuarial, dpi = 600,width = 8, height =6)

#-------------------------------------------------------------------------------
# Plot - economic  analysis                                                  ####

data <- list.files(output_directory,full.names = TRUE, pattern = "insurance_analysis")

data <- data.table::rbindlist(
  lapply(
    data[!grepl("200",data)],
    function(i) {
      tryCatch({
        # i <- list.files(output_directory,full.names = TRUE, pattern = "insurance_analysis")[2]
        data <- readRDS(i)
        data <- data[!baseline_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!alternative_total_premium_amount %in% c(0,NA,Inf,-Inf,NaN)]
        data <- data[!baseline_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]
        data <- data[!alternative_indemnity_amount %in% c(NA,Inf,-Inf,NaN)]

        data <- data[
          , lapply(.SD, function(x) sum(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment"),
          .SDcols = c( names(data)[grepl("ceded_|retained_|indifferent_",names(data))])]

        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data[,history_range := as.numeric(gsub("[^0-9]","",index_history_range))]


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

fig_actuarial <- ggplot(
  data[history_range >= 20],
  aes(
    x     = history_range,
    y     = economic_lr,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) +
  geom_point(size=2) +
  #geom_vline(xintercept = 40, linetype = "dashed", color="gray")+
  labs(
    x = "\nNumber of years of historical precipitation used in index design",
    y = "\n"
  ) +
  scale_colour_manual(values = colour_palt) +
  scale_fill_manual(values = colour_palt) +
  ers_theme() +
  theme(
    plot.caption      = element_blank(),
    axis.title.y      = element_text(size = 11),
    axis.title.x      = element_text(size = 11),
    axis.text.y       = element_text(size = 9),
    axis.text.x       = element_text(size = 9, color = "black"),
    legend.position=c(0.75,0.80),
    legend.background = element_blank(),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 9),
    strip.text        = element_text(size = 9),
    strip.background  = element_blank()
  )

ggsave(file.path("data-raw/output/figure","threshold_economic.png"),
       fig_actuarial, dpi = 600,width = 8, height =6)



ggplot(
  data,
  aes(
    x     = history_range,
    y     = economic_lr,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) + geom_point()




#-------------------------------------------------------------------------------
# Plot - Revenue  analysis                                                  ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- list.files(output_directory,full.names = TRUE, pattern = "revenue_analysis")

data <- data.table::rbindlist(
  lapply(
    data[!grepl("200",data)],
    function(i) {
      tryCatch({
        # i <- list.files(output_directory,full.names = TRUE, pattern = "revenue_analysis")[2]
        data <- readRDS(i)

        data <- data[
          ,.(mn_none = mean(yield, na.rm=T),
             sd_none = sd(yield, na.rm=T),
             ob_none = sum(!yield %in% c(NA,Inf,-Inf,NaN), na.rm=T),
             mn_base = mean(baseline_revenue, na.rm=T),
             sd_base = sd(baseline_revenue, na.rm=T),
             ob_base = sum(!baseline_revenue %in% c(NA,Inf,-Inf,NaN), na.rm=T),
             mn_altn = mean(alternative_revenue, na.rm=T),
             sd_altn = sd(alternative_revenue, na.rm=T),
             ob_altn = sum(!alternative_revenue %in% c(NA,Inf,-Inf,NaN), na.rm=T)),
          by=c("state_code", "county_code","county_fips","interval_code","coverage_level_percent",
               "index_history_range","adjustment")]

        data <- data[! ob_none %in%  c(NA,Inf,-Inf,NaN,0)]
        data <- data[! ob_base %in%  c(NA,Inf,-Inf,NaN,0)]
        data <- data[! ob_altn %in%  c(NA,Inf,-Inf,NaN,0)]
        data <- data[ob_none >= 4]
        data <- data[ob_base >= 4]
        data <- data[ob_altn >= 4]

        data[,cv_none := sd_none/mn_none]
        data[,cv_base := sd_none/mn_base]
        data[,cv_altn := sd_none/mn_altn]

        data <- data[, Relmean := mn_altn/mn_base]
        data <- data[, Relcv   := cv_altn/cv_base]

        data <- data[
          , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          by = c("index_history_range","adjustment","interval_code","coverage_level_percent"),
          .SDcols = c("Relmean","Relcv")]

        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data[,history_range := as.numeric(gsub("[^0-9]","",index_history_range))]
data[,adjustmentCat := as.factor(adjustment)]
data <- data[interval_code %in% NA]
data <- data[coverage_level_percent %in% NA]
data <- data[adjustment %in% 3]


ggplot(
  data,
  aes(
    x     = history_range,
    y     = Relmean,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) + geom_point()


ggplot(
  data,
  aes(
    x     = history_range,
    y     = Relcv,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) + geom_point()

#-------------------------------------------------------------------------------

