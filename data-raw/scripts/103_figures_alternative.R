#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE));library(data.table);gc()
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
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
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
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data_stats <- readRDS("data-raw/releases/alternative/summary_statistical.rds")
data_stats <- as.data.frame(data_stats[disaggregate %in% "full" & disaggregate_level %in% "full" ])

fig_stats <- ggplot(
  data_stats[data_stats$value_cat %in%c("Less than 0.10") &
               data_stats$history_years >= 10,],
  aes(
    x     = history_years,
    y     = prop*100,
    fill  = specification,
    colour = specification,
    shape = specification,
    group = specification
  )
) +
  geom_point(size=1) +
  geom_vline(aes(xintercept = turning_point), linetype = "dashed", color="gray")+
  facet_grid(statistic_name~variable, scales = "free_y") +
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
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data_actuarial <- as.data.table(readRDS("data-raw/releases/alternative/summary_actuarial.rds"))
data_actuarial <- data_actuarial[disaggregate %in% "full" & disaggregate_level %in% "full" ]

fig_actuarial <- ggplot(
  data_actuarial[history_years >= 20],
  aes(
    x     = history_years,
    y     = actuarial_index,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) +
  geom_point(size=2) +
  geom_vline(aes(xintercept = turning_point), linetype = "dashed", color="gray")+
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
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- as.data.table(readRDS("data-raw/releases/alternative/summary_economic.rds"))
data <- data[disaggregate %in% "full" & disaggregate_level %in% "full" ]



fig_actuarial <- ggplot(
  data[history_years >= 20],
  aes(
    x     = history_years,
    y     = economic_lr,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) +
  geom_point(size=2) +
  geom_vline(aes(xintercept = turning_point), linetype = "dashed", color="gray")+
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

#-------------------------------------------------------------------------------

