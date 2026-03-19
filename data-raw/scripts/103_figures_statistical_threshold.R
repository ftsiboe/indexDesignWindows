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
Keep.List<-c("Keep.List",ls())
#-------------------------------------------------------------------------------
# Plot - statistical analysis                                                ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- list.files(output_directory,full.names = TRUE, pattern = "statistical_analysis")

data <- data.table::rbindlist(
  lapply(
    data[!grepl("200",data)],
    function(i) {
      tryCatch({
        # i <- list.files(output_directory,full.names = TRUE, pattern = "statistical_analysis")[1]
        data <- as.data.frame(readRDS(i))

        data <- data[c(xlist,"index_history_range","state_code", "county_code","county_fips","coverage_level","y_level")]


        data <- data[data$y_level %in% c("alternative_index_adj01" ,"alternative_base_rate_adj03" ,"alternative_payment_factor_adj01"),]

        data$disaggregate <- ifelse(data$y_level %in% "alternative_index_adj01","Index",NA)

        data$disaggregate <- ifelse(data$y_level %in% "alternative_base_rate_adj03",
                                    paste0("Base rate at ",data$coverage_level,"%"),
                                    data$disaggregate)

        data$disaggregate <- ifelse(data$y_level %in% "alternative_payment_factor_adj01",
                                    paste0("Payment factor at ",data$coverage_level,"%"),
                                    data$disaggregate)

        unique(data$disaggregate)

        data <- data[
          data$coverage_level %in% 90 |
            data$y_level %in% "alternative_index_adj01", names(data)
        ]

        # unique(data$disaggregate)
        #
        # rbind(data[names(data)],
        #               data[data$coverage_level %in% 90, names(data)],
        #               data[data$coverage_level %in% 90, names(data)])

        data$disaggregate <- factor(
          data$disaggregate,
          levels = c("Index","Base rate at 90%","Payment factor at 90%"),
          labels = c("Index","Base rate\nfor 90% coverage level",
                     "Payment factor\nfor 90% coverage level")
        )

        data <- data |>
          tidyr::gather(variable, value, xlist)
        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data$variable_name <- factor(
  data$variable,
  levels = c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
             "pvalue_pearson_cor","pvalue_kendall_cor"),
  labels = c("Mean equality\nt-test",
             "F-test of equal\nvariances",
             "Kruskal-Wallis test\nof equal variances",
             "Kolmogorov-Smirnov\ntest",
             "Pearson\ncorrelation",
             "Kendall's\ntau")
)

data <- add_break_categories(
  data = data,
  variable = "value",
  break_levels = c(-Inf,0.01,0.05,0.10,0.50,Inf),
  break_labels = c("Less than 0.01","0.01 to 0.05","0.05 to 0.10","0.10 to 0.50","Greater than 0.50"))

data <- data[!data$value_cat %in% NA,]


# county_precipitation_trend <- readRDS("data-raw/releases/outputs/county_precipitation_trend.rds")


data <- as.data.table(data)[
  disaggregate %in% c("Index","Base rate\nfor 90% coverage level",
                      "Payment factor\nfor 90% coverage level") &
    variable %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
  .(
    n = .N
  ),
  by = .(index_history_range, disaggregate, variable_name, value_cat)
][
  , prop := n / sum(n),
  by = .(index_history_range, disaggregate, variable_name)
]

data$history_range <- as.numeric(gsub("[^0-9]","",data$index_history_range))

fig <- ggplot(
  data,
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
  facet_grid(variable_name ~ disaggregate) +
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
       fig, dpi = 600,width = 8, height =8)

#-------------------------------------------------------------------------------
# Plot - actuarial  analysis                                                ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
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
          .SDcols = c("alternative_indemnity_amount","alternative_total_premium_amount",
                      "baseline_indemnity_amount","baseline_total_premium_amount" )]

        data[,alternative_lr  := alternative_indemnity_amount/alternative_total_premium_amount]
        data[,baseline_lr     := baseline_indemnity_amount/baseline_total_premium_amount]
        #data[,actuarial_index := ((alternative_lr-1)^2)/((baseline_lr-1)^2)]
        data[,actuarial_index := (abs(alternative_lr-1) - abs(baseline_lr-1))*100]

        data
      }, error = function(e){NULL})
    }),fill = TRUE)

data[,history_range := as.numeric(gsub("[^0-9]","",index_history_range))]
data[,adjustmentCat := as.factor(adjustment)]


ggplot(
  data,
  aes(
    x     = history_range,
    y     = actuarial_index,
    fill  = adjustmentCat,
    color  = adjustmentCat,
    group = adjustmentCat
  )
) + geom_point()








+
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
      "#BE5E27"
    ),
    na.value = "white"
  ) +
  facet_grid(variable_name ~ disaggregate) +
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




data <- add_break_categories(
  data = data,
  variable = "value",
  break_levels = c(-Inf,0.01,0.05,0.10,0.50,Inf),
  break_labels = c("Less than 0.01","0.01 to 0.05","0.05 to 0.10","0.10 to 0.50","Greater than 0.50"))

data <- data[!data$value_cat %in% NA,]


county_precipitation_trend <- readRDS("data-raw/releases/outputs/county_precipitation_trend.rds")


data <- as.data.table(data)[
  disaggregate %in% c("Index","Base rate\nfor 90% coverage level",
                      "Payment factor\nfor 90% coverage level") &
    variable %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks"),
  .(
    n = .N
  ),
  by = .(index_history_range, disaggregate, variable_name, value_cat)
][
  , prop := n / sum(n),
  by = .(index_history_range, disaggregate, variable_name)
]

data$history_range <- as.numeric(gsub("[^0-9]","",data$index_history_range))

fig <- ggplot(
  data,
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
      "#BE5E27"
    ),
    na.value = "white"
  ) +
  facet_grid(variable_name ~ disaggregate) +
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
       fig, dpi = 600,width = 8, height =8)

#-------------------------------------------------------------------------------

