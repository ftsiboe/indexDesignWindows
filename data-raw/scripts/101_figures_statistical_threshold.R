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
output_directory <- "data-raw/releases/statistical_threshold"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
redesigns_directory  <- study_environment$wd$redesigns
prf_grid_weights <- readRDS("data/prf_grid_weights.rds")
xlist <- c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
           "pvalue_pearson_cor","pvalue_kendall_cor")
Keep.List<-c("Keep.List",ls())
#-------------------------------------------------------------------------------
# Plot - Balance Statistics                                                  ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()

data <- data.table::rbindlist(
  lapply(
    list.files(output_directory,full.names = TRUE),
    function(i) {
      tryCatch({
        # i <- "data-raw/releases/statistical_threshold/threshold_analysis_006.rds"
        data <- as.data.frame(readRDS(i))

        data <- data[c(xlist,"index_history_range","state_code", "county_code","county_fips","coverage_level","y_level")]
        unique(data$y_level)

        data$disaggregate <- ifelse(data$y_level %in% "alternative_index","Index",NA)

        data$disaggregate <- ifelse(data$y_level %in% "alternative_base_rate",
                                    paste0("Base rate (raw) at ",data$coverage_level,"%"),
                                    data$disaggregate)

        data$disaggregate <- ifelse(data$y_level %in% "alternative_base_rate_adj",
                                    paste0("Base rate (adjusted) at ",data$coverage_level,"%"),
                                    data$disaggregate)

        data$disaggregate <- ifelse(data$y_level %in% "alternative_payment_factor",
                                    paste0("Payment factor at ",data$coverage_level,"%"),
                                    data$disaggregate)

        data <- rbind(data[names(data)],
                      data[data$coverage_level %in% 90, names(data)],
                      data[data$coverage_level %in% 90,names(data)])

        data$disaggregate <- factor(
          data$disaggregate,
          levels = c("Index","Base rate (raw) at 90%","Base rate (adjusted) at 90%","Payment factor at 90%"),
          labels = c("Index","Base rate (raw)\nfor 90% coverage level",
                     "Base rate (adjusted)\nfor 90% coverage level",
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

data <- as.data.table(data)[
  disaggregate %in% c("Index","Base rate (raw)\nfor 90% coverage level",
                      "Base rate (adjusted)\nfor 90% coverage level",
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

