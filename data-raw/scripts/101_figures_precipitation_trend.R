#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE));library(data.table);library(future.apply);library(rfcipPRF);gc()
devtools::document()
study_environment <- readRDS("data/study_environment.rds")
output_directory <- "data-raw/releases/baseline"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
redesigns_directory  <- study_environment$wd$redesigns
prf_grid_weights <- readRDS("data/prf_grid_weights.rds")
Keep.List<-c("Keep.List",ls())

current_year <- max(readRDS("data/cpc_historic_precipitation.rds")$commodity_year,
                    na.rm = TRUE)
start_year   <- min(readRDS("data/cpc_historic_precipitation.rds")$commodity_year,
                    na.rm = TRUE)

data <- readRDS("data-raw/output/county_precipitation_trend.rds")

data <- data[history_range %in% c(5,10,20,30,40,50,60,200)]

#data <- data[model_estimator %in% "within"]
data <- data[
  , model_estimator :=
    factor(
      model_estimator,
      levels = c("pooling","random","within"),
      labels = c("Pooled OLS","Random effects","Fixed effects")
    )
]

data <- data[
  , history_lable := factor(
    history_range,
    levels = c(seq(5,60,1),200),
    labels = c(paste0("Recent ",seq(5,60,1)," years\n[",
                      current_year - seq(5,60,1) + 1,"-",current_year,"]"),
               paste0("Full history \n[",start_year,"-",current_year,"]"))
  )
]


data[
  ,trend_class := factor(
    trend_class_05,
    c("Positive Mean and Positive Variance",
      "Positive Mean and Negative Variance",
      "Negative Mean and Positive Variance",
      "Negative Mean and Negative Variance",
      "Positive Mean only",
      "Negative Mean only",
      "Positive Variance only",
      "Negative Variance only",
      "None"))]

trend_palette <- c(
  "Positive Mean and Positive Variance" = "#00583D",  # strong warm
  "Positive Mean and Negative Variance" = "#FFC425",  # warm yellow
  "Negative Mean and Positive Variance" = "purple",   # mixed negative/positive
  "Negative Mean and Negative Variance" = "#B83E27",  # strong cool/dark green

  "Positive Mean only"                 = "#F4A261",  # lighter warm
  "Negative Mean only"                 = "#9DD9F7",  # light cool

  "Positive Variance only"             = "#E76F51",  # medium warm
  "Negative Variance only"             = "#2A9D8F",  # medium cool-green

  "None"                               = "#BDBDBD"   # neutral gray
)



#
fig_trend <- plot_prf_statistics(
  data = data[! trend_class_05 %in% NA],
  outcome_variable = "trend_class",
  plot_title = NULL,
  spatial_unit ="county",
  palette = trend_palette) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) + facet_grid(model_estimator~history_lable)


ggsave(file.path("data-raw/output/figure","county_precipitation_trend_both.png"),
       fig_trend, dpi = 600,width = 10, height =4)

unique(data$mean_trend_class_05)
data[
  ,trend_class := factor(
    mean_trend_class_05,
    c("Positive",
      "Negative",
      "None"))]

trend_palette <- c(
  "Positive" = "#00583D",
  "Negative" = "#B83E27",
  "None" = "#BDBDBD"
)



#
fig_trend <- plot_prf_statistics(
  data = data[! mean_trend_class_05 %in% NA],
  outcome_variable = "trend_class",
  plot_title = NULL,
  spatial_unit ="county",
  palette = trend_palette) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) + facet_grid(model_estimator~history_lable)


ggsave(file.path("data-raw/output/figure","county_precipitation_trend_mean.png"),
       fig_trend, dpi = 600,width = 10, height =4)




data[
  ,trend_class := factor(
    var_trend_class_05,
    c("Positive",
      "Negative",
      "None"))]

trend_palette <- c(
  "Positive" = "#00583D",
  "Negative" = "#B83E27",
  "None" = "#BDBDBD"
)



#
fig_trend <- plot_prf_statistics(
  data = data[! var_trend_class_05 %in% NA],
  outcome_variable = "trend_class",
  plot_title = NULL,
  spatial_unit ="county",
  palette = trend_palette) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) + facet_grid(model_estimator~history_lable)


ggsave(file.path("data-raw/output/figure","county_precipitation_trend_var.png"),
       fig_trend, dpi = 600,width = 10, height =4)
