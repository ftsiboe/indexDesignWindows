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

data <- readRDS("data-raw/output/county_precipitation_trend.rds")
# unique(data$trend_class05)

unique(data$trend_class_05)

data <- data[model_estimator %in% "within"]
data <- data[!trend_class_05 %in% NA]
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

# "random","pooling","within"
fig_trend <- plot_prf_statistics(
  data = data[history_range %in% c(5,10,15,20,25,30,35,40,45,50,55,60,200)],
  outcome_variable = "trend_class",
  plot_title = NULL,
  spatial_unit ="county",
  palette = trend_palette) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(
    legend.position = c(0.60, 0.10),
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) + facet_wrap(~history_range,ncol=3)


ggsave(file.path("data-raw/output/figure","county_precipitation_trend.png"),
       fig_trend, dpi = 600,width = 6.5, height =7.1)











