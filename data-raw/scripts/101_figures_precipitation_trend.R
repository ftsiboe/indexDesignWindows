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
unique(data$trend_class)

data <- data[model_estimator %in% "random" & precipitation %in% "log"]
# "random","pooling","within"
fig_trend <- plot_prf_statistics(
  data = data[history_range %in% c(5,10,15,20,25,30,35,40,45,50,55,60,200)],
  outcome_variable = "trend_class05",
  plot_title = NULL,
  spatial_unit ="county",
  palette = c(
    "firebrick",
    "#9DD9F7",
    "#00583D"
  )) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) + facet_wrap(~history_range,nrow=4)

ggsave(file.path("data-raw/output/figure","county_precipitation_trend.png"),
       fig_trend, dpi = 600,width = 8, height =5)











