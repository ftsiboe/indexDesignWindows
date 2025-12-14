# Hard reset of workspace
rm(list = ls(all = TRUE));gc()
library(data.table);library(rfcipPRF)
#----------------------------------------------------
# Initialize environment                          ####
devtools::document()
study_environment <- setup_environment(
  year_beg = 2016, year_end = 2024, seed = 1980632,
  project_name="indexDesignWindows",
  local_directories = list(
    file.path("data-raw", "output","summary"),
    file.path("data-raw", "output","figure_data"),
    file.path("data-raw", "output","figure"),
    file.path("data-raw", "scripts")
  ),
  fastscratch_directories=list("redesigns"))
saveRDS(study_environment,file ="data/study_environment.rds")
Keep.List<-c("Keep.List",ls())
#----------------------------------------------------
