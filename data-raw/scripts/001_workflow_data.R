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
# Grid level official PRF ADM                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
rma_adm <- data.table::rbindlist(
  lapply(
    study_environment$year_beg:study_environment$year_end,
    function(i){
      tryCatch({
        temporary_dir <- tempdir()
        file_name <- paste0("prf_adm_grid_level_",i,".rds")
        piggyback::pb_download(
          file = file_name,
          dest = temporary_dir,
          repo = "ftsiboe/rfcipPRF",
          tag  = "prf_official",
          overwrite = TRUE)
        df <- readRDS(file.path(temporary_dir,file_name))[
          ,.(rma_county_base_value = mean(county_base_value, na.rm=T),
             rma_base_rate = mean(base_rate, na.rm=T),
             rma_payment_factor = mean(payment_factor, na.rm=T)),
          by=c("grid_id","interval_code","commodity_year","coverage_level_percent")]
        df
      }, error = function(e){NULL})
    }),fill = TRUE)
rma_adm[,coverage_level:=round(coverage_level_percent*100)]
saveRDS(rma_adm,"data/grid_level_official_prf_adm.rds")
#----------------------------------------------------
# Official interval names                         ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
temporary_dir <- tempdir()
file_name <- "intervalKey.rds"
piggyback::pb_download(
  file = file_name,
  dest = temporary_dir,
  repo = "ftsiboe/rfcipPRF",
  tag  = "prf_official",
  overwrite = TRUE)

intervalKey <- readRDS(file.path(temporary_dir,file_name))
intervalKey <- intervalKey[intervalKey$interval_code %in% 625:635,]
intervalKey <- intervalKey[intervalKey$commodity_year %in% max(intervalKey$commodity_year),]
intervalKey$interval_name <- gsub(" INDEX INTERVAL","",intervalKey$interval_name)
intervalKey <- intervalKey[order(intervalKey$interval_code),]
saveRDS(intervalKey,"data/official_interval_names.rds")
#----------------------------------------------------
# Official PRF index                              ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
temporary_dir <- tempdir()
file_name <- "rmaRainfallIndices.rds"
piggyback::pb_download(
  file = file_name,
  dest = temporary_dir,
  repo = "ftsiboe/rfcipPRF",
  tag  = "prf_official",
  overwrite = TRUE)
rma_data <- as.data.table(readRDS(file.path(temporary_dir,file_name)))[
  Commodity0088 %in% 1 & insurance_plan_code %in% 13 & commodity_year %in% study_environment$year_beg:study_environment$year_end,
  .(rma_index = mean(actual_index, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year")]

saveRDS(rma_data,"data/official_prf_index.rds")
#----------------------------------------------------
# Official PRF Grid Weights                       ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
temporary_dir <- tempdir()
file_name <- "potential_prf_grids.rds"
piggyback::pb_download(
  file = file_name,
  dest = temporary_dir,
  repo = "ftsiboe/rfcipPRF",
  tag  = "prf_extracts",
  overwrite = TRUE)
data <- readRDS(file.path(temporary_dir,file_name))
saveRDS(data,"data/prf_grid_weights.rds")
#----------------------------------------------------
# Official PRF county penetration                 ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
piggyback::pb_download(
  file = "prf_county_penetration.rds",
  dest = "data",
  repo = "ftsiboe/rfcipPRF",
  tag  = "prf_extracts",
  overwrite = TRUE)
#----------------------------------------------------
# Official PRF county penetration                 ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc()
piggyback::pb_download(
  file = "cpc_historic_precipitation.rds",
  dest = "data",
  repo = "ftsiboe/rfcipPRF",
  tag  = "cpc",
  overwrite = TRUE)
#----------------------------------------------------



