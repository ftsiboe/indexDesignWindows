rm(list = ls(all = TRUE));library(data.table);library(future.apply);library(rfcipPRF);gc()
devtools::document()
study_environment <- readRDS("data/study_environment.rds")

current_year  <- as.numeric(format(Sys.Date(), "%Y"))-1

if (Sys.info()['sysname'] %in% "Windows") {
  array_list <- 9
  available_cores <- min(c(round(availableCores() * 0.50),array_list))
}else{
  available_cores   <- availableCores()
}

output_directory  <- study_environment$wd$redesigns
if(!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

design_specs <- unique(c(200,seq(5,60,1)))

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))) {
  design_specs <- design_specs[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))]
}

plan(sequential)
plan(list(tweak(multisession, workers = available_cores)));gc()

future_lapply(
  design_specs,
  function(history_range){
    tryCatch({
      # history_range <- design_specs[1]

      range_tag <- stringr::str_pad(history_range,pad="0",3)

      history_range_directory <- file.path(output_directory, range_tag)

      if(!dir.exists(history_range_directory)) dir.create(history_range_directory, recursive = TRUE)

      out_file_index <- file.path(history_range_directory,paste0("prf_index_",range_tag,".rds"))

      if(!file.exists(out_file_index)){
        # Compute PRF Rainfall Index for Program Years
        prf_index_data <- data.table::rbindlist(
          future_lapply((study_environment$year_beg-25):current_year,compute_prf_index,
                        history_range = history_range,
                        github_token = Sys.getenv("rfcipPRF_TOKEN", unset = NA)),
          fill = TRUE
        )
        saveRDS(prf_index_data, out_file_index)
      }
      rm(prf_index_data);gc()
      invisible()
    }, error = function(e){invisible()})
  })

plan(sequential);gc()

function(){
  # Estimate RMA Rate Discretion Factor                                        ####
  rm(list = ls(all = TRUE));library(data.table);library(rfcipPRF);gc()
  devtools::document()
  study_environment <- readRDS("data/study_environment.rds")

  output_directory <- "data-raw/releases/baseline"
  if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

  prf_grid_weights <- readRDS("data/prf_grid_weights.rds")

  temporary_dir <- tempdir()

  piggyback::pb_download(
    file = "prf_index_200.rds",
    dest = temporary_dir,
    repo = "ftsiboe/indexDesignWindows",
    tag  = "redesigns",
    overwrite = TRUE)

  data <- readRDS(file.path(temporary_dir,"prf_index_200.rds"))

  data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
  data <- data[
    ,.(rma_index = weighted.mean(x=rma_index,w=potential_range_pasture, na.rm=T),
       cpc_index = weighted.mean(x=index,w=potential_range_pasture, na.rm=T)),
    by=c("commodity_year","state_code", "county_code","county_fips","interval_code")]

  # Weighted least squares
  data <- data[
    ,.(index_discretion_factor_all = coef(lm(rma_index~cpc_index - 1))),
    by=c("state_code", "county_code","county_fips")][
      data[
        ,.(index_discretion_factor = coef(lm(rma_index~cpc_index - 1))),
        by=c("state_code", "county_code","county_fips","interval_code")],
      on=c("state_code", "county_code","county_fips"),nomatch = 0]

  data <- data |> tidyr::spread(interval_code, index_discretion_factor)
  data[,"600"] <- data$index_discretion_factor_all
  data <- data |> tidyr::gather(interval_code, index_discretion_factor, names(data)[!names(data) %in% c("state_code", "county_code","county_fips","index_discretion_factor_all")])
  data$index_discretion_factor <- ifelse(data$index_discretion_factor %in% NA,data$index_discretion_factor_all,data$index_discretion_factor)
  data$interval_code <- as.numeric(data$interval_code)

  intervalKey <- readRDS("data/official_interval_names.rds")

  data$interval_name <- factor(
    data$interval_code,
    levels = c(600,intervalKey$interval_code),
    labels = c("All intervals",intervalKey$interval_name))

  saveRDS(as.data.table(data),file.path(output_directory,"rma_index_discretion_factor.rds"))
}


