rm(list = ls(all = TRUE));library(data.table);library(future.apply);library(rfcipPRF);gc()
devtools::document()
study_environment <- readRDS("data/study_environment.rds")

current_year  <- study_environment$year_end

if (Sys.info()['sysname'] %in% "Windows") {
  array_list <- 9
  available_cores <- min(c(round(availableCores() * 0.50),array_list))
}else{
  available_cores <- availableCores()
}
output_directory  <- study_environment$wd$redesigns
if(!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

done_list <- basename(list.files(output_directory,full.names = TRUE, recursive = TRUE))
table(done_list[grepl("prf_index_",done_list)])
done_list <- data.frame(name=done_list[!grepl("prf_index_",done_list)])
done_list <- tidyr::separate(done_list,"name",sep="_",into = c("prf","rates","range","year"))
table(done_list$range,done_list$year)

#table(done_list$range)

design_specs <- data.table::CJ(
  history_range = c(200,seq(5,60,1)),
  commodity_year = 2016:current_year,
  unique = TRUE)

# If running under a SLURM array job, filter the design_specs by the current task ID
if (!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))) {
  # Replicate the range [TASK_MIN:TASK_MAX] until it matches the length of design_specs
  design_specs$task <- rep(
    as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MIN")):as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MAX")),
    length = nrow(design_specs)
  )
  # Keep only rows assigned to this specific SLURM_ARRAY_TASK_ID
  design_specs <- design_specs[design_specs$task %in% as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")), ]
}

lapply(
  1:nrow(design_specs),
  function(i){
    tryCatch({
      # i <- 1
      year           <- design_specs$commodity_year[i]
      history_range  <- design_specs$history_range[i]
      range_tag      <- stringr::str_pad(history_range,pad="0",3)
      out_file_rates <- file.path(output_directory, range_tag,paste0("prf_rates_",range_tag,"_",year,".rds"))

      if(!file.exists(out_file_rates)){
        prf_index_data <- readRDS(file.path(output_directory, range_tag,paste0("prf_index_",range_tag,".rds")))
        # prf_index_data <- prf_index_data[grid_id %in% unique(prf_index_data$grid_id)[1:10]]
        # Compute PRF Premium Rates Across Program Years and Coverage Levels

        prf_rate_data <- compute_prf_premium_rate(
          program_years   = year,
          prf_index_data  = prf_index_data,
          identifiers     = c("index_history_range","grid_id","interval_code"),
          year_col        = "commodity_year",
          index_value_col = "index")
        saveRDS(prf_rate_data, out_file_rates)
        rm(prf_rate_data,prf_index_data);gc()
      }
      invisible()

    }, error = function(e){invisible()})
  })


# Upload the assets
function(){
  if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)
  piggyback::pb_upload(
    list.files(output_directory, full.names = TRUE, recursive = T),
    repo  = "ftsiboe/indexDesignWindows",
    tag   = "redesigns",
    overwrite = TRUE
  )
}
