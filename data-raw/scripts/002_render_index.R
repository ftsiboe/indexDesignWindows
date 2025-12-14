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
