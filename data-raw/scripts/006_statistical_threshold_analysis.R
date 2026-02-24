rm(list = ls(all = TRUE));library(data.table);library(future.apply);gc()

devtools::document()
study_environment    <- readRDS("data/study_environment.rds")
redesigns_directory  <- study_environment$wd$redesigns
output_directory <- "data-raw/releases/statistical_threshold"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)


prf_grid_weights     <- readRDS("data/prf_grid_weights.rds")
rma_index_discretion_factor <- readRDS("data-raw/releases/baseline/rma_index_discretion_factor.rds")

rma_rate_discretion_factor <- readRDS("data-raw/releases/baseline/rma_rate_discretion_factor.rds")
rma_rate_discretion_factor <- rma_rate_discretion_factor[
  ,.(rate_discretion_factor = mean(rate_discretion_factor, na.rm=T)),
  by=c("state_code","county_code","county_fips" ,"interval_code","discretion_flag")]
rma_rate_discretion_factor[discretion_flag %in% "raw",discretion_flag := "rate_discretion_factor_raw" ]
rma_rate_discretion_factor[discretion_flag %in% "adjusted",discretion_flag := "rate_discretion_factor_adjusted" ]
rma_rate_discretion_factor <- rma_rate_discretion_factor |> tidyr::spread(discretion_flag, rate_discretion_factor)
rma_rate_discretion_factor <- as.data.table(rma_rate_discretion_factor)


alternative_list <- list.files(redesigns_directory,pattern = "prf_rates_",full.names = T,recursive = T)
alternative_list <- alternative_list[!grepl("200",alternative_list)]

# alternative_list <- alternative_list[
#   ! paste0("threshold_analysis_",basename(dirname(alternative_list)),".rds") %in%
#     list.files(output_directory)]

baseline_program <- readRDS(file.path(redesigns_directory,"200/prf_index_200.rds"))[
  commodity_year %in% study_environment$year_beg:study_environment$year_end,.(baseline_index = mean(index, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year")]
baseline_program <- prf_grid_weights[baseline_program,on = intersect(names(baseline_program), names(prf_grid_weights)),nomatch = 0]
baseline_program <- rma_index_discretion_factor[baseline_program,on = intersect(names(baseline_program), names(rma_index_discretion_factor)),nomatch = 0]

baseline_program <- baseline_program[
  ,.(baseline_index     = round(weighted.mean(x=baseline_index,w=potential_range_pasture, na.rm=T),3),
     baseline_index_adj = round(weighted.mean(x=baseline_index*index_discretion_factor,w=potential_range_pasture, na.rm=T),3)),
  by=c("commodity_year","state_code", "county_code","county_fips","interval_code")]

baseline_program <- baseline_program[
  readRDS(file.path("data-raw/releases/baseline","baseline_adm.rds")),
  on = c("state_code", "county_code","county_fips","interval_code","commodity_year"),nomatch = 0]

baseline_program <- baseline_program[
  ,.(baseline_index          = mean(baseline_index, na.rm=T),
     baseline_index_adj01    = mean(baseline_index_adj, na.rm=T),
     baseline_base_rate      = mean(cpc_base_rate, na.rm=T),
     baseline_base_rate_adj01  = mean(cpc_base_rate_adj01, na.rm=T),
     baseline_base_rate_adj02  = mean(cpc_base_rate_adj02, na.rm=T),
     baseline_base_rate_adj03  = mean(cpc_base_rate_adj03, na.rm=T),
     baseline_payment_factor     = mean(cpc_payment_factor, na.rm=T),
     baseline_payment_factor_adj01 = mean(cpc_payment_factor_adj01, na.rm=T)),
  by=c("commodity_year","state_code", "county_code","county_fips","interval_code","coverage_level")]

# If running under a SLURM array job, filter the design_specs by the current task ID
if (!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))) {
  alternative_list <- alternative_list[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))]
}

lapply(
  unique(basename(dirname(alternative_list))),
  function(history_range) {
    tryCatch({
      # history_range <- unique(basename(dirname(alternative_list)))[1]
      out_file <- file.path(output_directory,paste0("threshold_analysis_",history_range,".rds"))
      if(!file.exists(out_file)){

        data <- data.table::rbindlist(
          lapply(
            alternative_list[grepl(history_range,alternative_list)],
            function(i) {
              tryCatch({
                # i <- alternative_list[grepl(history_range,alternative_list)][1]
                cpc_rates <- readRDS(i)[
                  ,.(alternative_base_rate = mean(base_rate, na.rm=T)),
                  by=c("grid_id","interval_code","commodity_year","coverage_level","index_history_range","discretion_flag")]
                cpc_rates[discretion_flag %in% "raw",discretion_flag := "alternative_base_rate" ]
                cpc_rates[discretion_flag %in% "adjusted",discretion_flag := "alternative_base_rate_adj02" ]
                cpc_rates <- cpc_rates |> tidyr::spread(discretion_flag, alternative_base_rate)
                cpc_rates <- as.data.table(cpc_rates)

                cpc_rates <- prf_grid_weights[cpc_rates,on = intersect(names(cpc_rates), names(prf_grid_weights)),nomatch = 0]

                cpc_rates <- cpc_rates[
                  ,.(alternative_base_rate = round(weighted.mean(x=alternative_base_rate,w=potential_range_pasture, na.rm=T),3),
                     alternative_base_rate_adj02 = round(weighted.mean(x=alternative_base_rate_adj02,w=potential_range_pasture, na.rm=T),3)),
                  by=c("commodity_year","state_code", "county_code","county_fips","interval_code","coverage_level","index_history_range")]

                cpc_rates <- rma_rate_discretion_factor[cpc_rates,on = intersect(names(cpc_rates), names(rma_rate_discretion_factor)),nomatch = 0]

                cpc_rates[,alternative_base_rate_adj01 := round(alternative_base_rate*rate_discretion_factor_raw,3)]
                cpc_rates[,alternative_base_rate_adj03 := round(alternative_base_rate_adj02*rate_discretion_factor_adjusted,3)]


                cpc_index <- readRDS(list.files(dirname(i),pattern = "prf_index_",full.names = T))[
                  commodity_year %in% unique(cpc_rates$commodity_year),.(alternative_index = mean(index, na.rm=T)),
                  by=c("grid_id","interval_code")]
                cpc_index <- prf_grid_weights[cpc_index,on = intersect(names(cpc_index), names(prf_grid_weights)),nomatch = 0]
                cpc_index <- rma_index_discretion_factor[cpc_index,on = intersect(names(cpc_index), names(rma_index_discretion_factor)),nomatch = 0]

                cpc_index <- cpc_index[
                  ,.(alternative_index = round(weighted.mean(x=alternative_index,w=potential_range_pasture, na.rm=T),3),
                     alternative_index_adj = round(weighted.mean(x=alternative_index*index_discretion_factor,w=potential_range_pasture, na.rm=T),3)),
                  by=c("state_code", "county_code","county_fips","interval_code")]

                cpc_rates <- cpc_index[cpc_rates,on = intersect(names(cpc_rates), names(cpc_index)),nomatch = 0]
                cpc_rates[,alternative_payment_factor := ifelse(alternative_index >= coverage_level, 0,coverage_level - alternative_index) / coverage_level ]
                cpc_rates[,alternative_payment_factor_adj := ifelse(alternative_index_adj >= coverage_level, 0,coverage_level - alternative_index_adj) / coverage_level ]

                cpc_rates <- cpc_rates[
                  ,.(alternative_index          = mean(alternative_index, na.rm=T),
                     alternative_index_adj01      = mean(alternative_index_adj, na.rm=T),
                     alternative_base_rate      = mean(alternative_base_rate, na.rm=T),
                     alternative_base_rate_adj01  = mean(alternative_base_rate_adj01, na.rm=T),
                     alternative_base_rate_adj02  = mean(alternative_base_rate_adj02, na.rm=T),
                     alternative_base_rate_adj03  = mean(alternative_base_rate_adj03, na.rm=T),
                     alternative_payment_factor     = mean(alternative_payment_factor, na.rm=T),
                     alternative_payment_factor_adj01 = mean(alternative_payment_factor_adj, na.rm=T)),
                  by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level","index_history_range")]

                cpc_rates
              }, error = function(e){NULL})
            }),fill = TRUE)

        data[,coverage_level:=round(coverage_level*100)]

        data <- baseline_program[data,on = intersect(names(data), names(baseline_program)),nomatch = 0]

        balance_final <- list()

        balance_final[[length(balance_final)+1]] <- compute_balance(
          dt=data[
            ,.(alternative_index = mean(alternative_index, na.rm=T),
               baseline_index = mean(baseline_index, na.rm=T)),
            by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level","index_history_range")],
          col_x="baseline_index", col_y="alternative_index",
          by = c("state_code", "county_code","county_fips","index_history_range"))

        balance_final[[length(balance_final)+1]] <- compute_balance(
          dt=data[
            ,.(alternative_index_adj01 = mean(alternative_index_adj01, na.rm=T),
               baseline_index_adj01 = mean(baseline_index_adj01, na.rm=T)),
            by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level","index_history_range")],
          col_x="baseline_index_adj01", col_y="alternative_index_adj01",
          by = c("state_code", "county_code","county_fips","index_history_range"))

        balance_list <- list(
          c("baseline_base_rate","alternative_base_rate"),
          c("baseline_base_rate_adj01","alternative_base_rate_adj01"),
          c("baseline_base_rate_adj02","alternative_base_rate_adj02"),
          c("baseline_base_rate_adj03","alternative_base_rate_adj03"),
          c("baseline_payment_factor","alternative_payment_factor"),
          c("baseline_payment_factor_adj01","alternative_payment_factor_adj01")
        )

        for(nm in 1:length(balance_list)){
          balance_final[[length(balance_final)+1]] <- compute_balance(
            dt=data,
            col_x=balance_list[[nm]][1], col_y=balance_list[[nm]][2],
            by = c("state_code", "county_code","county_fips","index_history_range","coverage_level"))
        }

        balance_final <- data.table::rbindlist(balance_final,fill = TRUE)

        rm(data);gc();gc()

        saveRDS(as.data.table(balance_final),out_file)
      }
    }, error = function(e){NULL})
  })


# Upload Baseline assets                                                     ####
function(){
  if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

  piggyback::pb_release_create(
    repo = "ftsiboe/indexDesignWindows",
    tag  = "statistical_threshold",
    name = "Statistical Threshold Outputs",
    body = paste(
      "This release contains outputs from alternative PRF index design experiments statistical threshold analysis.",
      sep = "\n"
    )
  )

  piggyback::pb_upload(
    list.files(output_directory, full.names = TRUE, recursive = T),
    repo  = "ftsiboe/indexDesignWindows",
    tag   = "statistical_threshold",
    overwrite = TRUE
  )
}
