rm(list = ls(all = TRUE));library(data.table);library(future.apply);gc()

devtools::document()
study_environment    <- readRDS("data/study_environment.rds")
redesigns_directory  <- study_environment$wd$redesigns
prf_grid_weights     <- readRDS("data/prf_grid_weights.rds")

output_directory <- "data-raw/releases/statistical_threshold"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

alternative_list <- list.files(redesigns_directory,pattern = "prf_rates_",full.names = T,recursive = T)
alternative_list <- alternative_list[!grepl("200",alternative_list)]

baseline_program <- readRDS(file.path(redesigns_directory,"200/prf_index_200.rds"))[
  commodity_year %in% study_environment$year_beg:study_environment$year_end,.(baseline_index = mean(index, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year")]

baseline_program <- prf_grid_weights[baseline_program,on = intersect(names(baseline_program), names(prf_grid_weights)),nomatch = 0]
baseline_program <- baseline_program[
  ,.(baseline_index = round(weighted.mean(x=baseline_index,w=potential_range_pasture, na.rm=T),3)),
  by=c("commodity_year","state_code", "county_code","county_fips","interval_code")]

baseline_program <- baseline_program[
  readRDS(file.path("data-raw/releases/baseline","baseline_adm.rds")),
  on = c("state_code", "county_code","county_fips","interval_code","commodity_year"),nomatch = 0]

baseline_program <- baseline_program[
  ,.(baseline_index = mean(baseline_index, na.rm=T),
     baseline_base_rate = mean(cpc_base_rate, na.rm=T),
     baseline_base_rate_adj = mean(cpc_base_rate_adj, na.rm=T),
     baseline_payment_factor = mean(cpc_payment_factor, na.rm=T)),
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
                  by=c("grid_id","interval_code","commodity_year","coverage_level","index_history_range")]

                cpc_rates <- prf_grid_weights[cpc_rates,on = intersect(names(cpc_rates), names(prf_grid_weights)),nomatch = 0]
                cpc_rates <- cpc_rates[
                  ,.(alternative_base_rate = round(weighted.mean(x=alternative_base_rate,w=potential_range_pasture, na.rm=T),3)),
                  by=c("commodity_year","state_code", "county_code","county_fips","interval_code","coverage_level","index_history_range")]

                cpc_rates <- cpc_rates[
                  readRDS("data-raw/releases/baseline/rma_rate_discretion_factor.rds"),
                  on = c("state_code", "county_code","county_fips","interval_code"),nomatch = 0]

                cpc_rates[,alternative_base_rate_adj := alternative_base_rate*rma_discretion_factor]

                cpc_index <- readRDS(list.files(dirname(i),pattern = "prf_index_",full.names = T))[
                  commodity_year %in% unique(cpc_rates$commodity_year),.(alternative_index = mean(index, na.rm=T)),
                  by=c("grid_id","interval_code")]
                cpc_index <- prf_grid_weights[cpc_index,on = intersect(names(cpc_index), names(prf_grid_weights)),nomatch = 0]
                cpc_index <- cpc_index[
                  ,.(alternative_index = round(weighted.mean(x=alternative_index,w=potential_range_pasture, na.rm=T),3)),
                  by=c("state_code", "county_code","county_fips","interval_code")]

                cpc_rates <- cpc_index[cpc_rates,on = intersect(names(cpc_rates), names(cpc_index)),nomatch = 0]
                cpc_rates[,alternative_payment_factor := ifelse(
                  alternative_index >= coverage_level, 0,
                  coverage_level - alternative_index) / coverage_level ]

                cpc_rates <- cpc_rates[
                  ,.(alternative_index = round(mean(alternative_index, na.rm=T),3),
                     alternative_base_rate = round(mean(alternative_base_rate, na.rm=T),3),
                     alternative_base_rate_adj = round(mean(alternative_base_rate_adj, na.rm=T),3),
                     alternative_payment_factor = round(mean(alternative_payment_factor, na.rm=T),3)),
                  by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level","index_history_range")]

                cpc_rates
              }, error = function(e){NULL})
            }),fill = TRUE)
        data[,coverage_level:=round(coverage_level*100)]

        data <- baseline_program[data,on = intersect(names(data), names(baseline_program)),nomatch = 0]

        balance_index <- compute_balance(
          dt=data[
            ,.(alternative_index = mean(alternative_index, na.rm=T),
               baseline_index = mean(baseline_index, na.rm=T)),
            by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level","index_history_range")],
          col_x="baseline_index", col_y="alternative_index",
          by = c("state_code", "county_code","county_fips","index_history_range"))

        balance_base_rate <- compute_balance(
          dt=data,
          col_x="baseline_base_rate", col_y="alternative_base_rate",
          by = c("state_code", "county_code","county_fips","index_history_range","coverage_level"))

        balance_base_rate_adj <- compute_balance(
          dt=data,
          col_x="baseline_base_rate_adj", col_y="alternative_base_rate_adj",
          by = c("state_code", "county_code","county_fips","index_history_range","coverage_level"))

        balance_payment_factor <- compute_balance(
          dt=data,
          col_x="baseline_payment_factor", col_y="alternative_payment_factor",
          by = c("state_code", "county_code","county_fips","index_history_range","coverage_level"))

        balance_final <- data.table::rbindlist(
          list(balance_index,balance_base_rate,balance_base_rate_adj,balance_payment_factor),fill = TRUE)

        rm(balance_index,balance_base_rate,balance_base_rate_adj,balance_payment_factor,data);gc();gc()

        saveRDS(as.data.table(balance_final),out_file)
      }
    }, error = function(e){NULL})
  })



