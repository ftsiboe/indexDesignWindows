#-------------------------------------------------------------------------------
# Preliminaries                                                              ####
rm(list = ls(all = TRUE));library(data.table);library(future.apply);library(rfcipPRF);gc()
devtools::document()
study_environment <- readRDS("data/study_environment.rds")
myline <- paste0(rep("---",5),collapse = "")
statistics_list <- c(
  "mean" = " a) Mean ",
  "q50"  = " b) Median ",
  "var"  = " c) Variance ",
  "cv"   = " d) Coefficient of variation ",
  "q10"  = " e) 10th percentile ",
  "q90"  = " f) 90th percentile "
)
output_directory <- "data-raw/releases/baseline"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
redesigns_directory  <- study_environment$wd$redesigns
prf_grid_weights <- readRDS("data/prf_grid_weights.rds")
Keep.List<-c("Keep.List",ls())
#-------------------------------------------------------------------------------
# Baseline Data                                                              ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
prf_grid_weights            <- readRDS("data/prf_grid_weights.rds")
rma_index_discretion_factor <- readRDS("data-raw/releases/baseline/rma_index_discretion_factor.rds")

rma_rate_discretion_factor  <- readRDS("data-raw/releases/baseline/rma_rate_discretion_factor.rds")
rma_rate_discretion_factor <- rma_rate_discretion_factor[
  ,.(rate_discretion_factor = mean(rate_discretion_factor, na.rm=T)),
  by=c("state_code","county_code","county_fips" ,"interval_code","discretion_flag")]
rma_rate_discretion_factor[discretion_flag %in% "raw",discretion_flag := "rate_discretion_factor_raw" ]
rma_rate_discretion_factor[discretion_flag %in% "adjusted",discretion_flag := "rate_discretion_factor_adjusted" ]
rma_rate_discretion_factor <- rma_rate_discretion_factor |> tidyr::spread(discretion_flag, rate_discretion_factor)
rma_rate_discretion_factor <- as.data.table(rma_rate_discretion_factor)

cpc_index <- readRDS(file.path(redesigns_directory,"200/prf_index_200.rds"))[
  commodity_year %in% study_environment$year_beg:study_environment$year_end,.(cpc = mean(index, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year")]

rma_index <- readRDS("data/official_prf_index.rds")

data <- cpc_index[rma_index,on   = intersect(names(cpc_index), names(rma_index)),nomatch = 0]
data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
data <- rma_index_discretion_factor[data,on = intersect(names(data), names(rma_index_discretion_factor)),nomatch = 0]


data <- data[
  ,.(cpc = round(weighted.mean(x=cpc,w=potential_range_pasture, na.rm=T),3),
     cpcAdj01 = round(weighted.mean(x=cpc*index_discretion_factor,w=potential_range_pasture, na.rm=T),3),
     rma = round(weighted.mean(x=rma_index,w=potential_range_pasture, na.rm=T),3)),
  by=c("commodity_year","state_code", "county_code","county_fips","interval_code")]

prf_baseline_balance_index <- compute_balance(
  data,
  col_x="rma", col_y=c("cpc","cpcAdj01"), by = c("state_code", "county_code","county_fips"))
saveRDS(prf_baseline_balance_index,file.path(output_directory,"baseline_balance_index.rds"))


cpc_rates <- data.table::rbindlist(
  lapply(
    list.files(file.path(redesigns_directory,"200"),pattern = "prf_rates_",full.names = T),
    function(i) {
      tryCatch({
        # i <- list.files(file.path(redesigns_directory,"200"),pattern = "prf_rates_",full.names = T)[1];
        df <- readRDS(i)
        df <- df[
          ,.(cpc_base_rate = mean(base_rate, na.rm=T)),
          by=c("grid_id","interval_code","commodity_year","coverage_level","discretion_flag")]
        df[discretion_flag %in% "raw",discretion_flag := "cpc_base_rate" ]
        df[discretion_flag %in% "adjusted",discretion_flag := "cpc_base_rate_adj02" ]
        df <- df |> tidyr::spread(discretion_flag, cpc_base_rate)
      }, error = function(e){NULL})
    }),fill = TRUE)

cpc_rates <- cpc_index[cpc_rates,on = intersect(names(cpc_rates), names(cpc_index)),nomatch = 0]
cpc_rates <- prf_grid_weights[cpc_rates,on = intersect(names(cpc_rates), names(prf_grid_weights)),nomatch = 0]
cpc_rates <- rma_index_discretion_factor[cpc_rates,on = intersect(names(cpc_rates), names(rma_index_discretion_factor)),nomatch = 0]
cpc_rates <- rma_rate_discretion_factor[cpc_rates,on = intersect(names(cpc_rates), names(rma_rate_discretion_factor)),nomatch = 0]

cpc_rates[,cpcAdj01 := cpc*index_discretion_factor]
cpc_rates[,cpc_payment_factor       := ifelse(cpc >= coverage_level, 0, coverage_level - cpc) / coverage_level ]
cpc_rates[,cpc_payment_factor_adj01 := ifelse(cpcAdj01 >= coverage_level, 0, coverage_level - cpcAdj01) / coverage_level ]

cpc_rates[,cpc_base_rate_adj01 := cpc_base_rate*rate_discretion_factor_raw]
cpc_rates[,cpc_base_rate_adj03 := cpc_base_rate_adj02*rate_discretion_factor_adjusted]

cpc_rates[,coverage_level:=round(coverage_level*100)]

rma_rates <- readRDS("data/grid_level_official_prf_adm.rds")[
  ,.(rma_base_rate = mean(rma_base_rate, na.rm=T),
     rma_payment_factor = mean(rma_payment_factor, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year","coverage_level_percent","coverage_level")]

data <- cpc_rates[rma_rates,on = intersect(names(cpc_rates), names(rma_rates)),nomatch = 0]

saveRDS(data,file.path(output_directory,"baseline_adm.rds"))



prf_baseline_balance_pcf <- compute_balance(
  data[
    ,.(rma = round(mean(rma_payment_factor, na.rm=T),3),
       cpc = round(mean(cpc_payment_factor, na.rm=T),3),
       cpcAdj = round(mean(cpc_payment_factor_adj01, na.rm=T),3)),
    by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level_percent")],
  col_x="rma", col_y=c("cpc","cpcAdj"), by = c("state_code", "county_code","county_fips","coverage_level_percent"))
saveRDS(prf_baseline_balance_pcf,file.path(output_directory,"baseline_balance_payment_factor.rds"))

prf_baseline_balance_rate <- compute_balance(
  data[
    ,.(rma      = round(mean(rma_base_rate, na.rm=T),4),
       cpc      = round(mean(cpc_base_rate, na.rm=T),4),
       cpcAdj01 = round(mean(cpc_base_rate_adj01, na.rm=T),4),
       cpcAdj02 = round(mean(cpc_base_rate_adj02, na.rm=T),4),
       cpcAdj03 = round(mean(cpc_base_rate_adj03, na.rm=T),4)),
    by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level_percent")],
  col_x="rma", col_y=c("cpc","cpcAdj01","cpcAdj02","cpcAdj03"), by = c("state_code", "county_code","county_fips","coverage_level_percent"))
saveRDS(prf_baseline_balance_rate,file.path(output_directory,"baseline_balance_rate.rds"))

#-------------------------------------------------------------------------------
# Upload Baseline assets                                                     ####
function(){
  if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

  piggyback::pb_release_create(
    repo = "ftsiboe/indexDesignWindows",
    tag  = "baseline",
    name = "Baseline Outputs",
    body = paste(
      "This release contains outputs from alternative PRF index design experiments baseline.",
      sep = "\n"
    )
  )

  piggyback::pb_upload(
    list.files(output_directory, full.names = TRUE, recursive = T),
    repo  = "ftsiboe/indexDesignWindows",
    tag   = "baseline",
    overwrite = TRUE
  )
}
#-------------------------------------------------------------------------------


