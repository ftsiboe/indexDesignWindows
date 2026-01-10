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
# Estimate RMA Rate Discretion Factor                                        ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()

cpc_rates <- data.table::rbindlist(
  lapply(
    list.files(file.path(redesigns_directory,"200"),pattern = "prf_rates_",full.names = T),
    function(i) {
      tryCatch({
        readRDS(i)[
          , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          by = c("grid_id","interval_code","commodity_year","coverage_level"),
          .SDcols = c("burnR.r","RMAtn.r","BSlnm.r","tnorm.r","lnorm.r","invgu.r","gamma.r","wibll.r","beta.r","llogis.r","raw_rate","base_rate")]
      }, error = function(e){NULL})
    }),fill = TRUE)
cpc_rates[,coverage_level:=round(coverage_level*100)]

rma_rates <- readRDS("data/grid_level_official_prf_adm.rds")[
  ,.(rma_base_rate = mean(rma_base_rate, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year","coverage_level_percent","coverage_level")]

data <- cpc_rates[rma_rates,on = intersect(names(cpc_rates), names(rma_rates)),nomatch = 0]
data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
data <- data[
  ,.(rma_base_rate = weighted.mean(x=rma_base_rate,w=potential_range_pasture, na.rm=T),
     base_rate     = weighted.mean(x=base_rate,w=potential_range_pasture, na.rm=T)),
  by=c("commodity_year","state_code", "county_code","county_fips",
       "interval_code","coverage_level_percent","coverage_level")]

# Weighted least squares
data <- data[
  ,.(rma_discretion_factor_all = coef(lm(rma_base_rate~base_rate - 1))),
  by=c("state_code", "county_code","county_fips")][
    data[
      ,.(rma_discretion_factor = coef(lm(rma_base_rate~base_rate - 1))),
      by=c("state_code", "county_code","county_fips","interval_code")],
    on=c("state_code", "county_code","county_fips"),nomatch = 0]

data <- data |> tidyr::spread(interval_code, rma_discretion_factor)
data[,"600"] <- data$rma_discretion_factor_all
data <- data |> tidyr::gather(interval_code, rma_discretion_factor, names(data)[!names(data) %in% c("state_code", "county_code","county_fips","rma_discretion_factor_all")])
data$rma_discretion_factor <- ifelse(data$rma_discretion_factor %in% NA,data$rma_discretion_factor_all,data$rma_discretion_factor)
data$interval_code <- as.numeric(data$interval_code)

intervalKey <- readRDS("data/official_interval_names.rds")

data$interval_name <- factor(
  data$interval_code,
  levels = c(600,intervalKey$interval_code),
  labels = c("All intervals",intervalKey$interval_name))

saveRDS(as.data.table(data),file.path(output_directory,"rma_rate_discretion_factor.rds"))

#-------------------------------------------------------------------------------
# Baseline Data                                                              ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()

cpc_index <- readRDS(file.path(redesigns_directory,"200/prf_index_200.rds"))[
  commodity_year %in% study_environment$year_beg:study_environment$year_end,.(cpc = mean(index, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year")]

rma_index <- readRDS("data/official_prf_index.rds")

data <- cpc_index[rma_index,on = intersect(names(cpc_index), names(rma_index)),nomatch = 0]
data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
data <- data[
  ,.(cpc = round(weighted.mean(x=cpc,w=potential_range_pasture, na.rm=T),3),
     rma = round(weighted.mean(x=rma_index,w=potential_range_pasture, na.rm=T),3)),
  by=c("commodity_year","state_code", "county_code","county_fips","interval_code")]

prf_baseline_balance_index <- compute_balance(
  data,
  col_x="rma", col_y="cpc", by = c("state_code", "county_code","county_fips"))
saveRDS(prf_baseline_balance_index,file.path(output_directory,"baseline_balance_index.rds"))

cpc_rates <- data.table::rbindlist(
  lapply(
    list.files(file.path(redesigns_directory,"200"),pattern = "prf_rates_",full.names = T),
    function(i) {
      tryCatch({
        readRDS(i)[
          ,.(cpc_base_rate = mean(base_rate, na.rm=T)),
          by=c("grid_id","interval_code","commodity_year","coverage_level")]
      }, error = function(e){NULL})
    }),fill = TRUE)

cpc_rates <- cpc_index[cpc_rates,on = intersect(names(cpc_rates), names(cpc_index)),nomatch = 0]
cpc_rates[,cpc_payment_factor := ifelse(
  cpc >= coverage_level, 0,
  coverage_level - cpc) / coverage_level ]

cpc_rates[,coverage_level:=round(coverage_level*100)]

rma_rates <- readRDS("data/grid_level_official_prf_adm.rds")[
  ,.(rma_base_rate = mean(rma_base_rate, na.rm=T),
     rma_payment_factor = mean(rma_payment_factor, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year","coverage_level_percent","coverage_level")]

data <- cpc_rates[rma_rates,on = intersect(names(cpc_rates), names(rma_rates)),nomatch = 0]

data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
data <- data[
  ,.(rma_base_rate = round(weighted.mean(x=rma_base_rate,w=potential_range_pasture, na.rm=T),4),
     cpc_base_rate = round(weighted.mean(x=cpc_base_rate,w=potential_range_pasture, na.rm=T),4),
     rma_payment_factor = round(weighted.mean(x=rma_payment_factor,w=potential_range_pasture, na.rm=T),3),
     cpc_payment_factor = round(weighted.mean(x=cpc_payment_factor,w=potential_range_pasture, na.rm=T),3)),
  by=c("commodity_year","state_code", "county_code","county_fips",
       "interval_code","coverage_level_percent","coverage_level")]

data <- data[
  readRDS(file.path(output_directory,"rma_rate_discretion_factor.rds")),
  on = c("state_code", "county_code","county_fips","interval_code"),nomatch = 0]

data[,cpc_base_rate_adj := round(cpc_base_rate*rma_discretion_factor,4)]
saveRDS(data,file.path(output_directory,"baseline_adm.rds"))

prf_baseline_balance_pcf <- compute_balance(
  data[
    ,.(rma = round(mean(rma_payment_factor, na.rm=T),3),
       cpc = round(mean(cpc_payment_factor, na.rm=T),3)),
    by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level_percent")],
  col_x="rma", col_y="cpc", by = c("state_code", "county_code","county_fips","coverage_level_percent"))
saveRDS(prf_baseline_balance_pcf,file.path(output_directory,"baseline_balance_payment_factor.rds"))

prf_baseline_balance_rate <- compute_balance(
  data[
    ,.(rma    = round(mean(rma_base_rate, na.rm=T),4),
       cpc    = round(mean(cpc_base_rate, na.rm=T),4),
       cpcAdj = round(mean(cpc_base_rate_adj, na.rm=T),4)),
    by=c("state_code", "county_code","county_fips","interval_code","commodity_year","coverage_level_percent")],
  col_x="rma", col_y=c("cpc","cpcAdj"), by = c("state_code", "county_code","county_fips","coverage_level_percent"))
saveRDS(prf_baseline_balance_rate,file.path(output_directory,"baseline_balance_rate.rds"))

#-------------------------------------------------------------------------------
# Upload Baseline assets                                                     ####
function(){
  if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)
  piggyback::pb_upload(
    list.files(output_directory, full.names = TRUE, recursive = T),
    repo  = "ftsiboe/indexDesignWindows",
    tag   = "baseline",
    overwrite = TRUE
  )
}
#-------------------------------------------------------------------------------


