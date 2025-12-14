rm(list = ls(all = TRUE));library(data.table);library(future.apply);gc()

#devtools::document()

source("R/compute_balance.R")

if (Sys.info()['sysname'] %in% "Windows") {
  array_list <- 9
  available_cores <- min(c(round(availableCores() * 0.50),array_list))
}else{
  available_cores <- availableCores()
}

output_directory <- "data-raw/releases/prf_history_redesign/statistical_threshold"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

alternative_list <- list.files("data-raw/releases/prf_history_redesign",pattern = "prf_rates_",full.names = T,recursive = T)
alternative_list <- alternative_list[!grepl("200",alternative_list)]
#alternative_list <- alternative_list[grepl("005|010|015|020|025|030|035|040|045|050|055|060",alternative_list)]

baseline_program <- readRDS("data-raw/releases/prf_history_redesign/200/prf_index_200.rds")[
  commodity_year %in% 2016:2024,.(baseline_index = mean(index, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year")]

baseline_program <- baseline_program[
  readRDS("data-raw/releases/prf_history_redesign/baseline/baseline_rates_and_pcf.rds"),
  on = c("grid_id","interval_code","commodity_year"),nomatch = 0]

baseline_program <- baseline_program[
  ,.(baseline_index = mean(baseline_index, na.rm=T),
     baseline_base_rate = mean(base_rate, na.rm=T),
     baseline_base_rate_adj = mean(cpc_base_rate_adj, na.rm=T),
     baseline_payment_factor = mean(payment_factor, na.rm=T)),
  by=c("grid_id","interval_code","commodity_year","coverage_level")]

plan(sequential)
plan(list(tweak(multisession, workers = available_cores)));gc()

future_lapply(
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
                # i <- alternative_list[1]
                cpc_rates <- readRDS(i)[
                  ,.(alternative_base_rate = mean(base_rate, na.rm=T)),
                  by=c("grid_id","interval_code","commodity_year","coverage_level","index_history_range")]
                cpc_rates <- cpc_rates[
                  readRDS("data-raw/releases/prf_history_redesign/baseline/rma_rate_discretion_factor.rds"),
                  on = c("grid_id","interval_code"),nomatch = 0]
                cpc_rates[,alternative_base_rate_adj := alternative_base_rate*rma_factor]
                
                cpc_index <- readRDS(list.files(dirname(i),pattern = "prf_index_",full.names = T))[
                  commodity_year %in% unique(cpc_rates$commodity_year),.(alternative_index = mean(index, na.rm=T)),
                  by=c("grid_id","interval_code")]
                
                cpc_rates <- cpc_index[cpc_rates,on = intersect(names(cpc_rates), names(cpc_index)),nomatch = 0]
                cpc_rates[,alternative_payment_factor := ifelse(
                  alternative_index >= coverage_level, 0,
                  coverage_level - alternative_index) / coverage_level ]
                
                cpc_rates <- cpc_rates[
                  ,.(alternative_index = mean(alternative_index, na.rm=T),
                     alternative_base_rate = mean(alternative_base_rate, na.rm=T),
                     alternative_base_rate_adj = mean(alternative_base_rate_adj, na.rm=T),
                     alternative_payment_factor = mean(alternative_payment_factor, na.rm=T)),
                  by=c("grid_id","interval_code","commodity_year","coverage_level","index_history_range")]
                
                cpc_rates
              }, error = function(e){NULL})
            }),fill = TRUE)
        data[,coverage_level:=round(coverage_level*100)]
        
        data <- baseline_program[data,on = intersect(names(data), names(baseline_program)),nomatch = 0]
        
        balance_index <- compute_balance(
          dt=data[
            ,.(alternative_index = mean(alternative_index, na.rm=T),
               baseline_index = mean(baseline_index, na.rm=T)),
            by=c("grid_id","interval_code","commodity_year","coverage_level","index_history_range")], 
          col_x="baseline_index", col_y="alternative_index", 
          by = c("grid_id","index_history_range"))
        
        balance_base_rate <- compute_balance(
          dt=data, 
          col_x="baseline_base_rate", col_y="alternative_base_rate", 
          by = c("grid_id","index_history_range","coverage_level"))
        
        balance_base_rate_adj <- compute_balance(
          dt=data, 
          col_x="baseline_base_rate_adj", col_y="alternative_base_rate_adj", 
          by = c("grid_id","index_history_range","coverage_level"))
        
        balance_payment_factor <- compute_balance(
          dt=data, 
          col_x="baseline_payment_factor", col_y="alternative_payment_factor", 
          by = c("grid_id","index_history_range","coverage_level"))
        
        balance_final <- data.table::rbindlist(
          list(balance_index,balance_base_rate,balance_base_rate_adj,balance_payment_factor),fill = TRUE)
        
        rm(balance_index,balance_base_rate,balance_base_rate_adj,balance_payment_factor,data);gc();gc()
        
        saveRDS(as.data.table(balance_final),out_file)
      }
    }, error = function(e){NULL})
  })
plan(sequential)





#-------------------------------------------------------------------------------
# Baseline Data                                                              ####
library(ggplot2);library(gridExtra)
library(terra)
library(dplyr)
library(sf)
library(cowplot)
data <- as.data.frame(readRDS("data-raw/releases/prf_history_redesign/results_statistical_threshold.rds"))
xlist <- c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks")
xlist <- c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
           "pvalue_pearson_cor","pvalue_kendall_cor")

data <- data[c(xlist,"index_history_range","grid_id","coverage_level","y_level")]
data$disaggregate <- as.character(
  factor(
    data$y_level,
    levels = c( "alternative_index","alternative_base_rate","alternative_base_rate_adj","alternative_payment_factor"),
    labels = c( "Index","Base rate (raw) at ","Base rate (adjusted) at ","Payment factor at ")
  )
)

data$disaggregate <- ifelse(data$disaggregate %in% "Index",data$disaggregate,paste0(data$disaggregate,data$coverage_level,"%"))

data <- rbind(data[data$disaggregate %in% "Index",],
              data[round(data$coverage_level) %in% 90,],
              data[round(data$coverage_level) %in% 90,])

data$disaggregate <- factor(
  data$disaggregate,
  levels = c("Index","Base rate (raw) at 90%","Base rate (adjusted) at 90%","Payment factor at 90%"),
  labels = c("Index","Base rate (raw)\nfor 90% coverage level",
             "Base rate (adjusted)\nfor 90% coverage level",
             "Payment factor\nfor 90% coverage level")
)

data <- data |> 
  tidyr::gather(variable, value, xlist)

data <- add_break_categories(
  data = data, 
  variable = "value",
  break_levels = c(0,0.01,0.05,0.10,0.50,1),
  break_labels = c("Less than 0.01","0.01 to 0.05","0.05 to 0.10","0.10 to 0.50","Greater than 0.50"))

data$variable_name <- factor(
  data$variable,
  levels = c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
             "pvalue_pearson_cor","pvalue_kendall_cor"),
  labels = c("Mean equality t-test",
             "F-test of equal variances",
             "Kruskal-Wallis test of equal variances",
             "Kolmogorov-Smirnov test",
             "Pearson correlation",
             "Kendall's tau")
)

for(pp in c(0.01,0.05,0.10)){
  data[,paste0("pvalue",stringr::str_pad(pp*100,pad="0",3))] <- as.numeric(!data$value <= pp)
}

data_rates <- as.data.table(data)[
  disaggregate %in% c("Base rate (raw)\nfor 90% coverage level",
                      "Base rate (adjusted)\nfor 90% coverage level",
                      "Payment factor\nfor 90% coverage level") & 
    variable %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks")
  , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = c("disaggregate", "variable_name","index_history_range"),
  .SDcols = c("pvalue001",  "pvalue005",  "pvalue010")] |> 
  tidyr::gather(p_value_range, prop, c("pvalue001",  "pvalue005",  "pvalue010"))


data_rates$p_value_range <- factor(
  data_rates$p_value_range,
  levels = c("pvalue001",  "pvalue005",  "pvalue010"),
  labels = c("p > 0.01",  "p > 0.05",  "p > 0.10")
)

fig <- ggplot(
  data_rates[data_rates$p_value_range %in% "p > 0.10",]
  , aes(
    x = disaggregate,
    y = prop,
    fill = index_history_range,
    group = index_history_range
  )) +
  geom_bar(
    stat = "identity",position = position_dodge(width = 0.8),width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of grids with specified p-value range",
    x = "",
    y = "Proportion"
  ) +
  scale_color_manual(
    values = c("#00583D", "#FFC425","#9DD9F7"),
    na.value = "white") +
  scale_fill_manual(
    values = c("#00583D", "#FFC425","#9DD9F7"),
    na.value = "white") +
  facet_wrap(~ variable_name, ncol = 1) +
  ers_theme() +
  theme(
    plot.caption     = element_blank(),
    axis.title.y     = element_blank(),
    axis.title.x     = element_blank(),
    axis.text.y      = element_text(size=8),
    axis.text.x      = element_text(size=8, color="black"),
    legend.position  = "top",
    legend.key.size  = unit(0.5,"cm"),
    legend.background = element_blank(),
    legend.title     = element_blank(),
    legend.text      = element_text(size= 10),
    strip.text       = element_text(size = 10),
    strip.background = element_blank())

ggsave(file.path("data-raw/output/figure/prf_history_redesign","prf_alternative_balance_proportion.png"), 
       fig, dpi = 600,width = 5, height =8)

