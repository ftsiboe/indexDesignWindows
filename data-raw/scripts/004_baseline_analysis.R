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
# Plot - RMA Rate Discretion Factor                                          ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- readRDS(file.path(output_directory,"rma_rate_discretion_factor.rds"))

data <- add_break_categories(
  data = data,
  variable = "rma_discretion_factor",
  break_levels = c(-Inf,seq(0.25,2,0.25),Inf),
  break_labels = c("Less than 0.25","0.25 to 0.50","0.50 to 0.75","0.75 to 1.00",
                   "1.00 to 1.25","1.25 to 1.50","1.50 to 1.75","1.75 to 2.00","Greater than 2.00"))

fig_rma_factor <- plot_prf_statistics(
  data = data,
  outcome_variable = "rma_discretion_factor_cat",
  disaggregate_variable = "interval_name",
  plot_title = NULL,
  spatial_unit ="county",
  palette = c(
    "firebrick",
    "#BE5E27",
    "#FFC425",
    "#FEF389",
    "#C5DE91",
    "#BED73B",
    "#A0BD78",
    "#00583D",
    "#0F374B"
  )) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) + facet_wrap(~interval_name,nrow=3)

ggsave(file.path("data-raw/output/figure","rma_rate_discretion_factor.png"),
       fig_rma_factor, dpi = 600,width = 8, height =5)

#-------------------------------------------------------------------------------
# Plot - Baseline Summary Statistics - PRF index                             ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- as.data.frame(readRDS(file.path(output_directory,"baseline_balance_index.rds")))

fig_baseline_summary <- lapply(
  names(statistics_list),
  function(i){
    tryCatch({
      # i <- "mean"
      data <- reshape_statistics(
        data = data,
        statistic_variables = paste0(i,c("_x","_y")),
        panel_variables = c("state_code", "county_code","county_fips"),
        disaggregate_labels = c("Official (RMA)", "Study (CPC-raw)"),
        rename = "mean")

      # data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
      # data <- data[
      #   ,.(mean = weighted.mean(x=mean,w=potential_range_pasture, na.rm=T)),
      #   by=c("state_code", "county_code","county_fips","disaggregate")]

      data <- add_break_categories(data = data, variable = "mean",break_n = 10)

      p <- plot_prf_statistics(
        data = data,
        outcome_variable = "mean_cat",
        disaggregate_variable = "disaggregate",
        spatial_unit ="county",
        plot_title = paste0(myline,statistics_list[i],myline))

      p

    }, error = function(e){NULL})
  })
fig_baseline_summary <- Filter(Negate(is.null), fig_baseline_summary)
fig_baseline_summary <- cowplot::plot_grid(plotlist = fig_baseline_summary ,ncol=2, align="v", greedy=T)
fig_baseline_summary <- cowplot::ggdraw() +
  cowplot::draw_label("Overall Title", fontface = "bold", size = 16) +
  cowplot::draw_plot(fig_baseline_summary)
ggsave(file.path("data-raw/output/figure","baseline_summary_index.png"),
       fig_baseline_summary, dpi = 600,width = 7.9, height =6.784)
#-------------------------------------------------------------------------------
# Plot - Baseline Summary Statistics - PRF Payment factor                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- as.data.frame(readRDS(file.path(output_directory,"baseline_balance_payment_factor.rds")))

lapply(
  unique(data$coverage_level_percent*100),
  function(cov){
    tryCatch({
      #
      fig_baseline_summary <- lapply(
        names(statistics_list),
        function(i){
          tryCatch({
            # i <- "mean";cov <- 80
            data <- reshape_statistics(
              data = data[round(data$coverage_level_percent*100) %in% cov,],
              statistic_variables = paste0(i,c("_x","_y")),
              panel_variables = c("state_code", "county_code","county_fips"),
              disaggregate_labels = c("Official (RMA)", "Study (CPC-raw)"),
              rename = "mean")

            # data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
            # data <- data[
            #   ,.(mean = weighted.mean(x=mean,w=potential_range_pasture, na.rm=T)),
            #   by=c("state_code", "county_code","county_fips","disaggregate")]

            data <- add_break_categories(data = data, variable = "mean",break_n = 10)

            p <- plot_prf_statistics(
              data = data,
              outcome_variable = "mean_cat",
              disaggregate_variable = "disaggregate",
              spatial_unit ="county",
              plot_title = paste0(myline,statistics_list[i],myline))

            p

          }, error = function(e){NULL})
        })
      fig_baseline_summary <- Filter(Negate(is.null), fig_baseline_summary)
      fig_baseline_summary <- cowplot::plot_grid(plotlist = fig_baseline_summary ,ncol=2, align="v", greedy=T)
      fig_baseline_summary <- cowplot::ggdraw() +
        cowplot::draw_label("Overall Title", fontface = "bold", size = 16) +
        cowplot::draw_plot(fig_baseline_summary)
      ggsave(file.path("data-raw/output/figure",
                       paste0("baseline_summary_payment_factor_",cov,".png")),
             fig_baseline_summary, dpi = 600,width = 7.9, height =6.784)
    }, error = function(e){NULL})
  })
#-------------------------------------------------------------------------------
# Plot - Baseline Summary Statistics - PRF base rate                         ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- as.data.frame(readRDS(file.path(output_directory,"baseline_balance_rate.rds")))

data <- data[c("state_code", "county_code","county_fips","coverage_level_percent","y_level",paste0(names(statistics_list),"_x"),paste0(names(statistics_list),"_y"))]
data <- data |> tidyr::gather(statistic, value, paste0(names(statistics_list),"_y"))
data$statistic <- paste0(data$statistic,"_",data$y_level)
data <- data[c("state_code", "county_code","county_fips","coverage_level_percent",paste0(names(statistics_list),"_x"),"statistic","value")] |>
  tidyr::spread(statistic, value)

lapply(
  unique(data$coverage_level_percent*100),
  function(cov){
    tryCatch({
      # cov <- 90
      fig_baseline_summary <- lapply(
        names(statistics_list),
        function(i){
          tryCatch({
            # i <- "mean";cov <- 90
            data <- reshape_statistics(
              data = data[round(data$coverage_level_percent*100) %in% cov,],
              statistic_variables = paste0(i,c("_x","_y_cpc","_y_cpcAdj")),
              panel_variables = c("state_code", "county_code","county_fips"),
              disaggregate_labels = c("Official (RMA)", "Study (CPC-raw)", "Study (CPC-adjusted)"),
              rename = "mean")

            # data <- prf_grid_weights[data,on = intersect(names(data), names(prf_grid_weights)),nomatch = 0]
            # data <- data[
            #   ,.(mean = weighted.mean(x=mean,w=potential_range_pasture, na.rm=T)),
            #   by=c("state_code", "county_code","county_fips","disaggregate")]

            data <- add_break_categories(data = data, variable = "mean",break_n = 10)

            p <- plot_prf_statistics(
              data = data,
              outcome_variable = "mean_cat",
              disaggregate_variable = "disaggregate",
              spatial_unit ="county",
              plot_title = paste0(myline,statistics_list[i],myline))+
              facet_wrap(
                stats::as.formula(paste("~", "disaggregate")),
                nrow = 1
              )

            p

            }, error = function(e){NULL})
        })
      fig_baseline_summary <- Filter(Negate(is.null), fig_baseline_summary)
      fig_baseline_summary <- cowplot::plot_grid(plotlist = fig_baseline_summary ,ncol=2, align="v", greedy=T)
      fig_baseline_summary <- cowplot::ggdraw() +
        cowplot::draw_label("Overall Title", fontface = "bold", size = 16) +
        cowplot::draw_plot(fig_baseline_summary)
      ggsave(file.path("data-raw/output/figure",
                       paste0("baseline_summary_base_rate_",cov,".png")),
             fig_baseline_summary, dpi = 600,width = 7.9, height =5.56)

    }, error = function(e){NULL})
  })

#-------------------------------------------------------------------------------
# Plot - Baseline Balance Statistics                                         ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
prf_index <- as.data.frame(readRDS(file.path(output_directory,"baseline_balance_index.rds")))
prf_rates <- as.data.frame(readRDS(file.path(output_directory,"baseline_balance_rate.rds")))
prf_pcf <- as.data.frame(readRDS(file.path(output_directory,"baseline_balance_payment_factor.rds")))

xlist <- c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks",
           "pvalue_pearson_cor","pvalue_kendall_cor")

df_index <- prf_index[c(xlist,"state_code", "county_code","county_fips")]
df_rates <- prf_rates[c(xlist,"state_code", "county_code","county_fips","coverage_level_percent","y_level")]
df_pcf   <- prf_pcf[  c(xlist,"state_code", "county_code","county_fips","coverage_level_percent")]

df_index$disaggregate <- "Index"
df_rates$disaggregate <- ifelse(df_rates$y_level %in% "cpc",
                                 paste0("Base rate (raw) at ",df_rates$coverage_level_percent*100,"%"),
                                 paste0("Base rate (adjusted) at ",df_rates$coverage_level_percent*100,"%"))
df_pcf$disaggregate   <- paste0("Payment factor at ",df_pcf$coverage_level_percent*100,"%")

data <- rbind(df_index[names(df_index)],
              df_rates[round(df_rates$coverage_level_percent*100) %in% 90, names(df_index)],
              df_pcf[round(df_pcf$coverage_level_percent*100) %in% 90,names(df_index)])

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


fig_baseline_balance <- plot_prf_statistics(
  data = data,
  outcome_variable = "value_cat",
  disaggregate_variable = "disaggregate",
  plot_title = NULL,
  spatial_unit ="county",
  palette = c(
    "#BE5E27", # Rust
    "#FFC425", # NDSU Yellow
    "#BED73B", # Sage
    "#A0BD78", # NDSU Green
    "#00583D"  # Dark Green
  )) +
  labs(fill = "p-value")  +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 9, hjust = 0.5)
  ) +
  facet_grid(disaggregate~variable_name)

ggsave(file.path("data-raw/output/figure","baseline_balance.png"),
       fig_baseline_balance, dpi = 600,width = 12, height =6)

for(pp in c(0.01,0.05,0.10)){
  data[,paste0("pvalue",stringr::str_pad(pp*100,pad="0",3))] <- as.numeric(!data$value <= pp)
}

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

data_rates <- as.data.table(data)[
  disaggregate %in% c("Base rate (raw)\nfor 90% coverage level",
                      "Base rate (adjusted)\nfor 90% coverage level",
                      "Payment factor\nfor 90% coverage level") &
    variable %in% c("pvalue_mean","pvalue_var","pvalue_kruskal_wallis","pvalue_ks")
  , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = c("disaggregate", "variable_name"),
  .SDcols = c("pvalue001",  "pvalue005",  "pvalue010")] |>
  tidyr::gather(p_value_range, prop, c("pvalue001",  "pvalue005",  "pvalue010"))



data_rates$p_value_range <- factor(
  data_rates$p_value_range,
  levels = c("pvalue001",  "pvalue005",  "pvalue010"),
  labels = c("p > 0.01",  "p > 0.05",  "p > 0.10")
)

fig <- ggplot(data_rates, aes(
  x = disaggregate,
  y = prop,
  fill = p_value_range,
  group = p_value_range
)) +
  geom_bar(
    stat = "identity",position = position_dodge(width = 0.8),width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of counties with specified p-value range",
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

ggsave(file.path("data-raw/output/figure","baseline_balance_proportion.png"),
       fig, dpi = 600,width = 5, height =8)

#-------------------------------------------------------------------------------

