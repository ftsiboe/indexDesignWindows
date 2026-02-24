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
# Plot - RMA Rate Discretion Factor                                          ####
rm(list= ls()[!(ls() %in% c(Keep.List))]);gc();gc()
data <- readRDS(file.path(output_directory,"rma_rate_discretion_factor.rds"))

data <- add_break_categories(
  data = data,
  variable = "rate_discretion_factor",
  break_levels = c(-Inf,seq(0.25,2,0.25),Inf),
  break_labels = c("Less than 0.25","0.25 to 0.50","0.50 to 0.75","0.75 to 1.00",
                   "1.00 to 1.25","1.25 to 1.50","1.50 to 1.75","1.75 to 2.00","Greater than 2.00"))

fig_rma_factor <- plot_prf_statistics(
  data = data,
  outcome_variable = "rate_discretion_factor_cat",
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
        panel_variables = c("state_code", "county_code","county_fips","y_level"),
        disaggregate_labels = c("Official (RMA)", "Study (CPC-raw)"),
        rename = "mean")
      data <- as.data.table(data)
      data[, disaggregate := as.character(disaggregate)]
      data[y_level %in% "cpcAdj01" & disaggregate %in% "Study (CPC-raw)", disaggregate := "Study (CPC-adjusted)"]
      data <- as.data.frame(data)
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

