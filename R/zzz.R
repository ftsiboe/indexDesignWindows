.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # Register global variables used by data.table (silence R CMD check NOTES)
  xx <- " .data aes coord_sf disaggregate element_blank element_text facet_wrap
    geom_sf get_official_prf_polygon ggplot guide_legend guides labs
    scale_fill_manual theme theme_bw . PPIPR comm commodity_year index index_for_price_recived value
  area_km2 centroid cx cy geometry is_small label ranking state_abbv
     value_cat ..expected_cols county_fips diag_mean diag_var fitted_log_var
    fitted_var joint_fisher_p_value joint_fisher_stat log_resid_sq
    model_estimator panelid precipitation resid_mean resid_sq residuals
    sqrt_w state_code term trend var_weight"

  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        xx,
        "\\s+"
      )[[1]]
    )
  }
}




