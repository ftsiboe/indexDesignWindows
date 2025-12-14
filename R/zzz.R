.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # Register global variables used by data.table (silence R CMD check NOTES)
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        ". ..cc_cols ..common_cols ..keep_cols ..r_cols ..to_drop AandO
    CAT_Fees CAT_LAE CLRCAT CLRmCAT COUNTY CRD CROP ERSReg_cd Expo Fu
    Fu_BU Fu_EU IDM_Ced IDM_Ret N PF PPIPR PRM_Ced PRM_Ret
    RMA_Appropriations Rd Rd50 Rd55 Rd60 Rd65 Rd70 Rd75 Rd80 Rd85 Rf Rr
    Rr_max TOPCROP UID UWG UWL Ycr adm_year aggregation
    alternate_premium_per_liability ao50 ao55 ao60 ao65 ao70 ao75 ao80
    ao85 ao_rate applied area area_actual area_insured area_insuredW
    arf_premium arf_retained_premium arf_retention as.formula assumption
    b0 b1 b2 b3 b4 b5 baseline_indemnity_per_acre
    baseline_liability_per_acre baseline_premium_per_liability
    baseline_subsidy_per_premium cExpo cRIDlvl cRIDlvlpct cRLR_old
    cRLRlvl cRLRlvlpct cRPMlvl cRPMlvlpct cRf cRr cYcr ceded ceded_NUW
    ceded_indemnity ceded_lr ceded_lr_old ceded_premium cf_premium
    cf_retained_premium cf_retention commodity_code commodity_group
    commodity_name commodity_year commodity_year_n county_code
    coverage_level_percent coverage_type_code cum_indemnity
    cum_indemnity_prev cum_lr cum_premium_prev cum_total_premium cumw
    damage damage_acre damage_acre_share damage_indem damage_indem_share
    damage_rc damage_u disag disag_level dollars dummy
    endorsed_commodity_reporting_level_amount
    enterprise_unit_residual_factor est eval_adm eval_window
    exponent_value fcic_NUW file_name fips fixed_rate flag fund fund_abb
    gamma_elasticity gross_area gross_indm gross_liab gross_n gross_prem
    gross_premium has_pos head indemnity indemnity_amount indm indmR
    insurance_plan_code insured_acres irrigation_recode liab liabR
    liab_actual liability liability_amount lower lr max_prior min_prior
    net_determined_quantity net_reported_quantity
    net_reporting_level_amount old_r11 organic_recode pExpo pRf pRr pYcr
    period_farmbill ppNAWlvl ppNAWpct practice_code prem premR premium
    premium_cum_sum premium_dif premium_percentile prev price_change_pct
    program_age_by_year r00 r01 r02 r03 r04 r05 r11 rPCT_area
    rPCT_area_old rPCT_idmn rPCT_liab rPCT_pool rPCT_prem rPCT_prem_arf
    rPCT_prem_arf_retained rPCT_prem_cf rPCT_prem_cf_retained rYield
    rate_differential_factor reference_amount reference_rate
    reinsurance_year reporting_level_type retain retain_ARF retain_CF
    retain_UWG_aip retain_UWG_fcic retain_UWL_aip retain_UWL_fcic
    retain_aip_wr retain_area retain_fcic_wr retain_indm retain_liab
    retain_n retain_prem retained retained_NUW_aip retained_NUW_fcic
    retained_UWG_aip retained_UWG_fcic retained_UWL_aip retained_UWL_fcic
    retained_area retained_indemnity retained_liability retained_lr
    retained_lr_old retained_n retained_premium retention
    revenue_per_acre sbdyR scenario setNames share sp50 sp55 sp60 sp65
    sp70 sp75 sp80 sp85 state state_abbreviation state_code
    state_total_gross_premium state_yr_fund_premiums state_yr_premiums
    study subsidy subsidy_amount subsidy_bins t_share theta_elasticity
    total_premium total_premium_amount type type_code
    unit_residual_factor unit_structure_code upper value value0 value10
    value20 value30 value_type variable width xrate00 xrate10 xrate20
    xrate30 yr_max_thru yr_min_thru Estimate county_acreage coverage_level_percent_aggregate demand
    drawID n na.omit pool price rate rent rp_eligible subsidy_rate_65
    subsidy_rate_75 tau tau0 theta trend baseline_rate_differential baseline_subsidy_percent comm cum_lr_100
    index index_for_price_recived",
        "\\s+"
      )[[1]]
    )
  }
}




