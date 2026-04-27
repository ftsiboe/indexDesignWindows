# ============================================================
# Precipitation Mean-Variance Trend Functions
# ============================================================
# This file contains helper functions for estimating county-level
# precipitation trends in both the conditional mean and conditional
# variance using panel models, residual-based variance modeling,
# Fisher combined tests, and trend classification.
# ============================================================

#' Extract diagnostics from a panel model
#'
#' Extracts basic model diagnostics from a fitted `plm` model object.
#' The function returns sample size, panel dimensions, R-squared values,
#' residual sum of squares, and root mean squared error.
#'
#' @param obj A fitted `plm` model object.
#'
#' @return A `data.table` with one row containing:
#' \describe{
#'   \item{n_obs}{Number of observations used in the model.}
#'   \item{n_panel}{Number of unique panel units.}
#'   \item{n_years}{Number of unique time periods.}
#'   \item{r_squared}{Model R-squared, if available.}
#'   \item{adj_r_squared}{Adjusted R-squared, if available.}
#'   \item{rss}{Residual sum of squares.}
#'   \item{rmse}{Root mean squared error.}
#' }
#'
#' @details
#' This helper is designed for `plm` objects. R-squared values are extracted
#' from `summary(obj)$r.squared` when available. Residual-based diagnostics
#' are computed directly from the model residuals. Panel dimensions are
#' recovered from `plm::index(obj)`.
#'
#' @export
extract_model_diagnostics <- function(obj) {

  smry <- summary(obj)

  r2_values <- tryCatch(
    smry$r.squared,
    error = function(e) NULL
  )

  r_squared <- NA_real_
  adj_r_squared <- NA_real_

  if (!is.null(r2_values)) {

    if ("rsq" %in% names(r2_values)) {
      r_squared <- as.numeric(r2_values["rsq"])
    }

    if ("adjrsq" %in% names(r2_values)) {
      adj_r_squared <- as.numeric(r2_values["adjrsq"])
    }
  }

  residuals_i <- as.numeric(residuals(obj))

  rss  <- sum(residuals_i^2, na.rm = TRUE)
  rmse <- sqrt(mean(residuals_i^2, na.rm = TRUE))

  panel_index <- plm::index(obj)

  data.table::data.table(
    n_obs         = stats::nobs(obj),
    n_panel       = length(unique(panel_index[[1]])),
    n_years       = length(unique(panel_index[[2]])),
    r_squared     = r_squared,
    adj_r_squared = adj_r_squared,
    rss           = rss,
    rmse          = rmse
  )
}



#' Estimate mean and variance trends for one state using an FGLS-style workflow
#'
#' Estimates a panel-data mean model, uses the residuals from that model to
#' estimate a variance model, constructs inverse-variance weights, and then
#' re-estimates the mean model after applying the weights directly to the data.
#'
#' @param dt A `data.table` or data frame containing the state-level panel data.
#'   The data must include `panelid`, `commodity_year`, `precipitation`,
#'   `trend`, and `county_fips`.
#' @param formula A model formula for the mean equation. In the current workflow,
#'   this is expected to be `precipitation ~ trend:factor(county_fips)`.
#' @param model Character string specifying the `plm` estimator. Common values
#'   include `"within"`, `"pooling"`, `"random"`, `"between"`, and `"fd"`.
#' @param eps Small positive value added to squared residuals and predicted
#'   variances to avoid taking `log(0)` or dividing by zero. Default is `1e-8`.
#'
#' @return A list containing:
#' \describe{
#'   \item{mean_model_initial}{Initial fitted mean model.}
#'   \item{variance_model}{Fitted residual-based log-variance model.}
#'   \item{mean_model_weighted}{FGLS-style pre-weighted fitted mean model.}
#'   \item{data}{Estimation sample used for the variance model.}
#'   \item{data_weighted}{Pre-weighted estimation sample used for the final mean model.}
#' }
#' Returns `NULL` if the state does not have enough panel units or time periods,
#' or if any model estimation step fails.
#'
#' @details
#' The workflow proceeds in six steps:
#'
#' \enumerate{
#'   \item Estimate an initial panel mean model.
#'   \item Extract residuals from the initial mean model.
#'   \item Construct a log-squared residual outcome:
#'         `log_resid_sq = log(residual^2 + eps)`.
#'   \item Estimate a variance equation using the same RHS variables as the
#'         mean equation.
#'   \item Predict the conditional variance and construct inverse-variance
#'         weights.
#'   \item Multiply `precipitation` and `trend` by `sqrt(weight)` and re-estimate
#'         the mean model without using the `weights` argument in `plm`.
#' }
#'
#' This pre-weighting strategy avoids the `plm` limitation that robust covariance
#' functions such as `vcovDC()` and `vcovHC()` are not implemented for weighted
#' panel regressions.
#'
#' @section Important:
#' The pre-weighting block is currently hard-coded for the model:
#'
#' `precipitation ~ trend:factor(county_fips)`
#'
#' If the outcome variable or trend variable changes, the pre-weighting block
#' must be updated accordingly.
#' @family Precipitation trend_analysis
#' @export
estimate_one_state_fgls <- function(dt, formula, model = "within", eps = 1e-8) {

  dt <- data.table::copy(dt)

  if (length(unique(dt$panelid)) < 2L) return(NULL)
  if (length(unique(dt$commodity_year)) < 3L) return(NULL)

  obj_mean_1 <- tryCatch(
    plm::plm(
      formula = formula,
      data    = dt,
      index   = c("panelid", "commodity_year"),
      model   = model
    ),
    error = function(e) NULL
  )

  if (is.null(obj_mean_1)) return(NULL)

  dt_v <- data.table::as.data.table(obj_mean_1$model)

  names(dt_v)[grepl("county_fips", names(dt_v))] <- "county_fips"

  idx <- plm::index(obj_mean_1)

  if (!("panelid" %in% names(dt_v))) {
    dt_v[, panelid := as.character(idx[[1]])]
  }

  if (!("commodity_year" %in% names(dt_v))) {
    dt_v[, commodity_year := as.integer(as.character(idx[[2]]))]
  }

  dt_v[, resid_mean := as.numeric(stats::residuals(obj_mean_1))]
  dt_v[, resid_sq := resid_mean^2]
  dt_v[, log_resid_sq := log(resid_sq + eps)]

  var_formula <- stats::update(formula, log_resid_sq ~ .)

  obj_var <- tryCatch(
    plm::plm(
      formula = var_formula,
      data    = dt_v,
      index   = c("panelid", "commodity_year"),
      model   = model
    ),
    error = function(e) NULL
  )

  if (is.null(obj_var)) return(NULL)

  dt_v[, fitted_log_var := as.numeric(stats::fitted(obj_var))]
  dt_v[, fitted_var := exp(fitted_log_var)]

  dt_v[, var_weight := 1 / pmax(fitted_var, eps)]
  dt_v[, var_weight := var_weight / mean(var_weight, na.rm = TRUE)]
  dt_v[, sqrt_w := sqrt(var_weight)]

  dt_vw <- data.table::copy(dt_v)

  dt_vw[, precipitation := precipitation * sqrt_w]
  dt_vw[, trend := trend * sqrt_w]

  obj_mean_w <- tryCatch(
    plm::plm(
      formula = formula,
      data    = dt_vw,
      index   = c("panelid", "commodity_year"),
      model   = model
    ),
    error = function(e) NULL
  )

  if (is.null(obj_mean_w)) return(NULL)

  list(
    mean_model_initial  = obj_mean_1,
    variance_model      = obj_var,
    mean_model_weighted = obj_mean_w,
    data                = dt_v,
    data_weighted       = dt_vw
  )
}


#' Estimate one state's mean and variance trend coefficients
#'
#' Runs the FGLS-style mean-variance workflow for one state and returns a
#' tidy coefficient table containing county-level mean and variance trend
#' estimates.
#'
#' @param dt A `data.table` or data frame containing one state's panel data.
#'   Required columns include `panelid`, `commodity_year`, `precipitation`,
#'   `trend`, and `county_fips`.
#' @param formula Formula for the mean trend model. In this workflow, the
#'   expected formula is `precipitation ~ trend:factor(county_fips)`.
#' @param model Character string specifying the `plm` estimator.
#'
#' @return A `data.table` with county-level trend estimates and diagnostics.
#' The returned table includes:
#' \describe{
#'   \item{term}{Coefficient term from the model.}
#'   \item{county_fips}{County FIPS extracted from the model term.}
#'   \item{mean_estimate}{Estimated mean trend coefficient.}
#'   \item{mean_standard_error}{Robust standard error for the mean trend.}
#'   \item{mean_t_value}{t-statistic for the mean trend.}
#'   \item{mean_p_value}{p-value for the mean trend.}
#'   \item{var_estimate}{Estimated variance trend coefficient.}
#'   \item{var_standard_error}{Robust standard error for the variance trend.}
#'   \item{var_t_value}{t-statistic for the variance trend.}
#'   \item{var_p_value}{p-value for the variance trend.}
#'   \item{diag_mean}{Model diagnostics for the final pre-weighted mean model.}
#'   \item{diag_var}{Model diagnostics for the variance model.}
#' }
#'
#' @details
#' Robust coefficient tables are computed using `lmtest::coeftest()` with
#' `plm::vcovDC()`. This is possible because the final mean model is estimated
#' using pre-weighted variables rather than the `weights` argument in `plm`.
#' @family Precipitation trend_analysis
#' @export
estimate_one_state <- function(dt, formula, model = "within") {

  dt <- data.table::copy(dt)

  if (length(unique(dt$panelid)) < 2L) return(NULL)
  if (length(unique(dt$commodity_year)) < 3L) return(NULL)

  out_fgls <- estimate_one_state_fgls(
    dt      = dt,
    formula = formula,
    model   = model
  )

  if (is.null(out_fgls)) return(NULL)

  ct_mean <- tryCatch(
    lmtest::coeftest(
      out_fgls$mean_model_weighted,
      vcov = plm::vcovDC(out_fgls$mean_model_weighted)
    ),
    error = function(e) NULL
  )

  ct_var <- tryCatch(
    lmtest::coeftest(
      out_fgls$variance_model,
      vcov = plm::vcovDC(out_fgls$variance_model)
    ),
    error = function(e) NULL
  )

  if (is.null(ct_mean) || is.null(ct_var)) return(NULL)

  out_mean <- data.table::data.table(
    term                = rownames(ct_mean),
    mean_estimate       = ct_mean[, "Estimate"],
    mean_standard_error = ct_mean[, "Std. Error"],
    mean_t_value        = ct_mean[, "t value"],
    mean_p_value        = ct_mean[, "Pr(>|t|)"]
  )

  out_var <- data.table::data.table(
    term               = rownames(ct_var),
    var_estimate       = ct_var[, "Estimate"],
    var_standard_error = ct_var[, "Std. Error"],
    var_t_value        = ct_var[, "t value"],
    var_p_value        = ct_var[, "Pr(>|t|)"]
  )

  out <- out_mean[
    out_var,
    on = "term",
    nomatch = 0
  ]

  if (nrow(out) == 0L) return(NULL)

  out[
    ,
    county_fips := gsub(
      "trend:factor[(]county_fips[)]",
      "",
      term
    )
  ]

  out[, diag_mean := list(extract_model_diagnostics(out_fgls$mean_model_weighted))]
  out[, diag_var  := list(extract_model_diagnostics(out_fgls$variance_model))]

  out[]
}


#' Estimate county-level trends for one panel estimator
#'
#' Applies the one-state estimation workflow separately by state for a given
#' panel estimator. The function returns county-level mean and variance trend
#' estimates for all states in the supplied data.
#'
#' @param dtw A `data.table` containing the rolling-window estimation sample.
#'   Must include `state_code`, `panelid`, `commodity_year`, `precipitation`,
#'   `trend`, and `county_fips`.
#' @param estimator Character string specifying the `plm` model estimator to use.
#'   Examples include `"pooling"`, `"within"`, `"random"`, `"between"`, and `"fd"`.
#'
#' @return A `data.table` with county-level trend estimates for the selected
#' estimator. The output includes state identifiers, model estimator labels,
#' mean trend estimates, variance trend estimates, and model diagnostics.
#'
#' @details
#' The function estimates each state separately using:
#'
#' `precipitation ~ trend:factor(county_fips)`
#'
#' This produces county-specific trend coefficients within each state. If a
#' state model fails, the function returns a structured empty row with missing
#' values, allowing downstream `rbindlist()` calls to remain stable.
#'
#' @family Precipitation trend_analysis
#' @export
estimate_by_estimator <- function(dtw, estimator) {

  expected_cols <- c(
    "term",
    "county_fips",
    "mean_estimate",
    "mean_standard_error",
    "mean_t_value",
    "mean_p_value",
    "var_estimate",
    "var_standard_error",
    "var_t_value",
    "var_p_value",
    "diag_mean",
    "diag_var"
  )

  empty_row <- function() {
    data.table::data.table(
      term                = NA_character_,
      county_fips         = NA_character_,
      mean_estimate       = NA_real_,
      mean_standard_error = NA_real_,
      mean_t_value        = NA_real_,
      mean_p_value        = NA_real_,
      var_estimate        = NA_real_,
      var_standard_error  = NA_real_,
      var_t_value         = NA_real_,
      var_p_value         = NA_real_,
      diag_mean           = list(NULL),
      diag_var            = list(NULL)
    )
  }

  res <- dtw[
    ,
    {
      out_i <- tryCatch(
        estimate_one_state(
          dt      = .SD,
          formula = precipitation ~ trend:factor(county_fips),
          model   = estimator
        ),
        error = function(e) NULL
      )

      if (is.null(out_i) || nrow(out_i) == 0L) {

        out_i <- empty_row()

      } else {

        out_i <- as.data.table(out_i)

        missing_cols <- setdiff(expected_cols, names(out_i))

        for (cc in missing_cols) {
          out_i[, (cc) := NA]
        }

        out_i <- out_i[, ..expected_cols]
      }

      out_i
    },
    by = .(state_code)
  ]

  res[, model_estimator := estimator]

  res <- res[grepl("county_fips", term)]

  res[]
}



#' Classify a trend by sign and statistical significance
#'
#' Classifies a coefficient estimate as positive, negative, or not significant
#' based on its sign and p-value.
#'
#' @param estimate Numeric vector of coefficient estimates.
#' @param p_value Numeric vector of p-values associated with the estimates.
#' @param p_value_sig Numeric significance threshold. Default is `0.05`.
#'
#' @return A character vector with values:
#' \describe{
#'   \item{"Positive"}{Estimate is positive and statistically significant.}
#'   \item{"Negative"}{Estimate is negative and statistically significant.}
#'   \item{"None"}{Estimate is not statistically significant or equals zero.}
#'   \item{NA}{Estimate or p-value is missing.}
#' }
#' @family Precipitation trend_analysis
#' @export
classify_trend <- function(estimate, p_value, p_value_sig = 0.05) {
  data.table::fifelse(
    is.na(p_value) | is.na(estimate),
    NA_character_,
    data.table::fifelse(
      p_value > p_value_sig,
      "None",
      data.table::fifelse(
        estimate > 0,
        "Positive",
        data.table::fifelse(
          estimate < 0,
          "Negative",
          "None"
        )
      )
    )
  )
}


#' Compute Fisher combined p-values for paired tests
#'
#' Combines two p-values using Fisher's method. In this workflow, the two
#' p-values correspond to the mean trend test and the variance trend test
#' for the same county.
#'
#' @param p1 Numeric vector of p-values from the first test.
#' @param p2 Numeric vector of p-values from the second test.
#'
#' @return A `data.table` with:
#' \describe{
#'   \item{joint_fisher_stat}{Fisher test statistic.}
#'   \item{joint_fisher_p_value}{Combined p-value from a chi-square distribution
#'   with 4 degrees of freedom.}
#' }
#'
#' @details
#' Fisher's method uses:
#'
#' `X^2 = -2 * (log(p1) + log(p2))`
#'
#' With two tests, the reference distribution is chi-square with `2 * 2 = 4`
#' degrees of freedom. Missing, zero, or negative p-values return `NA`.
#' @family Precipitation trend_analysis
#' @export
fisher_joint_pvalue <- function(p1, p2) {

  out_stat <- rep(NA_real_, length(p1))
  out_pval <- rep(NA_real_, length(p1))

  ok <- !is.na(p1) & !is.na(p2) & p1 > 0 & p2 > 0

  out_stat[ok] <- -2 * (log(p1[ok]) + log(p2[ok]))

  out_pval[ok] <- stats::pchisq(
    q = out_stat[ok],
    df = 4,
    lower.tail = FALSE
  )

  data.table::data.table(
    joint_fisher_stat    = out_stat,
    joint_fisher_p_value = out_pval
  )
}



#' Classify mean and variance trends jointly
#'
#' Creates statistical and descriptive classifications for county-level mean
#' and variance trend estimates across one or more significance thresholds.
#'
#' @param res A `data.table` containing coefficient estimates and p-values.
#' @param mean_estimate_col Name of the column containing mean trend estimates.
#'   Default is `"mean_estimate"`.
#' @param mean_p_value_col Name of the column containing mean trend p-values.
#'   Default is `"mean_p_value"`.
#' @param var_estimate_col Name of the column containing variance trend estimates.
#'   Default is `"var_estimate"`.
#' @param var_p_value_col Name of the column containing variance trend p-values.
#'   Default is `"var_p_value"`.
#' @param sig_levels Numeric vector of significance levels. Default is
#'   `c(0.01, 0.05, 0.10)`.
#'
#' @return A copy of `res` with additional classification columns:
#' \describe{
#'   \item{joint_fisher_stat}{Fisher combined test statistic.}
#'   \item{joint_fisher_p_value}{Fisher combined p-value.}
#'   \item{mean_trend_class_XX}{Mean trend class at significance level XX.}
#'   \item{var_trend_class_XX}{Variance trend class at significance level XX.}
#'   \item{joint_trend_class_XX}{Descriptive mean-variance pattern.}
#'   \item{joint_fisher_class_XX}{Whether Fisher's combined p-value indicates
#'   joint evidence at significance level XX.}
#'   \item{trend_class_XX}{Clean final classification for mapping and summary
#'   tables.}
#' }
#'
#' @details
#' The component classifications are based on separate mean and variance tests.
#' For example, a county may be classified as `"Positive Mean only"` if the mean
#' trend is positive and statistically significant while the variance trend is
#' not statistically significant.
#'
#' Fisher's method is reported separately as a combined-evidence test across the
#' mean and variance p-values. The final `trend_class_XX` columns are intended
#' for interpretation, mapping, and summary tables.
#' @family Precipitation trend_analysis
#' @export
classify_mean_variance_trends <- function(
    res,
    mean_estimate_col = "mean_estimate",
    mean_p_value_col  = "mean_p_value",
    var_estimate_col  = "var_estimate",
    var_p_value_col   = "var_p_value",
    sig_levels        = c(0.01, 0.05, 0.10)
) {

  res <- data.table::copy(res)

  fisher_out <- fisher_joint_pvalue(
    p1 = res[[mean_p_value_col]],
    p2 = res[[var_p_value_col]]
  )

  res[, joint_fisher_stat := fisher_out$joint_fisher_stat]
  res[, joint_fisher_p_value := fisher_out$joint_fisher_p_value]

  for (sig in sig_levels) {

    sig_label <- stringr::str_pad(sig * 100, width = 2, pad = "0")

    mean_class_col   <- paste0("mean_trend_class_", sig_label)
    var_class_col    <- paste0("var_trend_class_", sig_label)
    joint_col        <- paste0("joint_trend_class_", sig_label)
    fisher_class_col <- paste0("joint_fisher_class_", sig_label)
    final_class_col  <- paste0("trend_class_", sig_label)

    res[
      ,
      (mean_class_col) := classify_trend(
        estimate    = .SD[[mean_estimate_col]],
        p_value     = .SD[[mean_p_value_col]],
        p_value_sig = sig
      )
    ]

    res[
      ,
      (var_class_col) := classify_trend(
        estimate    = .SD[[var_estimate_col]],
        p_value     = .SD[[var_p_value_col]],
        p_value_sig = sig
      )
    ]

    res[
      ,
      (joint_col) := data.table::fcase(
        is.na(.SD[[mean_class_col]]) | is.na(.SD[[var_class_col]]),
        NA_character_,

        .SD[[mean_class_col]] == "None" &
          .SD[[var_class_col]] == "None",
        "No mean trend / No variance trend",

        .SD[[mean_class_col]] != "None" &
          .SD[[var_class_col]] == "None",
        paste0("Mean ", .SD[[mean_class_col]], " / No variance trend"),

        .SD[[mean_class_col]] == "None" &
          .SD[[var_class_col]] != "None",
        paste0("No mean trend / Variance ", .SD[[var_class_col]]),

        .SD[[mean_class_col]] != "None" &
          .SD[[var_class_col]] != "None",
        paste0("Mean ", .SD[[mean_class_col]], " / Variance ", .SD[[var_class_col]]),

        default = NA_character_
      )
    ]

    res[
      ,
      (fisher_class_col) := data.table::fifelse(
        is.na(joint_fisher_p_value),
        NA_character_,
        data.table::fifelse(
          joint_fisher_p_value <= sig,
          "Joint evidence",
          "No joint evidence"
        )
      )
    ]

    res[
      ,
      (final_class_col) := data.table::fcase(

        is.na(.SD[[mean_class_col]]) |
          is.na(.SD[[var_class_col]]) |
          is.na(.SD[[fisher_class_col]]),
        NA_character_,

        .SD[[mean_class_col]] == "None" &
          .SD[[var_class_col]] == "None",
        "None",

        .SD[[mean_class_col]] == "Positive" &
          .SD[[var_class_col]] == "None",
        "Positive Mean only",

        .SD[[mean_class_col]] == "Negative" &
          .SD[[var_class_col]] == "None",
        "Negative Mean only",

        .SD[[mean_class_col]] == "None" &
          .SD[[var_class_col]] == "Positive",
        "Positive Variance only",

        .SD[[mean_class_col]] == "None" &
          .SD[[var_class_col]] == "Negative",
        "Negative Variance only",

        .SD[[mean_class_col]] == "Positive" &
          .SD[[var_class_col]] == "Positive",
        "Positive Mean and Positive Variance",

        .SD[[mean_class_col]] == "Positive" &
          .SD[[var_class_col]] == "Negative",
        "Positive Mean and Negative Variance",

        .SD[[mean_class_col]] == "Negative" &
          .SD[[var_class_col]] == "Positive",
        "Negative Mean and Positive Variance",

        .SD[[mean_class_col]] == "Negative" &
          .SD[[var_class_col]] == "Negative",
        "Negative Mean and Negative Variance",

        default = "Other"
      )
    ]
  }

  res[]
}


