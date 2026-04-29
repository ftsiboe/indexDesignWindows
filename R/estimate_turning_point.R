#' Estimate Turning Points Relative to a Benchmark Window
#'
#' This function estimates the historical-window turning point for one or more
#' groups in a dataset. The turning point is defined as the earliest value of an
#' x-variable, such as the number of historical years, where the selected outcome
#' remains within a specified tolerance band around a benchmark value for a
#' required number of consecutive historical windows.
#'
#' The benchmark value is usually the outcome associated with the longest
#' historical window, such as the 60-year window or the full-sample window. For
#' each group, the function compares the outcome at each historical window to the
#' group-specific benchmark value and identifies the earliest window at which the
#' outcome is sufficiently close to the benchmark and remains close for the
#' required number of consecutive windows.
#'
#' @param data A data.frame or data.table containing the outcome variable,
#'   grouping variable, and historical-window variable.
#'
#' @param outcome Character string. Name of the outcome variable to evaluate.
#'   Examples include `"actuarial_index"`, `"alternative_lr"`, or
#'   `"baseline_lr"`.
#'
#' @param group_var Character string. Name of the grouping variable used to
#'   estimate separate turning points. For example, `"adjustment"`.
#'
#' @param x_var Character string. Name of the historical-window variable. This
#'   variable should represent the number of years in the historical window, such
#'   as `5`, `6`, ..., `60`. The function coerces this variable to numeric.
#'
#' @param benchmark_x Numeric value indicating the x-value used as the benchmark.
#'   If `NULL`, the function uses the maximum observed value of `x_var`. For
#'   example, if the longest historical window is 60 years, use
#'   `benchmark_x = 60`. If the full sample is coded as 200, use
#'   `benchmark_x = 200`.
#'
#' @param tolerance_share Numeric value. The acceptable proportional distance
#'   from the benchmark value. The default is `0.05`, meaning the outcome must be
#'   within 5 percent of the benchmark value.
#'
#' @param consecutive_years Integer. Number of consecutive historical-window
#'   values that must remain within the tolerance band before the first value is
#'   classified as the turning point. The default is `5`.
#'
#' @param require_consecutive_x Logical. If `TRUE`, the function requires the
#'   selected sequence to be consecutive in terms of the actual `x_var` values.
#'   For example, `5, 6, 7, 8, 9` is valid, but `5, 6, 8, 9, 10` is not. If
#'   `FALSE`, the function only requires consecutive rows after sorting by
#'   `x_var`.
#'
#' @return A data.table with one row per group. The output includes the grouping
#'   variable, outcome name, benchmark x-value, tolerance share, required number
#'   of consecutive years, and estimated turning point.
#'
#' @details
#' For each group, the function computes the absolute distance between the
#' outcome at historical window \eqn{k} and the benchmark value:
#'
#' \deqn{
#'   distance_k = |y_k - y_{benchmark}|
#' }
#'
#' This distance is compared with a tolerance band:
#'
#' \deqn{
#'   tolerance = tolerance\_share \times |y_{benchmark}|
#' }
#'
#' The turning point is the first historical window \eqn{k} such that
#' `distance_k <= tolerance` for `consecutive_years` consecutive historical
#' windows.
#'
#' If no such sequence is found for a group, the function returns `NA` for that
#' group's turning point.
#'
#' If duplicate observations exist for the same group and historical window, the
#' function averages the outcome within that group-window combination before
#' estimating the turning point.
#'
#' @import data.table
#' @export
estimate_turning_point_benchmark <- function(
    data,
    outcome,
    group_var,
    x_var,
    benchmark_x = NULL,
    tolerance_share = 0.05,
    consecutive_years = 5,
    require_consecutive_x = TRUE) {

  # Work on a local copy so the original object is not modified by reference.
  data <- data.table::as.data.table(data.table::copy(data))

  # Confirm that required variables exist.
  required_vars <- c(outcome, group_var, x_var)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "The following required variables are missing from data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Validate key scalar inputs.
  if (!is.numeric(tolerance_share) || length(tolerance_share) != 1L || is.na(tolerance_share)) {
    stop("tolerance_share must be a single non-missing numeric value.")
  }

  if (tolerance_share < 0) {
    stop("tolerance_share must be non-negative.")
  }

  if (!is.numeric(consecutive_years) || length(consecutive_years) != 1L || is.na(consecutive_years)) {
    stop("consecutive_years must be a single non-missing numeric value.")
  }

  consecutive_years <- as.integer(consecutive_years)

  if (consecutive_years < 1L) {
    stop("consecutive_years must be at least 1.")
  }

  # Ensure that the historical-window variable is numeric.
  data[, (x_var) := as.numeric(get(x_var))]

  # Remove observations with missing historical-window or outcome values.
  data <- data[!is.na(get(x_var)) & !is.na(get(outcome))]

  if (nrow(data) == 0) {
    stop("No valid observations remain after removing missing x_var or outcome values.")
  }

  # If no benchmark x-value is supplied, use the maximum observed x-value.
  if (is.null(benchmark_x)) {
    benchmark_x <- max(data[[x_var]], na.rm = TRUE)
  }

  if (!is.numeric(benchmark_x) || length(benchmark_x) != 1L || is.na(benchmark_x)) {
    stop("benchmark_x must be NULL or a single non-missing numeric value.")
  }

  # Collapse duplicate observations to one row per group and historical window.
  data <- data[
    ,
    .(outcome_value = mean(get(outcome), na.rm = TRUE)),
    by = c(group_var, x_var)
  ]

  data.table::setnames(data, "outcome_value", outcome)

  # Extract the group-specific benchmark outcome value.
  benchmark <- data[
    get(x_var) == benchmark_x,
    .(benchmark_value = mean(get(outcome), na.rm = TRUE)),
    by = group_var
  ]

  if (nrow(benchmark) == 0) {
    stop("No benchmark observations found. Check benchmark_x.")
  }

  # Attach each group-specific benchmark to all observations in that group.
  data <- merge(data, benchmark, by = group_var, all.x = TRUE)

  # Compute the absolute distance from the benchmark value.
  data[, distance := abs(get(outcome) - benchmark_value)]

  # Define the tolerance band around the benchmark value.
  data[, tolerance := tolerance_share * abs(benchmark_value)]

  # Flag observations that are within the tolerance band.
  data[, within_tolerance := distance <= tolerance]

  # Sort observations so the consecutive search follows historical-window order.
  data.table::setorderv(data, c(group_var, x_var))

  # Estimate the turning point separately for each group.
  out <- data[
    ,
    {
      x <- get(x_var)
      ok <- within_tolerance

      turning_point <- NA_real_

      for (i in seq_along(x)) {

        last_i <- i + consecutive_years - 1L

        if (last_i <= length(x)) {

          test_x <- x[i:last_i]
          test_ok <- ok[i:last_i]

          valid_tolerance_sequence <-
            all(!is.na(test_ok)) &&
            all(test_ok)

          valid_x_sequence <- TRUE

          if (require_consecutive_x && consecutive_years > 1L) {
            valid_x_sequence <- all(diff(test_x) == 1)
          }

          if (valid_tolerance_sequence && valid_x_sequence) {
            turning_point <- x[i]
            break
          }
        }
      }

      data.table::data.table(
        outcome = outcome,
        benchmark_year = benchmark_x,
        tolerance_share = tolerance_share,
        consecutive_years = consecutive_years,
        turning_point = turning_point
      )
    },
    by = group_var
  ]

  out[]
}


#' Select a Data-Driven Tolerance Using Post-Turning-Point Stability
#'
#' This function selects a tolerance value for the benchmark-distance
#' turning-point method by evaluating a grid of candidate tolerance values. For
#' each tolerance, it first estimates the turning point using
#' `estimate_turning_point_benchmark()`. It then measures how stable the outcome
#' is after the estimated turning point.
#'
#' The selected tolerance is the one that minimizes a normalized stability score:
#'
#' \deqn{
#'   score = post\_cv + penalty\_weight \times scaled\_turning\_point
#' }
#'
#' where `post_cv` is the post-turning-point coefficient of variation and
#' `scaled_turning_point` is the estimated turning point divided by the benchmark
#' x-value. This normalization makes the score more comparable across outcomes
#' with different units or scales.
#'
#' @param data A data.frame or data.table containing the outcome variable,
#'   grouping variable, and historical-window variable.
#'
#' @param outcome Character string. Name of the outcome variable used to evaluate
#'   stability. Examples include `"actuarial_index"`, `"alternative_lr"`, or
#'   `"baseline_lr"`.
#'
#' @param group_var Character string. Name of the grouping variable. Separate
#'   tolerance values and turning points are selected for each group.
#'
#' @param x_var Character string. Name of the numeric historical-window variable.
#'   For example, `"history_years"`.
#'
#' @param benchmark_x Numeric. The x-value used as the long-history benchmark in
#'   `estimate_turning_point_benchmark()`. If `NULL`, the maximum observed value
#'   of `x_var` is used.
#'
#' @param tolerance_grid Numeric vector. Candidate tolerance shares to evaluate.
#'   The default evaluates values from 1 percent to 15 percent in increments of
#'   0.5 percentage points.
#'
#' @param consecutive_years Integer. Number of consecutive historical-window
#'   values that must remain within the benchmark-distance tolerance before the
#'   first x-value is classified as the turning point. The default is `5`.
#'
#' @param penalty_weight Numeric. Penalty applied to later turning points in the
#'   score function. Larger values favor earlier turning points, while smaller
#'   values allow later turning points if they produce lower
#'   post-turning-point variability. The default is `0.01`.
#'
#' @param require_consecutive_x Logical. If `TRUE`, the function requires the
#'   selected sequence to be consecutive in terms of the actual `x_var` values.
#'   This argument is passed to `estimate_turning_point_benchmark()`.
#'
#' @return A data.table with one row per group. The output includes the selected
#'   tolerance share, estimated turning point, post-turning-point mean, standard
#'   deviation, coefficient of variation, range, scaled turning point, and the
#'   minimized score.
#'
#' @details
#' This function is designed to make the tolerance parameter in the benchmark
#' turning-point method more data-driven. Instead of imposing a fixed tolerance,
#' such as 5 percent, the function searches over a grid of possible tolerances.
#'
#' For each candidate tolerance:
#'
#' 1. A turning point is estimated using `estimate_turning_point_benchmark()`.
#' 2. The data are restricted to observations at or after the estimated turning
#'    point.
#' 3. The post-turning-point mean, standard deviation, coefficient of variation,
#'    and range of the outcome are calculated.
#' 4. A normalized score is computed using post-turning-point variability and a
#'    penalty for later turning points.
#'
#' The tolerance with the lowest score is selected for each group.
#'
#' If no valid turning point is found for a candidate tolerance, that candidate
#' does not contribute to the post-turning-point stability calculation for that
#' group.
#'
#' @import data.table
#' @export
select_tolerance_by_post_stability <- function(
    data,
    outcome,
    group_var,
    x_var,
    benchmark_x = NULL,
    tolerance_grid = seq(0.01, 0.15, by = 0.005),
    consecutive_years = 5,
    penalty_weight = 0.01,
    require_consecutive_x = TRUE) {

  # Work on a local copy so the original object is not modified by reference.
  data <- data.table::as.data.table(data.table::copy(data))

  # Confirm that required variables exist.
  required_vars <- c(outcome, group_var, x_var)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "The following required variables are missing from data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Validate tuning inputs.
  if (!is.numeric(tolerance_grid) || length(tolerance_grid) == 0L) {
    stop("tolerance_grid must be a non-empty numeric vector.")
  }

  tolerance_grid <- tolerance_grid[!is.na(tolerance_grid)]

  if (length(tolerance_grid) == 0L) {
    stop("tolerance_grid contains no non-missing numeric values.")
  }

  if (any(tolerance_grid < 0)) {
    stop("All values in tolerance_grid must be non-negative.")
  }

  if (!is.numeric(penalty_weight) || length(penalty_weight) != 1L || is.na(penalty_weight)) {
    stop("penalty_weight must be a single non-missing numeric value.")
  }

  if (penalty_weight < 0) {
    stop("penalty_weight must be non-negative.")
  }

  # Ensure that the historical-window variable is numeric.
  data[, (x_var) := as.numeric(get(x_var))]

  # Remove observations with missing historical-window or outcome values.
  data <- data[!is.na(get(x_var)) & !is.na(get(outcome))]

  if (nrow(data) == 0) {
    stop("No valid observations remain after removing missing x_var or outcome values.")
  }

  # If no benchmark x-value is supplied, use the maximum observed x-value.
  if (is.null(benchmark_x)) {
    benchmark_x <- max(data[[x_var]], na.rm = TRUE)
  }

  if (!is.numeric(benchmark_x) || length(benchmark_x) != 1L || is.na(benchmark_x)) {
    stop("benchmark_x must be NULL or a single non-missing numeric value.")
  }

  # Collapse duplicate observations to one row per group and historical window.
  data <- data[
    ,
    .(outcome_value = mean(get(outcome), na.rm = TRUE)),
    by = c(group_var, x_var)
  ]

  data.table::setnames(data, "outcome_value", outcome)

  # Evaluate each candidate tolerance value in the supplied grid.
  results <- data.table::rbindlist(
    lapply(tolerance_grid, function(tol) {

      # Estimate the benchmark-distance turning point for the current tolerance.
      tp <- estimate_turning_point_benchmark(
        data                  = data,
        outcome               = outcome,
        group_var             = group_var,
        x_var                 = x_var,
        benchmark_x           = benchmark_x,
        tolerance_share       = tol,
        consecutive_years     = consecutive_years,
        require_consecutive_x = require_consecutive_x
      )

      # Attach the estimated turning point to each observation.
      tmp <- merge(
        data,
        tp[, .SD, .SDcols = c(group_var, "turning_point")],
        by = group_var,
        all.x = TRUE
      )

      # Keep only observations at or after the estimated turning point.
      tmp <- tmp[
        !is.na(turning_point) & get(x_var) >= turning_point
      ]

      if (nrow(tmp) == 0) {
        return(NULL)
      }

      # Measure post-turning-point stability.
      stability <- tmp[
        ,
        .(
          post_mean = mean(get(outcome), na.rm = TRUE),
          post_sd = stats::sd(get(outcome), na.rm = TRUE),
          post_range = diff(range(get(outcome), na.rm = TRUE)),
          post_n = .N,
          turning_point = unique(turning_point)[1]
        ),
        by = group_var
      ]

      # Compute normalized stability statistics and the final score.
      stability[
        ,
        `:=`(
          post_cv = data.table::fifelse(
            !is.na(post_mean) & abs(post_mean) > 0,
            post_sd / abs(post_mean),
            NA_real_
          ),
          scaled_turning_point = turning_point / benchmark_x,
          tolerance_share = tol,
          consecutive_years = consecutive_years,
          benchmark_year = benchmark_x,
          outcome = outcome
        )
      ]

      stability[
        ,
        score := post_cv + penalty_weight * scaled_turning_point
      ]

      stability[]
    }),
    fill = TRUE
  )

  if (nrow(results) == 0) {
    stop("No valid turning points were found for any tolerance value.")
  }

  results <- results[!is.na(score)]

  if (nrow(results) == 0) {
    stop("All candidate tolerance values produced missing scores.")
  }

  # Select the tolerance with the lowest score separately for each group.
  selected <- results[
    ,
    .SD[which.min(score)],
    by = group_var
  ]

  data.table::setcolorder(
    selected,
    c(
      group_var,
      "outcome",
      "benchmark_year",
      "tolerance_share",
      "consecutive_years",
      "turning_point",
      "post_mean",
      "post_sd",
      "post_cv",
      "post_range",
      "post_n",
      "scaled_turning_point",
      "score"
    )
  )

  selected[]
}


#' Estimate Final Turning Points Using a Data-Driven Tolerance
#'
#' This function runs the full benchmark-distance turning-point pipeline. It
#' first selects the best tolerance value for each group using
#' `select_tolerance_by_post_stability()`. It then re-estimates the final
#' turning point for each group using the selected tolerance.
#'
#' The function returns both the final turning point and the diagnostics used to
#' select the tolerance. This makes the final output easier to interpret and
#' audit because each reported turning point is linked to the selected tolerance,
#' post-turning-point stability statistics, and final score.
#'
#' @param data A data.frame or data.table containing the outcome variable,
#'   grouping variable, and historical-window variable.
#'
#' @param outcome Character string. Name of the outcome variable to evaluate.
#'   Examples include `"actuarial_index"`, `"alternative_lr"`, or
#'   `"baseline_lr"`.
#'
#' @param group_var Character string. Name of the grouping variable used to
#'   estimate separate turning points. For example, `"adjustment"`.
#'
#' @param x_var Character string. Name of the historical-window variable. This
#'   variable should represent the number of years in the historical window.
#'
#' @param benchmark_x Numeric. The x-value used as the benchmark. If `NULL`, the
#'   maximum observed value of `x_var` is used. For example, use `60` for a
#'   60-year benchmark or `200` if the full sample is coded as 200.
#'
#' @param tolerance_grid Numeric vector. Candidate tolerance shares to evaluate.
#'   The default evaluates values from 1 percent to 15 percent in increments of
#'   0.5 percentage points.
#'
#' @param consecutive_years Integer. Number of consecutive historical-window
#'   values that must remain within the benchmark-distance tolerance before the
#'   first x-value is classified as the turning point. The default is `5`.
#'
#' @param penalty_weight Numeric. Penalty applied to later turning points in the
#'   score function. Larger values favor earlier turning points, while smaller
#'   values allow later turning points if they produce lower
#'   post-turning-point variability. The default is `0.01`.
#'
#' @param require_consecutive_x Logical. If `TRUE`, the function requires the
#'   selected sequence to be consecutive in terms of the actual `x_var` values.
#'
#' @return A data.table with one row per group. The output includes the grouping
#'   variable, outcome name, benchmark year, selected tolerance share, required
#'   number of consecutive years, final turning point, post-turning-point
#'   stability statistics, scaled turning point, and minimized score.
#'
#' @details
#' The full pipeline is:
#'
#' 1. For each tolerance in `tolerance_grid`, estimate the earliest benchmark-
#'    stable historical window for each group.
#' 2. For each candidate tolerance and group, calculate the stability of the
#'    outcome after the estimated turning point.
#' 3. Select the tolerance that minimizes:
#'
#' \deqn{
#'   score = post\_cv + penalty\_weight \times scaled\_turning\_point
#' }
#'
#' 4. Re-estimate the final turning point using the selected group-specific
#'    tolerance.
#'
#' This function is useful when the analyst does not want to impose an arbitrary
#' tolerance, such as 5 percent, and instead wants the tolerance to be selected
#' from the data based on post-turning-point stability.
#'
#' @import data.table
#' @export
estimate_final_turning_point <- function(
    data,
    outcome,
    group_var,
    x_var,
    benchmark_x       = NULL,
    tolerance_grid    = seq(0.01, 0.15, by = 0.005),
    consecutive_years = 5,
    penalty_weight    = 0.01,
    require_consecutive_x = TRUE) {

  # Work on a local copy so the original object is not modified by reference.
  data <- data.table::as.data.table(data.table::copy(data))

  # Confirm that required variables exist.
  required_vars <- c(outcome, group_var, x_var)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "The following required variables are missing from data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Ensure that the historical-window variable is numeric.
  data[, (x_var) := as.numeric(get(x_var))]

  # Remove observations with missing historical-window or outcome values.
  data <- data[!is.na(get(x_var)) & !is.na(get(outcome))]

  if (nrow(data) == 0) {
    stop("No valid observations remain after removing missing x_var or outcome values.")
  }

  # If no benchmark x-value is supplied, use the maximum observed x-value.
  if (is.null(benchmark_x)) {
    benchmark_x <- max(data[[x_var]], na.rm = TRUE)
  }

  if (!is.numeric(benchmark_x) || length(benchmark_x) != 1L || is.na(benchmark_x)) {
    stop("benchmark_x must be NULL or a single non-missing numeric value.")
  }

  # Collapse duplicate observations to one row per group and historical window.
  data <- data[
    ,
    .(outcome_value = mean(get(outcome), na.rm = TRUE)),
    by = c(group_var, x_var)
  ]

  data.table::setnames(data, "outcome_value", outcome)

  # Select the best tolerance for each group.
  best_tol <- select_tolerance_by_post_stability(
    data                  = data,
    outcome               = outcome,
    group_var             = group_var,
    x_var                 = x_var,
    benchmark_x           = benchmark_x,
    tolerance_grid        = tolerance_grid,
    consecutive_years     = consecutive_years,
    penalty_weight        = penalty_weight,
    require_consecutive_x = require_consecutive_x
  )

  # Re-estimate the final turning point using the selected tolerance for each group.
  tp_final <- data.table::rbindlist(
    lapply(seq_len(nrow(best_tol)), function(i) {

      g <- best_tol[[group_var]][i]
      tol <- best_tol$tolerance_share[i]

      estimate_turning_point_benchmark(
        data                  = data[get(group_var) == g],
        outcome               = outcome,
        group_var             = group_var,
        x_var                 = x_var,
        benchmark_x           = benchmark_x,
        tolerance_share       = tol,
        consecutive_years     = consecutive_years,
        require_consecutive_x = require_consecutive_x
      )
    }),
    fill = TRUE
  )

  # Combine final turning points with tolerance-selection diagnostics.
  final <- merge(
    tp_final,
    best_tol[
      ,
      .SD,
      .SDcols = c(
        group_var,
        "tolerance_share",
        "consecutive_years",
        "turning_point",
        "post_mean",
        "post_sd",
        "post_cv",
        "post_range",
        "post_n",
        "scaled_turning_point",
        "score"
      )
    ],
    by = c(group_var, "tolerance_share", "consecutive_years", "turning_point"),
    all.x = TRUE
  )

  data.table::setcolorder(
    final,
    c(
      group_var,
      "outcome",
      "benchmark_year",
      "tolerance_share",
      "consecutive_years",
      "turning_point",
      "post_mean",
      "post_sd",
      "post_cv",
      "post_range",
      "post_n",
      "scaled_turning_point",
      "score"
    )
  )

  final[]
}
