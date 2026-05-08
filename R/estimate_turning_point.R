#' Estimate Turning Point Based on Proximity to Minimum or Maximum Outcome
#'
#' For each group, this function finds the maximum x-value for which the
#' corresponding y-value is within `sd_multiplier` standard deviations of either
#' the minimum or maximum y-value.
#'
#' @param data A data.frame or data.table.
#' @param outcome Character string. Name of the y-variable.
#' @param x_var Character string. Name of the x-variable.
#' @param group_var Character string. Name of the grouping variable.
#' @param direction Character string. Either `"min"` or `"max"`.
#'   If `"min"`, the function selects the maximum x where y is close to min(y).
#'   If `"max"`, the function selects the maximum x where y is close to max(y).
#' @param sd_multiplier Numeric. Number of standard deviations used to define
#'   the cutoff. Default is 1.
#'
#' @return A data.table with one row per group and the selected turning_point.
#'
#' @import data.table
#' @export
estimate_turning_point_sd <- function(
    data,
    outcome,
    x_var,
    group_var,
    direction = c("min", "max"),
    sd_multiplier = 1) {

  data <- data.table::as.data.table(data.table::copy(data))

  direction <- match.arg(direction)

  required_vars <- c(outcome, x_var, group_var)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "The following required variables are missing from data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (!is.numeric(sd_multiplier) || length(sd_multiplier) != 1L || is.na(sd_multiplier)) {
    stop("sd_multiplier must be a single non-missing numeric value.")
  }

  if (sd_multiplier < 0) {
    stop("sd_multiplier must be non-negative.")
  }

  data[, (x_var) := as.numeric(get(x_var))]
  data[, (outcome) := as.numeric(get(outcome))]

  data <- data[!is.na(get(x_var)) & !is.na(get(outcome))]

  if (nrow(data) == 0) {
    stop("No valid observations remain after removing missing x_var or outcome values.")
  }

  # Collapse duplicate x-values within each group.
  data <- data[
    ,
    .(y_value = mean(get(outcome), na.rm = TRUE)),
    by = c(group_var, x_var)
  ]

  data.table::setnames(data, "y_value", outcome)

  out <- data[
    ,
    {
      y <- get(outcome)
      x <- get(x_var)

      y_sd <- stats::sd(y, na.rm = TRUE)

      if (direction == "min") {
        cutoff <- min(y, na.rm = TRUE) + sd_multiplier * y_sd
        selected_x <- max(x[y <= cutoff], na.rm = TRUE)
      }

      if (direction == "max") {
        cutoff <- max(y, na.rm = TRUE) - sd_multiplier * y_sd
        selected_x <- max(x[y >= cutoff], na.rm = TRUE)
      }

      data.table::data.table(
        turning_point = selected_x
      )
    },
    by = group_var
  ]

  out[]
}
