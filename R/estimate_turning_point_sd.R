#' Find X Values Where Y Is Within One Standard Deviation of Min or Max Y
#'
#' This function identifies the x-values for which the corresponding y-value is
#' within a specified number of standard deviations of either the minimum or
#' maximum y-value. The procedure can be applied separately by group.
#'
#' @param data A data.frame or data.table containing x, y, and optional group variables.
#'
#' @param outcome Character string. Name of the y-variable.
#'
#' @param x_var Character string. Name of the x-variable.
#'
#' @param group_var Character string or NULL. Name of the grouping variable.
#'   If NULL, the function is applied to the full dataset.
#'
#' @param sd_multiplier Numeric. Number of standard deviations to use around
#'   the minimum and maximum y-values. The default is 1.
#'
#' @return A data.table containing the selected x-values, y-values, group,
#'   minimum y, maximum y, standard deviation of y, and the classification.
#'
#' @import data.table
#' @export
find_x_within_sd_extremes <- function(
    data,
    outcome,
    x_var,
    group_var = NULL,
    sd_multiplier = 1) {

  # Work on a local copy so the original data is not modified by reference.
  data <- data.table::as.data.table(data.table::copy(data))

  # Check required variables.
  required_vars <- c(outcome, x_var, group_var)
  required_vars <- required_vars[!is.null(required_vars)]

  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "The following required variables are missing from data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Validate sd_multiplier.
  if (!is.numeric(sd_multiplier) || length(sd_multiplier) != 1L || is.na(sd_multiplier)) {
    stop("sd_multiplier must be a single non-missing numeric value.")
  }

  if (sd_multiplier < 0) {
    stop("sd_multiplier must be non-negative.")
  }

  # Ensure x and y are numeric.
  data[, (x_var) := as.numeric(get(x_var))]
  data[, (outcome) := as.numeric(get(outcome))]

  # Remove missing values.
  data <- data[!is.na(get(x_var)) & !is.na(get(outcome))]

  if (nrow(data) == 0) {
    stop("No valid observations remain after removing missing x_var or outcome values.")
  }

  # If no group variable is supplied, create a temporary group.
  temp_group <- FALSE

  if (is.null(group_var)) {
    group_var <- ".group"
    data[, ".group" := "all"]
    temp_group <- TRUE
  }

  # Collapse duplicate x-values within each group.
  data <- data[
    ,
    .(y_value = mean(get(outcome), na.rm = TRUE)),
    by = c(group_var, x_var)
  ]

  data.table::setnames(data, "y_value", outcome)

  # Compute group-specific min, max, and standard deviation.
  data[
    ,
    `:=`(
      y_min = min(get(outcome), na.rm = TRUE),
      y_max = max(get(outcome), na.rm = TRUE),
      y_sd  = stats::sd(get(outcome), na.rm = TRUE)
    ),
    by = group_var
  ]

  # Define lower and upper extreme bands.
  data[
    ,
    `:=`(
      lower_band_cutoff = y_min + sd_multiplier * y_sd,
      upper_band_cutoff = y_max - sd_multiplier * y_sd
    )
  ]

  # Identify observations within one SD of the minimum or maximum y.
  data[
    ,
    `:=`(
      within_sd_of_min = get(outcome) <= lower_band_cutoff,
      within_sd_of_max = get(outcome) >= upper_band_cutoff
    )
  ]

  # Keep selected observations only.
  out <- data[within_sd_of_min == TRUE | within_sd_of_max == TRUE]

  # Classify selected observations.
  out[
    ,
    extreme_group := data.table::fifelse(
      within_sd_of_min & within_sd_of_max,
      "within_sd_of_both",
      data.table::fifelse(
        within_sd_of_min,
        "within_sd_of_min",
        "within_sd_of_max"
      )
    )
  ]

  # Add outcome name and SD multiplier for clarity.
  out[
    ,
    `:=`(
      outcome = outcome,
      sd_multiplier = sd_multiplier
    )
  ]

  # Remove temporary group if needed.
  if (temp_group) {
    out[, ".group" := NULL]
  }

  # Order output.
  keep_cols <- c(
    if (!temp_group) group_var,
    "outcome",
    x_var,
    outcome,
    "y_min",
    "y_max",
    "y_sd",
    "sd_multiplier",
    "lower_band_cutoff",
    "upper_band_cutoff",
    "within_sd_of_min",
    "within_sd_of_max",
    "extreme_group"
  )

  data.table::setcolorder(out, keep_cols)

  out[]
}
