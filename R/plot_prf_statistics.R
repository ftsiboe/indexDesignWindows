#' Plot PRF Summary Statistics on the Official Grid
#'
#' @description
#' Creates a choropleth map of Pasture, Rangeland, Forage (PRF) statistics on
#' the official PRF grid. Optionally facets the map by a disaggregate variable
#' (e.g., data source: Official vs. Study Approximation).
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Loads the official PRF grid polygons via
#'         \code{get_official_prf_polygon()} if \code{prf_polygon} is not supplied.
#'   \item Filters PRF polygons to those with \code{grid_id} present in \code{data}.
#'   \item Transforms the PRF polygons to match the CRS of the state boundaries
#'         from \code{urbnmapr::get_urbn_map(map = "states")}.
#'   \item Joins \code{data} to the PRF polygons by \code{grid_id}.
#'   \item Filters out rows with missing values in \code{outcome_variable}.
#'   \item Plots a choropleth map using the supplied \code{palette}, overlaying
#'         U.S. state boundaries (excluding Alaska and Hawaii; FIPS 2 and 15).
#'   \item Optionally facets the plot by \code{disaggregate_variable}, with two
#'         columns of panels.
#' }
#'
#' The function assumes that \code{data} contains a column named \code{grid_id}
#' matching the PRF grid IDs, and a column whose name is given by
#' \code{outcome_variable}. When \code{disaggregate_variable} is provided, it
#' should also be a column in \code{data}.
#'
#' @param data A data frame containing at least \code{grid_id} and the variable
#'   specified in \code{outcome_variable}. If \code{disaggregate_variable} is
#'   used, it must also be present in \code{data}.
#' @param outcome_variable Character scalar giving the name of the variable in
#'   \code{data} to map (e.g., a binned category such as \code{"mean_cat"}).
#' @param plot_title Character scalar used as the main plot title.
#' @param prf_polygon Optional \code{sf} object containing PRF grid polygons
#'   with a \code{grid_id} column. If \code{NULL}, the function calls
#'   \code{get_official_prf_polygon()}.
#' @param disaggregate_variable Optional character scalar giving the name of a
#'   column in \code{data} used for faceting (e.g., \code{"disaggregate"} or
#'   \code{"source"}). If \code{NULL}, no facets are drawn.
#' @param spatial_unit Character scalar. One of \code{"prf_grid"} or \code{"county"}.
#'   Determines which polygons are used and which join key is required.
#' @param palette Character vector of hex color codes used for the fill scale.
#'   The default is a 10-color sequential/diverging palette tailored to PRF maps.
#' @param na.value Fill color for the background (non-data areas) and missing values.
#'
#' @return
#' A \code{ggplot} object representing the PRF statistics map, optionally with
#' facets by \code{disaggregate_variable}.
#'
#' @examples
#' \dontrun{
#' p <- plot_prf_statistics(
#'   data               = df,
#'   outcome_variable   = "mean_cat",
#'   plot_title         = "PRF Index Mean (2016-2024)",
#'   disaggregate_variable = "disaggregate"
#' )
#' print(p)
#' }
#'
#' @export
plot_prf_statistics <- function(
    data,
    outcome_variable,
    plot_title,
    prf_polygon = NULL,
    disaggregate_variable = NULL,
    spatial_unit ="prf_grid",
    palette = c(
      "#FEF9C5",
      "#FFC425",
      "#FEF389",
      "#E7F2B4",
      "#D7E5C8",
      "#C5DE91",
      "#BED73B",
      "#A0BD78",
      "#00583D",
      "#0F374B"
    ),
    na.value = "white") {

  if (!outcome_variable %in% names(data)) {
    stop("`outcome_variable` must be a column in `data`.")
  }

  if (!is.null(disaggregate_variable) &&
      !disaggregate_variable %in% names(data)) {
    stop("`disaggregate_variable` must be a column in `data` when supplied.")
  }

  us_states <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)

  if(spatial_unit == "prf_grid"){
    # ---- Basic checks
    if (!"grid_id" %in% names(data)) {
      stop("`data` must contain a `grid_id` column.")
    }

    # ---- Load official PRF polygons if none supplied
    if (is.null(prf_polygon)) {
      prf_polygon <- get_official_prf_polygon()
    }

    if (!"grid_id" %in% names(prf_polygon)) {
      stop("`prf_polygon` must contain a `grid_id` column.")
    }
    # Keep only PRF cells that appear in the data
    prf_polygon <- prf_polygon[prf_polygon$grid_id %in% unique(data$grid_id), ]

    # Ensure sf
    prf_polygon <- sf::st_as_sf(prf_polygon)

    # Get US state boundaries (sf) and align CRS
    prf_polygon <- sf::st_transform(prf_polygon, sf::st_crs(us_states))

    # ---- Join data to polygons and filter missing outcome
    sf_object <- prf_polygon |>
      dplyr::left_join(data, by = "grid_id") |>
      dplyr::filter(!is.na(.data[[outcome_variable]]))
  }

  if(spatial_unit == "county"){
    prf_polygon <- urbnmapr::get_urbn_map(map = "counties", sf = TRUE)

    if (!"county_fips" %in% names(data)) {
      stop("`data` must contain a `county_fips` column.")
    }

    # ---- Join data to polygons and filter missing outcome
    sf_object <- prf_polygon |>
      dplyr::left_join(data, by = "county_fips") |>
      dplyr::filter(!is.na(.data[[outcome_variable]]))
  }

  # ---- Base map
  p <- ggplot() +
    geom_sf(
      data   = us_states[!as.numeric(us_states$state_fips) %in% c(2, 15), ],
      colour = "black",
      fill   = na.value,
      size   = 0.1
    ) +
    geom_sf(
      data   = sf_object,
      aes(fill = .data[[outcome_variable]]),
      colour = NA,
      size   = 0.2
    ) +
    geom_sf(
      data   = us_states[!as.numeric(us_states$state_fips) %in% c(2, 15), ],
      colour = "black",
      fill   = NA,
      size   = 0.1
    ) +
    scale_fill_manual(
      values   = palette,
      na.value = na.value,
      name     = ""
    ) +
    labs(title = plot_title) +
    guides(fill = guide_legend(nrow = 2)) +
    theme_bw() +
    theme(
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.ticks        = element_blank(),
      axis.text         = element_blank(),
      axis.title.x      = element_blank(),
      axis.title.y      = element_blank(),
      panel.border      = ggplot2::element_blank(),
      legend.position   = "top",
      legend.background = element_blank(),
      legend.key.size   = grid::unit(0.2, "cm"),
      legend.text       = element_text(size = 5),
      legend.title      = element_blank(),
      plot.title        = element_text(size = 9, hjust = 0.5),
      strip.text        = element_text(size = 7),
      strip.background  = element_blank()
    ) +
    coord_sf()

  # ---- Optional facet by disaggregate
  if (!is.null(disaggregate_variable)) {
    p <- p +
      facet_wrap(
        stats::as.formula(paste("~", disaggregate_variable)),
        ncol = 2
      )
  }

  p
}



#' Reshape and Label Balance Statistics
#'
#' @description
#' Reshapes selected balance statistics from a data frame into long format and
#' assigns descriptive labels to each statistic disaggregate. This function is commonly
#' used to prepare datasets for balance comparison plots or summary tables.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Validates that \code{statistic_variables} and \code{disaggregate_labels} have equal length.
#'   \item Checks that all specified statistic variables exist in \code{data}.
#'   \item Selects \code{grid_id} and the variables listed in \code{statistic_variables}.
#'   \item Converts the data from wide to long format using \code{tidyr::gather()},
#'         creating columns:
#'         \code{grid_id}, \code{disaggregate}, and \code{value}.
#'   \item Converts \code{disaggregate} to a factor and replaces raw column names with
#'         human-readable labels using \code{disaggregate_labels}.
#'   \item Optionally renames the \code{value} column using the \code{rename} argument.
#' }
#'
#' @param data A data frame containing \code{grid_id} and the statistic variables to
#'   be reshaped.
#' @param statistic_variables Character vector of column names in \code{data} that
#'   contain the statistics to reshape.
#' @param disaggregate_labels Character vector of descriptive labels corresponding to
#'   \code{statistic_variables}. Must be the same length and in the same order.
#' @param rename Optional character scalar. If supplied, the output column
#'   \code{value} will be renamed to this string (e.g., \code{"std_diff"} or
#'   \code{"index"}).
#' @param panel_variables panel variables
#' @return A long-format data frame with the columns:
#' \describe{
#'   \item{grid_id}{Identifier for the spatial/grid unit.}
#'   \item{disaggregate}{Factor indicating the origin of each statistic, labeled using
#'                 \code{disaggregate_labels}.}
#'   \item{value}{Numeric value of the statistic, or renamed to \code{rename} if provided.}
#' }
#'
#' @export
reshape_statistics <- function(
    data,
    statistic_variables,
    disaggregate_labels,
    rename = NULL,
    panel_variables) {

  # ---- Input validation
  if (length(statistic_variables) != length(disaggregate_labels)) {
    stop("Length of `statistic_variables` must match length of `disaggregate_labels`.")
  }

  missing_vars <- setdiff(statistic_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(
      "The following variables are not in `data`: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (!is.null(rename)) {
    if (!is.character(rename) || length(rename) != 1L) {
      stop("`rename` must be a character scalar if provided.")
    }
  }

  # ---- Reshape data
  data <- data[c(panel_variables, statistic_variables)] |>
    tidyr::gather(disaggregate, value, statistic_variables)

  # ---- Assign factor labels
  data$disaggregate <- factor(
    data$disaggregate,
    levels = statistic_variables,
    labels = disaggregate_labels
  )

  # ---- Optionally rename `value` column
  if (!is.null(rename)) {
    names(data)[names(data) == "value"] <- rename
  }

  return(data)
}



#' Add Categorical Breaks to a Numeric Variable
#'
#' @description
#' Classifies a numeric variable into categorical intervals using either
#' Fisher-Jenks natural breaks (via \pkg{classInt}) or user-specified break
#' points, and appends the resulting factor variable to the input data.
#'
#' @details
#' The function supports two main workflows:
#' \itemize{
#'   \item \strong{Automatic breaks}: If \code{break_levels} is \code{NULL},
#'         Fisher-Jenks natural breaks are computed using
#'         \code{classInt::classIntervals()} with \code{style = "fisher"}.
#'         In this case, \code{break_n} must be supplied.
#'   \item \strong{Manual breaks}: If \code{break_levels} is provided, these
#'         are used directly and \code{break_n} is ignored. If
#'         \code{break_labels} is \code{NULL}, labels are generated from
#'         \code{break_levels} in the form \code{"a - b"} (two decimal places).
#' }
#'
#' Intervals are constructed with \code{include.lowest = TRUE} and
#' \code{right = FALSE}, so they are left-closed and right-open.
#'
#' @param data A data frame containing the variable to be classified.
#' @param variable Character scalar giving the name of the numeric column in
#'   \code{data} to classify.
#' @param break_n Integer number of classes to compute when using Fisher-Jenks
#'   breaks. Required when \code{break_levels} is \code{NULL}.
#' @param break_levels Optional numeric vector of break points. If supplied,
#'   these are used directly and \code{break_n} is ignored.
#' @param break_labels Optional character vector of labels to use for the
#'   resulting factor. If \code{NULL}, labels are generated from
#'   \code{break_levels}.
#'
#' @return
#' The original \code{data} with an additional factor column named
#' \code{<variable>_cat} containing the categorical breaks.
#'
#' @export
add_break_categories <- function(
    data,
    variable,
    break_n = NULL,
    break_levels = NULL,
    break_labels = NULL) {

  # --- sanity checks
  if (is.null(break_levels) && is.null(break_n)) {
    stop("Either `break_levels` must be supplied or `break_n` must be non-NULL.")
  }

  if (!variable %in% names(data)) {
    stop("`variable` must be a column in `data`.")
  }

  # --- compute breaks if not supplied
  if (is.null(break_levels)) {
    idx_vec <- data[[variable]]
    jenks  <- classInt::classIntervals(idx_vec, n = break_n, style = "fisher")

    break_levels <- jenks$brks
  }

  # --- generate labels if not supplied
  if (is.null(break_labels)) {
    break_labels <- paste0(
      sprintf("%.2f", utils::head(break_levels, -1)),
      " \u2013 ",
      sprintf("%.2f", utils::tail(break_levels, -1))
    )
  }

  # --- categorize variable
  new_var_name <- paste0(variable, "_cat")

  data[[new_var_name]] <- cut(
    data[[variable]],
    breaks = break_levels,
    labels = break_labels,
    include.lowest = TRUE,
    right = FALSE
  )

  data
}
