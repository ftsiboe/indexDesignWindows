#' Retrieve the official RMA PRF grid shapefile
#'
#' @description
#' Downloads (if necessary) and loads the official USDA RMA Pasture, Rangeland,
#' and Forage (PRF) Rainfall Index (RI) grid shapefile from the
#' \code{USFarmSafetyNetLab} GitHub repository. The shapefile is cached locally
#' in the user's R cache directory for subsequent use.
#'
#' @param official_prf_polygon_zip_file_path official_prf_polygon_zip_file_path.
#'
#' The cached shapefile will be reused on subsequent calls, reducing redundant
#' downloads.
#'
#' @return
#' A \link[terra]{SpatVector} object containing the official PRF RI grid polygons.
#' The field \code{grid_id} uniquely identifies each grid cell.
#'
#' @export
get_official_prf_polygon <- function(official_prf_polygon_zip_file_path = NULL) {

  # Define package-specific cache directory
  temporary_dir <- tempdir()

  if (!dir.exists(temporary_dir)) {
    dir.create(temporary_dir, recursive = TRUE)
  }

  shapefile_path <- file.path(temporary_dir, "official_RMA_RI_grid.shp")
  zip_path <- file.path(temporary_dir, "official_RMA_RI_grid_01.zip")

  # Download and unzip if shapefile is missing
  if (!file.exists(shapefile_path)) {
    message("Downloading official PRF grid shapefile ...")
    utils::unzip(official_prf_polygon_zip_file_path, exdir = temporary_dir)
  }

  # Load shapefile as SpatVector
  prf <- terra::vect(shapefile_path)

  # Normalize ID field name
  if ("GRIDCODE" %in% names(prf)) {
    names(prf)[names(prf) == "GRIDCODE"] <- "grid_id"
  }

  # Sanity check
  if (!"grid_id" %in% names(prf)) {
    stop("The PRF shapefile does not contain a 'grid_id' or 'GRIDCODE' field.")
  }

  return(prf)
}
