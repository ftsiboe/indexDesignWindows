#' Download all files from a GitHub release into a local directory
#'
#' Downloads **all assets** attached to a specified GitHub release tag using
#' the \pkg{piggyback} API. This is intended for replication packages and
#' study-specific data releases where release artifacts are stored on GitHub
#' and need to be pulled programmatically into a local project directory.
#'
#' By default, files are downloaded into
#' \code{data-raw/releases/{release_tag}}. If the directory does not exist,
#' it is created automatically.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Constructs a default output directory if none is supplied.
#'   \item Creates the output directory recursively if needed.
#'   \item Downloads **all assets** associated with the specified GitHub
#'         release tag using \code{piggyback::pb_download()}.
#'   \item Overwrites existing files with the same name.
#' }
#'
#' Authentication is optional for public repositories but recommended for
#' private repositories or when downloading large files to avoid GitHub
#' rate limits.
#'
#' @param owner Character string giving the GitHub repository owner
#'   (e.g., \code{"ftsiboe"}).
#' @param repository Character string giving the GitHub repository name
#'   (e.g., \code{"indexDesignWindows"}).
#' @param release_tag Character string specifying the GitHub release tag
#'   to download (e.g., \code{"baseline"}).
#' @param output_directory Optional character string specifying the local
#'   directory where release files should be saved. If \code{NULL},
#'   defaults to \code{data-raw/releases/{release_tag}}.
#' @param github_token Optional GitHub personal access token (PAT).
#'   Passed directly to \code{piggyback::pb_download()} via \code{.token}.
#'
#' @return
#' Invisibly returns \code{NULL}. Files are downloaded for their side effects.
#'
#' @export
get_study_releases <- function(
    owner,
    repository,
    release_tag,
    output_directory = NULL,
    github_token = NULL
){

  # --- Validate inputs --------------------------------------------------------
  if (missing(owner) || is.null(owner) || !nzchar(owner)) {
    stop("`owner` must be a non-empty character string.", call. = FALSE)
  }
  if (missing(repository) || is.null(repository) || !nzchar(repository)) {
    stop("`repository` must be a non-empty character string.", call. = FALSE)
  }
  if (missing(release_tag) || is.null(release_tag) || !nzchar(release_tag)) {
    stop("`release_tag` must be a non-empty character string.", call. = FALSE)
  }

  # If token not supplied, try common env vars (safe no-op if unset)
  # if (is.null(github_token) || !nzchar(github_token)) {
  #   # github_token <- Sys.getenv("GITHUB_TOKEN", unset = Sys.getenv("GITHUB_PAT", unset = ""))
  #   if (!nzchar(github_token)) github_token <- NULL
  # }

  # --- Output directory -------------------------------------------------------
  if (is.null(output_directory) || !nzchar(output_directory)) {
    output_directory <- file.path("data-raw", "releases", release_tag)
  }
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_directory)) {
    stop("Failed to create `output_directory`: ", output_directory, call. = FALSE)
  }

  # --- Download ---------------------------------------------------------------
  repo <- paste(owner, repository, sep = "/")  # safer than file.path for GitHub repo IDs

  tryCatch(
    piggyback::pb_download(
      file      = NULL,
      repo      = repo,
      tag       = release_tag,
      dest      = output_directory,
      overwrite = TRUE,
      .token    = github_token
    ),
    error = function(e) {
      stop(
        "Failed to download GitHub release assets.\n",
        "  repo: ", repo, "\n",
        "  tag:  ", release_tag, "\n",
        "  dest: ", output_directory, "\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(NULL)
}





