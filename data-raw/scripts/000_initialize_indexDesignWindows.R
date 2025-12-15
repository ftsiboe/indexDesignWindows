# usethis::edit_r_environ()
# devtools::install_github("ftsiboe/rfcipPRF",force = TRUE,upgrade = "never")
# Hard reset of workspace
rm(list = ls(all = TRUE)); gc()

# Clean generated artifacts
unlink(c(
  "NAMESPACE",
  #list.files("./data", full.names = TRUE),
  list.files("./man",  full.names = TRUE)
))

if(toupper(as.character(Sys.info()[["sysname"]])) %in% "WINDOWS"){
  source( file.path(dirname(dirname(getwd())),"codeLibrary.R"))
  list_function <- c(
    file.path(codeLibrary,"plot/ers_theme.R"),
    file.path(codeLibrary,"plot/plot_helpers.R"),
    file.path(codeLibrary,"github_tools/get_study_releases.R")
  )
  file.copy(from= list_function, to = "R/", overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
}

list_function <- c(
  "https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/R/get_price_indices.R",
  "https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/R/setup_environment.R"
)

for(i in list_function){
  download.file(i, paste0("./R/",basename(i)), mode = "wb", quiet = TRUE)
}

list_tests <- c(
  "https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/tests/testthat/test-setup_environment.R"
)

for(i in list_tests){
  download.file(i, paste0("tests/testthat/test-",basename(i)), mode = "wb", quiet = TRUE)
}

# source("data-raw/scripts/run_internal_datasets_rfcipDemand.R")
# unlink(list.files("R",full.names = TRUE,pattern = "build_internal_datasets.R"))

# Sanity pass through R/ sources: shows any non-ASCII characters per file
for (i in list.files("R", full.names = TRUE)) {
  print(paste0("********************", i, "********************"))
  tools::showNonASCIIfile(i)
}

# Rebuild documentation from roxygen comments
devtools::document()

# Check man pages only (faster than full devtools::check)
devtools::check_man()

# Build PDF manual into the current working directory
devtools::build_manual(path = getwd())

# Optional: run tests / full package check (uncomment when needed)
# devtools::test()
devtools::check()


if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

piggyback::pb_release_create(
  repo = "ftsiboe/indexDesignWindows",
  tag  = "redesigns",
  name = "PRF Index Redesign Outputs",
  body = paste(
    "This release contains outputs from alternative PRF index design experiments.",
    "",
    "## Contents",
    "- **Index files**: `prf_index_WWW.rds` — the underlying PRF index constructed using a historical window of length `WWW` (years).",
    "- **Rates & payment factors**: `prf_rates_WWW_YYYY.rds` — rates and payment factors for crop year `YYYY`, based on the same `WWW`-year index design.",
    "",
    "## Naming convention",
    "- `WWW` = length of the historical window (years), zero-padded to three digits.",
    "  - Example: `005` = 5-year historical window.",
    "- `YYYY` = crop year for which rates/payment factors are produced.",
    "",
    "## Examples",
    "- `prf_index_005.rds`",
    "- `prf_rates_005_2024.rds`",
    sep = "\n"
  )
)


piggyback::pb_release_create(
  repo = "ftsiboe/indexDesignWindows",
  tag  = "baseline",
  name = "Baseline Outputs",
  body = paste(
    "This release contains outputs from alternative PRF index design experiments baseline.",
    sep = "\n"
  )
)

piggyback::pb_release_create(
  repo = "ftsiboe/indexDesignWindows",
  tag  = "statistical_threshold",
  name = "Statistical Threshold Outputs",
  body = paste(
    "This release contains outputs from alternative PRF index design experiments statistical threshold analysis.",
    sep = "\n"
  )
)
