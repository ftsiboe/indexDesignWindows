
rm(list = ls(all = TRUE));gc()
devtools::document()
# Data
source("data-raw/scripts/001_workflow_data.R")

# Download Rendered redesigns
fastscratch_directory <- "D:/personal/francis_tsiboe/fastscratch/indexDesignWindows"

# Download Precipitation Trends

# Download baseline analysis
get_study_releases(
    owner = "ftsiboe",
    repository = "indexDesignWindows",
    release_tag = "baseline",
    output_directory = "data-raw/releases/baseline"
)

# Download alternative analysis
get_study_releases(
  owner = "ftsiboe",
  repository = "indexDesignWindows",
  release_tag = "alternative",
  output_directory = "data-raw/releases/alternative"
)



list.files("data-raw/scripts")


