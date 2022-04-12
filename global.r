# Packages/dependencies to import
packages <- c(
  "dplyr", 
  "DT", 
  "ggplot2",
  "plotly",
  "readr", 
  "readxl",
  "scales",
  "shiny", 
  "shinycssloaders", 
  "shinyjs", 
  "shinyWidgets", 
  "stringr", 
  "summarytools", 
  "tidyr",
  "viridis"
)

# If any packages are not installed, install
uninstalled_packages <- packages[! packages %in% installed.packages()[,"Package"]]
if(length(uninstalled_packages) > 0) install.packages(uninstalled_packages)

# Load packages
import_packages <- lapply(packages, library, character.only = TRUE)

# Import user-defined functions
source_functions <- lapply(list.files("functions/", pattern = ".r"), function(x) source(paste0("functions/", x)))

# Define allowed scenario file extensions
allowed_file_extensions <- c(".xls", ".xlsx")