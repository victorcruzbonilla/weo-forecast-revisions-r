# ============================================================
# IMF WORLD ECONOMIC OUTLOOK FORECAST REVISIONS
# Coding Sample
#
# Author: Victor Manuel Cruz Bonilla
#
# Purpose:
# This script provides a reproducible analysis of changes in
# IMF World Economic Outlook (WEO) real GDP growth forecasts
# across successive publication vintages.
#
# The workflow focuses on:
#   1. Importing multiple WEO vintages
#   2. Harmonizing heterogeneous file structures
#   3. Cleaning and validating economic data
#   4. Preparing a comparable country-year dataset
#
# Data source:
# International Monetary Fund (IMF)
# World Economic Outlook Database
# ============================================================


# ------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------

# Required packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(readr)


# Project folders
data_dir   <- "data"
output_dir <- "output"


# Input files
file_oct_2025 <- file.path(data_dir, "WEO2025Oct.xlsx")
file_apr_2026 <- file.path(data_dir, "WEO2026Apr.xlsx")
file_oct_2026 <- file.path(data_dir, "WEO2026Oct.xlsx")


# Verify that all required files exist
required_files <- c(
  file_oct_2025,
  file_apr_2026,
  file_oct_2026
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required input file(s): ",
    paste(missing_files, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 2. DATA IMPORT
# ------------------------------------------------------------

# Function to read the first worksheet of each WEO workbook.
#
# The WEO files used in this project come from different
# publication vintages and therefore do not have identical
# worksheet names.

read_weo_file <- function(path) {

  sheet_name <- excel_sheets(path)[1]

  read_excel(
    path,
    sheet = sheet_name
  )
}


weo_oct_2025_raw <- read_weo_file(file_oct_2025)

weo_apr_2026_raw <- read_weo_file(file_apr_2026)

weo_oct_2026_raw <- read_weo_file(file_oct_2026)


# ------------------------------------------------------------
# 3. BASIC DATA VALIDATION
# ------------------------------------------------------------

# Confirm that imported datasets are not empty
stopifnot(
  nrow(weo_oct_2025_raw) > 0,
  nrow(weo_apr_2026_raw) > 0,
  nrow(weo_oct_2026_raw) > 0
)


# Variables expected in all WEO vintages
required_columns <- c(
  "COUNTRY",
  "INDICATOR"
)


validate_columns <- function(data, dataset_name) {

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0) {

    stop(
      dataset_name,
      " is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
}


validate_columns(
  weo_oct_2025_raw,
  "WEO October 2025"
)

validate_columns(
  weo_apr_2026_raw,
  "WEO April 2026"
)

validate_columns(
  weo_oct_2026_raw,
  "WEO October 2026"
)


# ------------------------------------------------------------
# 4. DATA HARMONIZATION
# ------------------------------------------------------------

# WEO vintages contain metadata columns followed by annual
# observations. The number of metadata fields differs across
# vintages, so year columns are identified dynamically.

harmonize_weo <- function(data, vintage_name) {

  year_columns <- names(data)[
    str_detect(names(data), "^\\d{4}$")
  ]


  if (length(year_columns) == 0) {
    stop(
      "No annual observation columns detected in ",
      vintage_name
    )
  }


  data %>%
    select(
      COUNTRY,
      INDICATOR,
      all_of(year_columns)
    ) %>%
    pivot_longer(
      cols = all_of(year_columns),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(
      year = as.integer(year),
      value = suppressWarnings(as.numeric(value)),
      vintage = vintage_name
    ) %>%
    filter(
      !is.na(COUNTRY),
      !is.na(INDICATOR)
    )
}


weo_oct_2025 <- harmonize_weo(
  weo_oct_2025_raw,
  "October 2025"
)

weo_apr_2026 <- harmonize_weo(
  weo_apr_2026_raw,
  "April 2026"
)

weo_oct_2026 <- harmonize_weo(
  weo_oct_2026_raw,
  "October 2026"
)


# Combine the three harmonized vintages
weo_all <- bind_rows(
  weo_oct_2025,
  weo_apr_2026,
  weo_oct_2026
)


# ------------------------------------------------------------
# 5. INITIAL CHECKS
# ------------------------------------------------------------

# Confirm that the three vintages are present
expected_vintages <- c(
  "October 2025",
  "April 2026",
  "October 2026"
)

stopifnot(
  all(
    expected_vintages %in%
      unique(weo_all$vintage)
  )
)


# Display a compact summary
data_summary <- weo_all %>%
  group_by(vintage) %>%
  summarise(
    observations = n(),
    countries = n_distinct(COUNTRY),
    indicators = n_distinct(INDICATOR),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  )

print(data_summary)
