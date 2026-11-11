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
# Research question:
# How did IMF real GDP growth forecasts for selected Latin
# American economies change across the October 2025,
# April 2026, and October 2026 WEO vintages?
#
# Workflow:
#   1. Project setup
#   2. Data import
#   3. Data validation
#   4. Data harmonization
#   5. Forecast extraction
#   6. Variable construction
#   7. Cross-country analysis
#   8. Visualization
#   9. Reproducible output generation
#
# Data source:
# International Monetary Fund (IMF)
# World Economic Outlook Database
# ============================================================


# ------------------------------------------------------------
# 1. PROJECT SETUP
# ------------------------------------------------------------

# Required packages
required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "readr"
)


# Check whether required packages are installed
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )
]


if (length(missing_packages) > 0) {

  stop(
    paste0(
      "The following required R packages are not installed: ",
      paste(missing_packages, collapse = ", "),
      "\n\nPlease install them using:\n",
      "install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}


# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)


# ------------------------------------------------------------
# 2. AUTOMATIC PROJECT ROOT DETECTION
# ------------------------------------------------------------

# The script determines its own location so users do not need
# to manually set the working directory.
#
# This allows the repository to be downloaded, unzipped,
# opened in RStudio, and executed directly.


get_script_path <- function() {

  # Option 1: RStudio
  if (
    interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {

    path <- rstudioapi::getSourceEditorContext()$path

    if (nzchar(path)) {
      return(
        normalizePath(
          path,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }


  # Option 2: Rscript from command line
  command_args <- commandArgs(
    trailingOnly = FALSE
  )

  file_argument <- grep(
    "^--file=",
    command_args,
    value = TRUE
  )

  if (length(file_argument) > 0) {

    path <- sub(
      "^--file=",
      "",
      file_argument[1]
    )

    return(
      normalizePath(
        path,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }


  # Fallback
  return(
    normalizePath(
      file.path(
        getwd(),
        "coding_sample.R"
      ),
      winslash = "/",
      mustWork = FALSE
    )
  )
}


script_path <- get_script_path()

project_root <- dirname(
  script_path
)


# Project folders
data_dir <- file.path(
  project_root,
  "data"
)

output_dir <- file.path(
  project_root,
  "output"
)


# Create output folder if needed
if (!dir.exists(output_dir)) {

  dir.create(
    output_dir,
    recursive = TRUE
  )
}


# ------------------------------------------------------------
# 3. INPUT FILES
# ------------------------------------------------------------

file_oct_2025 <- file.path(
  data_dir,
  "WEO2025Oct.xlsx"
)

file_apr_2026 <- file.path(
  data_dir,
  "WEO2026Apr.xlsx"
)

file_oct_2026 <- file.path(
  data_dir,
  "WEO2026Oct.xlsx"
)


required_files <- c(
  file_oct_2025,
  file_apr_2026,
  file_oct_2026
)


missing_files <- required_files[
  !file.exists(required_files)
]


if (length(missing_files) > 0) {

  stop(
    paste0(
      "Missing required input file(s):\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  )
}


message(
  "Project root detected: ",
  project_root
)


message(
  "All required input files were found."
)


# ------------------------------------------------------------
# 4. DATA IMPORT
# ------------------------------------------------------------

# WEO workbooks may use different worksheet names across
# publication vintages. The first worksheet is therefore
# selected dynamically.


read_weo_file <- function(path) {

  sheet_name <- excel_sheets(path)[1]

  read_excel(
    path,
    sheet = sheet_name
  )
}


weo_oct_2025_raw <- read_weo_file(
  file_oct_2025
)

weo_apr_2026_raw <- read_weo_file(
  file_apr_2026
)

weo_oct_2026_raw <- read_weo_file(
  file_oct_2026
)


# ------------------------------------------------------------
# 5. BASIC DATA VALIDATION
# ------------------------------------------------------------

# Confirm that imported datasets contain observations.

stopifnot(
  nrow(weo_oct_2025_raw) > 0,
  nrow(weo_apr_2026_raw) > 0,
  nrow(weo_oct_2026_raw) > 0
)


# Variables required for the analysis.

required_columns <- c(
  "SERIES_CODE",
  "COUNTRY",
  "INDICATOR"
)


validate_columns <- function(
  data,
  dataset_name
) {

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )


  if (length(missing_columns) > 0) {

    stop(
      paste0(
        dataset_name,
        " is missing required column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
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
# 6. DATA HARMONIZATION
# ------------------------------------------------------------

# WEO vintages contain metadata followed by annual observations.
# Because metadata structures vary across vintages, year columns
# are identified dynamically rather than by fixed column numbers.


harmonize_weo <- function(
  data,
  vintage_name
) {

  year_columns <- names(data)[
    str_detect(
      names(data),
      "^\\d{4}$"
    )
  ]


  if (length(year_columns) == 0) {

    stop(
      paste0(
        "No annual observation columns detected in ",
        vintage_name,
        "."
      )
    )
  }


  harmonized_data <- data %>%

    select(
      SERIES_CODE,
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
      year = as.integer(
        year
      ),

      value = suppressWarnings(
        as.numeric(value)
      ),

      vintage = vintage_name
    ) %>%

    filter(
      !is.na(COUNTRY),
      !is.na(INDICATOR),
      !is.na(SERIES_CODE)
    )


  return(
    harmonized_data
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


# Combine harmonized datasets
weo_all <- bind_rows(
  weo_oct_2025,
  weo_apr_2026,
  weo_oct_2026
)


# ------------------------------------------------------------
# 7. DATA INTEGRITY CHECKS
# ------------------------------------------------------------

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


# Check uniqueness of country-series-year observations.

duplicate_observations <- weo_all %>%

  count(
    vintage,
    COUNTRY,
    SERIES_CODE,
    year
  ) %>%

  filter(
    n > 1
  )


if (nrow(duplicate_observations) > 0) {

  warning(
    paste0(
      nrow(duplicate_observations),
      " duplicate country-series-year observations detected."
    )
  )
}


# Summary of harmonized datasets.

data_summary <- weo_all %>%

  group_by(
    vintage
  ) %>%

  summarise(
    observations = n(),

    countries = n_distinct(
      COUNTRY
    ),

    indicators = n_distinct(
      SERIES_CODE
    ),

    first_year = min(
      year,
      na.rm = TRUE
    ),

    last_year = max(
      year,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


print(
  data_summary
)


# ------------------------------------------------------------
# 8. REAL GDP GROWTH FORECASTS
# ------------------------------------------------------------

# The main analysis focuses on the IMF WEO series
# NGDP_RPCH: annual percentage change in real GDP.
#
# The target forecast year is 2026.


gdp_growth_2026 <- weo_all %>%

  filter(
    str_detect(
      SERIES_CODE,
      "NGDP_RPCH"
    ),
    year == 2026
  ) %>%

  select(
    COUNTRY,
    vintage,
    value
  )


if (nrow(gdp_growth_2026) == 0) {

  stop(
    "No 2026 real GDP growth observations were found."
  )
}


# ------------------------------------------------------------
# 9. RESHAPE FORECAST VINTAGES
# ------------------------------------------------------------

# Convert publication vintages into separate columns so that
# forecast revisions can be calculated directly.


forecast_revisions <- gdp_growth_2026 %>%

  pivot_wider(
    names_from = vintage,
    values_from = value
  ) %>%

  rename(
    forecast_oct_2025 = `October 2025`,
    forecast_apr_2026 = `April 2026`,
    forecast_oct_2026 = `October 2026`
  )


# ------------------------------------------------------------
# 10. CONSTRUCT FORECAST REVISION VARIABLES
# ------------------------------------------------------------

# Revisions are measured in percentage points.


forecast_revisions <- forecast_revisions %>%

  mutate(

    revision_oct26_vs_oct25 =
      forecast_oct_2026 -
      forecast_oct_2025,

    revision_oct26_vs_apr26 =
      forecast_oct_2026 -
      forecast_apr_2026

  )


# ------------------------------------------------------------
# 11. VALIDATE FORECAST DATASET
# ------------------------------------------------------------

duplicate_check <- forecast_revisions %>%

  count(
    COUNTRY
  ) %>%

  filter(
    n > 1
  )


if (nrow(duplicate_check) > 0) {

  stop(
    "Duplicate country observations detected in forecast dataset."
  )
}


# Identify incomplete forecast histories.

missing_forecasts <- forecast_revisions %>%

  filter(
    is.na(forecast_oct_2025) |
      is.na(forecast_apr_2026) |
      is.na(forecast_oct_2026)
  )


if (nrow(missing_forecasts) > 0) {

  message(
    nrow(missing_forecasts),
    " countries contain at least one missing forecast."
  )
}


# ------------------------------------------------------------
# 12. SELECTED LATIN AMERICAN ECONOMIES
# ------------------------------------------------------------

# The coding sample uses a transparent, manually defined group
# of economies for a compact regional comparison.


selected_countries <- c(
  "El Salvador",
  "Guatemala",
  "Honduras",
  "Nicaragua",
  "Costa Rica",
  "Panama",
  "Mexico",
  "Colombia",
  "Peru",
  "Chile",
  "Brazil",
  "Argentina"
)


latin_america_revisions <- forecast_revisions %>%

  filter(
    COUNTRY %in%
      selected_countries
  ) %>%

  arrange(
    desc(
      revision_oct26_vs_oct25
    )
  )


# Check that requested comparison countries were found.

countries_found <- unique(
  latin_america_revisions$COUNTRY
)


countries_missing <- setdiff(
  selected_countries,
  countries_found
)


if (length(countries_missing) > 0) {

  message(
    "Selected countries not found in the final dataset: ",
    paste(
      countries_missing,
      collapse = ", "
    )
  )
}


# ------------------------------------------------------------
# 13. ANALYTICAL TABLE
# ------------------------------------------------------------

forecast_table <- latin_america_revisions %>%

  select(
    COUNTRY,
    forecast_oct_2025,
    forecast_apr_2026,
    forecast_oct_2026,
    revision_oct26_vs_oct25,
    revision_oct26_vs_apr26
  ) %>%

  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )


print(
  forecast_table
)


# ------------------------------------------------------------
# 14. EL SALVADOR EXAMPLE
# ------------------------------------------------------------

# El Salvador is extracted as a country-level example because
# it is the author's home economy and provides an intuitive
# illustration of how forecast revisions are calculated.


el_salvador <- forecast_table %>%

  filter(
    COUNTRY == "El Salvador"
  )


if (nrow(el_salvador) == 1) {

  print(
    el_salvador
  )

} else {

  warning(
    "El Salvador was not uniquely identified in the final dataset."
  )
}


# ------------------------------------------------------------
# 15. EXPORT ANALYTICAL TABLE
# ------------------------------------------------------------

table_output_path <- file.path(
  output_dir,
  "gdp_growth_forecast_revisions_2026.csv"
)


write_csv(
  forecast_table,
  table_output_path
)


# ------------------------------------------------------------
# 16. VISUALIZATION
# ------------------------------------------------------------

# The chart shows how the IMF's forecast for 2026 real GDP
# growth changed between the October 2025 and October 2026
# WEO vintages.


forecast_plot_data <- forecast_table %>%

  filter(
    !is.na(
      revision_oct26_vs_oct25
    )
  ) %>%

  mutate(
    COUNTRY = reorder(
      COUNTRY,
      revision_oct26_vs_oct25
    )
  )


forecast_revision_plot <- ggplot(
  forecast_plot_data,
  aes(
    x = revision_oct26_vs_oct25,
    y = COUNTRY
  )
) +

  geom_col(
    width = 0.68,
    fill = "#2A5298"
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +

  labs(
    title =
      "Revisions to IMF 2026 Real GDP Growth Forecasts",

    subtitle =
      "Change between the October 2025 and October 2026 WEO vintages",

    x =
      "Forecast revision (percentage points)",

    y = NULL,

    caption =
      "Source: IMF World Economic Outlook Database."
  ) +

  theme_minimal(
    base_size = 12
  ) +

  theme(

    panel.grid.major.y =
      element_blank(),

    panel.grid.minor =
      element_blank(),

    plot.title =
      element_text(
        face = "bold",
        size = 15
      ),

    plot.subtitle =
      element_text(
        size = 11
      ),

    plot.caption =
      element_text(
        hjust = 0,
        size = 9
      )

  )


print(
  forecast_revision_plot
)


# ------------------------------------------------------------
# 17. EXPORT FIGURE
# ------------------------------------------------------------

figure_output_path <- file.path(
  output_dir,
  "gdp_growth_forecast_revisions_2026.png"
)


ggsave(
  filename = figure_output_path,

  plot = forecast_revision_plot,

  width = 9,

  height = 6,

  dpi = 300
)


# ------------------------------------------------------------
# 18. COMPLETION CHECK
# ------------------------------------------------------------

stopifnot(
  file.exists(
    table_output_path
  ),
  file.exists(
    figure_output_path
  )
)


message(
  "\nAnalysis completed successfully."
)

message(
  "Table saved to: ",
  table_output_path
)

message(
  "Figure saved to: ",
  figure_output_path
)
