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
#   5. Constructing forecast revision variables
#   6. Producing reproducible analytical outputs
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


# Verify that all required input files exist
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
    "Missing required input file(s): ",
    paste(missing_files, collapse = ", ")
  )
}


# Create output directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# ------------------------------------------------------------
# 2. DATA IMPORT
# ------------------------------------------------------------

# Function to read the first worksheet of each WEO workbook.
#
# The WEO files used in this project come from different
# publication vintages and therefore do not necessarily have
# identical worksheet names.

read_weo_file <- function(path) {

  sheet_name <- excel_sheets(path)[1]

  read_excel(
    path,
    sheet = sheet_name
  )
}


# Import raw WEO vintages
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
  "SERIES_CODE",
  "COUNTRY",
  "INDICATOR"
)


# Function to verify required variables
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
      dataset_name,
      " is missing required column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }
}


# Validate each WEO vintage
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


  # Stop if no annual columns are identified
  if (length(year_columns) == 0) {

    stop(
      "No annual observation columns detected in ",
      vintage_name
    )
  }


  data %>%

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
      year = as.integer(year),

      value = suppressWarnings(
        as.numeric(value)
      ),

      vintage = vintage_name
    ) %>%

    filter(
      !is.na(COUNTRY),
      !is.na(INDICATOR)
    )
}


# Harmonize each vintage
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

# Confirm that the expected vintages are present
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


# Check for duplicate country-indicator-year observations
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
    "Duplicate country-indicator-year observations detected."
  )
}


# Produce compact summary by WEO vintage
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
# 6. REAL GDP GROWTH FORECAST REVISIONS
# ------------------------------------------------------------

# The main analysis focuses on real GDP growth forecasts
# for 2026.
#
# The IMF WEO indicator for real GDP growth is NGDP_RPCH.


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


# ------------------------------------------------------------
# 7. RESHAPE FORECAST VINTAGES
# ------------------------------------------------------------

# Convert the three WEO vintages into separate columns
# so forecast revisions can be calculated directly.

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
# 8. CONSTRUCT FORECAST REVISION VARIABLES
# ------------------------------------------------------------

# Forecast revisions are measured in percentage points.

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
# 9. VALIDATE FORECAST DATA
# ------------------------------------------------------------

# Verify that each country has only one observation
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


# Identify countries with incomplete forecasts
missing_forecasts <- forecast_revisions %>%

  filter(
    is.na(forecast_oct_2025) |
      is.na(forecast_apr_2026) |
      is.na(forecast_oct_2026)
  )


if (nrow(missing_forecasts) > 0) {

  message(
    nrow(missing_forecasts),
    " country observations contain at least one missing forecast."
  )
}


# ------------------------------------------------------------
# 10. SELECTED LATIN AMERICAN ECONOMIES
# ------------------------------------------------------------

# A selected group of Latin American economies is used
# to illustrate cross-country forecast revisions.

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


# ------------------------------------------------------------
# 11. ANALYTICAL TABLE
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
# 12. EL SALVADOR CHECK
# ------------------------------------------------------------

# Extract El Salvador as a country-level example.

el_salvador <- forecast_table %>%

  filter(
    COUNTRY == "El Salvador"
  )


print(
  el_salvador
)


# ------------------------------------------------------------
# 13. EXPORT ANALYTICAL TABLE
# ------------------------------------------------------------

write_csv(
  forecast_table,
  file.path(
    output_dir,
    "gdp_growth_forecast_revisions_2026.csv"
  )
)


# ------------------------------------------------------------
# 14. FORECAST REVISION FIGURE
# ------------------------------------------------------------

# The figure shows the cumulative change in the IMF 2026
# real GDP growth forecast between October 2025 and
# October 2026.

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
    width = 0.7
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +

  labs(
    title = "Revisions to IMF 2026 Real GDP Growth Forecasts",
    subtitle = "Change between the October 2025 and October 2026 WEO vintages",
    x = "Forecast revision (percentage points)",
    y = NULL,
    caption = "Source: IMF World Economic Outlook Database."
  ) +

  theme_minimal(
    base_size = 12
  ) +

  theme(
    panel.grid.major.y =
      element_blank(),

    plot.title =
      element_text(
        face = "bold"
      )
  )


print(
  forecast_revision_plot
)


# ------------------------------------------------------------
# 15. EXPORT FIGURE
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "gdp_growth_forecast_revisions_2026.png"
  ),

  plot = forecast_revision_plot,

  width = 9,

  height = 6,

  dpi = 300
)


# ------------------------------------------------------------
# 16. COMPLETION MESSAGE
# ------------------------------------------------------------

message(
  "Analysis completed successfully."
)

message(
  "Outputs saved in: ",
  output_dir
)
