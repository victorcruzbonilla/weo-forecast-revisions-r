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

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "readr",
  "patchwork"
)


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


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(patchwork)


# ------------------------------------------------------------
# 2. AUTOMATIC PROJECT ROOT DETECTION
# ------------------------------------------------------------

get_script_path <- function() {

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


  normalizePath(
    file.path(
      getwd(),
      "coding_sample.R"
    ),
    winslash = "/",
    mustWork = FALSE
  )
}


script_path <- get_script_path()

project_root <- dirname(
  script_path
)


data_dir <- file.path(
  project_root,
  "data"
)

output_dir <- file.path(
  project_root,
  "output"
)


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

stopifnot(
  nrow(weo_oct_2025_raw) > 0,
  nrow(weo_apr_2026_raw) > 0,
  nrow(weo_oct_2026_raw) > 0
)


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
      !is.na(INDICATOR),
      !is.na(SERIES_CODE)
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


data_summary <- weo_all %>%

  group_by(
    vintage
  ) %>%

  summarise(
    observations = n(),
    countries = n_distinct(COUNTRY),
    indicators = n_distinct(SERIES_CODE),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  )


print(
  data_summary
)


# ------------------------------------------------------------
# 8. REAL GDP GROWTH FORECASTS
# ------------------------------------------------------------

gdp_growth_2026 <- weo_all %>%

  filter(
    str_detect(
      SERIES_CODE,
      "\\.NGDP_RPCH\\.A$"
    ),
    year == 2026
  ) %>%

  select(
    COUNTRY,
    vintage,
    value
  ) %>%

  mutate(
    value = as.numeric(value)
  )


if (nrow(gdp_growth_2026) == 0) {

  stop(
    "No 2026 real GDP growth observations were found."
  )
}


# ------------------------------------------------------------
# 9. CHECK UNIQUENESS BEFORE RESHAPING
# ------------------------------------------------------------

gdp_duplicates <- gdp_growth_2026 %>%

  count(
    COUNTRY,
    vintage
  ) %>%

  filter(
    n > 1
  )


if (nrow(gdp_duplicates) > 0) {

  stop(
    paste0(
      "Multiple real GDP growth observations were found ",
      "for the same country and vintage."
    )
  )
}


# ------------------------------------------------------------
# 10. RESHAPE FORECAST VINTAGES
# ------------------------------------------------------------

forecast_revisions <- gdp_growth_2026 %>%

  pivot_wider(
    names_from = vintage,
    values_from = value
  ) %>%

  rename(
    forecast_oct_2025 = `October 2025`,
    forecast_apr_2026 = `April 2026`,
    forecast_oct_2026 = `October 2026`
  ) %>%

  mutate(
    forecast_oct_2025 = as.numeric(forecast_oct_2025),
    forecast_apr_2026 = as.numeric(forecast_apr_2026),
    forecast_oct_2026 = as.numeric(forecast_oct_2026)
  )


# ------------------------------------------------------------
# 11. CONSTRUCT FORECAST REVISION VARIABLES
# ------------------------------------------------------------

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
# 12. VALIDATE FORECAST DATASET
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
# 13. SELECTED LATIN AMERICAN ECONOMIES
# ------------------------------------------------------------

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
# 14. ANALYTICAL TABLE
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
# 15. EL SALVADOR EXAMPLE
# ------------------------------------------------------------

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
# 16. EXPORT ANALYTICAL TABLE
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
# 17. INFOGRAPHIC DATA PREPARATION
# ------------------------------------------------------------

infographic_data <- forecast_table %>%

  filter(
    !is.na(
      revision_oct26_vs_oct25
    )
  ) %>%

  mutate(

    revision_clean = if_else(
      abs(revision_oct26_vs_oct25) < 0.05,
      0,
      revision_oct26_vs_oct25
    ),

    direction = case_when(
      revision_clean > 0 ~ "Upward revision",
      revision_clean < 0 ~ "Downward revision",
      TRUE ~ "No revision"
    ),

    country_label = if_else(
      COUNTRY == "El Salvador",
      paste0("★ ", COUNTRY),
      COUNTRY
    ),

    value_label = case_when(

      revision_clean > 0 ~
        paste0(
          "+",
          sprintf(
            "%.1f",
            revision_clean
          )
        ),

      revision_clean < 0 ~
        sprintf(
          "%.1f",
          revision_clean
        ),

      TRUE ~ "0.0"
    )
  )


infographic_data <- infographic_data %>%

  arrange(
    revision_clean
  ) %>%

  mutate(
    country_label = factor(
      country_label,
      levels = country_label
    )
  )


# ------------------------------------------------------------
# 18. SUMMARY STATISTICS
# ------------------------------------------------------------

largest_upward <- infographic_data %>%

  filter(
    revision_clean > 0
  ) %>%

  slice_max(
    revision_clean,
    n = 1,
    with_ties = FALSE
  )


largest_downward <- infographic_data %>%

  filter(
    revision_clean < 0
  ) %>%

  slice_min(
    revision_clean,
    n = 1,
    with_ties = FALSE
  )


countries_upward <- infographic_data %>%

  filter(
    revision_clean > 0
  ) %>%

  nrow()


countries_downward <- infographic_data %>%

  filter(
    revision_clean < 0
  ) %>%

  nrow()


countries_unchanged <- infographic_data %>%

  filter(
    revision_clean == 0
  ) %>%

  nrow()


countries_total <- nrow(
  infographic_data
)


top_positive <- infographic_data %>%

  filter(
    revision_clean > 0,
    COUNTRY != largest_upward$COUNTRY
  ) %>%

  arrange(
    desc(
      revision_clean
    )
  ) %>%

  slice_head(
    n = 2
  )


top_negative <- infographic_data %>%

  filter(
    revision_clean < 0
  ) %>%

  arrange(
    revision_clean
  ) %>%

  slice_head(
    n = 2
  )


# ------------------------------------------------------------
# 19. MAIN BAR CHART
# ------------------------------------------------------------

revision_chart <- ggplot(
  infographic_data,
  aes(
    x = revision_clean,
    y = country_label,
    fill = direction
  )
) +

  geom_col(
    width = 0.68
  ) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6,
    color = "#0F1F33"
  ) +

  geom_text(
    aes(
      label = value_label
    ),
    hjust = ifelse(
      infographic_data$revision_clean >= 0,
      -0.15,
      1.15
    ),
    fontface = "bold",
    size = 4,
    color = "#0F1F33"
  ) +

  scale_fill_manual(
    values = c(
      "Upward revision" = "#2A5A9E",
      "Downward revision" = "#B83250",
      "No revision" = "#A7B2C1"
    )
  ) +

  scale_x_continuous(
    breaks = seq(
      -2,
      2,
      0.5
    ),
    limits = c(
      -2.15,
      2.35
    ),
    expand = expansion(
      mult = c(
        0.02,
        0.04
      )
    )
  ) +

  labs(
    x = "Forecast revision (percentage points)",
    y = NULL,
    fill = NULL
  ) +

  theme_minimal(
    base_size = 13
  ) +

  theme(
    legend.position = "bottom",

    legend.text = element_text(
      size = 10
    ),

    panel.grid.major.y = element_blank(),

    panel.grid.minor = element_blank(),

    panel.grid.major.x = element_line(
      color = "#E2E7EE",
      linewidth = 0.5
    ),

    axis.text.y = element_text(
      size = 12,
      color = "#172033"
    ),

    axis.text.x = element_text(
      size = 10,
      color = "#4A5568"
    ),

    axis.title.x = element_text(
      size = 11,
      margin = margin(
        t = 10
      )
    ),

    plot.margin = margin(
      5,
      25,
      5,
      5
    )
  )


# ------------------------------------------------------------
# 20. KPI PANEL
# ------------------------------------------------------------

kpi_text <- paste0(

  "LARGEST UPWARD REVISION\n",
  largest_upward$COUNTRY,
  ": +",
  sprintf(
    "%.1f",
    largest_upward$revision_clean
  ),
  " pp\n\n",

  "LARGEST DOWNWARD REVISION\n",
  largest_downward$COUNTRY,
  ": ",
  sprintf(
    "%.1f",
    largest_downward$revision_clean
  ),
  " pp\n\n",

  "COUNTRIES REVISED UPWARD\n",
  countries_upward,
  " of ",
  countries_total,
  "\n\n",

  "COUNTRIES REVISED DOWNWARD\n",
  countries_downward,
  " of ",
  countries_total
)


kpi_panel <- ggplot() +

  annotate(
    "rect",
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    fill = "#EEF3FA",
    color = NA
  ) +

  annotate(
    "text",
    x = 0.06,
    y = 0.95,
    label = kpi_text,
    hjust = 0,
    vjust = 1,
    lineheight = 1.28,
    size = 4.0,
    color = "#0F2747",
    fontface = "bold"
  ) +

  coord_cartesian(
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0,
      1
    ),
    clip = "off"
  ) +

  theme_void()


# ------------------------------------------------------------
# 21. KEY TAKEAWAYS PANEL
# ------------------------------------------------------------

positive_takeaway <- paste0(
  largest_upward$COUNTRY,
  " recorded the largest upward revision (+",
  sprintf(
    "%.1f",
    largest_upward$revision_clean
  ),
  " pp)."
)


second_positive_takeaway <- paste0(
  paste(
    top_positive$COUNTRY,
    collapse = " and "
  ),
  " also received stronger growth forecasts."
)


negative_takeaway <- paste0(
  paste(
    top_negative$COUNTRY,
    collapse = " and "
  ),
  " experienced the largest downward revisions."
)


takeaway_lines <- c(
  str_wrap(
    paste0(
      "• ",
      positive_takeaway
    ),
    width = 46
  ),

  "",

  str_wrap(
    paste0(
      "• ",
      second_positive_takeaway
    ),
    width = 46
  ),

  "",

  str_wrap(
    paste0(
      "• ",
      negative_takeaway
    ),
    width = 46
  )
)


takeaway_text <- paste(
  takeaway_lines,
  collapse = "\n"
)


takeaway_panel <- ggplot() +

  annotate(
    "rect",
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    fill = "#FAF7F3",
    color = NA
  ) +

  annotate(
    "rect",
    xmin = 0,
    xmax = 1,
    ymin = 0.83,
    ymax = 1,
    fill = "#0F3158",
    color = NA
  ) +

  annotate(
    "text",
    x = 0.06,
    y = 0.915,
    label = "Key takeaways",
    hjust = 0,
    vjust = 0.5,
    size = 5.0,
    color = "white",
    fontface = "bold"
  ) +

  annotate(
    "text",
    x = 0.06,
    y = 0.76,
    label = takeaway_text,
    hjust = 0,
    vjust = 1,
    lineheight = 1.2,
    size = 3.65,
    color = "#172033"
  ) +

  coord_cartesian(
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0,
      1
    ),
    clip = "off"
  ) +

  theme_void()


# ------------------------------------------------------------
# 22. COMBINE INFOGRAPHIC
# ------------------------------------------------------------

right_panel <- patchwork::wrap_plots(
  kpi_panel,
  takeaway_panel,
  ncol = 1,
  heights = c(
    0.48,
    0.52
  )
)


infographic_plot <- patchwork::wrap_plots(
  revision_chart,
  right_panel,
  ncol = 2,
  widths = c(
    1.45,
    1.15
  )
)


infographic_plot <- infographic_plot +

  patchwork::plot_annotation(

    title =
      "IMF 2026 Growth Forecast Revisions in Latin America",

    subtitle =
      paste0(
        "Change in real GDP growth forecasts between the ",
        "October 2025 and October 2026 WEO vintages"
      ),

    caption =
      paste0(
        "Source: IMF World Economic Outlook Database.    ",
        "Author: Victor Manuel Cruz Bonilla"
      ),

    theme = theme(

      plot.title = element_text(
        size = 23,
        face = "bold",
        color = "#0F2747"
      ),

      plot.subtitle = element_text(
        size = 12.5,
        color = "#384454",
        margin = margin(
          b = 18
        )
      ),

      plot.caption = element_text(
        size = 9,
        color = "#334155",
        hjust = 0
      ),

      plot.margin = margin(
        18,
        18,
        12,
        18
      )
    )
  )


print(
  infographic_plot
)


# ------------------------------------------------------------
# 23. EXPORT INFOGRAPHIC
# ------------------------------------------------------------

infographic_output_path <- file.path(
  output_dir,
  "imf_growth_forecast_revisions_infographic.png"
)


ggsave(
  filename = infographic_output_path,
  plot = infographic_plot,
  width = 14,
  height = 8.2,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# 24. COMPLETION CHECK
# ------------------------------------------------------------

stopifnot(
  file.exists(
    table_output_path
  ),
  file.exists(
    infographic_output_path
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
  "Infographic saved to: ",
  infographic_output_path
)
