# IMF World Economic Outlook Forecast Revisions

This repository contains a reproducible R coding sample that analyzes revisions to IMF World Economic Outlook (WEO) real GDP growth forecasts across successive publication vintages.

The project was developed as a compact research-oriented example of economic data cleaning, harmonization, validation, transformation, and visualization in R.

## Research Question

How did IMF real GDP growth forecasts for selected Latin American economies change across the October 2025, April 2026, and October 2026 World Economic Outlook vintages?

## Data

Source: International Monetary Fund (IMF), World Economic Outlook Database.

The analysis uses three WEO vintages:

- October 2025
- April 2026
- October 2026

The raw files differ in structure across publication vintages. The R script dynamically identifies annual observation columns and harmonizes the datasets into a common country-indicator-year format.

## Workflow

The coding sample follows the workflow:

```text
Raw WEO Data
     ↓
Data Import
     ↓
Data Validation
     ↓
Data Harmonization
     ↓
Variable Construction
     ↓
Forecast Revision Analysis
     ↓
Visualization
     ↓
Reproducible Outputs
