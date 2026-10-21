# E-commerce Operations KPI Pipeline (Python and R)

The same reporting pipeline built twice, once in Python (pandas) and once in R (dplyr), to produce weekly KPIs and marketing ROI for leadership.

## Data
`data/ecommerce_operations_data.xlsx`: three sheets (orders, customers, marketing_spend). 150 orders from 39 active customers over seven weeks (late November 2025 to early January 2026).

## Pipeline
1. Load the three sheets and run schema checks (types, nulls, unique keys)
2. Standardize text fields; label organic orders as No Campaign so they stay in totals
3. Join orders with customers and marketing spend
4. Calculate gross and net revenue after discounts and returns, margin (55% estimate, since no cost data was given), return rate, and average order value
5. Aggregate weekly KPIs and ROAS by channel and campaign
6. Validate totals across tables and export CSV files

## Findings
* Return rate of about **19%**, high enough to investigate by product
* Average order value of about **$363**, pushed up by a few large orders
* Some campaign and region pairs **lost money after ad spend**
* Net revenue after returns reconciles to **$53,226.05** across all output tables

## Files
* `python/ecommerce_pipeline.ipynb`
* `r/ecommerce_pipeline.R`
* `output/Weekly_KPIs.csv`, `output/ROI.csv`

*Tools: Python (pandas), R (readxl, dplyr, lubridate, tidyr, ggplot2). Herzing College coursework, March 2026.*
