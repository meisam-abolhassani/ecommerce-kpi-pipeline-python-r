# =============================================================================
# E-Commerce Operations Pipeline (Herzing College coursework)
# Author : Meisam Abolhassani
# Date   : 2026-03-27
# Purpose: Build a transformation pipeline in R that produces
#          weekly KPIs and ROI metrics for leadership reporting.
# =============================================================================


# -- 0. Load Required Libraries ------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


# -- 1. Load Data --------------------------------------------------------------
# Read all three sheets from the Excel workbook
file_path <- "../data/ecommerce_operations_data.xlsx"

orders <- read_excel(file_path, sheet = "orders")
customers <- read_excel(file_path, sheet = "customers")
mkt_spend <- read_excel(file_path, sheet = "marketing_spend")

# Check row counts
nrow(orders)
nrow(customers)
nrow(mkt_spend)


# -- 2. Schema Checks ---------------------------------------------------------
# Check data types for each table
str(orders)
str(customers)
str(mkt_spend)

# Check missing values in each table
colSums(is.na(orders))
colSums(is.na(customers))
colSums(is.na(mkt_spend))

# Check if keys are unique
n_distinct(orders$order_id) == nrow(orders)
n_distinct(customers$customer_id) == nrow(customers)


# -- 3. Standardize Categorical Fields -----------------------------------------
# Trim whitespace from text columns
orders$region <- trimws(orders$region)
orders$channel <- trimws(orders$channel)
orders$campaign <- trimws(orders$campaign)
orders$product_category <- trimws(orders$product_category)
orders$returned <- trimws(toupper(orders$returned))

# Convert the string "NA" in campaign to a real NA value
# Organic orders do not have a campaign, so this is expected
orders$campaign[orders$campaign == "NA"] <- NA

# Check: should be 60 NAs (all Organic orders)
sum(is.na(orders$campaign))

# Clean customers and marketing_spend too
customers$segment <- trimws(customers$segment)
customers$city <- trimws(customers$city)
mkt_spend$channel <- trimws(mkt_spend$channel)
mkt_spend$campaign <- trimws(mkt_spend$campaign)


# -- 4. Parse Dates ------------------------------------------------------------
# Convert date columns from text to Date format
orders$order_date <- ymd(orders$order_date)
customers$signup_date <- ymd(customers$signup_date)
mkt_spend$week_start <- ymd(mkt_spend$week_start)

# Verify
class(orders$order_date)
min(orders$order_date)
max(orders$order_date)


# -- 5. Create Derived Revenue Metrics -----------------------------------------
# gross_revenue = units sold times unit price
orders$gross_revenue <- orders$units_sold * orders$unit_price

# net_revenue = gross_revenue after discount
orders$net_revenue <- orders$gross_revenue * (1 - orders$discount / 100)

# If the order was returned, revenue is set to 0
orders$net_revenue_after_returns <- ifelse(orders$returned == "Y", 0, orders$net_revenue)

# Quick check
head(orders[, c("order_id", "units_sold", "unit_price", "discount",
                "returned", "gross_revenue", "net_revenue",
                "net_revenue_after_returns")])


# -- 6. Margin Proxy -----------------------------------------------------------
# ASSUMPTION: estimated COGS = 55% of gross revenue
# This is a simplified proxy since actual cost data is not available
COGS_RATE <- 0.55

orders$estimated_cogs <- orders$gross_revenue * COGS_RATE
orders$margin <- orders$net_revenue_after_returns - orders$estimated_cogs

# Quick check
head(orders[, c("order_id", "gross_revenue", "net_revenue_after_returns",
                "estimated_cogs", "margin")])


# -- 7. Create week_start from order_date -------------------------------------
# Round each order date down to the Monday of that week
orders$week_start <- floor_date(orders$order_date, unit = "week", week_start = 1)

# Check: should see 7 distinct weeks
unique(sort(orders$week_start))


# -- 8. Weekly KPI Aggregation -------------------------------------------------
# Group by week, channel, campaign, region, and product category
weekly_kpis <- orders %>%
  group_by(week_start, channel, campaign, region, product_category) %>%
  summarise(
    total_orders = n_distinct(order_id),
    unique_customers = n_distinct(customer_id),
    total_gross_revenue = sum(gross_revenue),
    total_net_revenue = sum(net_revenue_after_returns),
    total_margin = sum(margin),
    return_rate = round(mean(returned == "Y"), 4),
    aov = round(sum(net_revenue_after_returns) / n_distinct(order_id), 2),
    .groups = "drop"
  )

# Check row count
nrow(weekly_kpis)

# Revenue cross-check: these two should match
sum(weekly_kpis$total_net_revenue)
sum(orders$net_revenue_after_returns)


# -- 9. Join Marketing Spend and Compute ROI -----------------------------------
# First aggregate to (week_start, channel, campaign) level
# because marketing_spend does not have region or product_category
roi <- weekly_kpis %>%
  group_by(week_start, channel, campaign) %>%
  summarise(
    total_orders = sum(total_orders),
    unique_customers = sum(unique_customers),
    total_gross_revenue = sum(total_gross_revenue),
    total_net_revenue = sum(total_net_revenue),
    total_margin = sum(total_margin),
    avg_return_rate = round(mean(return_rate), 4),
    avg_aov = round(mean(aov), 2),
    .groups = "drop"
  )

# Join with marketing spend
roi <- merge(roi, mkt_spend, by = c("week_start", "channel", "campaign"), all.x = TRUE)

# Replace missing spend with 0 (Organic orders have no spend)
roi$spend[is.na(roi$spend)] <- 0

# Calculate ROAS and profit after spend
roi$roas <- ifelse(roi$spend > 0, round(roi$total_net_revenue / roi$spend, 2), NA)
roi$profit_after_spend <- round(roi$total_margin - roi$spend, 2)

# Check
nrow(roi)
head(roi)


# -- 10. Validation ------------------------------------------------------------
# AOV distribution summary
summary(weekly_kpis$aov)

# Histogram of AOV
hist(weekly_kpis$aov, breaks = 20, col = "steelblue",
     main = "Distribution of AOV", xlab = "AOV ($)", ylab = "Count")

# Boxplot of revenue by channel
boxplot(total_net_revenue ~ channel, data = weekly_kpis, col = "lightblue",
        main = "Net Revenue by Channel", xlab = "Channel",
        ylab = "Net Revenue ($)")

# Join cardinality check
paid_roi <- roi[roi$channel != "Organic" & !is.na(roi$campaign), ]
nrow(paid_roi)
sum(paid_roi$spend == 0)


# -- 11. Export CSV Files ------------------------------------------------------
write.csv(weekly_kpis, "weekly_kpis.csv", row.names = FALSE)
write.csv(roi, "roi.csv", row.names = FALSE)

# Save plots as images
png("aov_distribution.png", width = 800, height = 500)
hist(weekly_kpis$aov, breaks = 20, col = "steelblue",
     main = "Distribution of AOV", xlab = "AOV ($)", ylab = "Count")
dev.off()

png("revenue_by_channel.png", width = 800, height = 500)
boxplot(total_net_revenue ~ channel, data = weekly_kpis, col = "lightblue",
        main = "Net Revenue by Channel", xlab = "Channel",
        ylab = "Net Revenue ($)")
dev.off()

# Verify files exist
file.exists("weekly_kpis.csv")
file.exists("roi.csv")