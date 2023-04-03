
# Load Libraries and sources

library(tidyr)
library(purrr)
library(broom)
library(dplyr)
library(scales)
library(plotly)
library(nortest)
library(ggplot2)
library(forcats)
library(reshape2)
library(gridExtra)
source("data_cleaning.R")

#-----------------------> KPI's

# Click Through Rate: Number of Impressions vs. Clicks
# Conversion Rate: Number of Clicks vs. Purchase
# Cart Abandonment: 
# Cost per Conversion: Cost per Click, Cost per Purchase

# -------------- totals

# Cost per click (CPC)
combined_data %>%
  group_by(group) %>%
  summarise(CPC = 
              round(
                sum(value[variable == "Spend (USD)"]) /
                  sum(value[variable == "Website Clicks"]),4))
CPC

# Cost per Purchase (CPP)
CPP <- combined_data %>%
  group_by(group) %>%
  summarise(CPP = 
              round(
                sum(value[variable == "Spend (USD)"]) /
                  sum(value[variable == "Purchase"]),2))

# Cost per Impression (CPI)
CPI <- combined_data %>%
  group_by(group) %>%
  summarise(CPI = 
              round(
                sum(value[variable == "Spend (USD)"]) /
                  sum(value[variable == "Impressions"]),2))

# --------------- Interest

# Click Through Rate
combined_data %>%
  group_by(group) %>%
  summarise(CTR = 
              round(sum(value[variable == "Website Clicks"]) / 
                      sum(value[variable == "Impressions"]), 4)*100)

# --------------- Action

# Impressions to Purchase
combined_data %>%
  group_by(group) %>%
  summarise(reach_to_purchase_rate = 
              sum(value[variable == "Purchase"]) / 
              sum(value[variable == "Reach"]) * 100)

# Clicks to Purchase
combined_data %>%
  group_by(group) %>%
  summarise(Purchase_Rate = 
              sum(value[variable == "Purchase"]) / 
              sum(value[variable == "Website Clicks"]) * 100)

# Add to Cart
combined_data %>%
  group_by(group) %>%
  summarise(Purchase_Rate = 
              sum(value[variable == "Purchase"]) / 
              sum(value[variable == "Searches"]) * 100)

unique(combined_data$variable)


