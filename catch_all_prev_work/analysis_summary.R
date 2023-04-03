
# Load Libraries
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

# Summary
output$analysis_summary <- renderText({
  
  summary_text
  
})



# ----------------------- Count Summary Functions

# Subset the data to only include the specified variables
win_percent <- combined_data %>%
  filter(variable %in% include_vars) %>%
  group_by(group, variable) %>%
  summarize(sum = sum(value)) %>%
  arrange(desc(sum))

# Reshape the data frame into a wide format
win_percent_wide <- win_percent %>%
  spread(key = group, value = sum)

# Calculate the performance_difference column
win_percent_wide <- win_percent_wide %>%
  mutate(performance_difference = (`Maximum Bidding` - `Average Bidding`) / `Average Bidding` * 100)

# Display the resulting data frame
win_percent_wide <- win_percent_wide %>%
  select(variable, performance_difference)


format_difference <- function(difference) {
  if (abs(difference) <= 1) {
    significance <- "insignificant"
  } else if (abs(difference) <= 5) {
    significance <- "mildly significant"
  } else if (abs(difference) <= 10) {
    significance <- "significant"
  } else if (abs(difference) <= 20) {
    significance <- "very significant"
  } else {
    significance <- "extremely significant"
  }
  
  color <- ifelse(difference > 0, "green", "red")
  direction <- ifelse(difference > 0, "more", "less")
  formatted_text <- paste0("<span style='color: ", color, "'>", round(difference, 2), "%</span> (", significance, ") ", direction)
  return(formatted_text)
}

format_roi <- function(spend_diff, purchase_diff) {
  combined_diff <- spend_diff + purchase_diff
  if (abs(combined_diff) <= 1) {
    significance <- "insignificant"
  } else if (abs(combined_diff) <= 5) {
    significance <- "mildly significant"
  } else if (abs(combined_diff) <= 10) {
    significance <- "significant"
  } else if (abs(combined_diff) <= 20) {
    significance <- "very significant"
  } else {
    significance <- "extremely significant"
  }
  
  color <- ifelse(combined_diff > 0, "green", "red")
  direction <- ifelse(combined_diff > 0, "better", "worse")
  formatted_text <- paste0("Experiment (", significance, ") <span style='color: ", color, "'>", direction, "</span> investment")
  return(formatted_text)
}

# Count Summary Adjustment

spend_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Spend (USD)"]
purchase_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Purchase"]
impressions_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Impressions"]
reach_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Reach"]
clicks_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Website Clicks"]
searches_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Searches"]
view_content_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "View Content"]
add_to_cart_diff <- win_percent_wide$performance_difference[win_percent_wide$variable == "Add to Cart"]

spend_diff_formatted <- format_difference(spend_diff)
purchase_diff_formatted <- format_difference(purchase_diff)
impressions_diff_formatted <- format_difference(impressions_diff)
reach_diff_formatted <- format_difference(reach_diff)
clicks_diff_formatted <- format_difference(clicks_diff)
searches_diff_formatted <- format_difference(searches_diff)
view_content_diff_formatted <- format_difference(view_content_diff)
add_to_cart_diff_formatted <- format_difference(add_to_cart_diff)

summary_text <- paste("ROI: Experimental Spend was", spend_diff_formatted, " than Control, however, Experimental Purchases were", purchase_diff_formatted, " Based on this, Control is a better investment.",
                      "<br><br>Awareness: Experiment made", impressions_diff_formatted, " Impressions, and", reach_diff_formatted, " reach, indicating Control performs significantly better than experiments for awareness.",
                      "<br><br>Interest: Experiment had", clicks_diff_formatted, " clicks and", searches_diff_formatted, " searches, and", view_content_diff_formatted, "less View Content's, indicating significantly higher rate of interest than the control group",
                      "<br><br>Desire: Experiment had", add_to_cart_diff_formatted, " Add to Carts, and", purchase_diff_formatted, "purchases, indicating still significant less performance in desire")
summary_text

# ----------------------- Cost Summary Functions


