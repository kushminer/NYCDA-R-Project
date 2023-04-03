
source("data_cleaning.R")
source("normalization_check_and_p_value.R")
source("win_probability.R")
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# ------------------------ > Dummy Data -> stat_significant and win_probability

# Updated Formatting
plot_win_percent <- function(include_vars) {
  
  # Subset the data to only include the specified variables
  win_percent <- combined_data %>%
    filter(variable %in% include_vars) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value), .groups = 'drop') %>%
    arrange(desc(sum))
  
  # Reshape the data frame into a wide format
  win_percent_wide <- win_percent %>%
    spread(key = group, value = sum)
  
  # Calculate the winner column
  win_percent_wide$winner <- ifelse(win_percent_wide$`Average Bidding` > win_percent_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  win_percent_wide <- win_percent_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Add stat_significant and win_probability columns
  win_percent_wide$stat_significant <- sample(c(TRUE, FALSE), nrow(win_percent_wide), replace = TRUE)
  win_percent_wide$win_probability <- runif(nrow(win_percent_wide), 0, 100)
  
  # Define the order
  level_order <- rev(c("Spend (USD)", "Impressions", "Reach", 
                       "Website Clicks", "Searches", "View Content", 
                       "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  win_percent_wide$variable <- fct_relevel(win_percent_wide$variable, level_order)
  
  # Create the bar chart
  plot_win_percent <- ggplot(win_percent_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col(aes(color = ifelse(stat_significant, "red", "green")), size = 1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    scale_color_identity() +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%"), 
                  hjust = -0.1),
              vjust = .5, size = 5) +
    geom_text(aes(label = paste0(round(win_probability, 2), "%"),
                  y = win_percentage / 2,
                  hjust = 0.5),
              vjust = .5, size = 3, color = "white") +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    coord_flip() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)) +
    expand_limits(y = c(max(win_percent_wide$win_percentage) * 3))
  
  return(plot_win_percent)
}

include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach","Purchase")
plot_win_percent(include_vars)

# ------------------------ > Real Data -> stat_significant and win_probability

plot_win_percent <- function() {
  
  # Filter the data for Impressions and Reach
  win_percent <- combined_data %>%
    filter(variable %in% c("Impressions", "Reach")) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value), .groups = 'drop') %>%
    arrange(desc(sum))
  
  # Reshape the data frame into a wide format
  win_percent_wide <- win_percent %>%
    spread(key = group, value = sum)
  
  # Calculate the winner column
  win_percent_wide$winner <- ifelse(win_percent_wide$`Average Bidding` > win_percent_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  win_percent_wide <- win_percent_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Calculate win_probability
  win_probability <- c(
    win_probability("Impressions"),
    win_probability("Reach")
  )
  
  # Calculate stat_significance
  stat_significance <- c(
    wilcox_test_impressions(combined_data)$p_value < 0.05,
    wilcox_test_reach(combined_data)$p_value < 0.05
  )
  
  # Add stat_significant and win_probability columns
  win_percent_wide$stat_significant <- stat_significance
  win_percent_wide$win_probability <- win_probability
  
  # Define the order
  level_order <- rev(c("Impressions", "Reach"))
  
  # Reorder the 'variable' factor according to level_order
  win_percent_wide$variable <- fct_relevel(win_percent_wide$variable, level_order)
  
  # Create the bar chart
  plot_win_percent <- ggplot(win_percent_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col(aes(color = ifelse(stat_significant, "red", "green")), size = 1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    scale_color_identity() +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%"), 
                  hjust = -0.1),
              vjust = .5, size = 5) +
    geom_text(aes(label = paste0(round(win_probability, 2), "%"),
                  y = win_percentage / 2,
                  hjust = 0.5),
              vjust = .5, size = 3, color = "white") +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    coord_flip() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)) +
    expand_limits(y = c(max(win_percent_wide$win_percentage) * 3))
  
  return(plot_win_percent)
}

#include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach","Purchase")
plot_win_percent()

# ------------------------ > Dummy Data -> stat_significant 

# Updated Formatting
plot_win_percent <- function(include_vars) {
  
  # Subset the data to only include the specified variables
  win_percent <- combined_data %>%
    filter(variable %in% include_vars) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value), .groups = 'drop') %>%
    arrange(desc(sum))
  
  # Reshape the data frame into a wide format
  win_percent_wide <- win_percent %>%
    spread(key = group, value = sum)
  
  # Calculate the winner column
  win_percent_wide$winner <- ifelse(win_percent_wide$`Average Bidding` > win_percent_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  win_percent_wide <- win_percent_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Add stat_significant and win_probability columns
  win_percent_wide$stat_significant <- sample(c(TRUE, FALSE), nrow(win_percent_wide), replace = TRUE)

  # Define the order
  level_order <- rev(c("Spend (USD)", "Impressions", "Reach", 
                       "Website Clicks", "Searches", "View Content", 
                       "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  win_percent_wide$variable <- fct_relevel(win_percent_wide$variable, level_order)
  
  # Create the bar chart
  plot_win_percent <- ggplot(win_percent_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col(aes(color = ifelse(stat_significant, "red", "green")), size = 1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    scale_color_identity() +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%"), 
                  hjust = -0.1),
              vjust = .5, size = 5) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    coord_flip() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)) +
    expand_limits(y = c(max(win_percent_wide$win_percentage) * 3))
  
  return(plot_win_percent)
}

include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach","Purchase")
plot_win_percent(include_vars)

# ------------------------ > Real Data -> stat_significant

plot_win_percent <- function() {
  
  # Filter the data for Impressions and Reach
  win_percent <- combined_data %>%
    filter(variable %in% c("Impressions", "Reach")) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value), .groups = 'drop') %>%
    arrange(desc(sum))
  
  # Reshape the data frame into a wide format
  win_percent_wide <- win_percent %>%
    spread(key = group, value = sum)
  
  # Calculate the winner column
  win_percent_wide$winner <- ifelse(win_percent_wide$`Average Bidding` > win_percent_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  win_percent_wide <- win_percent_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Get the stat_significance from all_results dataframe
  stat_significance <- all_results %>%
    filter(variable %in% c("Impressions", "Reach")) %>%
    pull(significance)
  
  # Add stat_significant column
  win_percent_wide$stat_significant <- stat_significance
  win_percent_wide
  # Define the order
  level_order <- rev(c("Impressions", "Reach"))
  
  # Reorder the 'variable' factor according to level_order
  win_percent_wide$variable <- fct_relevel(win_percent_wide$variable, level_order)
  
  # Create the bar chart
  plot_win_percent <- ggplot(win_percent_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col(aes(color = ifelse(stat_significant == "Significant", "green", "red")), size = 1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    scale_color_identity() +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%"), 
                  hjust = -0.1),
              vjust = .5, size = 5) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    coord_flip() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)) +
    expand_limits(y = c(max(win_percent_wide$win_percentage) * 3))
  
  return(plot_win_percent)
}

#include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach","Purchase")
plot_win_percent()
all_results

# ------------> Analysis

# Subset the data to only include the specified variables
win_percent <- combined_data %>%
  filter(variable %in% include_vars) %>%
  group_by(group, variable) %>%
  summarize(sum = sum(value), .groups = 'drop') %>%
  arrange(desc(sum))

# Reshape the data frame into a wide format
win_percent_wide <- win_percent %>%
  spread(key = group, value = sum)

# Calculate the winner column
win_percent_wide$winner <- ifelse(win_percent_wide$`Average Bidding` > win_percent_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")

# Calculate the win_percentage column
win_percent_wide <- win_percent_wide %>%
  mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
         win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)

# Add stat_significant and win_probability columns
win_percent_wide$stat_significant <- sample(c(TRUE, FALSE), nrow(win_percent_wide), replace = TRUE)
win_percent_wide$win_probability <- runif(nrow(win_percent_wide), 0, 100)


format_difference <- function(value) {
  if (value > 0) {
    return(paste0("higher (", round(value, 2), "%)"))
  } else {
    return(paste0("lower (", round(abs(value), 2), "%)"))
  }
}

get_group_info <- function(variable) {
  win_percent_wide %>%
    filter(variable == !!variable) %>%
    select(winner, win_probability, stat_significant) %>%
    as.list()
}

roi_group <- get_group_info("Spend (USD)")
awareness_group <- get_group_info("Reach")
interest_group <- get_group_info("Website Clicks")
desire_group <- get_group_info("Add to Cart")
action_group <- get_group_info("Purchase")

summary_text <- paste("ROI: The ", roi_group$winner, " group had a ", format_difference(spend_diff), " spend and a ", format_difference(purchase_diff), " in purchases, with a win probability of ", round(roi_group$win_probability, 2), "% and statistically ", ifelse(roi_group$stat_significant, "significant", "insignificant"), " difference.",
                      "<br><br>Awareness: The ", awareness_group$winner, " group had ", format_difference(impressions_diff), " impressions and ", format_difference(reach_diff), " reach, with a win probability of ", round(awareness_group$win_probability, 2), "% and statistically ", ifelse(awareness_group$stat_significant, "significant", "insignificant"), " difference.",
                      "<br><br>Interest: The ", interest_group$winner, " group had ", format_difference(clicks_diff), " clicks, ", format_difference(searches_diff), " searches, and ", format_difference(view_content_diff), " view contents, with a win probability of ", round(interest_group$win_probability, 2), "% and statistically ", ifelse(interest_group$stat_significant, "significant", "insignificant"), " difference.",
                      "<br><br>Desire: The ", desire_group$winner, " group had ", format_difference(add_to_cart_diff), " add to carts, with a win probability of ", round(desire_group$win_probability, 2), "% and statistically ", ifelse(desire_group$stat_significant, "significant", "insignificant"), " difference.",
                      "<br><br>Action: The ", action_group$winner, " group had ", format_difference(purchase_diff), " purchases, with a win probability of ", round(action_group$win_probability, 2), "% and statistically ", ifelse(action_group$stat_significant, "significant", "insignificant"), " difference.")
summary_text

summary_text <- paste("ROI: The ", win_percent_wide$winner[win_percent_wide$variable == "Spend (USD)"], " group had a ", format_difference(spend_diff), " spend and a ", format_difference(purchase_diff), " in purchases, with a win probability of ", round(win_percent_wide$win_probability[win_percent_wide$variable == "Spend (USD)"], 2), "% and statistically ", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Spend (USD)"], "significant", "insignificant"), " difference.",
                      "<br><br>Awareness: The ", win_percent_wide$winner[win_percent_wide$variable == "Reach"], " group had ", format_difference(impressions_diff), " impressions and ", format_difference(reach_diff), " reach, with a win probability of ", round(win_percent_wide$win_probability[win_percent_wide$variable == "Reach"], 2), "% and statistically ", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Reach"], "significant", "insignificant"), " difference.",
                      "<br><br>Interest: The ", win_percent_wide$winner[win_percent_wide$variable == "Website Clicks"], " group had ", format_difference(clicks_diff), " clicks, ", format_difference(searches_diff), " searches, and ", format_difference(view_content_diff), " View Content's, with a win probability of ", round(win_percent_wide$win_probability[win_percent_wide$variable == "Website Clicks"], 2), "% and statistically ", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Website Clicks"], "significant", "insignificant"), " difference.",
                      "<br><br>Desire: The ", win_percent_wide$winner[win_percent_wide$variable == "Add to Cart"], " group had ", format_difference(add_to_cart_diff), " Add to Carts, with a win probability of ", round(win_percent_wide$win_probability[win_percent_wide$variable == "Add to Cart"], 2), "% and statistically ", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Add to Cart"], "significant", "insignificant"), " difference.",
                      "<br><br>Action: The ", win_percent_wide$winner[win_percent_wide$variable == "Purchase"], " group had ", format_difference(purchase_diff), " purchases, with a win probability of ", round(win_percent_wide$win_probability[win_percent_wide$variable == "Purchase"], 2), "% and statistically ", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Purchase"], "significant", "insignificant"), " difference.")

cat(summary_text)

awareness_summary <- paste("Awareness:",
                           "<br><br>Impressions (number of ad views) --> Best Choice:", win_percent_wide$winner[win_percent_wide$variable == "Impressions"], ".", format_difference(impressions_diff), "more impressions, with a win probability of", round(win_percent_wide$win_probability[win_percent_wide$variable == "Impressions"], 2), "% and statistically", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Impressions"], "significant", "insignificant"), "difference.",
                           "<br><br>Reach (number of unique users reached with ad) --> Best choice:", win_percent_wide$winner[win_percent_wide$variable == "Reach"], ".", format_difference(reach_diff), "more reach, with a win probability of", round(win_percent_wide$win_probability[win_percent_wide$variable == "Reach"], 2), "% and statistically", ifelse(win_percent_wide$stat_significant[win_percent_wide$variable == "Reach"], "significant", "insignificant"), "difference.")

cat(awareness_summary)



