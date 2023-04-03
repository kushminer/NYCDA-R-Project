
# Load Libraries
library(tidyr)
library(dplyr)
library(scales)
library(plotly)
library(ggplot2)
library(forcats)
library(reshape2)
library(gridExtra)
library(tidyverse)

source('data_cleaning.R')
source('cost_per_var.R')


# ----------------------- Plotting Functions

# Define the label size function
label_size <- function(n) {
  if (n <= 5) {
    return(3)
  } else if (n <= 10) {
    return(2.5)
  } else {
    return(2)
  }
}

#size = label_size(length(unique(monthly_data$variable)))


# ----------------------- Count

# Define Function
plot_count_comparison <- function() {
  
  # Initialize dataframe
  monthly_data <- combined_data %>%
    filter(variable != 'Spend (USD)') %>%
    group_by(group,variable) %>%
    summarize(sum = sum(value))
  
  # Subset the data to only include the specified variables
  monthly_data <- monthly_data[monthly_data$variable %in% c("Impressions", "Reach"), ]
  
  # Create a vector of levels in the desired order
  sum_variable_level_order <- rev(c("Impressions", "Reach"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  monthly_data$variable <- factor(monthly_data$variable, levels = sum_variable_level_order, ordered = TRUE)
  
  # Plot the selected data
  ggplot(monthly_data, aes(x = variable, y = sum, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = ifelse(variable == "Spend (USD)", paste0("$", format(round(sum), big.mark = ",")), format(round(sum), big.mark = ","))),
              position = position_dodge(width = 1), 
              vjust = .5, size = 5, 
              hjust = -.1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    theme_minimal() +
    coord_flip() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)) +
    labs(fill = "") +
    expand_limits(y = c(0, max(monthly_data$sum)*1.2))
}

plot_count_comparison()

# winners
 plot_count_winner_percent <- function() {
   # Subset the data to only include the specified variables
   # Filter the data for Impressions and Reach
   win_percent <- combined_data %>%
     filter(variable %in% c("Impressions", "Reach", 
                            "Website Clicks", "Searches", 
                            "View Content", "Add to Cart", 
                            "Purchase")) %>%
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
     filter(variable %in% c("Impressions", "Reach", 
                            "Website Clicks", "Searches", 
                            "View Content", "Add to Cart", 
                            "Purchase")) %>%
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
 
 plot_count_winner_percent()
 
 # winners_amount
 
 plot_count_winner_amount <- function() {
   
   # Subset the data to only include the specified variables
   win_amount <- combined_data %>%
     filter(variable %in% c("Impressions", "Reach")) %>%
     group_by(group, variable) %>%
     summarize(sum = sum(value)) %>%
     arrange(desc(sum))
   
   # Reshape the data frame into a wide format
   win_wide_amount <- win_amount %>%
     spread(key = group, value = sum)
   
   # Calculate the winner column
   win_wide_amount$winner <- ifelse(win_wide_amount$`Average Bidding` > win_wide_amount$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
   
   # Calculate the win_percentage column
   win_wide_amount <- win_wide_amount %>%
     mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`))
   
   # Get the stat_significance from all_results dataframe
   stat_significance <- all_results %>%
     filter(variable %in% c("Impressions", "Reach")) %>%
     pull(significance)
   
   # Add stat_significant column
   win_wide_amount$stat_significant <- stat_significance
   
   # Define the order
   level_order <- rev(c("Impressions", "Reach"))
   
   # Reorder the 'variable' factor according to level_order
   win_wide_amount$variable <- fct_relevel(win_wide_amount$variable, level_order)
   
   # Create the bar chart
   plot_win_amount <- ggplot(win_wide_amount, aes(x = variable, y = abs_difference, fill = winner)) +
     geom_col(aes(color = ifelse(stat_significant == "Significant", "green", "red")), size = 1) +
     scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
     scale_color_identity() +
     geom_text(aes(hjust = -.1, vjust = .5), size = 5, label = paste0(format(win_wide_amount$abs_difference, big.mark = ","))) +
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
     expand_limits(y = c(max(win_wide_amount$abs_difference) * 1.2))
   
   return(plot_win_amount)
 } 
 
 plot_count_winner_amount()
 
# ----------------------- Cost

# Cost Count
plot_cost_comparison <- function(include_cost_vars) {
  
  # Cost data dataframe
  cost_data <- combined_data %>%
    group_by(group,variable) %>%
    summarize(sum = sum(value)) %>%
    mutate(cost = ifelse(variable != "Spend (USD)", sum(sum[variable == "Spend (USD)"]) / sum, NA),
           cost = round(cost, 2)) %>%
    filter(variable != "Spend (USD)",
           variable %in% c("Impressions", "Reach"))
  
  # Create a vector of levels in the desired order
  cost_level_order <- rev(c("Impressions", "Reach"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  cost_data$variable <- factor(cost_data$variable, levels = cost_level_order, ordered = TRUE)
  
  # Plot the selected data
  ggplot(cost_data, aes(x = variable, y = cost, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0("$", format(cost, big.mark = ","))),
              position = position_dodge(width = 1), 
              vjust = .5, size = 5, 
              hjust = -.1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080")) +
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
      axis.text = element_text(size = 12)
      ) +
    labs(fill = "") +
    expand_limits(y = c(0, max(cost_data$cost) * 1.2)) +
    guides(fill = FALSE) +
    theme(legend.position = "top")
  
}

plot_cost_comparison()





# Winner by amount
plot_cost_winner_percent <- function(include_cost_vars) {
  
  # Cost data dataframe
  cost_data <- combined_data %>%
    group_by(group,variable) %>%
    summarize(sum = sum(value)) %>%
    mutate(cost = ifelse(variable != "Spend (USD)", sum(sum[variable == "Spend (USD)"]) / sum, NA),
           cost = round(cost, 2)) %>%
    filter(variable != "Spend (USD)",
           variable %in% c("Impressions", "Reach"))
  
  # Subset the data to only include the specified variables
  cost_winners <- cost_data %>%
    filter(variable %in% c("Impressions", "Reach")) %>%
    group_by(group, variable) %>%
    summarize(cost)
  
  # Reshape the data frame into a wide format
  cost_winners_wide <- cost_winners %>%
    spread(key = group, value = cost)
  
  # Calculate the winner column
  cost_winners_wide$winner <- ifelse(cost_winners_wide$'Average Bidding' > cost_winners_wide$'Maximum Bidding', "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  cost_winners_wide <- cost_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Get the stat_significance from all_results dataframe
  stat_significance <- all_wilcox_results_cost %>%
    filter(variable %in% c("CPI", "CPR")) %>%
    pull(significance)
  
  # Add stat_significant column
  cost_winners_wide$stat_significant <- stat_significance
  
  # Define the order
  cost_level_order <- rev(c("Impressions", "Reach"))
  
  # Reorder the 'variable' factor according to level_order
  cost_winners_wide$variable <- fct_relevel(cost_winners_wide$variable, cost_level_order)
  
  # Create the bar chart
  cost_winners_plot <- ggplot(cost_winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col(aes(color = ifelse(stat_significant == "Significant", "green", "red")), size = 1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), 
                      breaks = c("Average Bidding", "Maximum Bidding"),
                      labels = c("Variation A", "Variation B"),
                      limits = c("Average Bidding", "Maximum Bidding")
                      ) +
    scale_color_identity() +
    geom_text(aes(label = paste0(round(win_percentage), "%")), vjust = .5, size = 5,hjust= -.1) +
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
      axis.text = element_text(size = 12)
    ) +
    expand_limits(y = c(0,max(cost_winners_wide$win_percentage) *1.2)) +
    guides(fill = FALSE) +
    theme(legend.position = "top")
  
  
  return(cost_winners_plot)
}

plot_cost_winner_percent()

# Winners
plot_cost_winner_amount <- function() {
  
  # Cost data dataframe
  cost_data <- combined_data %>%
    group_by(group,variable) %>%
    summarize(sum = sum(value)) %>%
    mutate(cost = ifelse(variable != "Spend (USD)", sum(sum[variable == "Spend (USD)"]) / sum, NA),
           cost = round(cost, 2)) %>%
    filter(variable != "Spend (USD)",
           variable %in% c("Impressions", "Reach"))
  
  # Subset the data to only include the specified variables
  cost_winners <- cost_data %>%
    filter(variable %in% c("Impressions", "Reach")) %>%
    group_by(group, variable) %>%
    summarize(cost)
  
  # Reshape the data frame into a wide format
  cost_winners_wide <- cost_winners %>%
    spread(key = group, value = cost)
  
  # Calculate the winner column
  cost_winners_wide$winner <- ifelse(cost_winners_wide$'Average Bidding' > cost_winners_wide$'Maximum Bidding', "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  cost_winners_wide <- cost_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference)
  
  # Get the stat_significance from all_results dataframe
  stat_significance <- all_wilcox_results_cost %>%
    filter(variable %in% c("CPI", "CPR")) %>%
    pull(significance)
  
  # Add stat_significant column
  cost_winners_wide$stat_significant <- stat_significance
  
  # Define the order
  cost_level_order <- rev(c("Impressions", "Reach"))
  
  # Reorder the 'variable' factor according to level_order
  cost_winners_wide$variable <- fct_relevel(cost_winners_wide$variable, cost_level_order)
  
  # Create the bar chart
  cost_winners_plot <- ggplot(cost_winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col(aes(color = ifelse(stat_significant == "Significant", "green", "red")), size = 1) +
    scale_color_identity() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"),
                      breaks = c("Average Bidding", "Maximum Bidding"),
                      labels = c("Variation A", "Variation B"),
                      limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0("$",round(win_percentage, 2))), vjust = .5, size = 5,hjust=-.1) +
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
      axis.text = element_text(size = 12)
    ) +
    expand_limits(y = c(0,max(cost_winners_wide$win_percentage) *1.2)) +
    guides(fill = FALSE) +
    theme(legend.position = "top")
  
  return(cost_winners_plot)
}

plot_cost_winner_amount()

# ----------------------- Conversion Rate 

plot_conversion_comparison <- function() {
  
  # Conversion Rate DF
  conversion_funnel_df <- 
    combined_data %>%
    filter(variable %in% c("Reach", 
                           "Website Clicks", "Searches", 
                           "View Content", 
                           "Add to Cart", 
                           "Purchase")) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum))  %>%
    mutate(conversion_rate = sum/sum[1])
  
  # Conversion Funnel
  conversion_funnel_plot <- conversion_funnel_df %>%
    ggplot(aes(x = reorder(variable, conversion_rate), 
               y = conversion_rate, 
               fill = group)) +
    geom_bar(stat = "identity", 
             position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", 
         y = "", 
         title = "") +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    coord_flip() +
    geom_text(aes(
      label = scales::percent(conversion_rate, accuracy = .1)),
      position = position_dodge(width = 1), 
      vjust = 0.5, 
      size = 5, 
      hjust = -.1) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)
    ) +
    labs(fill = "") +
    expand_limits(y = c(0, 
                        max(conversion_funnel_df$conversion_rate)
                        *1.3))
  
  return(conversion_funnel_plot)
}

plot_conversion_comparison()

# winner percent
plot_conversion_winner_percent <- function(include_conversion_vars) {
  
  # Subset the data to only include the specified variables
  conversion_winners_df <- 
    combined_data %>%
    filter(variable %in% include_conversion_vars) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum))  %>%
    mutate(conversion_rate = sum/sum[1])
  
  # Reshape the data frame into a wide format
  conversion_winners_wide <- conversion_winners_df %>%
    select(-sum) %>%
    spread(key = group, value = conversion_rate)
  
  # Calculate the winner column
  conversion_winners_wide$winner <- ifelse(conversion_winners_wide$`Average Bidding` > conversion_winners_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  conversion_winners_wide <- conversion_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  conversion_winners_wide$statistically_significant <- ifelse(conversion_winners_wide$win_percentage > threshold, TRUE, FALSE)
  
  # Create a new dataframe for the plot
  conversion_winners_plot <- conversion_winners_wide %>%
    select(variable, winner, win_percentage, statistically_significant)
  
  # Define the order
  conversion_level_order <- rev(c("Impressions", "Reach", 
                                  "Website Clicks", "Searches", "View Content", 
                                  "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  conversion_winners_plot$variable <- fct_relevel(conversion_winners_plot$variable, conversion_level_order)
  
  # Create the bar chart
  conversion_winners_plot <- ggplot(conversion_winners_plot, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage), "%")), vjust = .25, size = 5,hjust=-.1) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)
    ) +
    coord_flip() +
    expand_limits(y = c(0, max(conversion_winners_plot$win_percentage) * 1.2))
  
  return(conversion_winners_plot)
}
include_conversion_vars <- c("Reach", 
                             "Website Clicks", "Searches", 
                             "View Content", "Add to Cart", 
                             "Purchase")
plot_conversion_winner_percent(include_conversion_vars)

# winner amount
plot_conversion_winner_amount <- function() {
  
  # Subset the data to only include the specified variables
  conversion_winners_df <- 
    combined_data %>%
    filter(variable %in% c("Impressions", "Reach", 
                           "Website Clicks", "Searches", 
                           "View Content", "Add to Cart", 
                           "Purchase")) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum))  %>%
    mutate(conversion_rate = sum/sum[1])
  conversion_winners_df
  # Reshape the data frame into a wide format
  conversion_winners_wide <- conversion_winners_df %>%
    select(-sum) %>%
    spread(key = group, value = conversion_rate)
  
  # Calculate the winner column
  conversion_winners_wide$winner <- ifelse(conversion_winners_wide$`Average Bidding` > conversion_winners_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  conversion_winners_wide <- conversion_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           abs_difference = abs_difference* 100)
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  conversion_winners_wide$statistically_significant <- ifelse(conversion_winners_wide$abs_difference > threshold, TRUE, FALSE)
  
  # Create a new dataframe for the plot
  conversion_winners_plot <- conversion_winners_wide %>%
    select(variable, winner, abs_difference, statistically_significant)
  
  # Define the order
  conversion_level_order <- rev(c("Impressions", "Reach", 
                                  "Website Clicks", "Searches", "View Content", 
                                  "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  conversion_winners_plot$variable <- fct_relevel(conversion_winners_plot$variable, conversion_level_order)
  
  # Create the bar chart
  conversion_winners_plot <- ggplot(conversion_winners_plot, aes(x = variable, y = abs_difference, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(abs_difference,2), "%")), vjust = .25, size = 5,hjust=-.1) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12)
    ) +
    coord_flip() +
    expand_limits(y = c(0, max(conversion_winners_plot$abs_difference) * 1.2))
  
  return(conversion_winners_plot)
}


plot_conversion_winner_amount()

#Count: Impressions and Reach
#Cost: Impressions and Reach
#Conversions: Impressions, Reach, Website Clicks, Search, View Content, Add to Cart, Purchase
#include_conversion_vars <- c("Impressions", "Reach", 
#                             "Website Clicks", "Searches", "View Content", 
#                             "Add to Cart", "Purchase")


