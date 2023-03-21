
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


# ----------------- Raw Data

average_bidding <- read.csv("/Users/samuelminer/Downloads/7 R.R/*R-Project/R-Shiny-App----A-B-Test-Analysis/control_group.csv", 
                            sep = ";", 
                            header = F)
maximum_bidding <- read.csv("/Users/samuelminer/Downloads/7 R.R/*R-Project/R-Shiny-App----A-B-Test-Analysis/test_group.csv",
                            sep = ";",
                            header = F)


# ----------------- Cleaning

# Column Names Vector
column_names <- c("Campaign Name", 
                  "Date", 
                  "Spend (USD)",
                  "Impressions", 
                  "Reach", 
                  "Website Clicks", 
                  "Searches", 
                  "View Content", 
                  "Add to Cart", 
                  "Purchase")

# Change Column Names
colnames(maximum_bidding) <- column_names
colnames(average_bidding) <- column_names

# Remove the rows with column labels
average_bidding <- average_bidding[-1,] 
maximum_bidding <- maximum_bidding[-1,]       

# Change the data type of columns to integer
numeric_cols <- c("Spend (USD)", "Impressions", "Reach", "Website Clicks", 
                  "Searches", "View Content", "Add to Cart", "Purchase")
average_bidding[, numeric_cols] <- sapply(average_bidding[, numeric_cols], as.integer)
maximum_bidding[, numeric_cols] <- sapply(maximum_bidding[, numeric_cols], as.integer)

# Change the data type of the date column to Date
average_bidding$Date <- as.Date(average_bidding$Date, "%d.%m.%Y")
maximum_bidding$Date <- as.Date(maximum_bidding$Date, "%d.%m.%Y")

# Check for missing values
which(is.na(average_bidding) | average_bidding == "", arr.ind = TRUE)
sum(is.na(average_bidding))
which(is.na(maximum_bidding) | maximum_bidding == "", arr.ind = TRUE)
sum(is.na(maximum_bidding))

# Remove rows with missing values
average_bidding <- average_bidding[-5, ] # 7 Missing Values in this row 
maximum_bidding <- maximum_bidding[-5, ] # Removing to balance data

# Check for missing values again
which(is.na(average_bidding) | average_bidding == "", arr.ind = TRUE)
sum(is.na(average_bidding))
which(is.na(maximum_bidding) | maximum_bidding == "", arr.ind = TRUE)
sum(is.na(maximum_bidding))

# Reset the row numbers
rownames(average_bidding) <- 1:nrow(average_bidding)
rownames(maximum_bidding) <- 1:nrow(maximum_bidding)

# Add Day of Week
average_bidding$Weekday <- weekdays(average_bidding$Date)
maximum_bidding$Weekday <- weekdays(maximum_bidding$Date)

# ---------- Melt data to new DF

# Melt the data into long format
columns_to_melt <- c("Spend (USD)", "Impressions", "Reach",
                     "Website Clicks", "Searches", "View Content",
                     "Add to Cart", "Purchase")

average_bidding_melt <- melt(average_bidding, id.vars = "Date", measure.vars = columns_to_melt)
maximum_bidding_melt <- melt(maximum_bidding, id.vars = "Date", measure.vars = columns_to_melt)

# Add a column indicating the group (average_bidding or maximum_bidding)
average_bidding_melt$group <- "average_bidding"
maximum_bidding_melt$group <- "maximum_bidding"

# Combine the two melted data frames into one data frame
combined_data <- rbind(average_bidding_melt, maximum_bidding_melt)

# Add Day of Week
combined_data$Weekday <- weekdays(combined_data$Date)

# Change Group Names
combined_data$group <- gsub("average_bidding", "Average Bidding", 
                            gsub("maximum_bidding", "Maximum Bidding", combined_data$group))


# ----------------------- Totals

# Define Function
plot_sum_by_variable <- function(include_vars) {
  
  # Initialize dataframe
  monthly_data <- combined_data %>%
    group_by(group,variable) %>%
    summarize(sum = sum(value))
  
  # Subset the data to only include the specified variables
  monthly_data <- monthly_data[monthly_data$variable %in% include_vars, ]
  
  # Create a vector of levels in the desired order
  sum_variable_level_order <- rev(c("Spend (USD)","Impressions", "Reach", 
                                "Website Clicks", "Searches", "View Content", 
                                "Add to Cart", "Purchase"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  monthly_data$variable <- factor(monthly_data$variable, levels = sum_variable_level_order, ordered = TRUE)
  
  # Plot the selected data
  ggplot(monthly_data, aes(x = variable, y = sum, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    geom_text(aes(label = ifelse(variable == "Spend (USD)", paste0("$", format(round(sum), big.mark = ",")), format(round(sum), big.mark = ","))),
              position = position_dodge(width = 1), 
              vjust = 0.5, size = 3, 
              hjust = -0.1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, max(monthly_data$sum)*1.3))
}

 include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", 
                  "View Content", "Add to Cart", "Reach","Purchase")
 plot_sum_by_variable(include_vars)

# winners
plot_winners <- function(include_vars) {
  
  # Subset the data to only include the specified variables
  winners <- combined_data %>%
    filter(variable %in% include_vars) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value)) %>%
    arrange(desc(sum))
  
  # Reshape the data frame into a wide format
  winners_wide <- winners %>%
    spread(key = group, value = sum)
  
  # Calculate the winner column
  winners_wide$winner <- ifelse(winners_wide$'Average Bidding' > winners_wide$'Maximum Bidding', "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  winners_wide <- winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  winners_wide$statistically_significant <- ifelse(winners_wide$win_percentage > threshold, TRUE, FALSE)
  
  # Display the resulting data frame
  winners_wide <- winners_wide %>%
    select(-'Average Bidding',-'Maximum Bidding')
  
  # Define the order
  level_order <- rev(c("Impressions", "Reach", 
                       "Website Clicks", "Searches", "View Content", 
                       "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  winners_wide$variable <- fct_relevel(winners_wide$variable, level_order)
  
  # Create the bar chart
  plot_winners <- ggplot(winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%")), vjust = .25, size = 3,hjust=-.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + # Add horizontal line at 5%
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()) +
    coord_flip() +
    expand_limits(y = c(0, 
                        max(winners_wide$win_percentage)
                        *1.2))
  
  return(plot_winners)
}

include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach","Purchase")
plot_winners(include_vars)

# ----------------------- Cost

# Cost Count
plot_cost_by_variable <- function(include_cost_vars) {
  
  # Filter the cost_data to only include the specified variables
  filtered_cost_data <- cost_data %>%
    filter(variable %in% include_cost_vars)
  
  # Create a vector of levels in the desired order
  purchase_level_order <- rev(c("Impressions", "Reach", 
                                "Website Clicks", "Searches", "View Content", 
                                "Add to Cart", "Purchase"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  filtered_cost_data$variable <- factor(filtered_cost_data$variable, levels = purchase_level_order, ordered = TRUE)
  
  # Plot the selected data
  ggplot(filtered_cost_data, aes(x = variable, y = cost, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0("$", format(cost, big.mark = ","))),
              position = position_dodge(width = 1), 
              vjust = 0.5, size = 3, 
              hjust = -0.1) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    theme_minimal() +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, max(filtered_cost_data$cost) * 1.3))
}

include_cost_vars <- c("Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach")
plot_cost_by_variable(include_cost_vars)

# Winners
plot_cost_winners <- function(include_cost_vars) {
  
  # Cost data dataframe
  cost_data <- combined_data %>%
    group_by(group,variable) %>%
    summarize(sum = sum(value)) %>%
    mutate(cost = ifelse(variable != "Spend (USD)", sum(sum[variable == "Spend (USD)"]) / sum, NA),
           cost = round(cost, 2)) %>%
    filter(variable != "Spend (USD)")
  
  # Subset the data to only include the specified variables
  cost_winners <- cost_data %>%
    filter(variable %in% include_cost_vars) %>%
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
  
  # Define the order
  cost_level_order <- rev(c("Impressions", "Reach", 
                            "Website Clicks", "Searches", "View Content", 
                            "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  cost_winners_wide$variable <- fct_relevel(cost_winners_wide$variable, cost_level_order)
  
  # Create the bar chart
  cost_winners_plot <- ggplot(cost_winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%")), vjust = .25, size = 3,hjust=-.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + 
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()) +
    coord_flip() +
    expand_limits(y = c(0, 
                        max(cost_winners_wide$win_percentage)
                        *1.2))
  
  return(cost_winners_plot)
}

include_cost_vars <- c("Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach")
plot_cost_winners(include_cost_vars)

# ----------------------- Conversion Rate 

plot_conversion_funnel <- function(include_conversion_vars) {
  
  # Conversion Rate DF
  conversion_funnel_df <- 
    combined_data %>%
    filter(variable %in% include_conversion_vars) %>%
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
      size = 3, 
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
      panel.border = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, 
                        max(conversion_funnel_df$conversion_rate)
                        *1.3))
  
  return(conversion_funnel_plot)
}

include_conversion_vars <- c("Add to Cart", "Purchase")
plot_conversion_funnel(include_conversion_vars)

# winners
plot_conversion_winners <- function(include_conversion_vars) {
  
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
    geom_text(aes(label = paste0(round(win_percentage, 1), "%")), vjust = .25, size = 3,hjust=-.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + # Add horizontal line at 5%
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()) +
    coord_flip() +
    expand_limits(y = c(0, max(conversion_winners_plot$win_percentage) * 1.2))
  
  return(conversion_winners_plot)
}

include_conversion_vars <- c("Add to Cart", "Purchase")
plot_conversion_winners(include_conversion_vars)

# ----------------------------- Retention Rate

# Retention Funnel
plot_retention_funnel <- function(include_retention_vars) {
  
  retention_funnel_df <-
    combined_data %>%
    filter(variable != 'Spend (USD)',variable %in% include_retention_vars) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum)) %>%
    mutate(retention = (sum / lag(sum)))
  
  # Create a vector of levels in the desired order
    level_order <- rev(c("Impressions", "Reach", 
                         "Website Clicks", "Searches", "View Content", 
                         "Add to Cart", "Purchase"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  retention_funnel_df$variable <- factor(retention_funnel_df$variable, levels = level_order, ordered = TRUE)
  
  # Plot retential funnel
  retention_funnel_plot <- retention_funnel_df %>%
    ggplot(aes(x = variable, y = retention, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", 
         y = "", 
         title = "") +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    coord_flip() +
    geom_text(aes(label = scales::percent(retention)), 
              position = position_dodge(width = 1), vjust = 0.5, size = 3, hjust = -.1) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # axis.text.x = element_text(),
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, max(retention_funnel_df$retention,na.rm = TRUE)*1.2))
  return(retention_funnel_plot)
}

include_retention_vars <- c("Impressions", "Reach", "Website Clicks")
plot_retention_funnel(include_retention_vars)

# Winners
plot_retention_winners <- function(include_retention_vars) {
  
  # Subset the data to only include the specified variables
  retention_winners_df <- 
    combined_data %>%
    filter(variable %in% include_retention_vars) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum))  %>%
    mutate(retention_rate = sum / lag(sum))
  
  # Reshape the data frame into a wide format
  retention_winners_wide <- retention_winners_df %>%
    select(-sum) %>%
    spread(key = group, value = retention_rate)
  
  # Calculate the winner column
  retention_winners_wide$winner <- ifelse(retention_winners_wide$`Average Bidding` > retention_winners_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  retention_winners_wide <- retention_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  retention_winners_wide$statistically_significant <- ifelse(retention_winners_wide$win_percentage > threshold, TRUE, FALSE)
  
  # Create a new dataframe for the plot
  retention_winners_plot <- retention_winners_wide %>%
    select(variable, winner, win_percentage, statistically_significant)
  
  # Define the order
  retention_level_order <- rev(c("Impressions", "Reach", 
                                 "Website Clicks", "Searches", "View Content", 
                                 "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  retention_winners_plot$variable <- fct_relevel(retention_winners_plot$variable, retention_level_order)
  
  # Create the bar chart
  retention_winners_plot <- ggplot(retention_winners_plot, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 1), "%")), vjust = .25, size = 3, hjust = -.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + # Add horizontal line at 5%
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()) +
    coord_flip() +
    expand_limits(y = c(0, max(retention_winners_plot$win_percentage, na.rm = TRUE) * 1.2))
  
  return(retention_winners_plot)
}

 include_retention_vars <- c("Impressions", "Reach", "Website Clicks")
 plot_retention_winners(include_retention_vars)

# ----------------------- Purchase Rate 

# Purchase Rate Funnel
plot_purchase_funnel <- function(include_purchase_vars) {
  
  # Purchase Funnel DF
  purchase_funnel_df <- 
    combined_data %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value), .groups = "drop") %>%
    arrange(desc(sum)) %>%
    mutate(purchase_rate = ifelse(variable != "Purchase", sum[variable == "Purchase"] / sum, NA),
           purchase_rate = (round(purchase_rate, 5))) %>%
    filter(variable %in% include_purchase_vars)
  
  # Create a vector of levels in the desired order
  purchase_level_order <- rev(c("Impressions", "Reach", 
                                "Website Clicks", "Searches", "View Content", 
                                "Add to Cart"))
  purchase_funnel_df
  # Convert the 'variable' column to an ordered factor with the specified levels
  purchase_funnel_df$variable <- factor(purchase_funnel_df$variable, levels = purchase_level_order, ordered = TRUE)
  
  # Purchase Funnel
  purchase_funnel_plot <- purchase_funnel_df %>%
    ggplot(aes(x = variable,
               y = purchase_rate, 
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
      label = scales::percent(purchase_rate)),
      position = position_dodge(width = 1), 
      vjust = 0.5, 
      size = 3, 
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
      panel.border = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, 
                        max(purchase_funnel_df$purchase_rate, na.rm = TRUE)
                        *1.2))
  
  return(purchase_funnel_plot)
}
include_purchase_vars <- c("Impressions","Website Clicks", "Searches", "View Content","Add to Cart","Reach")
plot_purchase_funnel(include_purchase_vars)


#winners
plot_purchase_winners <- function(include_purchase_vars) {
  
  # Purchase Funnel DF
  purchase_winners_df <- 
    combined_data %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value), .groups = "drop") %>%
    arrange(desc(sum)) %>%
    mutate(purchase_rate = ifelse(variable != "Purchase", sum(sum[variable == "Purchase"]) / sum, NA),
           purchase_rate = (round(purchase_rate, 5))) %>%
    filter(variable %in% include_purchase_vars)
  
  # Reshape the data frame into a wide format
  purchase_winners_wide <- purchase_winners_df %>%
    select(-sum) %>%
    spread(key = group, value = purchase_rate)
  
  # Calculate the winner column
  purchase_winners_wide$winner <- ifelse(purchase_winners_wide$`Average Bidding` > purchase_winners_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  # purchase_winners_wide$win_percentage <- abs(purchase_winners_wide$'Average Bidding' - purchase_winners_wide$'Maximum Bidding') / ((purchase_winners_wide$'Average Bidding' + purchase_winners_wide$'Maximum Bidding') / 2) * 100
  purchase_winners_wide <- purchase_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  purchase_winners_wide
  # Create a vector of levels in the desired order
  purchase_level_order <- rev(c("Impressions", "Reach", 
                                "Website Clicks", "Searches", "View Content", 
                                "Add to Cart", "Purchase"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  purchase_winners_wide$variable <- factor(purchase_winners_wide$variable, levels = purchase_level_order, ordered = TRUE)
  
  # Purchase Rate Winners Plot
  purchase_winners_plot <- ggplot(purchase_winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 1), "%")), vjust = .25, size = 3, hjust = -.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + # Add horizontal line at 5%
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank()) +
    coord_flip() +
    expand_limits(y = c(0, max(purchase_winners_wide$win_percentage) * 1.2))
  
  return(purchase_winners_plot)
}

include_purchase_vars <- c("Impressions","Website Clicks", "Searches", "View Content","Add to Cart","Reach")
plot_purchase_winners(include_purchase_vars)

#here
# unique(combined_data$variable)
