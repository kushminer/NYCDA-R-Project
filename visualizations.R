
# Load Libraries
list.files()
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

# Select Conversion Variable to Include
monthly_data <- combined_data %>%
  group_by(group,variable) %>%
  summarize(sum = sum(value))

# Define Function
plot_sum_by_variable <- function(monthly_data, include_vars) {
  
  # Subset the data to only include the specified variables
  monthly_data <- monthly_data[monthly_data$variable %in% include_vars, ]
  
  # Plot the selected data
  ggplot(monthly_data, aes(x = group, y = sum, fill = group)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    geom_text(aes(label = ifelse(variable == "Spend (USD)", paste0("$", format(round(sum), big.mark = ",")), format(round(sum), big.mark = ","))),
              position = position_dodge(width = 1), 
              vjust = 0.5, size = 3, 
              hjust = 0,
              size = 3) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) + 
    facet_wrap(~variable, scales = "free_y",ncol=4) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, max(monthly_data$sum)*1.3))
}

# ----------------------- KPI'S --> Cost

cost_data <- combined_data %>%
  group_by(group,variable) %>%
  summarize(sum = sum(value)) %>%
  mutate(cost = ifelse(variable != "Spend (USD)", sum(sum[variable == "Spend (USD)"]) / sum, NA),
         cost = round(cost, 2)) %>%
  filter(variable != "Spend (USD)")

# Define Function
plot_cost_by_variable <- function(cost_data, include_cost_vars) {
  
  # Subset the data to only include the specified variables
  cost_data <- cost_data[cost_data$variable %in% include_cost_vars, ]
  
  # Plot the selected data
  ggplot(cost_data, aes(x = group, y = cost, fill = group)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0("$", format(cost))),
              position = position_dodge(width = 1), 
              vjust = 0.5, size = 3, 
              hjust = 0,
              size = 3) +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE) + 
    facet_wrap(~variable, scales = "free_y", ncol = 4) +
    theme_minimal() +
    coord_flip() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank()) +
    labs(fill = "") +
    expand_limits(y = c(0, max(cost_data$cost)*1.3))
}

plot_cost_by_variable(cost_data, include_cost_vars)

# ----------------------- Funnels

# {Funnel of Count}
count_funnel_df <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  filter(variable != "Spend (USD)")

# Plot funnel of Count
count_funnel_plot <- count_funnel_df %>%
  ggplot(aes(x = reorder(variable,sum), 
             y = sum, 
             fill = group)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Count") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  theme(legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = formatC(formatC(count_funnel_df$sum, format="f", big.mark=",", digits=0), 
                                format="f", 
                                big.mark=",", 
                                digits=0)),
            position = position_dodge(width = 1), 
            vjust = 0.5, size = 3, 
            hjust = 0) +
  theme_minimal() +
  theme(
    # axis.text.x = element_text(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()) +
  labs(fill = "", ) +
  expand_limits(y = c(0, max(count_funnel_df$sum)*1.3))

# {Funnel of Retention}

retention_funnel_df =
  combined_data %>%
  filter(variable != 'Spend (USD)') %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  arrange(desc(sum)) %>%
  mutate(retention = (sum / lag(sum)))
retention_funnel_df
# Create a vector of levels in the desired order
level_order <- rev(c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"))

# Convert the 'variable' column to an ordered factor with the specified levels
retention_funnel_df$variable <- factor(retention_funnel_df$variable, levels = level_order, ordered = TRUE)

# Plot retential funnel
retention_funnel_plot <- retention_funnel_df %>%
  ggplot(aes(x = variable, y = retention, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Retention per Step") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  coord_flip() +
  geom_text(aes(label = scales::percent(retention, accuracy = 1)), 
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
  expand_limits(y = c(0, max(retention_funnel_df$retention)*1.2))

# {r, Funnel of Cost}

# Cost Funnel Dataframe
cost_funnel_df <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  mutate(cost = ifelse(variable != "Spend (USD)",sum(sum[variable == "Spend (USD)"]), NA) / sum,
         cost = round(cost, 2)) %>%
  filter(variable != "Spend (USD)")
cost_funnel_df

# Barchart of Cost per Instance
cost_funnel_plot <- cost_funnel_df %>%
  ggplot(aes(x = reorder(variable, cost), y = cost, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Cost Funnel") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  theme(legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = paste0("$",cost)), 
            position = position_dodge(width = 1), vjust = 0.5, size = 3, hjust = -.1) +
  theme_minimal() +
  theme(
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
  expand_limits(y = c(0, max(cost_funnel_df$cost)*1.15))

# {r, Funnel of Impressions}

# Conversion Rate DF
impressions_funnel_df = 
  combined_data %>%
  filter(variable != 'Spend (USD)') %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  arrange(desc(sum))  %>%
  mutate(impressions_rate = sum/sum[1])

# Impressions Funnel

impressions_funnel_plot <- impressions_funnel_df %>%
  ggplot(aes(x = reorder(variable, impressions_rate), 
             y = impressions_rate, 
             fill = group)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Conversion of Impressions") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  coord_flip() +
  geom_text(aes(
    label = scales::percent(impressions_rate, accuracy = .1)),
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
                      max(impressions_funnel_df$impressions_rate)
                      *1.3))

# ---------------------------- Winners

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
  winners_wide$winner <- ifelse(winners_wide$average_bidding > winners_wide$maximum_bidding, "average_bidding", "maximum_bidding")
  
  # Calculate the win_percentage column
  winners_wide$win_percentage <- abs(winners_wide$average_bidding - winners_wide$maximum_bidding) / ((winners_wide$average_bidding + winners_wide$maximum_bidding) / 2) * 100
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  winners_wide$statistically_significant <- ifelse(winners_wide$win_percentage > threshold, TRUE, FALSE)
  
  # Display the resulting data frame
  winners_wide %>%
    select(-average_bidding,-maximum_bidding)
  
  # Convert the wide format data frame back to the long format
  data_long <- winners_wide %>%
    gather(key = "group", value = "sum", average_bidding, maximum_bidding)
  
  # Merge the winner and statistically_significant columns
  data_long <- merge(data_long, winners_wide[, c("variable", "winner", "statistically_significant")], by = "variable")
  
  # Create a separate dataframe for the annotations
  annotations <- data_long %>%
    group_by(variable) %>%
    top_n(n = 1, wt = sum) %>%
    left_join(winners_wide, by = "variable")
  
  # Create a new dataframe for the plot
  winners_plot <- winners_wide %>%
    select(variable, winner, win_percentage, statistically_significant)
  
  # Define the order
  level_order <- rev(include_vars)
  
  # Reorder the 'variable' factor according to level_order
  winners_plot$variable <- fct_relevel(winners_plot$variable, level_order)
  
  # Create the bar chart
  ggplot(winners_plot, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    geom_text(aes(label = paste0(round(win_percentage, 1), "%")), vjust = .25, size = 3,hjust=-.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + # Add horizontal line at 5%
    labs(title = "Win Percentage of Average Bidding vs Maximum Bidding",
         x = "Variable",
         y = "Win Percentage") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    coord_flip() +
    expand_limits(y = c(0, 
                        max(winners_plot$win_percentage)
                        *1.2))
}

# ---------------------------- Funnel Plots Function

plot_funnels <- function(include_funnels) {
  # Your funnel data processing code remains the same
  # ...
  
  # Create an empty list to store the plots
  funnel_plots <- list()
  
  if ("Count" %in% include_funnels) {
    count_funnel_plot <- count_funnel_df %>%
      # ... your ggplot code for Count funnel ...
      funnel_plots[["Count"]] <- count_funnel_plot
  }
  
  if ("Retention" %in% include_funnels) {
    retention_funnel_plot <- retention_funnel_df %>%
      # ... your ggplot code for Retention funnel ...
      funnel_plots[["Retention"]] <- retention_funnel_plot
  }
  
  if ("Cost" %in% include_funnels) {
    cost_funnel_plot <- cost_funnel_df %>%
      # ... your ggplot code for Cost funnel ...
      funnel_plots[["Cost"]] <- cost_funnel_plot
  }
  
  if ("Impressions" %in% include_funnels) {
    impressions_funnel_plot <- impressions_funnel_df %>%
      # ... your ggplot code for Impressions funnel ...
      funnel_plots[["Impressions"]] <- impressions_funnel_plot
  }
  
  return(funnel_plots)
}

# Utilizing Functions
selected_funnels <- c("Count", "Retention", "Cost", "Impressions")
funnel_plots <- plot_funnels(selected_funnels)

# Selected Returns
funnel_plots[["Count"]]       # Access the Count funnel plot
funnel_plots[["Retention"]]   # Access the Retention funnel plot
funnel_plots[["Cost"]]        # Access the Cost funnel plot
funnel_plots[["Impressions"]] # Access the Impressions funnel plot


