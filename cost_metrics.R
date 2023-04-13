
source("data_cleaning.R")

###########################################################################

                              # Cost Metrics #

###########################################################################

# Daily Cost Metrics
daily_cost_metrics <- combined_data %>%
  spread(variable, value) %>%
  mutate(CPI = `Spend (USD)` / Impressions,
         CPR = `Spend (USD)` / Reach,
         CPC = `Spend (USD)` / `Website Clicks`,
         CPS = `Spend (USD)` / Searches,
         CPV = `Spend (USD)` / `View Content`,
         CPA = `Spend (USD)` / `Add to Cart`,
         CPP = `Spend (USD)` / Purchase) %>%
  select(Date, group, `Spend (USD)`,CPI,CPC,CPP) %>% 
  rename(Spend = `Spend (USD)`)

# Mean cost per var
cost_per_var_mean <- daily_cost_metrics %>%
  group_by(group) %>%
  summarise(mean_CPI = mean(CPI),
            mean_Spend = mean(Spend),
            mean_CPP = mean(CPP),
            mean_CPC = mean(CPC))

# Cumulative Cost Metrics
cumulative_cost_metrics <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  spread(variable, sum) %>%
  mutate(CPI = `Spend (USD)` / Impressions,
         CPR = `Spend (USD)` / Reach,
         CPC = `Spend (USD)` / `Website Clicks`,
         CPS = `Spend (USD)` / Searches,
         CPV = `Spend (USD)` / `View Content`,
         CPA = `Spend (USD)` / `Add to Cart`,
         CPP = `Spend (USD)` / Purchase) %>%
  select(group, `Spend (USD)`, CPI,CPC,CPP) %>% 
  rename(Spend = `Spend (USD)`) %>%
  pivot_longer(cols = -group, names_to = "variable", values_to = "sum")

###########################################################################

                        # Daily Cost Metrics Plot #

###########################################################################

daily_cost_metrics_plot <- function(data, selected_cost_metric, colors = c("#ADD8E6", "#000080")) {
  p <- plot_ly()
  
  for (i in 1:length(unique(data$group))) {
    group <- unique(data$group)[i]
    group_data <- data %>% filter(group == !!group)
    mean_value <- mean(group_data[[selected_cost_metric]], na.rm = TRUE)
    
    # Define a mapping of selected variables to titles
    title_mapping <- list(
      CPP = "Cost per Purchase",
      Spend = "Spend",
      CPI = "Cost per Impression",
      CPC = "Cost per Click"
    )
    
    # Look up the title based on the selected variable
    title <- title_mapping[[selected_cost_metric]]
    
    p <- p %>%
      add_trace(data = group_data, x = ~Date, y = as.formula(paste0("~", selected_cost_metric)), type = "scatter", mode = "lines+markers",
                name = paste(group, selected_cost_metric), line = list(color = colors[i], width = 2),
                marker = list(color = colors[i]),
                text = ~paste(group,
                              "<br>", Date,
                              "<br>", scales::dollar(get(selected_cost_metric))),
                hoverinfo = "text") %>%
      add_trace(x = group_data$Date, y = rep(mean_value, length(group_data$Date)), type = "scatter", mode = "lines",
                name = paste("Mean", group, selected_cost_metric), line = list(color = colors[i], width = 1, dash = "dash"),
                text = ~paste("Group:", group,
                              "<br>Mean Line"),
                hoverinfo = "text")
  }
  
  p <- p %>%
    layout(title = paste0("Daily ",title),
           xaxis = list(title = ""),
           yaxis = list(title = "", tickformat = "$.2f"),
           showlegend = FALSE)
  
  return(p)
}

selected_cost_metric <- "Spend"
daily_cost_metrics_plot(daily_cost_metrics, selected_cost_metric)

###########################################################################

                          # Accumulated Cost Metrics Plot #

###########################################################################

plot_cumulative_cost_metrics <- function(data, selected_cost_metric, colors = c("#ADD8E6", "#000080")) {
  
  group_sums <- data %>%
    filter(variable %in% selected_cost_metric)
  
  # Define a mapping of selected variables to titles
  title_mapping <- list(
    CPP = "Cost per Purchase",
    Spend = "Spend",
    CPI = "Cost per Impression",
    CPC = "Cost per Click"
  )
  
  # Look up the title based on the selected variable
  title <- title_mapping[[selected_cost_metric]]
  
  p <- ggplot(group_sums, aes(x = variable, y = sum, fill = group)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = group, color = "black")) +
    scale_fill_manual(values = colors, guide = FALSE) +
    scale_color_manual(values = colors, guide = FALSE) +
    geom_text(aes(label = dollar(sum)), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text=element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20)) +
    labs(fill = "",x =NULL, title = paste("Cumulative", title)) +
    scale_y_continuous(labels = comma)
  
  return(p)
}

selected_cost_metric <- "CPP"
plot_cumulative_cost_metrics(cumulative_cost_metrics,selected_cost_metric)

###########################################################################

# COST METRICS WILCOXON TEST #

###########################################################################

wilcox_test_CPI <- function(data) {
  # Filter the dataset to only include the "CPI" variable and convert it to numeric
  filtered_data <- data %>%
    select(Date,group, CPI)
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = group, values_from = CPI)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)
  
  return(wilcox_results)
}

wilcox_test_CPC <- function(data) {
  # Filter the dataset to only include the "CPI" variable and convert it to numeric
  filtered_data <- data %>%
    select(Date,group, CPC)
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = group, values_from = CPC)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)
  
  return(wilcox_results)
}

wilcox_test_CPP <- function(data) {
  # Filter the dataset to only include the "CPI" variable and convert it to numeric
  filtered_data <- data %>%
    select(Date,group, CPP)
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = group, values_from = CPP)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)
  
  return(wilcox_results)
}

# Combine the results
# Run each Wilcoxon test and create a dataframe with results
cpi_results <- wilcox_test_CPI(daily_cost_metrics) %>% mutate(variable = "CPI")
cpc_results <- wilcox_test_CPC(daily_cost_metrics) %>% mutate(variable = "CPC")
cpp_results <- wilcox_test_CPP(daily_cost_metrics) %>% mutate(variable = "CPP")


# Combine the results into a single dataframe
all_wilcox_results_cost <- bind_rows(cpi_results,
                                     cpc_results,
                                     cpp_results)

# Reorder columns to put 'variable' as the first column
all_wilcox_results_cost <- all_wilcox_results_cost %>%
  select(variable, test_statistic, p_value) %>%
  mutate(significance = ifelse(p_value < 0.05, "Significant", "Not Significant"))


# Display the dataframe
all_wilcox_results_cost


###########################################################################

                          # Cost Metrics Win Probability #

###########################################################################

# Function to calculate the win probability of the maximum bidding group over the average bidding group
calculate_cost_win_probability <- function(cost_variable_pairs) {
  
  # Initialize an empty data frame to store the results
  results <- data.frame()
  
  # Loop through each cost and variable pair
  for (pair in cost_variable_pairs) {
    # Filter the combined data for the cost and variable pair
    daily_data <- combined_data %>%
      filter(variable %in% c(pair[1], pair[2]))
    
    # Calculate the cost per variable for each day and group
    daily_cpv <- daily_data %>%
      group_by(Date, group, variable) %>%
      summarize(sum = sum(value), .groups = 'drop') %>%
      spread(key = variable, value = sum) %>%
      mutate(CPV = !!sym(pair[1]) / !!sym(pair[2]))
    
    # Calculate the win probability for each day using a beta distribution
    win_probability_daily <- daily_cpv %>%
      group_by(Date) %>%
      do({
        # Separate the data for the average bidding and maximum bidding groups
        group_A <- filter(., group == "Average Bidding")
        group_B <- filter(., group == "Maximum Bidding")
        
        # Get the number of successes and trials for each group
        successes_A <- group_A %>% pull(!!sym(pair[1]))
        trials_A <- group_A %>% pull(!!sym(pair[2]))
        successes_B <- group_B %>% pull(!!sym(pair[1]))
        trials_B <- group_B %>% pull(!!sym(pair[2]))
        
        # Set the number of samples to generate from the beta distribution
        n_samples <- 10000
        
        # Check if there are any missing values in the data
        if (all(!is.na(successes_A) & !is.na(trials_A) & !is.na(successes_B) & !is.na(trials_B))) {
          # Generate samples from the beta distribution for each group
          samples_A <- rbeta(n_samples, shape1 = successes_A + 1, shape2 = trials_A - successes_A + 1)
          samples_B <- rbeta(n_samples, shape1 = successes_B + 1, shape2 = trials_B - successes_B + 1)
          
          # Calculate the win probability of the maximum bidding group over the average bidding group
          win_probability_B <- mean(samples_B > samples_A)
        } else {
          # If there are missing values, set the win probability to NA
          win_probability_B <- NA
        }
        
        # Return the win probability for each day and group as a data frame
        data.frame(Date = unique(group_A$Date), KPI = paste(pair[1], pair[2], sep = "_"), win_prob_average_bidding = win_probability_B)
      })
    
    # Calculate the average win probability across all days and groups for the cost and variable pair
    average_win_probability_A <- mean(win_probability_daily$win_prob_average_bidding, na.rm = TRUE)
    
    # Add the average win probability to the results data frame
    results <- rbind(results, data.frame(CPV = paste(pair[1], pair[2], sep = "_"), win_prob_average_bidding = average_win_probability_A))
  }
  
  # Return the results data frame
  return(results)
}

# List of cost and variable pairs to calculate the win probability for
cost_variable_pairs <- list(
  c("Spend (USD)","Impressions"),
  c("Spend (USD)","Website Clicks"),
  c("Spend (USD)","Purchase"),
  c("Spend (USD)", "Spend (USD)")
  )

# Save results in a DataFrame
cost_win_probability <- calculate_cost_win_probability(cost_variable_pairs)
cost_win_probability

###########################################################################

                          # Cost Metrics DataFrame #

###########################################################################

# Update row names in cost_win_probability
cost_win_probability$CPV <- c("CPI", "CPC","CPP","Spend")

# adjust column names in cost_win_probability
cost_win_probability <- cost_win_probability %>%
  rename("variable" = CPV)

# Reshape wide_total_cost_metrics to wide format
wide_total_cost_metrics <- cumulative_cost_metrics %>%
  pivot_wider(names_from = group, values_from = sum)

# Join all_wilcox_results_cost with wide_total_cost_metrics
wide_total_cost_metrics <- wide_total_cost_metrics %>%
  left_join(all_wilcox_results_cost %>% 
              select(variable, p_value), by = "variable")

# Calculate Lift
wide_total_cost_metrics <- wide_total_cost_metrics %>%
  mutate(Lift = (`Average Bidding` - `Maximum Bidding`) / `Maximum Bidding`)

# Merge wide_total_cost_metrics with cost_win_probability
wide_total_cost_metrics<- merge(wide_total_cost_metrics, cost_win_probability, by = "variable")

# Rename Columns
wide_total_cost_metrics <- wide_total_cost_metrics %>%
  rename("p-value" = p_value,
         "Cost Metric" = variable,
         "Win Probability (Average Bidding)"="win_prob_average_bidding")

# Change variable names
wide_total_cost_metrics$`Cost Metric` <- recode(wide_total_cost_metrics$`Cost Metric`,
                                             "CPC" = "Cost per Click",
                                             "CPP" = "Cost per Purchase",
                                             "CPI" = "Cost per Impression")

# Cost_per_var_mean data prep
cost_per_var_mean <- cost_per_var_mean %>%
  pivot_longer(cols = -group, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(Lift = (`Average Bidding` - `Maximum Bidding`) / `Maximum Bidding`)

# Cost_per_var_mean change col names
cost_per_var_mean <- cost_per_var_mean %>%
  rename("Average Bidding Mean" = `Average Bidding`,
         "Maximum Bidding Mean" = `Maximum Bidding`,
         "Lift Mean" = Lift,
         "Cost Metric" = variable
         ) 

# Cost_per_var_mean change Var Names
cost_per_var_mean$`Cost Metric` <-   recode(cost_per_var_mean$`Cost Metric`,
         "mean_Spend" = "Spend",
         "mean_CPC" = "Cost per Click",
         "mean_CPP" = "Cost per Purchase",
         "mean_CPI" = "Cost per Impression")

# Join Cost per Var Mean
wide_total_cost_metrics <- wide_total_cost_metrics %>%
  left_join(cost_per_var_mean, by = "Cost Metric", suffix = c(".wide", ".mean"))

# Format scales
wide_total_cost_metrics <- wide_total_cost_metrics %>%
  mutate(
    `Average Bidding` = scales::dollar(`Average Bidding`, prefix = "$", accuracy = 0.001),
    `Maximum Bidding` = scales::dollar(`Maximum Bidding`, prefix = "$", accuracy = 0.001),
    `Average Bidding Mean` = scales::dollar(`Average Bidding Mean`, prefix = "$", accuracy = 0.001),
    `Maximum Bidding Mean` = scales::dollar(`Maximum Bidding Mean`, prefix = "$", accuracy = 0.001),
    `Lift Mean` = scales::percent(Lift, accuracy = 0.01),
    Lift = scales::percent(Lift, accuracy = 0.01),
    `p-value` = formatC(`p-value`, format = "f", digits = 8),
    `Win Probability (Average Bidding)` = scales::percent(`Win Probability (Average Bidding)`, accuracy = 0.01))

# Organize Columns
wide_total_cost_metrics <- wide_total_cost_metrics %>%
  select(`Cost Metric`, `Average Bidding`,`Maximum Bidding`,Lift, `Average Bidding Mean`,`Maximum Bidding Mean`,`Lift Mean`,`p-value`,`Win Probability (Average Bidding)`)

# Organize Rows
wide_total_cost_metrics <-wide_total_cost_metrics %>%
  mutate(`Cost Metric` = factor(`Cost Metric`, levels = c("Spend", "Cost per Impression", "Cost per Click", "Cost per Purchase"))) %>%
  arrange(`Cost Metric`)

wide_total_cost_metrics
