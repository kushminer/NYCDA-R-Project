
source("data_cleaning.R")

###########################################################################

# KEY METRICS #

###########################################################################

# DataFrame used is combined_data, from data_cleaning.R

###########################################################################

# DAILY KEY METRICS PLOT #

###########################################################################

plot_key_metrics_daily <- function(data, include_daily_vars, colors = c("#ADD8E6", "#000080")) {
  plot_data <- data %>% filter(variable %in% include_daily_vars)
  
  p <- plot_ly()
  
  for (var in include_daily_vars) {
    for (i in 1:length(unique(plot_data$group))) {
      group <- unique(plot_data$group)[i]
      group_data <- plot_data %>% filter(group == !!group, variable == !!var)
      mean_value <- mean(group_data$value, na.rm = TRUE)
      
      p <- p %>%
        add_trace(data = group_data, x = ~Date, y = ~value, type = "scatter", mode = "lines+markers",
                  name = paste(group, var), line = list(color = colors[i], width = 2),
                  marker = list(color = colors[i]),
                  text = ~paste(group,
                                "<br>",include_daily_vars,"on", Date,
                                "<br>", value),
                  hoverinfo = "text") %>%
        add_trace(x = group_data$Date, y = rep(mean_value, length(group_data$Date)), type = "scatter", mode = "lines",
                  name = paste("Mean", group, var), line = list(color = colors[i], width = 1, dash = "dash"),
                  text = ~paste("Mean Line",
                                "<br>Group:", group),
                  hoverinfo = "text")
    }
  }
  
  p <- p %>%
    layout(title = paste("Daily", include_daily_vars),
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           showlegend = FALSE)
  
  return(p)
}

# Run function:
include_daily_vars <- c("Impressions")
plot_key_metrics_daily(combined_data, include_daily_vars)

###########################################################################

# CUMULATIVE KEY METRICS  #

###########################################################################


plot_cumulative_key_metrics <- function(data, include_daily_vars, colors = c("#ADD8E6", "#000080")) {
  group_sums <- data %>%
    filter(variable %in% include_daily_vars) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value))
  
  p <- ggplot(group_sums, aes(x = variable, y = sum, fill = group)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = group, color = "black")) +
    scale_fill_manual(values = colors, guide = FALSE) +
    scale_color_manual(values = colors, guide = FALSE) +
    geom_text(aes(label = comma(sum)), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
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
    labs(fill = "",x =NULL, title = paste("Cumulative", include_daily_vars)) +
    scale_y_continuous(labels = comma)
  
  return(p)
}


plot_cumulative_key_metrics(combined_data,include_daily_vars)



#####################################################################################################################################

# WILCOXON TEST ON KEY METRICS

# Variables are not normally distributed, use Wilcoxon rank-sum test per variable
# Non Parametric (used for non-normalized data) test used to compare medians of two independent groups
# Concludes there is a significant difference in the median of two groups when p-value is less than 0.05
# The test ranks all the values in both groups together, from lowest to highest, and then calculates the sum of the ranks for each group. It then calculates the test statistic, which is the smaller of the two sums. The p-value of the test is then calculated, which indicates the probability of obtaining the observed test statistic or a more extreme value under the null hypothesis.
# The Wilcoxon rank sum test is a useful alternative to the t-test when the normality assumption of the t-test is violated, or when the sample size is small. It is also appropriate when the data are ordinal, interval, or ratio, but not normally distributed.


#####################################################################################################################################

# Spend (USD)
wilcox_test_spend <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "Spend (USD)")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_impressions <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "Impressions")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_reach <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "Reach")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_website_clicks <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "Website Clicks")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_searches <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- combined_data %>%
    filter(variable == "Searches")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = suppressWarnings(wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic),
              p_value = suppressWarnings(wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_view_content <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "View Content")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = suppressWarnings(wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic),
              p_value = suppressWarnings(wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_add_to_cart <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "Add to Cart")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value) %>%
    ungroup()
  
  return(wilcox_results)
}

wilcox_test_purchase <- function(data) {
  # Filter the dataset to only include the "Impressions" variable
  filtered_data <- data %>%
    filter(variable == "Add to Cart")
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    select(-variable) %>%
    pivot_wider(names_from = group, values_from = value)
  
  # Group the data by variable and group, and calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value) %>%
    ungroup()
  
  return(wilcox_results)
}

# Print Individual Results
wilcox_test_spend(combined_data)
wilcox_test_impressions(combined_data)
wilcox_test_reach(combined_data)
wilcox_test_website_clicks(combined_data)
wilcox_test_searches(combined_data)
wilcox_test_view_content(combined_data)
wilcox_test_add_to_cart(combined_data)
wilcox_test_purchase(combined_data)

# Run each Wilcoxon test and create a dataframe with results
spend_results <- wilcox_test_spend(combined_data) %>% mutate(variable = "Spend (USD)")
impressions_results <- wilcox_test_impressions(combined_data) %>% mutate(variable = "Impressions")
reach_results <- wilcox_test_reach(combined_data) %>% mutate(variable = "Reach")
website_clicks_results <- wilcox_test_website_clicks(combined_data) %>% mutate(variable = "Website Clicks")
searches_results <- wilcox_test_searches(combined_data) %>% mutate(variable = "Searches")
view_content_results <- wilcox_test_view_content(combined_data) %>% mutate(variable = "View Content")
add_to_cart_results <- wilcox_test_add_to_cart(combined_data) %>% mutate(variable = "Add to Cart")
purchase_results <- wilcox_test_purchase(combined_data) %>% mutate(variable = "Purchase")

# Combine the results into a single dataframe
all_results <- bind_rows(spend_results,
                         impressions_results,
                         reach_results,
                         website_clicks_results,
                         searches_results,
                         view_content_results,
                         add_to_cart_results,
                         purchase_results)

# Reorder columns to put 'variable' as the first column
all_results <- all_results %>%
  select(variable, test_statistic, p_value) %>%
  mutate(significance = ifelse(p_value < 0.05, "Significant", "Not Significant"))


# Display the dataframe
all_results

#####################################################################################################################################

# KEY METRICS DATAFRAME

#####################################################################################################################################


# Total derived from sum of all values per group
key_metrics <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value))  %>%
  spread(variable, sum) %>%
  select(group, Impressions, `Website Clicks`, Purchase)

# Reshape conversion_rate_total to long format
long_key_metrics <- key_metrics %>%
  ungroup() %>%
  pivot_longer(cols = -group, names_to = "variable", values_to = "value")

# Join all_wilcox_results_conversions with long_conversion_rate_total
key_metrics <- all_results %>%
  filter(variable %in% c("Impressions", "Website Clicks","Purchase")) %>%
  left_join(long_key_metrics, by = "variable") %>%
  pivot_wider(names_from = group, values_from = value)

key_metrics <- key_metrics %>%
  mutate(Lift = (`Average Bidding` - `Maximum Bidding`) / `Maximum Bidding`)

key_metrics <- key_metrics %>%
  select(variable, "Average Bidding","Maximum Bidding",Lift, p_value)

key_metrics <- key_metrics %>%
  rename("p-value" = p_value,
         "Key Metric" = variable)

key_metrics <- key_metrics %>%
  mutate(`Average Bidding` = scales::comma(`Average Bidding`, accuracy = 1),
         `Maximum Bidding` = scales::comma(`Maximum Bidding`, accuracy = 1),
         Lift = scales::percent(Lift, accuracy = 0.01),
         `p-value` = formatC(`p-value`, format = "f", digits = 5))

key_metrics

