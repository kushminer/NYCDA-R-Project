
source("data_cleaning.R")

# ----------------------------> Variables are not normally distributed, use Wilcoxon rank-sum test per variable  filtered_data <- data %>%


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

wilcox_test_spend(combined_data)

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

wilcox_test_impressions(combined_data)

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

wilcox_test_reach(combined_data)

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

wilcox_test_reach(combined_data)

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

wilcox_test_website_clicks(combined_data)

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

wilcox_test_searches(combined_data)


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

wilcox_test_view_content(combined_data)

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

wilcox_test_add_to_cart(combined_data)

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

wilcox_test_purchase(combined_data)

# Combine the results

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

# Text Output
# Split the data frame based on significance
split_results <- split(all_results, all_results$significance)

# Extract variable names
significantly_different <- paste(split_results[["Significant"]]$variable, collapse = ", ")
not_significantly_different <- paste(split_results[["Not Significant"]]$variable, collapse = ", ")

# Display the text output
cat("Significantly different variables:", significantly_different, "\n")
cat("Not significantly different variables:", not_significantly_different, "\n")

# Notes

# The Wilcoxon rank-sum test compares the medians of the two groups 
# and is less sensitive to outliers than parametric tests.


# The Wilcoxon rank sum test, also known as the Mann-Whitney U test, is a non-parametric statistical hypothesis test used to compare two independent groups of continuous data. It tests the null hypothesis that the two groups have the same distribution of values against the alternative hypothesis that the two groups have different distributions of values.

# The test ranks all the values in both groups together, from lowest to highest, and then calculates the sum of the ranks for each group. It then calculates the test statistic, which is the smaller of the two sums. The p-value of the test is then calculated, which indicates the probability of obtaining the observed test statistic or a more extreme value under the null hypothesis.

# The Wilcoxon rank sum test is a useful alternative to the t-test when the normality assumption of the t-test is violated, or when the sample size is small. It is also appropriate when the data are ordinal, interval, or ratio, but not normally distributed.



