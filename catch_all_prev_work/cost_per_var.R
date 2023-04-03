source("data_cleaning.R")

# Daily cost per var
cost_per_var <- combined_data %>%
  spread(variable, value) %>%
  mutate(CPI = `Spend (USD)` / Impressions,
         CPR = `Spend (USD)` / Reach,
         CPC = `Spend (USD)` / `Website Clicks`,
         CPS = `Spend (USD)` / Searches,
         CPV = `Spend (USD)` / `View Content`,
         CPA = `Spend (USD)` / `Add to Cart`,
         CPP = `Spend (USD)` / Purchase) %>%
  select(Date, group, `Spend (USD)`,CPI,CPC,CPP)

# Mean cost per var
cost_per_var_mean <- cost_per_var %>%
  group_by(group) %>%
  summarise(mean_CPI = mean(CPI),
         mean_Spend = mean(CPI),
         mean_CPP = mean(CPI),
         mean_CPC = mean(CPI))


# Accumulated cost per var
combined_data %>%
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
  select(group, CPI, CPR,CPC,CPP)

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
cpi_results <- wilcox_test_CPI(cost_per_var) %>% mutate(variable = "CPI")
cpc_results <- wilcox_test_CPC(cost_per_var) %>% mutate(variable = "CPC")
cpp_results <- wilcox_test_CPP(cost_per_var) %>% mutate(variable = "CPP")


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

# Text Output
# Split the data frame based on significance
wilcox_cost_split_results <- split(all_wilcox_results_cost, all_wilcox_results_cost$significance)

# Extract variable names
significantly_different <- paste(wilcox_cost_split_results[["Significant"]]$variable, collapse = ", ")
not_significantly_different <- paste(wilcox_cost_split_results[["Not Significant"]]$variable, collapse = ", ")

# Display the text output
cat("Significantly different variables:", significantly_different, "\n")
cat("Not significantly different variables:", not_significantly_different, "\n")

