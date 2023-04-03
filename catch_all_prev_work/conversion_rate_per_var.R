
source("data_cleaning.R")

# Create a function to plot on a plotly plot the date and value of the variables in each column
# the user can select a variable which will be translated:
# -- Click through Rate from Unique Users Reached = CTR_R 
# -- Purchase Rate from Website Clicks = PR_C
# -- Purchase Completion from Add to Cart = PR_ATC
# the user can also select to see Rate, or Lift, 

# Conversion Rate Dataframe
conversion_rate_per_var <- combined_data %>%
  spread(variable, value) %>%
  mutate(CTR_R  = `Website Clicks` / Impressions,
         PR_R   =  Purchase        / Impressions,
         PR_ATC =  Purchase        / `Add to Cart`
         ) %>%
  select(Date,group,Reach,`Website Clicks`,`Purchase`,CTR_R,PR_R,PR_ATC)

# Conversion Rate for Period
combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  spread(variable, sum) %>%
  mutate(CTR_R  = `Website Clicks` / Impressions,
         PR_R   =  Purchase        / Impressions,
         PR_ATC =  Purchase        / `Add to Cart`) %>%
  select(group, CTR_R,PR_R,PR_ATC)

# -------------------------------------> Wilcox Rank Sum Test

# Click through Rate, Reach
wilcox_test_CTR_R <- function(data) {
  
  # Filter the dataset
  filtered_data <- data %>%
    select(Date, group, CTR_R)
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = group, values_from = CTR_R)
  
  # Group the data by variable and group
  # Calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)
  
  return(wilcox_results)
}

# Purchase Rate, Reach
wilcox_test_PR_R <- function(data) {
  
  # Filter the dataset
  filtered_data <- data %>%
    select(Date, group, PR_R)
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = group, values_from = PR_R)
  
  # Group the data by variable and group
  # Calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)
  
  return(wilcox_results)
}


# Purchase Rate, Add to Cart
wilcox_test_PR_ATC <- function(data) {
  
  # Filter the dataset
  filtered_data <- data %>%
    select(Date, group, PR_ATC)
  
  # Pivot the data to have "Average Bidding" and "Maximum Bidding" as separate columns
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = group, values_from = PR_ATC)
  
  # Group the data by variable and group
  # Calculate the Wilcoxon rank sum test statistic and p-value for each group
  wilcox_results <- pivoted_data %>%
    summarize(test_statistic = wilcox.test(`Average Bidding`, `Maximum Bidding`)$statistic,
              p_value = wilcox.test(`Average Bidding`, `Maximum Bidding`)$p.value)
  
  return(wilcox_results)
}


# Run each Wilcoxon test and create a dataframe with results
CTR_R_results <- wilcox_test_CTR_R(conversion_rate_per_var) %>% mutate(variable = "CTR_R")
PR_R_results <- wilcox_test_PR_R(conversion_rate_per_var) %>% mutate(variable = "PR_R")
PR_ATC_results <- wilcox_test_PR_ATC(conversion_rate_per_var) %>% mutate(variable = "PR_ATC")

# Combine the results into a single dataframe
all_wilcox_results_conversions <- bind_rows(CTR_R_results,
                                            PR_R_results,
                                            PR_ATC_results)

# Reorder columns to put 'variable' as the first column
all_wilcox_results_conversions <- all_wilcox_results_conversions %>%
  select(variable, test_statistic, p_value) %>%
  mutate(significance = ifelse(p_value < 0.05, "Significant", "Not Significant"))


# Display the dataframe
all_wilcox_results_conversions

# Text Output
# Split the data frame based on significance
wilcox_conversions_split_results <- split(all_wilcox_results_conversions, all_wilcox_results_conversions$significance)

# Extract variable names
significantly_different <- paste(wilcox_conversions_split_results[["Significant"]]$variable, collapse = ", ")
not_significantly_different <- paste(wilcox_conversions_split_results[["Not Significant"]]$variable, collapse = ", ")

# Display the text output
cat("Significantly different variables:", significantly_different, "\n")
cat("Not significantly different variables:", not_significantly_different, "\n")

# ------------------> Plotting Daily Values


plot_daily_values_2 <- function(data, selected_var, colors = c("#ADD8E6", "#000080")) {
  p <- plot_ly()
  
  for (i in 1:length(unique(data$group))) {
    group <- unique(data$group)[i]
    group_data <- data %>% filter(group == !!group)
    mean_value <- mean(group_data[[selected_var]], na.rm = TRUE)
    
    # Define a mapping of selected variables to titles
    title_mapping <- list(
      CTR_R = "Daily Click Through Rate",
      PR_R = "Daily Purchase Rate",
      PR_ATC = "Daily Cart Completion Rate",
      Reach = "Daily Reach",
      `Website Clicks` = "Daily Website Clicks",
      Purchase = "Daily Purchases"
    )
    
    # Look up the title based on the selected variable
    title <- title_mapping[[selected_var]]
    
    p <- p %>%
      add_trace(data = group_data, x = ~Date, y = as.formula(paste0("~", selected_var)), type = "scatter", mode = "lines+markers",
                name = paste(group, selected_var), line = list(color = colors[i], width = 2),
                marker = list(color = colors[i]),
                text = ~paste(group,
                              "<br>", Date,
                              "<br>", paste0(sprintf("%.2f%%", 100 * get(selected_var)))),
                hoverinfo = "text") %>%
      add_trace(x = group_data$Date, y = rep(mean_value, length(group_data$Date)), type = "scatter", mode = "lines",
                name = paste("Mean", group, selected_var), line = list(color = colors[i], width = 1, dash = "dash"),
                text = ~paste("Group:", group,
                              "<br>Mean Line"),
                hoverinfo = "text")
  }
  
  p <- p %>%
    layout(title = title,
           xaxis = list(title = ""),
           yaxis = list(title = "", tickformat = ".2%"),
           showlegend = FALSE)
  
  return(p)
}

selected_var <- "PR_R"
plot_daily_values_2(conversion_rate_per_var, selected_var)
conversion_rate_per_var
CTR_R
PR_R
conversion_rate_per_var

