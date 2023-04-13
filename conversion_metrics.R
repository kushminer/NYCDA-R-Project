

source("data_cleaning.R")

###########################################################################

# Conversion Metrics #

###########################################################################

# 
conversion_rate_per_var <- combined_data %>%
  spread(variable, value) %>%
  mutate(CTR_R  = `Website Clicks` / Impressions,
         PR_R   =  Purchase        / Impressions,
         PR_ATC =  Purchase        / `Add to Cart`
  ) %>%
  select(Date,group,Reach,`Website Clicks`,`Purchase`,CTR_R,PR_R,PR_ATC)

# conversion rate derived from total
cumulative_conversion_rate <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  spread(variable, sum) %>%
  mutate(CTR_R  = (`Website Clicks` / Impressions),
         PR_R   =  (Purchase        / Impressions),
         PR_ATC =  (Purchase        / `Add to Cart`)) %>%
  select(group, CTR_R,PR_R,PR_ATC)

# conversion rate daily mean (unused in shiny app)
combined_data %>%
  spread(variable, value) %>%
  mutate(CTR_R  = `Website Clicks` / Reach,
         PR_R   =  Purchase        / Reach,
         PR_C   =  Purchase        / `Website Clicks`,
         PR_ATC =  Purchase        / `Add to Cart`) %>%
  select(group, CTR_R,PR_R,PR_C,PR_ATC) %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), mean)) %>%
  arrange(group)


###########################################################################

# CONVERSION METRICS DAILY PLOT #

###########################################################################

plot_conversion_metrics_daily <- function(data, selected_var, colors = c("#ADD8E6", "#000080")) {
  p <- plot_ly()
  
  for (i in 1:length(unique(data$group))) {
    group <- unique(data$group)[i]
    group_data <- data %>% filter(group == !!group)
    mean_value <- mean(group_data[[selected_var]], na.rm = TRUE)
    
    # Define a mapping of selected variables to titles
    title_mapping <- list(
      CTR_R = "Daily Clickthrough Rate",
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
plot_conversion_metrics_daily(conversion_rate_per_var, selected_var)

###########################################################################

# CONVERSION METRICS CUMULATIVE PLOT #

###########################################################################

plot_cumulative_conversion_metrics <- function(data, daily_kpis, colors = c("#ADD8E6", "#000080")) {
  kpi_title <- ""
  
  if (daily_kpis == "CTR_R") {
    group_sums <- data %>%
      filter(variable %in% c("Website Clicks", "Impressions")) %>%
      group_by(group) %>%
      summarise(sum = round(sum(value[variable == "Website Clicks"]) / sum(value[variable == "Impressions"]), 4) * 100)
    kpi_title <- "Clickthrough Rate"
  } else if (daily_kpis == "PR_R") {
    group_sums <- data %>%
      filter(variable %in% c("Purchase", "Impressions")) %>%
      group_by(group) %>%
      summarise(sum = round(sum(value[variable == "Purchase"]) / sum(value[variable == "Impressions"]), 4) * 100)
    kpi_title <- "Purchase Rate"
  } else if (daily_kpis == "PR_ATC") {
    group_sums <- data %>%
      filter(variable %in% c("Purchase", "Add to Cart")) %>%
      group_by(group) %>%
      summarise(sum = round(sum(value[variable == "Purchase"]) / sum(value[variable == "Add to Cart"]), 4) * 100)
    kpi_title <- "Cart Completion Rate"
  } else {
    stop("daily_kpis should be either 'CTR Total' or 'CPC Total'")
  }
  
  p <- ggplot(group_sums, aes(x = group, y = sum, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colors, guide = FALSE) +
    geom_text(aes(label = sprintf("%.2f%%", sum)), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
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
    labs(title = paste0("Cumulative ",kpi_title), fill = "") 
  
  return(p)
}
plot_cumulative_conversion_metrics(combined_data, "PR_ATC")



###########################################################################

# WILCOX RANK SUM TEST #

###########################################################################

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


###########################################################################

# CONVERSION METRICS WIN PROBABILITY #

###########################################################################

calculate_average_win_probability <- function(variable_pairs) {
  results <- data.frame()
  
  for (pair in variable_pairs) {
    daily_data <- combined_data %>%
      filter(variable %in% c(pair[1], pair[2]))
    
    daily_cm <- daily_data %>%
      group_by(Date, group, variable) %>%
      summarize(sum = sum(value), .groups = 'drop') %>%
      spread(key = variable, value = sum) %>%
      mutate(CM = !!sym(pair[1]) / !!sym(pair[2]))
    
    win_probability_daily <- daily_cm %>%
      group_by(Date) %>%
      do({
        group_A <- filter(., group == "Average Bidding")
        group_B <- filter(., group == "Maximum Bidding")
        
        successes_A <- group_A %>% pull(!!sym(pair[1]))
        trials_A <- group_A %>% pull(!!sym(pair[2]))
        successes_B <- group_B %>% pull(!!sym(pair[1]))
        trials_B <- group_B %>% pull(!!sym(pair[2]))
        
        n_samples <- 10000
        
        if (all(!is.na(successes_A) & !is.na(trials_A) & !is.na(successes_B) & !is.na(trials_B))) {
          samples_A <- rbeta(n_samples, shape1 = successes_A + 1, shape2 = trials_A - successes_A + 1)
          samples_B <- rbeta(n_samples, shape1 = successes_B + 1, shape2 = trials_B - successes_B + 1)
          
          win_probability_A <- mean(samples_A > samples_B)
        } else {
          win_probability_A <- NA
        }
        
        data.frame(Date = unique(group_A$Date), CM = paste(pair[1], pair[2], sep = "_"), win_prob_average_bidding = win_probability_A)
      })
    
    average_win_probability_A <- mean(win_probability_daily$win_prob_average_bidding, na.rm = TRUE)
    results <- rbind(results, data.frame(CM = paste(pair[1], pair[2], sep = "_"), win_prob_average_bidding = average_win_probability_A))
  }
  
  return(results)
}


variable_pairs <- list(
  c("Website Clicks", "Impressions"),
  c("Purchase", "Impressions"),
  c("Purchase", "Add to Cart"))

conversion_win_prob <- calculate_average_win_probability(variable_pairs)

conversion_win_prob

###########################################################################

# CONVERSION METRICS DATAFRAME #

###########################################################################

# Reshape cumulative_conversion_rate to long format
long_cumulative_conversion_rate <- cumulative_conversion_rate %>%
  ungroup() %>%
  pivot_longer(cols = -group, names_to = "variable", values_to = "value")

# Join all_wilcox_results_conversions with long_cumulative_conversion_rate
conversion_rate_dataframe <- all_wilcox_results_conversions %>%
  left_join(long_cumulative_conversion_rate, by = "variable") %>%
  pivot_wider(names_from = group, values_from = value)


# Update row names in conversion_win_prob
conversion_win_prob$variable <- c("CTR_R", "PR_R", "PR_ATC")

# Join conversion_rate_dataframe with conversion_win_prob
conversion_rate_dataframe <- conversion_rate_dataframe %>%
  left_join(conversion_win_prob, by = "variable")

# Change row names
conversion_rate_dataframe$variable <- recode(conversion_rate_dataframe$variable,
                                             "CTR_R" = "Clickthrough Rate",
                                             "PR_R" = "Purchase Rate",
                                             "PR_ATC" = "Cart Completion Rate")


conversion_rate_dataframe <- conversion_rate_dataframe %>%
  mutate(Lift = (`Maximum Bidding` - `Average Bidding`) / `Maximum Bidding`*-1)

conversion_rate_dataframe <- conversion_rate_dataframe %>%
  select (variable, `Average Bidding`, `Maximum Bidding`,Lift, p_value,win_prob_average_bidding)

conversion_rate_dataframe <- conversion_rate_dataframe %>%
  rename("Conversion Metrics" = variable,
         "Average Bidding" = `Average Bidding`,
         "Maximum Bidding" = `Maximum Bidding`,
         "Lift" = Lift,
         "p-value" = p_value,
         "Win Probability (Average Bidding)" = win_prob_average_bidding)

conversion_rate_dataframe <- conversion_rate_dataframe %>%
  mutate(`Average Bidding` = scales::percent(`Average Bidding`, accuracy = 0.01),
         `Maximum Bidding` = scales::percent(`Maximum Bidding`, accuracy = 0.01),
         Lift = scales::percent(Lift, accuracy = 0.01),
         `p-value` = formatC(`p-value`, format = "f", digits = 5),
         `Win Probability (Average Bidding)` = scales::percent(`Win Probability (Average Bidding)`, accuracy = 0.01))





