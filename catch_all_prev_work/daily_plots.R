
source('data_cleaning.R')

# ----------------------- Plotting Functions

format_value <- function(value, variable) {
  if (variable == "Spend (USD)") {
    return(paste0("$", format(round(value), big.mark = ",")))
  } else if (value >= 1e6) {
    return(paste0(round(value / 1e6, 2), "m"))
  } else if (value >= 1e3) {
    return(paste0(round(value / 1e3, 2), "k"))
  } else {
    return(value)
  }
}

# ---------------------- Key Metrics Daily

plot_daily_values <- function(data, include_daily_vars, colors = c("#ADD8E6", "#000080")) {
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
include_daily_vars <- c("Reach")
plot_daily_values(combined_data, include_daily_vars)


# -------------------------------------- Cumulative Key Metrics


plot_group_sums <- function(data, include_daily_vars, colors = c("#ADD8E6", "#000080")) {
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


plot_group_sums(combined_data,include_daily_vars)

# -------------------------------------- KPI'S

# DataFrame

# Filter spend, website clicks, and impressions data
spend_data <- combined_data[combined_data$variable == "Spend (USD)", ]
clicks_data <- combined_data[combined_data$variable == "Website Clicks", ]
impressions_data <- combined_data[combined_data$variable == "Impressions", ]

# Merge spend and website clicks data
spend_clicks <- merge(spend_data, clicks_data, by = c("Date", "group", "Weekday"))
impressions_clicks <- merge(impressions_data, clicks_data, by = c("Date", "group", "Weekday"))

# Calculate CPC and CTR
spend_clicks$cpc <- spend_clicks$value.x / spend_clicks$value.y
impressions_clicks$CTR <- (impressions_clicks$value.y / impressions_clicks$value.x)

# Create the KPI dataframe
KPI <- data.frame(
  Date = spend_clicks$Date,
  Weekday = spend_clicks$Weekday,
  group = spend_clicks$group,
  CPC = spend_clicks$cpc,
  CTR = impressions_clicks$CTR
)

# Plot Daily KPI value

plot_daily_kpi_values <- function(data, daily_kpis, colors = c("#ADD8E6", "#000080")) {
  
  p <- plot_ly()
  
  for (i in 1:length(unique(data$group))) {
    group <- unique(data$group)[i]
    group_data <- data %>% filter(group == !!group)
    mean_value <- mean(group_data[[daily_kpis]], na.rm = TRUE)
    
    if (daily_kpis == "CTR") {
      y_axis_title <- ""
      y_tick_format <- ".2%"
    } else if (daily_kpis == "CPC") {
      y_axis_title <- ""
      y_tick_format <- "$.2f"
    }
    
    p <- p %>%
      add_trace(data = group_data, x = ~Date, y = as.formula(paste0("~", daily_kpis)), type = "scatter", mode = "lines+markers",
                name = paste(group, daily_kpis), line = list(color = colors[i], width = 2),
                marker = list(color = colors[i], size = 8),
                text = ~paste(group,
                              "<br>", Date,
                              "<br>", paste0(daily_kpis, ": ", round(.data[[daily_kpis]], 2))),
                hoverinfo = "text") %>%
      add_trace(x = group_data$Date, y = rep(mean_value, length(group_data$Date)), type = "scatter", mode = "lines",
                name = paste("Mean", group, daily_kpis), line = list(color = colors[i], width = 1, dash = "dash"),
                text = ~paste("Group:", group,
                              "<br>Mean Line"),
                hoverinfo = "text")
  }
  
  p <- p %>%
    layout(title = "",
           xaxis = list(title = ""),
           yaxis = list(title = y_axis_title, tickformat = y_tick_format),
           showlegend = FALSE)
  
  return(p)
}



plot_daily_kpi_values(KPI, "CPC")
plot_daily_kpi_values(KPI, "CTR")

KPI

# Plot Total KPI Value

plot_kpi_sums <- function(data, daily_kpis, colors = c("#ADD8E6", "#000080")) {
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
plot_kpi_sums(combined_data, "PR_ATC")
plot_kpi_sums(combined_data, "CPC")







