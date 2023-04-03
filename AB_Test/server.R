
library(shiny)
library(ggplot2)
library(plotly)
source("cost_metrics.R")
source("key_metrics.R")
source("conversion_metrics.R")

# Define server
server <- function(input, output) {
  
  # ------------ CONVERSION METRICS
  
  # Daily Conversion Metrics Plot
  output$daily_conversion_metrics_plot <- renderPlotly({
    if (length(input$conversion_metric) == 0) {
      return(NULL)
    }
    selected_var <- input$conversion_metric
    plot_conversion_metrics_daily(conversion_rate_per_var, selected_var)
  })
  
  # Cumulative Conversion Metrics Plot
  output$cumulative_conversion_metrics <- renderPlot({
    if (length(input$conversion_metric) == 0) {
      return(NULL)
    }
    daily_kpis <- input$conversion_metric
    plot_cumulative_conversion_metrics(combined_data, daily_kpis)
  })
  
  # Conversion Metrics DataFrame
  output$conversion_metrics_table <- renderTable({
    conversion_rate_dataframe
  }, rownames = FALSE)
  
  # ------------ KEY METRICS
  
  # Daily Key Metrics Plot
  output$daily_key_metrics_plot <- renderPlotly({
    if (length(input$raw_metric) == 0) {
      return(NULL)
    }
    include_daily_vars <- input$raw_metric
    plot_key_metrics_daily(combined_data, include_daily_vars)
  })
  
  # Cumulative Key Metrics Plot
  output$cumulative_key_metrics_plot <- renderPlot({
    if (length(input$raw_metric) == 0) {
      return(NULL)
    }
    include_daily_vars <- input$raw_metric
    plot_cumulative_key_metrics(combined_data, include_daily_vars)
  })
  
  # Key Metrics DataFrame
  output$key_metrics_table <- renderTable({
    key_metrics
  }, rownames = FALSE)
  
  # ------------ COST METRICS
  
  # Daily Cost Metrics Plot
  output$daily_cost_metrics_plot <- renderPlotly({
    if (length(input$cost_metric) == 0) {
      return(NULL)
    }
    selected_cost_metric <- input$cost_metric
    daily_cost_metrics_plot(daily_cost_metrics, selected_cost_metric)
  })
  
  # Cumulative Cost Metrics Plot
  output$cumulative_cost_plot <- renderPlot({
    if (length(input$cost_metric) == 0) {
      return(NULL)
    }
    selected_cost_metric <- input$cost_metric
    plot_cumulative_cost_metrics(cumulative_cost_metrics,selected_cost_metric)
  })
  
  # Cost Metrics DataFrame
  output$cost_metrics_table <- renderTable({
    wide_total_cost_metrics
  }, rownames = FALSE)
  
  
}

shiny::shinyApp(ui = ui, server = server)

