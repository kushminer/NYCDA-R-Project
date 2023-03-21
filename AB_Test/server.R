
library(shiny)
library(ggplot2)
source("visualizations.R")

# Define server
server <- function(input, output) {
  
  # ------------ Total
  
  # Plots
  output$totals_plot <- renderPlot({
    if (length(input$variables) == 0) {
      return(NULL)
    }
    include_vars <- input$variables
    plot_sum_by_variable(include_vars)
  })
  
  # Analysis
  output$totals_analysis_plot <- renderPlot({
    if (length(input$variables) == 0) {
      return(NULL)
    }
    include_vars <- input$variables
    plot_winners(include_vars)
  })
  
  # ------------ Cost
  
  # Plots
  output$costs_plot <- renderPlot({
    if (length(input$costs) == 0) {
      return(NULL)
    }
    include_cost_vars <- input$costs
    plot_cost_by_variable(include_cost_vars)
  })
  
  # Analysis
  output$cost_analysis_plot <- renderPlot({
    if (length(input$costs) == 0) {
      return(NULL)
    }
    include_cost_vars <- input$costs
    plot_cost_winners(include_cost_vars)
  })
  
  # ------------ Conversion 
  
  # Plots
  output$conversions_plot <- renderPlot({
    if (length(input$conversions) == 0) {
      return(NULL)
    }
    include_conversion_vars <- input$conversions
    plot_conversion_funnel(include_conversion_vars)
  })
  
  # Analysis
  output$conversion_analysis_plot <- renderPlot({
    if (length(input$conversions) == 0) {
      return(NULL)
    }
    include_conversion_vars <- input$conversions
    plot_conversion_winners(include_conversion_vars)
  })
  
  # ------------ Retention 
  
  # Plots
  output$retention_rate_plot <- renderPlot({
    if (length(input$retention_rate) == 0) {
      return(NULL)
    }
    include_retention_vars <- input$retention_rate
    plot_retention_funnel(include_retention_vars)
  })
  
  # Analysis
  output$retention_rate_analysis_plot <- renderPlot({
    if (length(input$retention_rate) == 0) {
      return(NULL)
    }
    include_retention_vars <- input$retention_rate
    plot_retention_winners(include_retention_vars)
  })
  
  # ------------ Purchase 
  
  # Plots
  output$purchase_rate_plot <- renderPlot({
    if (length(input$purchase_rate) == 0) {
      return(NULL)
    }
    include_purchase_vars <- input$purchase_rate
    plot_purchase_funnel(include_purchase_vars)
  })
  
  # Analysis
  output$purchase_rate_analysis_plot <- renderPlot({
   if (length(input$purchase_rate) == 0) {
    return(NULL)
  }
  include_retention_vars <- input$purchase_rate
  plot_purchase_winners(include_purchase_vars)
  })
  
    
}

shiny::shinyApp(ui = ui, server = server)

