
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
    plot_sum_by_variable(monthly_data, include_vars)
  })
  
  # Analysis
  output$totals_analysis_plot <- renderPlot({
    if (length(input$variables) == 0) {
      return(NULL)
    }
    include_vars <- input$variables
    plot_winners(include_vars)
  })
  
  # ------------ Cost Plots
  
  output$costs_plot <- renderPlot({
    if (length(input$costs) == 0) {
      return(NULL)
    }
    include_cost_vars <- input$costs
    plot_cost_by_variable(cost_data, include_cost_vars)
  })
  

  # ------------- Funnel Plots
  
  output$funnel_selector <- renderUI({
    selectInput("funnel_plot", "Select Funnel Type", choices = c("Count", "Retention", "Cost", "Impressions"))
  })
  
  output$retention_funnel_plot <- renderPlot({
    if ("Retention" %in% input$funnel_type) {
      retention_funnel_plot
    } else {
      return(NULL)
    }
  })
  
  output$impressions_funnel_plot <- renderPlot({
    if ("Impressions" %in% input$funnel_type) {
      impressions_funnel_plot
    } else {
      return(NULL)
    }
  })
  
  output$cost_funnel_plot <- renderPlot({
    if ("Cost" %in% input$funnel_type) {
      cost_funnel_plot
    } else {
      return(NULL)
    }
  })
  
  output$count_funnel_plot <- renderPlot({
    if ("Count" %in% input$funnel_type) {
      count_funnel_plot
    } else {
      return(NULL)
    }
  })
  
}

shiny::shinyApp(ui = ui, server = server)

