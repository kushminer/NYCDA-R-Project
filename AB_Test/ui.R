

library(shiny)
library(shinydashboard)



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "A/B Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      checkboxGroupInput("variables", "Variables", 
                         choices = c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Spend (USD)", "Purchase")),
      checkboxGroupInput("costs", "Costs", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Impressions", "Purchase")),
      checkboxGroupInput("funnel_type",
                         "Funnel:",
                         choices = c("Retention", "Impressions", "Cost", "Count"),
                         selected = c("Cost"))
    )
  ),
  dashboardBody(
    h4(paste0("Groups")),
    fluidRow(
      box(title = "Control | Average Bidding", width = 4, background = "light-blue"),
      box(title = "Test | Maximum Bidding", width = 4, background = "navy"),
    ),
    h4(paste0("Totals")),
    fluidRow(
      box(title = "View",
          width = 6,
          plotOutput("totals_plot")),
      box(title = "Analysis",
          width = 6,
          plotOutput("totals_analysis_plot"))
    ),
    h4(paste0("Costs")),
    fluidRow(
      box(width = 6,
          plotOutput("costs_plot"))
    ),
    h4(paste0("Funnels")),
    fluidRow(
      box(width = 6,
          conditionalPanel(condition = "input.funnel_type.indexOf('Retention') > -1", plotOutput("retention_funnel_plot")),
          conditionalPanel(condition = "input.funnel_type.indexOf('Impressions') > -1", plotOutput("impressions_funnel_plot")),
          conditionalPanel(condition = "input.funnel_type.indexOf('Cost') > -1", plotOutput("cost_funnel_plot")),
          conditionalPanel(condition = "input.funnel_type.indexOf('Count') > -1", plotOutput("count_funnel_plot"))
      )
    )
  )
)
