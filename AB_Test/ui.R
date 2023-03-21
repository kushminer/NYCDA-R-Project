

library(shiny)
library(shinydashboard)



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "A/B Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      checkboxGroupInput("variables", "Count per Funnel Step", 
                         choices = c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Spend (USD)", "Purchase")),
      checkboxGroupInput("costs", "Cost per Funnel Step", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Website Clicks", "Purchase")),
      checkboxGroupInput("conversions", "Conversion Rate", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Impressions", "Website Clicks","Purchase")),
      checkboxGroupInput("retention_rate", "Retention Rate", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Impressions", "Website Clicks","Purchase")),
      checkboxGroupInput("purchase_rate", "Purchase Rate", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart"),
                         selected = c("Reach", "Website Clicks","View Content","Add to Cart"))
    )
  ),
  dashboardBody(
    h4(paste0("Groups")),
    fluidRow(
      box(title = "Control | Average Bidding", width = 4, background = "light-blue"),
      box(title = "Test | Maximum Bidding", width = 4, background = "navy"),
    ),
    h3(paste0("Totals")),
    fluidRow(
      box(title = "View",
          width = 6,
          plotOutput("totals_plot")),
      box(title = "Analysis",
          width = 6,
          plotOutput("totals_analysis_plot"))
    ),
    h3(paste0("Costs")),
    fluidRow(
      box(title = "View",
          width = 6,
          plotOutput("costs_plot")),
      box(title = "Analysis",
          width = 6,
          plotOutput("cost_analysis_plot"))
    ),
    h3(paste0("Conversion")),
    fluidRow(
      box(title = "View",
          width = 6,
          plotOutput("conversions_plot")),
      box(title = "Analysis",
          width = 6,
          plotOutput("conversion_analysis_plot"))
    ),
    h3(paste0("Retention Rate")),
    fluidRow(
      box(title = "View",
          width = 6,
          plotOutput("retention_rate_plot")),
      box(title = "Analysis",
          width = 6,
          plotOutput("retention_rate_analysis_plot"))
    ),
    h3(paste0("Purchase Rate")),
    fluidRow(
      box(title = "View",
          width = 6,
          plotOutput("purchase_rate_plot")),
      box(title = "Analysis",
          width = 6,
          plotOutput("purchase_rate_analysis_plot"))
    )
  )
)
