
library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  # Header
  dashboardHeader(title = "A/B Test Dashboard"),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem('Google Ads: Bidding Strategy',
                  menuSubItem("Test Introduction", tabName = "test_introduction", icon = icon("info-circle")),
                  menuSubItem("Conversion Metrics", tabName = "conversion_metrics", icon = icon("percent")),
                  menuSubItem("Key Metrics", tabName = "key_metrics", icon = icon("key")),
                  menuSubItem("Cost Metrics", tabName = "cost_metrics", icon = icon("dollar"))
                        )
               )
  ),
  # Body
  dashboardBody(
    # Independent Scrolling
    tags$style(type = "text/css", "
                .content-wrapper {
                    overflow-y: scroll;
                    height: calc(100vh - 80px);
                }
                .sidebar {
                    overflow-y: scroll;
                    height: calc(100vh - 80px);
                }
            "),
    tags$head(
      # Text and Box alignment
      tags$style(HTML("
                      h1 {
                        text-align: center;
                      }
                      h3 {
                        font-size: 24px;
                        text-align: center;
                      }
                      .box {
                        display: flex;
                        flex-direction: column;
                        justify-content: center;
                        text-align: center;
                      }
                    "
                      )
                 )
            ),
    tabItems(
      # Test Introductions Tab
      tabItem(tabName = "test_introduction",
              # Header
              fluidRow(
                box(title = "Google Ads: Bidding Strategy", width = 12, background = "navy")
              ),
              # Test Introduction
              h1("Test Introduction"),
              p("A company recently introduced a new bidding type, “average bidding”, as an alternative to its existing bidding type, called “maximum bidding”."),
              p("One of our clients, ….com, has decided to test this new feature."),
              p("To test, the client would like to to conduct an A/B test to understand if average bidding brings more conversions than maximum bidding."),
              p("It's worth noting that the context of dataset is limited, still, we will do our best to draw the most meaningful insights possible"),
              h4("Link to Test Data"),
              a(href = "https://www.kaggle.com/datasets/ilkeryildiz/example-dataset-for-ab-test?resource=download", "Kaggle"),
              # Average Bidding Description
              h3("Average Bidding"),
              p("Also known as: Maximum Clicks or Automated Bid Strategy"),
              p("Description: Set an average daily budget, and the Google Ads system automatically manages your bids to bring the most clicks possible within your budget."),
              p("Example: You have a website that sells a variety of art supplies, and your main goal is to bring more customers to your site. You have a set amount that you want to spend on advertising each month, and there isn't a particular product you want to emphasize most. Maximize Clicks lets you decide the overall amount of your budget, then we'll find you the most customers based on that."),
              # Maximum Bidding Description
              h3("Maximum Bidding"),
              p("Also known as: Manual CPC bidding"),
              p("Description: Manage your maximum cost per click. You can set different bids for each ad group in your campaign, or for individual keywords or placements. If you’ve found certain keywords or placements are more profitable, you can use manual bidding to allocate more of your ad budget to those keywords or placements."),
              p("Example: Although your website sells a wide range of art supplies, you're most interested in selling paint brushes. With Manual CPC bidding, even if your ad group has 15 keywords, you can choose to set a higher bid for only the keyword \"paint brushes,\" which will apply whenever that keyword triggers your ad."),
              h4("Link to Bidding Strategy Info"),
              a(href = "https://support.google.com/google-ads/answer/2472725?hl=en", "Google"),
              # Data Housekeeping
              h2("Data Housekeeping", align = "center"),
              h4("NaN Values"),
              p("During the analysis, we found that Average bidding was missing values for 8/5/2019. To balance the data, we decided to remove the same data from Maximum bidding."),
              p("We considered other options, such as imputing the missing data or using a different time period for the analysis, but decided that removing the data was the most balanced option."),
              p("Removing a line of data from 30 rows may seem drastic, but it ensures that the comparison between the two bidding strategies is based on the same time period and does not introduce bias into the analysis."),
              p("We acknowledge that this decision may have some impact on the accuracy of the results, but we believe that it was the most reasonable choice given the circumstances."),
      ),
      # Conversion Metrics Tab
      tabItem(tabName = "conversion_metrics",
              # Tab Header
              fluidRow(
                box(title = "Google Ads: Bidding Strategy", width = 12, background = "navy"),
                box(width = 6, style = "background-color: #ADD8E6; border: none;", "Average Bidding"),
                box(width = 6, style = "background-color: #000080; border: none; color: white;", "Maximum Bidding")
              ),
              # First Row
              h1("Conversion Metrics",align = "center"),
              fluidRow(
                # Drop Down Selection
                column(width = 2, selectInput("conversion_metric", "Conversion Metric",
                                              choices = c("Clickthrough Rate" = "CTR_R", "Purchase Rate" = "PR_R", "Cart Completion Rate" = "PR_ATC"),
                                              selected = "CTR_R")),
                # Daily Conversion Metrics Plotted
                column(width = 7, offset = 0, box(plotlyOutput("daily_conversion_metrics_plot"), width = "100%")),
                # Cumulative Conversion Metrics Plotted 
                column(width = 3, box(plotOutput("cumulative_conversion_metrics"),width = "100%"))
              ),
              # Second Row
              fluidRow(
                # Conversion Metrics Table
                column(width = 2),
                column(width = 7, box(tableOutput("conversion_metrics_table"), width = "100%")),
                column(width = 3)
              ),
              # Third Row
              # Conversion Metrics Dictionary
              h3("Conversion Metrics Dictionary"),
              p("Clickthrough Rate: Website Clicks /Impressions"),
              p("Purchase Rate: Purchases / Impressions"),
              p("Cart Completion Rate: Purchases / add to carts."),
              p("Lift: Percentage that Average Bidding is less or more than Maximum Bidding"),
              p("p-value: Determined using Wilcox Rank Sum Test, indicates if difference between Average and Maximum Bidding is Significant. Uses Daily Values for Analysis"),
              p("Win Probability: The probability at which average bidding will win in the future. Uses Monte Carlo Simulation")
      ),
      # Key Metrics
      tabItem(tabName = "key_metrics",
              # Header
              fluidRow(
                box(title = "Google Ads: Bidding Strategy", width = 12, background = "navy"),
                box(width = 6, style = "background-color: #ADD8E6; border: none;", "Average Bidding"),
                box(width = 6, style = "background-color: #000080; border: none; color: white;", "Maximum Bidding")
              ),
              # First Row
              fluidRow(
                h1("Key Metrics",align = "center"),
                # Drop Down Selection
                column(width = 2, selectInput("raw_metric", "Key Metric",
                                              choices = c("Impressions","Website Clicks","Purchase"),
                                              selected = c("Impressions"))),
                # Daily Key Metrics Plotted
                column(width = 7, offset = 0, box(plotlyOutput("daily_key_metrics_plot"), width = "100%")),
                # Cumulative Key Metrics Plotted
                column(width = 3, box(plotOutput("cumulative_key_metrics_plot"),width = "100%"))
              ),
              # Second Row
              fluidRow(
                column(width = 2),
                # Key Metrics Table
                column(width = 7, box(tableOutput("key_metrics_table"), width = "100%")),
                column(width = 3)
              ),
              # Third Row
              # Key Metrics Dictionary
              h3("Dictionary"),
              p("Impressions: Number of Ad Views"),
              p("Website Clicks: Number of Clicks on the Ad"),
              p("Purchase: Number of Purchases"),
              p("Lift: Percentage that Average Bidding is less or more than Maximum Bidding"),
              p("p-value: Determined using Wilcox Rank Sum Test, indicates if difference between Average and Maximum Bidding is Significant. Uses Daily Values for Analysis"),
      ),
      # Cost Metrics
      tabItem(tabName = "cost_metrics",
              fluidRow(
                # Header
                box(title = "Google Ads: Bidding Strategy", width = 12, background = "navy"),
                box(width = 6, style = "background-color: #ADD8E6; border: none;", "Average Bidding"),
                box(width = 6, style = "background-color: #000080; border: none; color: white;", "Maximum Bidding")
              ),
              # First Row
              fluidRow(
                h1("Cost Metrics",align = "center"),
                # Drop Down Selection
                column(width = 2, selectInput("cost_metric", "Cost Metric",
                                              choices = c("Spend",
                                                          "Cost per Purchase" = "CPP",
                                                          "Cost per Impression" = "CPI", 
                                                          "Cost per Click" = "CPC"),
                                              selected = c("Spend"))),
                # Daily Cost Metrics Plotted
                column(width = 7, offset = 0, box(plotlyOutput("daily_cost_metrics_plot"), width = "100%")),
                # Cumulative Cost Metrics Plotted
                column(width = 3, box(plotOutput("cumulative_cost_plot"),width = "100%"))
              ),
              # Second Row
              fluidRow(
                # Cost Metrics Table
                column(width = 12, box(tableOutput("cost_metrics_table"), width = "100%"))
              ),
              # Third Row
              # Cost Metrics Dictionary
              h3("Dictionary"),
              p("Spend: Dollars spent"),
              p("Cost per Purchase: Amount Spent / Number of Purchases"),
              p("Cost per Click: Amount Spent / Number of Website Clicks"),
              p("Cost per Impression: Amount Spent / Number of Impressions"),
              p("Lift: Percentage that Average Bidding is less or more than Maximum Bidding"),
              p("p-value: Determined using Wilcox Rank Sum Test, indicates if difference between Average and Maximum Bidding is Significant. Uses Daily Values for Analysis"),
              p("Win Probability: The probability at which average bidding will win in the future. Uses Monte Carlo Simulation")
      )
    )
  )
)

