
# ----------------------------- > Unused Funnel Plots:

# ----------------------------- Retention Rate

# Retention Funnel
plot_retention_funnel <- function(include_retention_vars) {
  
  retention_funnel_df <-
    combined_data %>%
    filter(variable != 'Spend (USD)',variable %in% include_retention_vars) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum)) %>%
    mutate(retention = (sum / lag(sum)))
  
  # Create a vector of levels in the desired order
  level_order <- rev(c("Impressions", "Reach", 
                       "Website Clicks", "Searches", "View Content", 
                       "Add to Cart", "Purchase"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  retention_funnel_df$variable <- factor(retention_funnel_df$variable, levels = level_order, ordered = TRUE)
  
  # Plot retential funnel
  retention_funnel_plot <- retention_funnel_df %>%
    ggplot(aes(x = variable, y = retention, fill = group)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", 
         y = "", 
         title = "") +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    coord_flip() +
    geom_text(aes(label = scales::percent(retention)), 
              position = position_dodge(width = 1), vjust = 0.5, size = 5, hjust = -.1) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # axis.text.x = element_text(),
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text=element_text(size=12),
    ) +
    labs(fill = "") +
    expand_limits(y = c(0, max(retention_funnel_df$retention,na.rm = TRUE)*1.2))
  return(retention_funnel_plot)
}

include_retention_vars <- c("Impressions", "Reach", 
                            "Website Clicks", "Searches", "View Content", 
                            "Add to Cart", "Purchase")
plot_retention_funnel(include_retention_vars)

# Winners
plot_retention_winners <- function(include_retention_vars) {
  
  # Subset the data to only include the specified variables
  retention_winners_df <- 
    combined_data %>%
    filter(variable %in% include_retention_vars) %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value)) %>%
    arrange(desc(sum))  %>%
    mutate(retention_rate = sum / lag(sum))
  
  # Reshape the data frame into a wide format
  retention_winners_wide <- retention_winners_df %>%
    select(-sum) %>%
    spread(key = group, value = retention_rate)
  
  # Calculate the winner column
  retention_winners_wide$winner <- ifelse(retention_winners_wide$`Average Bidding` > retention_winners_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  retention_winners_wide <- retention_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  retention_winners_wide$statistically_significant <- ifelse(retention_winners_wide$win_percentage > threshold, TRUE, FALSE)
  
  # Create a new dataframe for the plot
  retention_winners_plot <- retention_winners_wide %>%
    select(variable, winner, win_percentage, statistically_significant)
  
  # Define the order
  retention_level_order <- rev(c("Impressions", "Reach", 
                                 "Website Clicks", "Searches", "View Content", 
                                 "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  retention_winners_plot$variable <- fct_relevel(retention_winners_plot$variable, retention_level_order)
  
  # Create the bar chart
  retention_winners_plot <- ggplot(retention_winners_plot, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 1), "%")), vjust = .25, size = 5, hjust = -.1) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # axis.text.x = element_text(),
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text=element_text(size=12),
    ) +
    coord_flip() +
    expand_limits(y = c(0, max(retention_winners_plot$win_percentage, na.rm = TRUE) * 1.2))
  
  return(retention_winners_plot)
}

include_retention_vars <- c("Impressions", "Reach", 
                            "Website Clicks", "Searches", "View Content", 
                            "Add to Cart", "Purchase")
plot_retention_winners(include_retention_vars)

# ----------------------- Purchase Rate 

# Purchase Rate Funnel
plot_purchase_funnel <- function(include_purchase_vars) {
  
  # Purchase Funnel DF
  purchase_funnel_df <- 
    combined_data %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value), .groups = "drop") %>%
    arrange(desc(sum)) %>%
    mutate(purchase_rate = ifelse(variable != "Purchase", sum[variable == "Purchase"] / sum, NA),
           purchase_rate = (round(purchase_rate, 5))) %>%
    filter(variable %in% include_purchase_vars)
  
  # Create a vector of levels in the desired order
  purchase_level_order <- rev(c("Impressions", "Reach", 
                                "Website Clicks", "Searches", "View Content", 
                                "Add to Cart"))
  purchase_funnel_df
  # Convert the 'variable' column to an ordered factor with the specified levels
  purchase_funnel_df$variable <- factor(purchase_funnel_df$variable, levels = purchase_level_order, ordered = TRUE)
  
  # Purchase Funnel
  purchase_funnel_plot <- purchase_funnel_df %>%
    ggplot(aes(x = variable,
               y = purchase_rate, 
               fill = group)) +
    geom_bar(stat = "identity", 
             position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", 
         y = "", 
         title = "") +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    coord_flip() +
    geom_text(aes(
      label = scales::percent(purchase_rate)),
      position = position_dodge(width = 1), 
      vjust = 0.5, 
      size = 5, 
      hjust = -.1) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # axis.text.x = element_text(),
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text=element_text(size=12)
    ) +
    labs(fill = "") +
    expand_limits(y = c(0, 
                        max(purchase_funnel_df$purchase_rate, na.rm = TRUE)
                        *1.2))
  
  return(purchase_funnel_plot)
}
include_purchase_vars <- c("Impressions","Website Clicks", "Searches", "View Content","Add to Cart","Reach")
plot_purchase_funnel(include_purchase_vars)


#winners
plot_purchase_winners <- function(include_purchase_vars) {
  
  # Purchase Funnel DF
  purchase_winners_df <- 
    combined_data %>%
    group_by(group, variable) %>%
    summarise(sum = sum(value), .groups = "drop") %>%
    arrange(desc(sum)) %>%
    mutate(purchase_rate = ifelse(variable != "Purchase", sum(sum[variable == "Purchase"]) / sum, NA),
           purchase_rate = (round(purchase_rate, 5))) %>%
    filter(variable %in% include_purchase_vars)
  
  # Reshape the data frame into a wide format
  purchase_winners_wide <- purchase_winners_df %>%
    select(-sum) %>%
    spread(key = group, value = purchase_rate)
  
  # Calculate the winner column
  purchase_winners_wide$winner <- ifelse(purchase_winners_wide$`Average Bidding` > purchase_winners_wide$`Maximum Bidding`, "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  
  purchase_winners_wide <- purchase_winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  purchase_winners_wide
  # Create a vector of levels in the desired order
  purchase_level_order <- rev(c("Impressions", "Reach", 
                                "Website Clicks", "Searches", "View Content", 
                                "Add to Cart", "Purchase"))
  
  # Convert the 'variable' column to an ordered factor with the specified levels
  purchase_winners_wide$variable <- factor(purchase_winners_wide$variable, levels = purchase_level_order, ordered = TRUE)
  
  # Purchase Rate Winners Plot
  purchase_winners_plot <- ggplot(purchase_winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 1), "%")), vjust = .25, size = 5, hjust = -.1) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # axis.text.x = element_text(),
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text=element_text(size=12)
    ) +
    coord_flip() +
    expand_limits(y = c(0, max(purchase_winners_wide$win_percentage) * 1.2))
  
  return(purchase_winners_plot)
}

include_purchase_vars <- c("Impressions","Website Clicks", "Searches", "View Content","Add to Cart","Reach")
plot_purchase_winners(include_purchase_vars)

# ------------------------------------------> Previous ui.R


ui <- dashboardPage(
  dashboardHeader(title = "A/B Test Dashboard"),
  dashboardSidebar(
    tags$head(tags$style(HTML("
      .sidebar {
        height: calc(100vh - 52px); /* Subtract the header's height */
        overflow-y: scroll;
      }
      .content-wrapper {
          height: calc(100vh - 70px); /* Subtract the header's height (adjusted to 70px) */
          overflow-y: scroll; /* Use 'auto' instead of 'scroll' */
      }
    "))),
    sidebarMenu(
      checkboxGroupInput("variables", "Count", 
                         choices = c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Spend (USD)", "Purchase")),
      checkboxGroupInput("costs", "Cost", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Website Clicks", "Purchase")),
      checkboxGroupInput("conversions", "Conversions", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Impressions", "Website Clicks","Purchase")),
      checkboxGroupInput("retention_rate", "Retention", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                         selected = c("Impressions", "Website Clicks","Purchase")),
      checkboxGroupInput("purchase_rate", "Purchases", 
                         choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart"),
                         selected = c("Reach", "Website Clicks","View Content","Add to Cart"))
    )
  ),
  dashboardBody(
    h4(paste0("Project Introduction")),
    fluidRow(
      box(title = title, width = 12, background = "navy",background),
      box(title = "Average Bidding (control)", width = 12, background = "blue",control),
      box(title = "Maximum Bidding (test)", width = 12, background = "blue",test),
      box(title = "Goal", width = 12, background = "light-blue",goal)
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


# Statistcal analysis:

The data scientist on the team will need to be there to ensure that the experiments are set up right. 
The questions should be about hypothesis testing, confidence intervals, or p-values.

original server.R


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
    include_purchase_vars <- input$purchase_rate
    plot_purchase_winners(include_purchase_vars)
  })
  
  
}

shiny::shinyApp(ui = ui, server = server)



# original ui.R

fluidRow(
  box(title = "Control | Average Bidding: ", width = 6, background = "light-blue",control),
  box(title = "Test | Maximum Bidding: ", width = 6, background = "navy"),
),

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
    h3(paste0("Summary")),
    fluidRow(
      column(width = 12,
             h4(summary_text))
    ),
    h4(paste0("Groups")),
    fluidRow(
      box(title = title, width = 12, background = "navy",background),
      box(title = "Average Bidding (control)", width = 12, background = "blue",control),
      box(title = "Maximum Bidding (test)", width = 12, background = "blue",test),
      box(title = "Goal", width = 12, background = "light-blue",goal)
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


# New ui

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "A/B Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Count", tabName = "count"),
      menuItem("Cost", tabName = "cost"),
      menuItem("Conversion", tabName = "conversion"),
      menuItem("Retention", tabName = "retention"),
      menuItem("Purchase", tabName = "purchase")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              # checkboxGroupInput("variables", "Metrics to Display", 
              #                   choices = c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
              #                   selected = c("Spend (USD)", "Purchase")),
              fluidRow(
                box(title = title, width = 12, background = "navy",background),
                box(title = "Average Bidding (control)", width = 12, background = "blue",control),
                box(title = "Maximum Bidding (test)", width = 12, background = "blue",test),
                box(title = "Goal", width = 12, background = "light-blue",goal),
              )
      ),
      tabItem(tabName = "count",
              checkboxGroupInput("variables", "Metrics", 
                                 choices = c("Spend (USD)", "Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                                 selected = c("Spend (USD)", "Purchase")),
              fluidRow(
                box(title = "Overview", plotOutput("totals_plot"),width = 6),
                box(title = "Analysis", plotOutput("totals_analysis_plot"),width = 6)
              )
      ),
      tabItem(tabName = "cost",
              checkboxGroupInput("costs", "Metrics", 
                                 choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                                 selected = c("Website Clicks", "Purchase")),
              fluidRow(
                box(title = "Overview", plotOutput("costs_plot"),width = 6),
                box(title = "Analysis", plotOutput("cost_analysis_plot"),width = 6)
              )
      ),
      tabItem(tabName = "conversion",
              checkboxGroupInput("conversions", "Metrics to Display", 
                                 choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                                 selected = c("Impressions", "Website Clicks","Purchase")),
              fluidRow(
                box(title = "Overview", plotOutput("conversions_plot"),width = 6),
                box(title = "Analysis", plotOutput("conversion_analysis_plot"),width = 6)
              )
      ),
      tabItem(tabName = "retention",
              checkboxGroupInput("retention_rate", "Metrics to Display", 
                                 choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart", "Purchase"),
                                 selected = c("Impressions", "Website Clicks","Purchase")),
              fluidRow(
                box(title = "Overview", plotOutput("retention_rate_plot"),width = 6),
                box(title = "Analysis", plotOutput("retention_rate_analysis_plot"),width = 6)
              )
      ),
      tabItem(tabName = "purchase",
              checkboxGroupInput("purchase_rate", "Metrics to Display", 
                                 choices = c("Impressions", "Reach", "Website Clicks", "Searches", "View Content", "Add to Cart"),
                                 selected = c("Reach", "Website Clicks","View Content","Add to Cart")),
              fluidRow(
                box(title = "Overview", plotOutput("purchase_rate_plot"),width = 6),
                box(title = "Analysis", plotOutput("purchase_rate_analysis_plot"),width = 6)
              )
      )
    )
  )
)


# ---------------------------- Funnel Plots Function

plot_funnels <- function(include_funnels) {
  # Your funnel data processing code remains the same
  # ...
  
  # Create an empty list to store the plots
  funnel_plots <- list()
  
  if ("Count" %in% include_funnels) {
    count_funnel_plot <- count_funnel_df %>%
      # ... your ggplot code for Count funnel ...
      funnel_plots[["Count"]] <- count_funnel_plot
  }
  
  if ("Retention" %in% include_funnels) {
    retention_funnel_plot <- retention_funnel_df %>%
      # ... your ggplot code for Retention funnel ...
      funnel_plots[["Retention"]] <- retention_funnel_plot
  }
  
  if ("Cost" %in% include_funnels) {
    cost_funnel_plot <- cost_funnel_df %>%
      # ... your ggplot code for Cost funnel ...
      funnel_plots[["Cost"]] <- cost_funnel_plot
  }
  
  if ("Impressions" %in% include_funnels) {
    impressions_funnel_plot <- impressions_funnel_df %>%
      # ... your ggplot code for Impressions funnel ...
      funnel_plots[["Impressions"]] <- impressions_funnel_plot
  }
  
  return(funnel_plots)
}

# Utilizing Functions
selected_funnels <- c("Count", "Retention", "Cost", "Impressions")
funnel_plots <- plot_funnels(selected_funnels)

# Selected Returns
funnel_plots[["Count"]]       # Access the Count funnel plot
funnel_plots[["Retention"]]   # Access the Retention funnel plot
funnel_plots[["Cost"]]        # Access the Cost funnel plot
funnel_plots[["Impressions"]] # Access the Impressions funnel plot

# {r, Funnel of Impressions}

# Conversion Rate DF
impressions_funnel_df <- 
  combined_data %>%
  filter(variable %in% include_impression_vars) %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  arrange(desc(sum))  %>%
  mutate(impressions_rate = sum/sum[1])

# Impressions Funnel

impressions_funnel_plot <- impressions_funnel_df %>%
  ggplot(aes(x = reorder(variable, impressions_rate), 
             y = impressions_rate, 
             fill = group)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Conversion of Impressions") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  coord_flip() +
  geom_text(aes(
    label = scales::percent(impressions_rate, accuracy = .1)),
    position = position_dodge(width = 1), 
    vjust = 0.5, 
    size = 3, 
    hjust = -.1) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()) +
  labs(fill = "") +
  expand_limits(y = c(0, 
                      max(impressions_funnel_df$impressions_rate)
                      *1.3))

# {r, Funnel of Cost}

# Cost Funnel Dataframe
cost_funnel_df <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  mutate(cost = ifelse(variable != "Spend (USD)",sum(sum[variable == "Spend (USD)"]), NA) / sum,
         cost = round(cost, 2)) %>%
  filter(variable != "Spend (USD)")
cost_funnel_df

# Barchart of Cost per Instance
cost_funnel_plot <- cost_funnel_df %>%
  ggplot(aes(x = reorder(variable, cost), y = cost, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Cost Funnel") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  theme(legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = paste0("$",cost)), 
            position = position_dodge(width = 1), vjust = 0.5, size = 3, hjust = -.1) +
  theme_minimal() +
  theme(
    # axis.text.x = element_text(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()) +
  labs(fill = "") +
  expand_limits(y = c(0, max(cost_funnel_df$cost)*1.15))

# {Funnel of Count}
count_funnel_df <- combined_data %>%
  group_by(group, variable) %>%
  summarise(sum = sum(value)) %>%
  filter(variable != "Spend (USD)")

# Plot funnel of Count
count_funnel_plot <- count_funnel_df %>%
  ggplot(aes(x = reorder(variable,sum), 
             y = sum, 
             fill = group)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(x = "", 
       y = "Conversion Rate", 
       title = "Count") +
  scale_fill_manual(values = c("#ADD8E6", "#000080"),guide = FALSE) +
  theme(legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = formatC(formatC(count_funnel_df$sum, format="f", big.mark=",", digits=0), 
                                format="f", 
                                big.mark=",", 
                                digits=0)),
            position = position_dodge(width = 1), 
            vjust = 0.5, size = 3, 
            hjust = 0) +
  theme_minimal() +
  theme(
    # axis.text.x = element_text(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()) +
  labs(fill = "", ) +
  expand_limits(y = c(0, max(count_funnel_df$sum)*1.3))


# original winner plots

# winners
plot_winners <- function(include_vars) {
  
  # Subset the data to only include the specified variables
  winners <- combined_data %>%
    filter(variable %in% include_vars) %>%
    group_by(group, variable) %>%
    summarize(sum = sum(value)) %>%
    arrange(desc(sum))
  
  # Reshape the data frame into a wide format
  winners_wide <- winners %>%
    spread(key = group, value = sum)
  
  # Calculate the winner column
  winners_wide$winner <- ifelse(winners_wide$'Average Bidding' > winners_wide$'Maximum Bidding', "Average Bidding", "Maximum Bidding")
  
  # Calculate the win_percentage column
  winners_wide <- winners_wide %>%
    mutate(abs_difference = abs(`Average Bidding` - `Maximum Bidding`),
           win_percentage = abs_difference / pmin(`Average Bidding`, `Maximum Bidding`) * 100)
  
  # Define the threshold for statistical significance (e.g., 5%)
  threshold <- 5
  
  # Create the statistically_significant column
  winners_wide$statistically_significant <- ifelse(winners_wide$win_percentage > threshold, TRUE, FALSE)
  
  # Display the resulting data frame
  winners_wide <- winners_wide %>%
    select(-'Average Bidding',-'Maximum Bidding')
  
  # Define the order
  level_order <- rev(c("Impressions", "Reach", 
                       "Website Clicks", "Searches", "View Content", 
                       "Add to Cart", "Purchase"))
  
  # Reorder the 'variable' factor according to level_order
  winners_wide$variable <- fct_relevel(winners_wide$variable, level_order)
  
  # Create the bar chart
  plot_winners <- ggplot(winners_wide, aes(x = variable, y = win_percentage, fill = winner)) +
    geom_col() +
    scale_fill_manual(values = c("#ADD8E6", "#000080"), guide = FALSE, limits = c("Average Bidding", "Maximum Bidding")) +
    geom_text(aes(label = paste0(round(win_percentage, 2), "%")), vjust = .25, size = 5,hjust=-.1) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "black") + # Add horizontal line at 5%
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      # axis.text.x = element_text(),
      axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text=element_text(size=12)
    ) +
    coord_flip() +
    expand_limits(y = c(0, 
                        max(winners_wide$win_percentage)
                        *1.2))
  
  return(plot_winners)
}

include_vars <- c("Spend (USD)","Impressions", "Website Clicks", "Searches", "View Content", "Add to Cart", "Reach","Purchase")
plot_winners(include_vars)



