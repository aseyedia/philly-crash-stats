library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)

# Load preprocessed data
# load("data/processed_rdata/preprocessed.Rdata")

ui <- dashboardPage(
  dashboardHeader(title = "Philly Auto Collisions Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    width = 250
  ),
  
  dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 6,
                       box(title = "Collision Trend Over Time", width = NULL, plotlyOutput("collisionTrend", height = 350))
                ),
                column(width = 6,
                       box(
                         title = "Summary Statistics",
                         width = NULL,
                         selectInput("flagSelection", "Select Crash Flags to Display:",
                                     choices = colnames(data$flag)[-1],  # Exclude CRN
                                     selected = c("FATAL", "INJURY"),
                                     multiple = TRUE,
                                     selectize = TRUE),
                         plotlyOutput("summaryStats", height = 350)
                       )
                )
              )
      ),
      tabItem(tabName = "about",
              h2("About this dashboard"),
              p("This dashboard visualizes auto collision data in Philadelphia.")
      )
    )
  )
)

server <- function(input, output, session) {
  # Colors
  colors <- c("Total Collisions" = "#66c2a5", "Total Fatalities" = "#fc8d62", "Total Severe Injuries" = "#8da0cb")
  
  # Render collision trend over time
  output$collisionTrend <- renderPlotly({
    trend_df <- data$crash %>%
      group_by(Year = CRASH_YEAR) %>%
      summarise(Total = n())
    
    plot_ly(trend_df, x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers', source = "collisionTrend",
            text = ~Total, hoverinfo = 'text', line = list(color = colors["Total Collisions"])) %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = "Total Collisions")) %>%
      config(displayModeBar = FALSE)
  })
  
  # Render summary statistics as a pie chart based on selected flags
  output$summaryStats <- renderPlotly({
    selected_flags <- input$flagSelection
    if (is.null(selected_flags) || length(selected_flags) == 0) {
      return(NULL)
    }
    
    flag_counts <- data$flag %>%
      select(CRN, all_of(selected_flags)) %>%
      summarise(across(all_of(selected_flags), ~ sum(. == 1, na.rm = TRUE)))
    
    flag_counts_long <- pivot_longer(flag_counts, cols = everything(), names_to = "Flag", values_to = "Count")
    
    plot_ly(flag_counts_long, labels = ~Flag, values = ~Count, type = 'pie', source = "summaryStats",
            textinfo = 'percent', hoverinfo = 'label+percent', insidetextorientation = 'radial') %>%
      config(displayModeBar = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)