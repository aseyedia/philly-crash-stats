library(shiny)
library(dplyr)
library(plotly)
library(tidyr)

# Load processed data
# load("data/processed/preprocessed.Rdata")

ui <- fluidPage(
  titlePanel("Philly Auto Collisions Dashboard"),
  sidebarLayout(
    sidebarPanel(
      actionButton("toggleSidebar", "Toggle Sidebar"),
      div(id = "checkboxPanel", style = "max-height: 400px; overflow-y: auto;",
          checkboxGroupInput("flagSelection", "Select Crash Flags to Display:",
                             choices = colnames(data$flag)[-1],  # Exclude CRN
                             selected = c("FATAL", "INJURY"))
      )
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("collisionTrend")),
        column(6, plotlyOutput("summaryStats"))
      )
    )
  ),
  tags$script(HTML('
    $(document).ready(function() {
      $("#toggleSidebar").click(function() {
        $("#checkboxPanel").toggle();
      });
    });
  '))
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
      layout(title = "Collision Trend Over Time", xaxis = list(title = "Year"), yaxis = list(title = "Total Collisions")) %>%
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
      layout(title = "Summary Statistics") %>%
      config(displayModeBar = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

