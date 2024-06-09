library(shiny)
library(dplyr)
library(plotly)

# Load processed data
# load("data/processed/preprocessed.Rdata")

# Define the UI
ui <- fluidPage(
  titlePanel("Philly Auto Collisions Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Add input controls here if needed
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("collisionTrend")),
        column(6, plotlyOutput("summaryStats"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # browser()
  # Colors
  colors <- c("Total Collisions" = "#66c2a5", "Total Fatalities" = "#fc8d62", "Total Severe Injuries" = "#8da0cb")
  
  # Global summary data frame
  total_collisions <- nrow(data$crash)
  total_fatalities <- sum(data$crash$FATAL_COUNT)
  total_severe_injuries <- sum(data$crash$SUSP_SERIOUS_INJ_COUNT)
  
  summary_df <- data.frame(
    Metric = c("Total Collisions", "Total Fatalities", "Total Severe Injuries"),
    Count = c(total_collisions, total_fatalities, total_severe_injuries)
  )
  
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
  
  # Render summary statistics as a pie chart
  output$summaryStats <- renderPlotly({
    plot_ly(summary_df, labels = ~Metric, values = ~Count, type = 'pie', source = "summaryStats",
            textinfo = 'label+percent', insidetextorientation = 'radial', marker = list(colors = colors)) %>%
      layout(title = "Summary Statistics") %>%
      config(displayModeBar = FALSE)
  })
  
  # Observe hover events from the collision trend plot and update the summary stats
  observeEvent(event_data("plotly_hover", source = "collisionTrend"), {
    hover_data <- event_data("plotly_hover", source = "collisionTrend")
    # browser()
    if (!is.null(hover_data)) {
      year_hovered <- hover_data$x
      cat("Hovering over year:", year_hovered, "\n")
      
      filtered_data <- data$crash %>%
        filter(CRASH_YEAR == year_hovered)
      
      total_collisions <- nrow(filtered_data)
      total_fatalities <- sum(filtered_data$FATAL_COUNT)
      total_severe_injuries <- sum(filtered_data$SUSP_SERIOUS_INJ_COUNT)
      
      summary_df <- data.frame(
        Metric = c("Total Collisions", "Total Fatalities", "Total Severe Injuries"),
        Count = c(total_collisions, total_fatalities, total_severe_injuries)
      )
      
      plotlyProxy("summaryStats", session) %>%
        plotlyProxyInvoke("restyle", list(
          values = list(summary_df$Count),
          marker = list(colors = colors)
        ))
    }
  })
  
  # Observe hover events from the summary stats plot and update the collision trend
  observeEvent(event_data("plotly_hover", source = "summaryStats"), {
    hover_data <- event_data("plotly_hover", source = "summaryStats")
    # browser()
    if (!is.null(hover_data)) {
      point_number <- hover_data$pointNumber + 1  # Add 1 because R is 1-indexed
      metric_hovered <- summary_df$Metric[point_number]
      cat("Hovering over metric:", metric_hovered, "\n")
      
      if (!is.null(metric_hovered) && metric_hovered %in% c("Total Collisions", "Total Fatalities", "Total Severe Injuries")) {
        cat("Valid metric hovered:", metric_hovered, "\n")
        
        filtered_data <- switch(metric_hovered,
                                "Total Collisions" = data$crash,
                                "Total Fatalities" = data$crash %>% filter(FATAL_COUNT > 0),
                                "Total Severe Injuries" = data$crash %>% filter(SUSP_SERIOUS_INJ_COUNT > 0),
                                data$crash)  # Default case to handle errors
        cat("Filtered data count:", nrow(filtered_data), "\n")
        
        trend_df <- filtered_data %>%
          group_by(Year = CRASH_YEAR) %>%
          summarise(Total = n())
        
        base_trend_df <- data$crash %>%
          group_by(Year = CRASH_YEAR) %>%
          summarise(Total = n())
        
        plotlyProxy("collisionTrend", session) %>%
          plotlyProxyInvoke("restyle", list(
            y = list(base_trend_df$Total, trend_df$Total),
            mode = list("lines+markers", "lines+markers"),
            line = list(list(color = 'grey', dash = 'dash'), list(color = colors[metric_hovered]))
          )) %>%
          plotlyProxyInvoke("relayout", list(
            annotations = list(
              list(
                x = trend_df$Year,
                y = trend_df$Total,
                text = trend_df$Total,
                showarrow = FALSE
              )
            )
          ))
      } else {
        cat("Invalid metric hovered:", metric_hovered, "\n")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
