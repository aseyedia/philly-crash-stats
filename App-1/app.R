library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(tidyverse)

# Load preprocessed data
# load("data/processed_rdata/preprocessed.Rdata")

ui <- dashboardPage(
  dashboardHeader(title = "Philly Auto Collisions Dashboard"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  ), width = 250),
  
  dashboardBody(useShinyjs(), # Initialize shinyjs
                tabItems(
                  tabItem(tabName = "dashboard", fluidRow(
                    column(
                      width = 6,
                      box(
                        title = "Collision Trend Over Time",
                        width = NULL,
                        plotlyOutput("collisionTrend", height = 350)
                      )
                    ), column(
                      width = 6,
                      box(
                        title = "Summary Statistics",
                        width = NULL,
                        selectInput(
                          "flagSelection",
                          "Select Crash Flags to Display:",
                          choices = colnames(data$flag)[-1],
                          # Exclude CRN
                          selected = c("FATAL", "INJURY"),
                          multiple = TRUE,
                          selectize = TRUE
                        ),
                        plotlyOutput("summaryStats", height = 350)
                      )
                    )
                  )),
                  tabItem(
                    tabName = "about",
                    h2("About this dashboard"),
                    p("This dashboard visualizes auto collision data in Philadelphia.")
                  )
                ))
)

server <- function(input, output, session) {
  # Colors
  colors <- c(
    "Total Collisions" = "#66c2a5",
    "Total Fatalities" = "#fc8d62",
    "Total Severe Injuries" = "#8da0cb"
  )
  
  # Render collision trend over time
  output$collisionTrend <- renderPlotly({
    trend_df <- data$crash %>%
      group_by(Year = CRASH_YEAR) %>%
      summarise(Total = n())
    
    selected_flags <- input$flagSelection
    
    trend_data <- trend_df %>%
      mutate(Flag = "Total Collisions")
    
    if (!is.null(selected_flags) && length(selected_flags) > 0) {
      for (flag in selected_flags) {
        trend_df_filtered <- data$flag %>%
          filter(!!sym(flag) == 1) %>%
          select(CRN) %>%
          inner_join(data$crash, by = "CRN") %>%
          group_by(Year = CRASH_YEAR) %>%
          summarise(Count = n()) %>%
          mutate(Flag = flag)
        
        trend_data <- bind_rows(trend_data, trend_df_filtered)
      }
    }
    
    colors <- c(
      "Total Collisions" = "#66c2a5",
      setNames(RColorBrewer::brewer.pal(n = length(selected_flags), name = "Set1"), selected_flags)
    )
    
    plot_ly() %>%
      add_lines(data = trend_data, x = ~Year, y = ~Count, color = ~Flag, colors = colors) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Collisions"),
        hovermode = "x unified"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Render summary statistics as a bar chart based on selected flags
  output$summaryStats <- renderPlotly({
    selected_flags <- input$flagSelection
    if (is.null(selected_flags) || length(selected_flags) == 0) {
      return(NULL)
    }
    
    flag_counts <- data$flag %>%
      select(CRN, all_of(selected_flags)) %>%
      summarise(across(all_of(selected_flags), ~ sum(. == 1, na.rm = TRUE)))
    
    flag_counts_long <- pivot_longer(
      flag_counts,
      cols = everything(),
      names_to = "Flag",
      values_to = "Count"
    )
    
    # Sort by count
    flag_counts_long <- flag_counts_long %>%
      arrange(desc(Count))
    
    # Assign colors
    colors <- RColorBrewer::brewer.pal(n = max(3, nrow(flag_counts_long)), name = "Set1")
    
    plot_ly(
      flag_counts_long,
      x = ~Flag,
      y = ~Count,
      type = 'bar',
      marker = list(color = colors),
      text = ~Count,
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = "Crash Flags"),
        yaxis = list(title = "Count"),
        title = "Summary Statistics"
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
