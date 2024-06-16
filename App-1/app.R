library(shiny)
library(shinytest2)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(tidyverse)

# Load preprocessed data
if !exists(data)
  load("~/githubProjects/philly-crash-stats/data/processed_rdata/preprocessed.Rdata")

# Generate a dynamic color palette for all flags
all_flags <- colnames(data$flag)[-1] # Exclude CRN
color_palette <- setNames(colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(length(all_flags)), all_flags)

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
  
  dashboardBody(
    useShinyjs(), # Initialize shinyjs
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
              choices = all_flags,
              selected = c("FATAL", "INJURY"),
              multiple = TRUE,
              selectize = TRUE
            ),
            plotlyOutput("summaryStats", height = 350),
            uiOutput("resetButtonUI")
          )
        )
      )),
      tabItem(
        tabName = "about",
        h2("About this dashboard"),
        p("This dashboard visualizes auto collision data in Philadelphia.")
      )
    )
  )
)

server <- function(input, output, session) {
  # Define reactive values to store the selected year and range of years
  selected_year <- reactiveVal(NULL)
  selected_year_range <- reactiveVal(NULL)
  
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
      color_palette[selected_flags]
    )
    
    p <- plot_ly(trend_data, x = ~Year, y = ~Count, color = ~Flag, colors = colors, type = 'scatter', mode = 'lines+markers', source = "collisionTrend") %>%
      layout(
        dragmode = "select",  # Set dragmode to select
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Collisions"),
        hovermode = "x unified"
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # Update summary stats based on click and double-click events
  observe({
    click_data <- event_data("plotly_click", source = "collisionTrend")
    
    if (!is.null(click_data)) {
      selected_year(click_data$x)
      selected_year_range(NULL)  # Clear the selected year range
    }
  })
  
  # Update summary stats based on selection events
  observe({
    select_data <- event_data("plotly_selected", source = "collisionTrend")
    
    if (!is.null(select_data) && length(select_data$x) > 0) {
      selected_year_range(range(select_data$x))
      selected_year(NULL)  # Clear the selected single year
    }
  })
  
  # Render reset button UI conditionally
  output$resetButtonUI <- renderUI({
    if (!is.null(selected_year()) || !is.null(selected_year_range())) {
      actionButton("resetButton", "Reset Selections")
    }
  })
  
  # Reset selections when reset button is clicked
  observeEvent(input$resetButton, {
    selected_year(NULL)
    selected_year_range(NULL)
  })
  
  # Render summary statistics as a bar chart based on selected flags and selected year or range of years
  output$summaryStats <- renderPlotly({
    selected_flags <- input$flagSelection
    year <- selected_year() %>% unique()
    year_range <- selected_year_range()
    
    if (is.null(selected_flags) || length(selected_flags) == 0) {
      return(NULL)
    }
    
    if (!is.null(year)) {
      flag_counts <- data$flag %>%
        filter(CRN %in% data$crash$CRN[data$crash$CRASH_YEAR == year]) %>%
        select(CRN, all_of(selected_flags)) %>%
        summarise(across(all_of(selected_flags), ~ sum(. == 1, na.rm = TRUE)))
    } else if (!is.null(year_range)) {
      flag_counts <- data$flag %>%
        filter(CRN %in% data$crash$CRN[data$crash$CRASH_YEAR >= year_range[1] & data$crash$CRASH_YEAR <= year_range[2]]) %>%
        select(CRN, all_of(selected_flags)) %>%
        summarise(across(all_of(selected_flags), ~ sum(. == 1, na.rm = TRUE)))
    } else {
      flag_counts <- data$flag %>%
        select(CRN, all_of(selected_flags)) %>%
        summarise(across(all_of(selected_flags), ~ sum(. == 1, na.rm = TRUE)))
    }
    
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
    colors <- color_palette[flag_counts_long$Flag]
    
    plot_title <- if (!is.null(year)) {
      paste("Summary Statistics for Year", year)
    } else if (!is.null(year_range)) {
      paste("Summary Statistics for Years", year_range[1], "to", year_range[2])
    } else {
      "Summary Statistics"
    }
    
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
        title = plot_title
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
