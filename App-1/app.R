library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(tidyverse)
library(here)

# List files in the current working directory
print("Files in current working directory:")
print(list.files(getwd(), full.names = TRUE))

# List files one directory up
print("Files in the parent directory:")
print(list.files(dirname(getwd()), full.names = TRUE))

# List files in one directory down (subdirectories)
print("Files in subdirectories:")
subdirs <- list.dirs(getwd(), recursive = FALSE)
for (subdir in subdirs) {
  print(paste("Files in subdirectory", subdir, ":"))
  print(list.files(subdir, full.names = TRUE))
}

# Load preprocessed data
data_path <- "preprocessed.Rdata"
if (file.exists(data_path)) {
  load(data_path)
} else {
  stop("Data file not found: ", data_path)
}

# Verify that the data has been loaded correctly
if (!exists("data") || is.null(data)) {
  stop("Failed to load data from: ", data_path)
}

# Generate a dynamic color palette for all flags
all_flags <- colnames(data$flag)[-1] # Exclude CRN
color_palette <- setNames(colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(length(all_flags)), all_flags)

# Define the Young Drivers color palette with more contrasting colors
young_drivers_palette <- setNames(c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"), 
                                  c("DRIVER_16YR", "DRIVER_17YR", "DRIVER_18YR", "DRIVER_19YR", "DRIVER_20YR"))

ui <- dashboardPage(
  dashboardHeader(title = "Philly Auto Collisions Dashboard"),
  
  dashboardSidebar(
    collapsed = TRUE,  # Sidebar is collapsed by default
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ), 
    width = 250
  ),
  
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
            title = "Total Collisions",
            width = NULL,
            selectInput(
              "flagSelection",
              "Select Crash Flags to Display:",
              choices = all_flags,
              selected = c("INJURY_OR_FATAL"),
              multiple = TRUE,
              selectize = TRUE
            ),
            actionButton("enableAllFlags", "Enable All Flags"),
            actionButton("disableAllFlags", "Disable All Flags"),
            br(),
            actionButton("presetInjuryFatal", "Injury or Fatal"),
            actionButton("presetSpeedRelated", "Speed Related"),
            actionButton("presetAlcoholDrugs", "Alcohol and Drugs"),
            actionButton("presetYoungDrivers", "Young Drivers"),
            actionButton("presetVulnerableUsers", "Vulnerable Users"),
            actionButton("presetWeatherRelated", "Weather Related"),
            actionButton("presetIntersectionRelated", "Intersection Related"),
            actionButton("presetDistractedDriving", "Distracted Driving"),
            actionButton("presetCommercialVehicles", "Commercial Vehicles"),
            actionButton("presetTimeOfDay", "Time of Day"),
            actionButton("presetRoadConditions", "Road Conditions"),
            plotlyOutput("summaryStats", height = 350),
            textOutput("noFlagsSelected"),
            uiOutput("resetButtonUI")
          )
        )
      )),
      tabItem(
        tabName = "about",
        h2("About this dashboard"),
        p("This dashboard visualizes auto collision data in Philadelphia. The data is sourced from the Pennsylvania Department of Transportation (PennDOT) and includes information about the circumstances of each collision, such as weather conditions, road conditions, and driver-related factors."),
        p("The dashboard allows users to explore trends in collision data over time and analyze the impact of various factors on collision rates. Users can select specific crash flags to display total collisions for different types of collisions."),
        # include a link to this data dictionary pdf
        p("For more information about the data fields and their meanings, please refer to the ", a("data dictionary", href = "https://gis.penndot.gov/gishub/crashZip/Crash%20Data%20Dictionary%2005.2023.pdf"), ".")
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
          group_by(CRASH_YEAR) %>%
          summarise(Count = n()) %>%
          mutate(Flag = flag) %>%
          rename(Year = CRASH_YEAR)
        
        trend_data <- bind_rows(trend_data, trend_df_filtered)
      }
    }
    
    if (any(selected_flags %in% names(young_drivers_palette))) {
      cat("Young Drivers palette applied\n") # Debug message
      colors <- young_drivers_palette[selected_flags]
    } else {
      colors <- c(
        "Total Collisions" = "#66c2a5",
        color_palette[selected_flags]
      )
    }
    
    p <- plot_ly(trend_data, x = ~Year, y = ~Count, color = ~Flag, colors = colors, type = 'scatter', mode = 'lines+markers', source = "collisionTrend") %>%
      layout(
        dragmode = "select",  # Set dragmode to select
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Collisions"),
        hovermode = "x unified",
        selectdirection = "h"
      ) %>%
      config(displayModeBar = FALSE) %>%
      event_register("plotly_selecting") %>%
      event_register("plotly_click")
    
    return(p)
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
    select_data <- event_data("plotly_selecting", source = "collisionTrend")
    
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
    
    plotlyProxy("collisionTrend", session) %>%
      plotlyProxyInvoke("relayout", list("xaxis.range" = NULL, "yaxis.range" = NULL))
    
    plotlyProxy("collisionTrend", session) %>%
      plotlyProxyInvoke("restyle", list(selectedpoints = list(NULL)), list(0, 1))
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
    
    if (nrow(flag_counts) == 0) {
      return(plotly_empty() %>%
               layout(title = "No flags selected"))
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
    if (any(selected_flags %in% names(young_drivers_palette))) {
      cat("Young Drivers palette applied to bar chart\n") # Debug message
      colors <- young_drivers_palette[flag_counts_long$Flag]
    } else {
      colors <- color_palette[flag_counts_long$Flag]
    }
    
    plot_title <- if (!is.null(year)) {
      paste("Total Collisions for Year", year)
    } else if (!is.null(year_range)) {
      paste("Total Collisions for Years", year_range[1], "to", year_range[2])
    } else {
      "Total Collisions"
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
  
  # Show a message when no flags are selected
  output$noFlagsSelected <- renderText({
    selected_flags <- input$flagSelection
    if (is.null(selected_flags) || length(selected_flags) == 0) {
      "No flags selected"
    } else {
      NULL
    }
  })
  
  # Show the total collisions trendline when no flags are selected
  observe({
    selected_flags <- input$flagSelection
    if (is.null(selected_flags) || length(selected_flags) == 0) {
      trend_df <- data$crash %>%
        group_by(Year = CRASH_YEAR) %>%
        summarise(Total = n())
      
      output$collisionTrend <- renderPlotly({
        plot_ly(trend_df, x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers') %>%
          layout(
            title = "Total Collisions Over Time",
            xaxis = list(title = "Year"),
            yaxis = list(title = "Total Collisions")
          ) %>%
          config(displayModeBar = FALSE)
      })
    } else {
      output$collisionTrend <- renderPlotly({
        trend_df <- data$crash %>%
          group_by(Year = CRASH_YEAR) %>%
          summarise(Total = n())
        
        trend_data <- trend_df %>%
          mutate(Flag = "Total Collisions")
        
        for (flag in selected_flags) {
          trend_df_filtered <- data$flag %>%
            filter(!!sym(flag) == 1) %>%
            select(CRN) %>%
            inner_join(data$crash, by = "CRN") %>%
            group_by(CRASH_YEAR) %>%
            summarise(Count = n()) %>%
            mutate(Flag = flag) %>%
            rename(Year = CRASH_YEAR)
          
          trend_data <- bind_rows(trend_data, trend_df_filtered)
        }
        
        if (any(selected_flags %in% names(young_drivers_palette))) {
          cat("Young Drivers palette applied\n") # Debug message
          colors <- young_drivers_palette[selected_flags]
        } else {
          colors <- c(
            "Total Collisions" = "#66c2a5",
            color_palette[selected_flags]
          )
        }
        
        plot_ly(trend_data, x = ~Year, y = ~Count, color = ~Flag, colors = colors, type = 'scatter', mode = 'lines+markers', source = "collisionTrend") %>%
          layout(
            dragmode = "select",  # Set dragmode to select
            xaxis = list(title = "Year"),
            yaxis = list(title = "Total Collisions"),
            hovermode = "x unified",
            selectdirection = "h"
          ) %>%
          config(displayModeBar = FALSE) %>%
          event_register("plotly_selecting") %>%
          event_register("plotly_click")
      })
    }
  })
  
  # Enable all flags
  observeEvent(input$enableAllFlags, {
    updateSelectInput(session, "flagSelection", selected = all_flags)
  })
  
  # Disable all flags
  observeEvent(input$disableAllFlags, {
    updateSelectInput(session, "flagSelection", selected = character(0))
  })
  
  # Preselected flag groups
  observeEvent(input$presetInjuryFatal, {
    updateSelectInput(session, "flagSelection", selected = c("INJURY_OR_FATAL"))
  })
  
  observeEvent(input$presetSpeedRelated, {
    updateSelectInput(session, "flagSelection", selected = c("SPEEDING", "SPEEDING_RELATED"))
  })
  
  observeEvent(input$presetAlcoholDrugs, {
    updateSelectInput(session, "flagSelection", selected = c("ALCOHOL_RELATED", "DRUG_RELATED", "DRUGGED_DRIVER"))
  })
  
  observeEvent(input$presetYoungDrivers, {
    updateSelectInput(session, "flagSelection", selected = c("DRIVER_16YR", "DRIVER_17YR", "DRIVER_18YR", "DRIVER_19YR", "DRIVER_20YR"))
  })
  
  observeEvent(input$presetVulnerableUsers, {
    updateSelectInput(session, "flagSelection", selected = c("PEDESTRIAN", "BICYCLE", "MOTORCYCLE", "VULNERABLE_ROADWAY_USER"))
  })
  
  observeEvent(input$presetWeatherRelated, {
    updateSelectInput(session, "flagSelection", selected = c("RAIN", "SNOW", "FOG", "SLEET", "WEATHER_RELATED"))
  })
  
  observeEvent(input$presetIntersectionRelated, {
    updateSelectInput(session, "flagSelection", selected = c("INTERSECTION", "INTERSECTION_RELATED"))
  })
  
  observeEvent(input$presetDistractedDriving, {
    updateSelectInput(session, "flagSelection", selected = c("DISTRACTED", "DISTRACTED_DRIVER"))
  })
  
  observeEvent(input$presetCommercialVehicles, {
    updateSelectInput(session, "flagSelection", selected = c("COMM_VEHICLE"))
  })
  
  observeEvent(input$presetTimeOfDay, {
    updateSelectInput(session, "flagSelection", selected = c("RUSH_HOUR", "NIGHT", "DAY"))
  })
  
  observeEvent(input$presetRoadConditions, {
    updateSelectInput(session, "flagSelection", selected = c("ROAD_CONDITION", "WET_ROAD", "ICY_ROAD", "DRY_ROAD"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
