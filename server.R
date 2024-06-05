# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(leaflet)
library(leaflet.extras)

dataset <- read.csv("data/kek.csv")

dataset_map <- dataset %>%
  select(c(AREA.NAME, LON, LAT)) %>%
  na.omit()

areas_df <- dataset_map %>%
  group_by(AREA.NAME) %>%
  summarize(count = n(), lon = sum(LON) / n(), lat = sum(LAT) / n())

# Define server logic
shinyServer(function(input, output, session) {

  # Reactive value to keep track of the table state
  table_state <- reactiveVal("full")

  # Render the full data table with all columns
  output$full_table <- renderDataTable({
    datatable(areas_df, selection = "single", options = list(pageLength = 10))
  })

  # Render the short data table with only two columns
  output$short_table <- renderDataTable({
    datatable(areas_df[, c(1, 2)], selection = "single", options = list(pageLength = 10)) # Assuming columns 1 and 2 are needed
  })

  # Observe the table selection
  observeEvent(input$full_table_rows_selected, {
    selected_row <- input$full_table_rows_selected

    if (length(selected_row)) {
      table_state("short")
      selected_row <- areas_df[selected_row, ]

      leafletProxy("crimemap") %>%
        setView(lng = selected_row["lon"], lat = selected_row["lat"], zoom = 12)
    }
  })

  observeEvent(input$short_table_rows_selected, {
    selected_row <- input$short_table_rows_selected

    if (length(selected_row)) {
      selected_row <- areas_df[selected_row, ]

      leafletProxy("crimemap") %>%
        setView(lng = selected_row["lon"], lat = selected_row["lat"], zoom = 12)
    }
  })

  # Listen for custom message to reset the UI
  observeEvent(input$esc_key, {
    table_state("full")
  })

  # Example plots
  output$plot1 <- renderPlot({ plot(cars) })
  output$plot2 <- renderPlot({ plot(pressure) })
  output$plot3 <- renderPlot({ plot(iris) })

  output$crimemap <- renderLeaflet({
    leaflet(data = dataset) %>%
      addTiles() %>%
      setView(lng = -118.2437, lat = 34.0522, zoom = 10) %>%
      setMaxBounds(lng1 = -118.7, lat1 = 33.7, lng2 = -118.1, lat2 = 34.4) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset View",
        onClick = JS("function(btn, map){ map.setView([34.0522, -118.2437], 10); }")
      )) %>%
      addCircleMarkers(
        lng = dataset_map$LON,
        lat = dataset_map$LAT,
        clusterOptions = markerClusterOptions()
      )
  })

  # Output table state for conditional panels
  output$table_state <- reactive({
    table_state()
  })
  outputOptions(output, "table_state", suspendWhenHidden = FALSE)
})
