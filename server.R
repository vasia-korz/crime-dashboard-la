# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)

# Load dataset
dataset <- read.csv("data/dataset_recent.csv")

dataset_map <- dataset %>%
  select(c(AREA.NAME, LON, LAT, Vict.Descent, Status.Desc, Vict.Sex)) %>%
  na.omit() %>%
  filter(Vict.Descent != "")  # Filter out empty strings in Vict.Descent

# Exclude "O" from filtering choices
vict_descent_choices <- unique(dataset_map$Vict.Descent[dataset_map$Vict.Descent != "Other"])

areas_df <- dataset_map %>%
  group_by(AREA.NAME) %>%
  summarize(count = n(), lon = mean(LON), lat = mean(LAT))

# Define server logic
shinyServer(function(input, output, session) {
  table_state <- reactiveVal("full")
  
  # Reactive value to keep track of the table state
  filtered_data <- reactive({
    # Start with the entire dataset
    result <- dataset_map
    
    # Apply the first filter if necessary
    if (input$vict.descent != "All") {
      result <- result %>% filter(Vict.Descent == input$vict.descent)
    }
    
    # Apply the second filter if necessary
    if (input$vict.sex != "All") {
      result <- result %>% filter(Vict.Sex == input$vict.sex)
    }
    
    # Return the filtered dataset
    result
  })
  
  filtered_data_without_desc <- reactive({
    # Apply the second filter if necessary
    if (input$vict.sex != "All") {
      dataset_map %>% filter(Vict.Sex == input$vict.sex)
    } else {
      dataset_map
    }
  })
  
  filtered_areas_df <- reactive({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    data %>%
      group_by(AREA.NAME) %>%
      summarize(count = n(), lon = mean(LON), lat = mean(LAT))
  })
  
  # Render the full data table with all columns
  output$full_table <- renderDataTable({
    data <- filtered_areas_df()
    if (is.null(data)) {
      return(NULL)
    }
    datatable(data, selection = "single", options = list(pageLength = 10))
  })
  
  # Render the short data table with only two columns
  output$short_table <- renderDataTable({
    data <- filtered_areas_df()
    if (is.null(data)) {
      return(NULL)
    }
    datatable(data[, c("AREA.NAME", "count")], selection = "single", options = list(pageLength = 10))
  })
  
  # Observe the table selection
  observeEvent(input$full_table_rows_selected, {
    selected_row <- input$full_table_rows_selected
    
    if (length(selected_row)) {
      table_state("short")
      data <- filtered_areas_df()
      selected_row <- data[selected_row, ]
      
      leafletProxy("crimemap") %>%
        setView(lng = selected_row$lon, lat = selected_row$lat, zoom = 12)
    }
  })
  
  observeEvent(input$short_table_rows_selected, {
    selected_row <- input$short_table_rows_selected
    
    if (length(selected_row)) {
      data <- filtered_areas_df()
      selected_row <- data[selected_row, ]
      
      leafletProxy("crimemap") %>%
        setView(lng = selected_row$lon, lat = selected_row$lat, zoom = 12)
    }
  })
  
  # Listen for custom message to reset the UI
  observeEvent(input$esc_key, {
    table_state("full")
  })
  
  # Pie chart for crimes solved vs not solved
  output$plot1 <- renderPlotly({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    solved <- sum(data$Status.Desc %in% c("Adult Arrest", "Juv Arrest", "Juv Other", "Adult Other"))
    not_solved <- sum(data$Status.Desc %in% c("Invest Cont", "UNK"))
    plot_ly(labels = c("Solved", "Not Solved"), values = c(solved, not_solved), type = 'pie') %>%
      layout(title = "Crimes Solved vs. Not Solved")
  })
  
  # Bar chart for number of crimes by Vict Descent with highlighting and substituting "Others"
  output$plot2 <- renderPlotly({
    data <- filtered_data_without_desc() %>%
      filter(!is.na(Vict.Descent) & Vict.Descent != "") %>%
      group_by(Vict.Descent) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    selected_vict_descent <- input$vict.descent
    
    top_4 <- head(filter(data, Vict.Descent != "Other"), 4)
    others <- data %>%
      filter(!(Vict.Descent %in% top_4$Vict.Descent)) %>%
      summarize(Vict.Descent = "Others", count = sum(count))
    
    if (selected_vict_descent != "All" && !selected_vict_descent %in% top_4$Vict.Descent) {
      others_count <- others$count
      selected_count <- data %>% filter(Vict.Descent == selected_vict_descent) %>% pull(count)
      # Subtract the selected count from "Others" and add the fifth top to "Others"
      others_count <- others_count - selected_count + top_4[4, "count"]
      top_4 <- top_4[1:3, ]
      top_4 <- bind_rows(top_4, data.frame(Vict.Descent = selected_vict_descent, count = selected_count))
      final_data <- bind_rows(top_4, data.frame(Vict.Descent = "Others", count = others_count))
    } else {
      final_data <- bind_rows(top_4, others)
    }
    
    final_data$Vict.Descent <- factor(final_data$Vict.Descent, levels = c(final_data$Vict.Descent[final_data$Vict.Descent != "Others"], "Others"))
    
    colors <- rep('rgb(31, 119, 180)', nrow(final_data))
    colors[final_data$Vict.Descent == selected_vict_descent] <- 'rgb(255, 127, 14)'
    
    plot_ly(final_data, x = ~Vict.Descent, y = ~count, type = 'bar', name = 'Number of Crimes', marker = list(color = colors)) %>%
      layout(title = "Number of Crimes by Victim Descent",
             xaxis = list(title = "Victim Descent"),
             yaxis = list(title = "Number of Crimes"),
             margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$crimemap <- renderLeaflet({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return(leaflet() %>% addTiles())
    }
    leaflet(data = data) %>%
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
      ) %>%
      setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>%
      setMaxBounds(lng1 = -118.7, lat1 = 33.7, lng2 = -118.1, lat2 = 34.4) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset View",
        onClick = JS("function(btn, map){ map.setView([34.0522, -118.2437], 9); }")
      )) %>%
      addCircleMarkers(
        lng = ~LON,
        lat = ~LAT,
        popup = ~paste0("Popup window ", AREA.NAME),
        clusterOptions = markerClusterOptions()
      )
  })
  
  observe({
    data <- filtered_data()
    proxy <- leafletProxy("crimemap")
    proxy %>% clearMarkers()
    if (!is.null(data) && nrow(data) > 0) {
      proxy %>%
        addCircleMarkers(
          lng = data$LON,
          lat = data$LAT,
          popup = paste0("Popup window ", data$AREA.NAME),
          clusterOptions = markerClusterOptions()
        )
    }
  })
  
  observeEvent(input$vict.descent, {
    sex <- levels(factor(c("All", dataset_map$Vict.Sex)))
    updateSelectInput(session, "vict.sex", choices = sex)
  })
  
  
  # Output table state for conditional panels
  output$table_state <- reactive({
    table_state()
  })
  outputOptions(output, "table_state", suspendWhenHidden = FALSE)
})