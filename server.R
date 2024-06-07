# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)

# Whole dataset
dataset <- read.csv("data/dataset_recent.csv")

# Truncated columns and NA values
dataset_cut <- dataset %>%
  select(c(AREA.NAME, LON, LAT, Vict.Descent, Status.Desc, Vict.Sex, Crm.Cd.Desc)) %>%
  na.omit() %>%
  filter(Vict.Descent != "")

# Initial choices for Vict.Descent filter
vict_descent_choices <- dataset_cut %>%
  select(Vict.Descent) %>%
  filter(Vict.Descent != "Other") %>%
  unique()

# map view
url_template <- "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png"
attribution <- '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'

shinyServer(function(input, output, session) {
  ### Filters ###
  
  # Limit choices from filter by area
  observeEvent(input$crm.cd.desc, {
    area_name <- unique(c("All", if (input$crm.cd.desc != "All") {
      dataset_cut %>%
        filter(Crm.Cd.Desc == input$crm.cd.desc) %>%
        pull(AREA.NAME)
    } else {
      dataset_cut %>%
        pull(AREA.NAME)
    }))
    
    updateSelectInput(
      session,
      "area.name",
      choices = area_name,
      selected = input$area.name
    )
    
    data <- filtered_areas_df()
    if (!is.null(data) && nrow(data) > 0) {
      selected_row <- data %>% filter(AREA.NAME == input$area.name)
      if (nrow(selected_row) > 0) {
        selected_index <- which(data$AREA.NAME == input$area.name)
        selectRows(proxy = dataTableProxy('full_table'), selected = selected_index)
      }
    }
  })
  
  # Limit choices from filter by crime type
  observeEvent(input$area.name, {
    data = NULL
    if (input$area.name != "All") {

      data_areas <- filtered_areas_df()
      if (!is.null(data_areas) && nrow(data_areas) > 0) {
        selected_row <- data_areas %>% filter(AREA.NAME == input$area.name)
        if (nrow(selected_row) > 0) {
          selected_index <- which(data_areas$AREA.NAME == input$area.name)
          selectRows(proxy = dataTableProxy('full_table'), selected = selected_index)
        }
      }

      data <- dataset_cut %>%
        filter(AREA.NAME == input$area.name) %>%
        pull(Crm.Cd.Desc)
    } else {
      selectRows(proxy = dataTableProxy('full_table'), selected = NULL)

      data <- dataset_cut %>%
        pull(Crm.Cd.Desc)
    }
    
    crime_type <- unique(c("All", data))
    
    updateSelectInput(
      session,
      "crm.cd.desc",
      choices = crime_type,
      selected = input$crm.cd.desc
    )
  })

  ### Datasets ###

  # General data
  filtered_data <- reactive({
    result <- dataset_cut

    if (input$crm.cd.desc != "All") {
      result <- result %>% filter(Crm.Cd.Desc == input$crm.cd.desc)
    }

    if (input$area.name != "All") {
      result <- result %>% filter(AREA.NAME == input$area.name)
    }

    result
  })

  # Bar chart data
  filtered_data_without_area <- reactive({
    # Apply the second filter if necessary
    if (input$crm.cd.desc != "All") {
      dataset_cut %>% filter(Crm.Cd.Desc == input$crm.cd.desc)
    } else {
      dataset_cut
    }
  })

  # Table data
  filtered_areas_df <- reactive({
    data <- filtered_data_without_area()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    data %>%
      group_by(AREA.NAME) %>%
      summarize(count = n(), lon = mean(LON), lat = mean(LAT))
  })

  ### Plots ###

  # safety index
  getSafetyLabel <- function(safety_percentage) {
    if (safety_percentage > 80) {
      return("Pretty Safe")
    } else if (safety_percentage > 50) {
      return("Moderately Safe")
    } else {
      return("Dangerous")
    }
  }

  getSafetyClass <- function(safety_percentage) {
    if (safety_percentage > 80) {
      return("pretty-safe")
    } else if (safety_percentage > 50) {
      return("moderately-safe")
    } else {
      return("dangerous")
    }
  }

  safety_percentage <- 23

  output$safetyBox <- renderUI({
    percentages <- filtered_areas_df() %>%
      mutate(count = count - min(count)) %>%
      mutate(count = count / max(count)) %>%
      pull(count)

    selected_index <- which(filtered_areas_df()$AREA.NAME == input$area.name)
    safety_percentage <- percentages[selected_index]

    safety_percentage <- round(safety_percentage * 100)

    div(class = paste("large-box", getSafetyClass(safety_percentage)),
      icon("shield-alt", class = "icon-large"),
      div(class = "value-large", paste0(safety_percentage, "%")),
      div(class = "label-large", getSafetyLabel(safety_percentage))
    )
  })


  # Pie chart
  output$plot1 <- renderPlotly({
    data <- filtered_data()

    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    finished_label <- c(
      "Adult Arrest",
      "Juv Arrest",
      "Juv Other",
      "Adult Other"
    )

    finished <- sum(data$Status.Desc %in% finished_label)
    investigation_continued <- sum(!data$Status.Desc %in% finished_label)

    plot_ly(
      labels = c("Finished", "Investigation Continuing"),
      values = c(finished, investigation_continued),
      type = "pie"
    ) %>%
      layout(title = "Crime status")
  })

  output$manwoman <- renderPlotly({
    data <- filtered_data() %>%
      filter(Vict.Sex == "M" | Vict.Sex == "F") %>%
      group_by(Vict.Sex) %>%
      summarize(count = n())

    data$Vict.Sex <- factor(data$Vict.Sex, levels = c("M", "F"))

    colors <- rep("rgb(31, 119, 180)", nrow(data))
    colors[data$Vict.Sex == "F"] <- "rgb(255, 127, 14)"
    plot_ly(
      data,
      x = ~Vict.Sex,
      y = ~count,
      type = "bar",
      name = "Number of Crimes",
      marker = list(color = colors)
    ) %>%
      layout(
        title = "Number of Crimes by Victim Descent",
        xaxis = list(title = "Victim Descent"),
        yaxis = list(title = "Number of Crimes"),
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })

  # Bar chart
  output$plot2 <- renderPlotly({
    data <- filtered_data() %>%
      filter(!is.na(Vict.Descent) & Vict.Descent != "") %>%
      group_by(Vict.Descent) %>%
      summarize(count = n()) %>%
      arrange(count)

    # selected_vict_descent <- input$vict.descent

    n <- 5
    top <- head(filter(data, Vict.Descent != "Other"), n)

    # Move "Others" to end
    top$Vict.Descent <- factor(
      top$Vict.Descent,
      levels = c(
        top$Vict.Descent[top$Vict.Descent != "Others"],
        "Others"
      )
    )

    colors <- rep("rgb(31, 119, 180)", nrow(top))

    plot_ly(
      top,
      x = ~count,
      y = ~Vict.Descent,
      type = "bar",
      name = "Number of Crimes",
      marker = list(color = colors)
    ) %>%
      layout(
        title = "Number of Crimes by Victim Descent",
        xaxis = list(title = "Number of Crimes"),
        yaxis = list(title = "Victim Descent"),
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })

  # Full table
  output$full_table <- renderDataTable({
    data <- filtered_areas_df() %>%
      select(AREA.NAME, count)
    if (is.null(data)) {
      return(NULL)
    }
    datatable(data, selection = "single", options = list(
      scrollX = TRUE,
      scrollY = "400px",
      paging = FALSE
    ))
  })

  # Observe full table selection
  observeEvent(input$full_table_rows_selected, {
    selected_row <- input$full_table_rows_selected

    if (length(selected_row)) {
      data <- filtered_areas_df()
      selected_row <- data[selected_row, ]

      updateSelectInput(
        session,
        "area.name",
        selected = selected_row$AREA.NAME
      )

      leafletProxy("crimemap") %>%
        setView(lng = selected_row$lon, lat = selected_row$lat, zoom = 12)
    }
  })


  observe({
    leafletProxy("crimemap") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        data = filtered_data(),
        lng = ~LON,
        lat = ~LAT,
        popup = ~paste0("Popup window ", AREA.NAME),
        clusterOptions = markerClusterOptions()
      )
  })


  # Map
  output$crimemap <- renderLeaflet({
    data <- dataset_cut
    if (is.null(data) || nrow(data) == 0) {
      return(leaflet() %>% addTiles())
    }
    leaflet(data = data) %>%
      addTiles(
        urlTemplate = url_template,
        attribution = attribution
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
})
