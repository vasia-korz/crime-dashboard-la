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
  select(c(AREA.NAME, LON, LAT, Vict.Descent, Status.Desc, Vict.Sex)) %>%
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

  # Limit choices from filter by vict.descent
  observeEvent(input$vict.descent, {
    sex <- unique(c("All", if (input$vict.descent != "All") {
      dataset_cut %>%
        filter(Vict.Descent == input$vict.descent) %>%
        pull(Vict.Sex)
    } else {
      dataset_cut$Vict.Sex
    }))

    updateSelectInput(
      session,
      "vict.sex",
      choices = sex,
      selected = input$vict.sex
    )
  })

  # Limit choices from filter by sex
  observeEvent(input$vict.sex, {
    descent <- unique(c("All", if (input$vict.sex != "All") {
      dataset_cut %>%
        filter(Vict.Sex == input$vict.sex) %>%
        filter(Vict.Descent != "Other") %>%
        pull(Vict.Descent)
    } else {
      dataset_cut %>%
        filter(Vict.Descent != "Other") %>%
        pull(Vict.Descent)
    }))

    updateSelectInput(
      session,
      "vict.descent",
      choices = descent,
      selected = input$vict.descent
    )
  })

  ### Datasets ###

  # General data
  filtered_data <- reactive({
    result <- dataset_cut

    if (input$vict.descent != "All") {
      result <- result %>% filter(Vict.Descent == input$vict.descent)
    }

    if (input$vict.sex != "All") {
      result <- result %>% filter(Vict.Sex == input$vict.sex)
    }

    result
  })

  # Bar chart data
  filtered_data_without_desc <- reactive({
    # Apply the second filter if necessary
    if (input$vict.sex != "All") {
      dataset_cut %>% filter(Vict.Sex == input$vict.sex)
    } else {
      dataset_cut
    }
  })

  # Table data
  filtered_areas_df <- reactive({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    data %>%
      group_by(AREA.NAME) %>%
      summarize(count = n(), lon = mean(LON), lat = mean(LAT))
  })

  ### Plots ###

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

  # Bar chart
  output$plot2 <- renderPlotly({
    data <- filtered_data_without_desc() %>%
      filter(!is.na(Vict.Descent) & Vict.Descent != "") %>%
      group_by(Vict.Descent) %>%
      summarize(count = n()) %>%
      arrange(desc(count))

    selected_vict_descent <- input$vict.descent

    n <- 4
    top <- head(filter(data, Vict.Descent != "Other"), n)

    others <- data %>%
      filter(!(Vict.Descent %in% top$Vict.Descent)) %>%
      summarize(Vict.Descent = "Others", count = sum(count))

    # if not in top
    if (selected_vict_descent != "All" &&
          !selected_vict_descent %in% top$Vict.Descent) {
      others_count <- others$count
      selected_count <- data %>%
        filter(Vict.Descent == selected_vict_descent) %>%
        pull(count)

      # Compensate
      others_count <- others_count - selected_count + top[n, "count"]
      top <- top[1:n - 1, ]

      selected_df <- data.frame(
        Vict.Descent = selected_vict_descent,
        count = selected_count
      )

      others_df <- data.frame(
        Vict.Descent = "Others",
        count = others_count
      )

      final_data <- bind_rows(top, selected_df, others_df)
    } else {
      final_data <- bind_rows(top, others)
    }

    # Move "Others" to end
    final_data$Vict.Descent <- factor(
      final_data$Vict.Descent,
      levels = c(
        final_data$Vict.Descent[final_data$Vict.Descent != "Others"],
        "Others"
      )
    )

    colors <- rep("rgb(31, 119, 180)", nrow(final_data))
    colors[final_data$Vict.Descent == selected_vict_descent] <- "rgb(255, 127, 14)"

    plot_ly(
      final_data,
      x = ~Vict.Descent,
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

  # Table view 
  table_state <- reactiveVal("full")
  output$table_state <- reactive({
    table_state()
  })
  outputOptions(output, "table_state", suspendWhenHidden = FALSE)

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
      table_state("short")
      data <- filtered_areas_df()
      selected_row <- data[selected_row, ]

      leafletProxy("crimemap") %>%
        setView(lng = selected_row$lon, lat = selected_row$lat, zoom = 12)
    }
  })


  # Map
  output$crimemap <- renderLeaflet({
    data <- filtered_data()
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
