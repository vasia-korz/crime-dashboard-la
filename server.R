# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

dataset <- read.csv("data/dataset.csv")
areas_df <- dataset %>%
  select(AREA.NAME) %>%
  na.omit %>%
  group_by(AREA.NAME) %>%
  summarize(count = n())

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

  # Output table state for conditional panels
  output$table_state <- reactive({
    table_state()
  })
  outputOptions(output, "table_state", suspendWhenHidden = FALSE)
})
