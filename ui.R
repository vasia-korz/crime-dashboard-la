# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)

# Predefined vector of possible values for "Vict Descent"
vict_descent_choices <- c("All", "H", "W", "X", "B", "O", "A", "K", "F", "C", "J", "V", "I", "Z", "U", "P", "L", "D", "G", "S")

# Define UI for application that displays a dashboard
dashboardPage(
  dashboardHeader(title = "Superstore Sales Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      selectInput("vict.descent", "Victim Descent:",
                  choices = vict_descent_choices)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$script(HTML(
        "
        document.addEventListener('keydown', function(event) {
          if (event.key === 'Escape') {
            Shiny.onInputChange('esc_key', Math.random());
          }
        });
        "
      ))
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Crimes Solved vs. Not Solved", status = "primary", solidHeader = TRUE, plotlyOutput("plot1"), width = 6),
                box(title = "Number of Crimes by Victim Descent", status = "primary", solidHeader = TRUE, plotlyOutput("plot2"), width = 6)
              ),
              fluidRow(
                box(title = "Top Customers", status = "primary", solidHeader = TRUE, leafletOutput("crimemap", height = 500)),
                box(title = "Sales per Customer by Category", status = "primary", solidHeader = TRUE, 
                    conditionalPanel(
                      condition = "output.table_state == 'full'",
                      dataTableOutput("full_table")
                    ),
                    conditionalPanel(
                      condition = "output.table_state == 'short'",
                      fluidRow(
                        column(6, dataTableOutput("short_table")),
                        column(6, div(id = "additional_content", "Additional content here"))
                      )
                    ), 
                    width = 6
                ),
              )
      )
    )
  )
)
