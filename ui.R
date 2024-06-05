# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)

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
                  value = 30)
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
                box(title = "Total Customers", status = "primary", solidHeader = TRUE, plotOutput("plot1"), width = 4),
                box(title = "Total Sales per Customer", status = "primary", solidHeader = TRUE, plotOutput("plot2"), width = 4),
                box(title = "Customer Distribution by Sales", status = "primary", solidHeader = TRUE, plotOutput("plot3"), width = 4)
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
