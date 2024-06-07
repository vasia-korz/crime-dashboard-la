# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)

vict_descent_choices <- c(
  "All",
  "Hispanic",
  "White",
  "Black",
  "Asian",
  "Korean",
  "Filipino",
  "Chinese",
  "Japanese",
  "Vietnamese",
  "Native",
  "Indian",
  "Hawaiian",
  "Pacific",
  "Laotian",
  "Cambodian",
  "Guamanian",
  "Samoan"
)

vict_sex_choices <- c("All", "M", "F", "X", "H")

# Define UI for application that displays a dashboard
dashboardPage(
  dashboardHeader(title = "Superstore Sales Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ),
    collapsed = TRUE
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
      )),
      tags$style(HTML(
        "
        .main-header .sidebar-toggle {
          display: none;
        }
        "
      ))
    ),

    fluidRow(
      box(width = 3,
        selectInput(
          "vict.descent",
          "Victim Descent:",
          choices = vict_descent_choices
        ),
      ),
      box(width = 3,
        selectInput(
          "vict.sex",
          "Victim Sex:",
          choices = vict_sex_choices
        ),
      )
    ),

    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Crimes Finishied vs. Not Finished",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot1"),
            width = 4
          ),
          box(
            title = "Number of Crimes by Victim Descent",
            status = "primary",
            plotlyOutput("manwoman"),
            solidHeader = TRUE,
            width = 4
          ),
          box(
            title = "Temp",
            status = "primary",
            solidHeader = TRUE,
            width = 4
          )
        ),
        fluidRow(
          box(
            title = "Sales per Customer by Category",
            status = "primary",
            solidHeader = TRUE,
            dataTableOutput("full_table"),
            width = 3
          ),
          box(
            title = "Top Customers",
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("crimemap", height = 500),
            width = 6
          ),
          box(
            title = "Number of crimers by Victim Descent",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot2"),
            width = 3
          )
        )
      )
    )
  )
)
