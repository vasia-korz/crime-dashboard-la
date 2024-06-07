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

area_choices <- c('All', 'N Hollywood', 'Hollywood', '77th Street', 'Van Nuys', 'Southeast',
                        'Southwest', 'Central', 'Pacific', 'Wilshire', 'Northeast', 'Newton', 'Olympic',
                        'Rampart', 'Mission', 'Topanga', 'Devonshire', 'Hollenbeck', 'West Valley',
                        'Harbor', 'West LA', 'Foothill')
crime_choices <- c('All', 'Mixed', 'Fraud', 'Property', 'Violent', 'Sexual')

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
          "area.name",
          "Area name:",
          choices = area_choices
        ),
      ),
      box(width = 3,
        selectInput(
          "crm.cd.desc",
          "Crime type:",
          choices = crime_choices
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
            title = "Monthly Crime Comparison",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("monthly_comparison"),
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
