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
  dashboardHeader(title = "LA Crimes"),
  
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
        html, body {
          height: 100vh;
          overflow: hidden;
        }
        .main-header {
          display: none;
        }
        .large-box {
          font-size: 50px;
          text-align: center;
          padding: 40px;
          border-radius: 10px;
          margin: 20px;
          color: white;
        }
        .icon-large {
          font-size: 70px;
        }
        .value-large {
          font-size: 80px;
        }
        .label-large {
          font-size: 40px;
        }
        .pretty-safe {
          background-color: green;
        }
        .moderately-safe {
          background-color: yellow;
          color: black;
        }
        .dangerous {
          background-color: red;
        }
        .box.box-solid.box-primary {
          border: none;
          border-radius: 10px;
          margin: 0;
        }
        .box.box-solid.box-primary>.box-header {
          background: none;
          color: black;
          text-align: center;
          display: none;
        }
        .box-body {
          padding: 1rem;
        }
        .large-box {
          margin: 0;
          height: 100%;
        }
        .dropdown .box {
          border-top: none;
          box-shadow: none;
          background: none;
        }
        .tab-content>.active {
          display: grid;
          gap: 2rem;
        }
        .first-row .box {
          height: 100%;
        }
        .first-row {
          height: 35vh;
        }
        .first-row .box {
          height: 35vh;
        }
        #safetyBox {
          width: 100%;
        }
        .safety-box .box-body {
          padding: 1rem;
          display: flex;
          height: 100%;
          width: 100%;
        }
        .dataTables_wrapper .dataTables_filter {
          display: none;
        }
        .dataTables_wrapper .dataTables_info {
          display: none;
        }
        table.dataTable tbody tr.selected td {
          box-shadow: none !important;
          background-color: magenta !important;
        }
        table.dataTable tbody tr.selected:hover td {
          box-shadow: none !important;
          background-color: magenta !important;
        }
        .second-row {
          height: 45vh;
        }
        .second-row .col-sm-6 {
          height: 100%;
          display: flex;
        }
        
        .second-row .col-sm-6 .box-body {
          display: flex;
          height: 100%;
          width: 100%;
        }
        
        .second-row .col-sm-6 .leaflet-container {
          height: 100% !important;
        }
        
        .second-row .col-sm-3, .second-row .col-sm-12,
        .second-row .row:nth-of-type(2),
        .second-row .box.box-solid.box-primary,
        .second-row .box-body, 
        .second-row .dataTables_wrapper, 
        .second-row .dataTables_wrapper .dataTables_scroll {
          height: 100%;
        }
        
        .second-row .dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody {
          max-height: calc(100% - 40px) !important;
          height: calc(100% - 40px) !important;
        }
        
        .second-row div.datatables {
          height: 100% !important;
        }
        .plot-container {
          width: 100%;
          height: 100%;
          padding: 10px;
        }
        .plot {
          width: 100%;
          height: 30vh;
        }
        html, .content-wrapper {
          background: #c8d9ed;
        }
        .header-text {
          font-size: 50px;
          color: darkblue;
          font-weight: bold;
          text-align: center;
        }
        
        .header-row {
          height: 10vh;
        }
        
        table.dataTable tr.active td, table.dataTable tr.active {
          box-shadow: inset 0 0 0 9999px magenta !important;
        }
        
        "
      ))
    ),
    
    div(class = "header-row",
      fluidRow(
        column(3,
               div(class = "dropdown",
                   selectInput(
                     "area.name",
                     "Area name:",
                     choices = area_choices
                   )
               )
        ),
        column(3,
               div(class = "dropdown",
                   selectInput(
                     "crm.cd.desc",
                     "Crime type:",
                     choices = crime_choices
                   )
               )
        ),
        column(6,
               div(class = "header-text",
                   "LA Area Advisor"
               )
        )
      )
    ),
    
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        div(class = "first-row",
            fluidRow(
              box(
                title = "Monthly Crime Comparison",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("monthly_comparison", height="32vh"),
                width = 6
              ),
              box(
                title = "Number of Crimes by Victim Descent",
                status = "primary",
                plotlyOutput("manwoman", height="32vh"),
                solidHeader = TRUE,
                width = 3
              ),
              div(class = "safety-box",
                  box(
                    title = "Crimes Finished vs. Not Finished",
                    status = "primary",
                    solidHeader = TRUE,
                    uiOutput("safetyBox"),
                    width = 3
                  )
              )    
            )
        ),
        fluidRow(
          div(class = "second-row",
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
                leafletOutput("crimemap"),
                width = 6
              ),
              box(
                title = "Number of Crimes by Victim Descent",
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("plot2", height="42vh"),
                width = 3
              )
          )
        )
      )
    )
  )
)
