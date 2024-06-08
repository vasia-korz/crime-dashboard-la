library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)
library(base64enc)

img <- file("www/logo.png", "rb")
img_bin <- readBin(img, what = "raw", n = file.info("www/logo.png")$size)
close(img)
img_base64 <- base64encode(img_bin)

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
          margin: 20px;
          color: white;
          height: 20vh;
        }
        .icon-large {
          font-size: 56px;
          display: flex;
          justify-content: flex-end;
        }
        .value-large {
          font-size: 70px;
          display: flex;
        }
        .label-large {
          font-size: 40px;
          grid-column: 1/3;
        }
        .pretty-safe {
          background-color: #A5D6A7;
          color: black;
        }
        .moderately-safe {
          background-color: #FFEB3B;
          color: black;
        }
        .dangerous {
          background-color: #F44336;
        }
        .initial {
          background-color: #B0BEC5;
          color: black;
        }
        .box.box-solid.box-primary {
          border: none;
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
          align-items: center;
          column-gap: 2rem;
          display: grid;
          padding: 0;
          grid-template-columns: auto auto;
          width: 100%;
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
          height: 60%;
          display: flex;
          justify-content: center;
          align-items: center;
        }
        .safety-box .box-body {
          padding: 1rem;
          display: flex;
          height: 100%;
          padding: 0;
          width: 100%;
          flex-direction: column;
        }
        .dataTables_wrapper .dataTables_filter {
          display: none;
        }
        .dataTables_wrapper .dataTables_info {
          display: none;
        }
        table.dataTable tbody tr.selected td {
          box-shadow: none !important;
          background-color: #FF69B4 !important;
        }
        table.dataTable tbody tr.selected:hover td {
          box-shadow: none !important;
          background-color: #FF69B4 !important;
        }
        
        table.dataTable tbody tr td {
          box-shadow: none !important;
          background-color: white !important;
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
        
        html, .content-wrapper {
          background: #F3F3F3;
        }
        
        .header-text {
          font-family: 'Arial', sans-serif;
          font-size: 16px;
          font-weight: 400;
          color: #6b6d6e;
          background-color: white;
          padding: 10px;
        }
        
        .header-row {
          height: 10vh;
        }
        
        table.dataTable tr.active td, table.dataTable tr.active {
          box-shadow: inset 0 0 0 9999px #FF69B4 !important;
        }
        
        .title_box {
          font-size: 18px;
          font-weight: 500;
          color: black;
          background-color: white;
          padding: 5px;
          width: 100%;
          display: flex;
          justify-content: center;
          text-align: cetner;
          flex-direction: column;
        }
        
        .title_box .title {
          font-size: 30px;
          font-weight: 500;
          align-items: center;
        }
        
        .title_box .description {
          color: #6b6d6e;
        }
        
        .box-body.descriptive {
          height: 100%;
        }
        
        .map-box {
          height: 100%;
          width: 100%;
          display: contents;
        }
        
        .map-box .box .box-body {
          padding: 0 !important;
        }
        
        .safety-box .box.box-solid.box-primary {
          background: none;
          box-shadow: none;
        }
        
        .safety-box .title_box {
          background: none;
          display: flex;
          justify-content: cetner;
          padding: 20px;
          text-align: center;
          font-size: 15px;
        }
        "
      ))
    ),
    
    div(class = "header-row",
        fluidRow(
          column(2,
                 div(class = "dropdown",
                     selectInput(
                       "area.name",
                       "Area name:",
                       choices = area_choices
                     )
                 )
          ),
          column(2,
                 div(class = "dropdown",
                     selectInput(
                       "crm.cd.desc",
                       "Crime type:",
                       choices = crime_choices
                     )
                 )
          ),
          column(7,
                 div(class = "header-text",
                     "Select an area of Los Angeles based on safety rankings and crime details. Compare crime rates with the LA average, see victim demographics, and assess overall safety to make informed decisions about where to live or visit."
                 )
          ),
          column(1,
                 tags$img(src = paste0("data:image/png;base64,", img_base64), height = "67px", width = "auto") 
          )
        )
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        div(class = "first-row",
            fluidRow(
              box(
                div(class = "title_box", 
                    div(class = "title", 
                        "Comparison with average"
                    ),
                    div(class = "description", 
                        "Choose an area in order to see how its crime rate differs from the LA Average."
                    )
                ),
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("monthly_comparison", height="25vh"),
                width = 6,
                class = "descriptive"
              ),
              box(
                div(class = "title_box", 
                    div(class = "title", 
                        "Victim Sex"
                    ),
                    div(class = "description", 
                        "Check out which sex is affected more."
                    )
                ),
                status = "primary",
                plotlyOutput("manwoman", height="25vh"),
                solidHeader = TRUE,
                width = 3
              ),
              div(class = "safety-box",
                  box(
                    status = "primary",
                    solidHeader = TRUE,
                    uiOutput("safetyBox"),
                    width = 3,
                    div(class = "title_box", 
                        div(class = "description", 
                            "Safety index is calculated relatively to other areas of LA. The safest area in each category is valued at 100% and the most dangerous obtains 0%. Index is recalculated for each crime type in the area."
                        )
                    ),
                  )
              )    
            )
        ),
        fluidRow(
          div(class = "second-row",
              box(
                status = "primary",
                solidHeader = TRUE,
                dataTableOutput("full_table"),
                width = 3
              ),
              div(class = "map-box",
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  leafletOutput("crimemap"),
                  width = 6
                )
              ),
              box(
                div(class = "title_box", 
                    div(class = "title", 
                        "Victim Race"
                    ),
                    div(class = "description", 
                        "Check out which races are affected more."
                    )
                ),
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput("plot2", height="32vh"),
                width = 3
              )
          )
        )
      )
    )
  )
)
