
spa_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("spa"))
  )
}

spa_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$spa_overview_text <- renderText({page_information(tbl=page_text, page_name="SPA", page_section = "Overview", page_info = "description")})
    output$spa_description <- renderText(pull_place_information(place_name=input$school_name, place_type=school_title, place_info = "information"))
    
    # Charts & Maps
    output$spa_map <- renderLeaflet(create_place_map(place_name=input$school_name, place_type=school_title))
    
    output$spa_age_chart <- renderEcharts4r({
      echart_multi_column_chart(df = age_data |>
                                  filter(geography %in% c(input$school_name, "Region", "All Schools") & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel")})
    
    # Tab layout
    output$spa <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("school_name"), label="Select School:", choices=school_list, selected = random_school))),
        fluidRow(column(5, leafletOutput(ns("spa_map"))),
                 column(7, strong("Description:"),
                        br(),
                        textOutput(ns("spa_description")))),
        hr(style = "border-top: 1px solid #000000;"),
        
        # Age Group
        strong(tags$div(class="chart_title","Total Population by Age Group")),
        fluidRow(column(12, echarts4rOutput(ns("spa_age_chart")))),
        br(),
        #fluidRow(column(12, dataTableOutput(ns("age_table")))),
        #br(),
        tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B01001"),
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) 
  })  # end moduleServer
}
