
rgc_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("rgc"))
  )
}

rgc_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$rgc_overview_text <- renderText({page_information(tbl=page_text, page_name="RGC", page_section = "Overview", page_info = "description")})
    output$rgc_description <- renderText(pull_place_information(place_name=input$rgc_name, place_type=rgc_title, place_info = "information"))
    
    # Charts & Maps
    output$rgc_map <- renderLeaflet(create_place_map(place_name=input$rgc_name, place_type=rgc_title))
    
    output$rgc_age_chart <- renderEcharts4r({
      echart_multi_column_chart(df = age_data |>
                                  filter(geography %in% c(input$rgc_name, "Region", "All RGCs") & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel")})

    # Tab layout
    output$rgc <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("rgc_name"), label="Select Regional Growth Center:", choices=rgc_list, selected = random_rgc))),
        fluidRow(column(5, leafletOutput(ns("rgc_map"))),
                 column(7, strong("Description:"),
                        br(),
                        textOutput(ns("rgc_description")))),
        hr(style = "border-top: 1px solid #000000;"),
        
        # Age Group
        strong(tags$div(class="chart_title","Total Population by Age Group")),
        fluidRow(column(12, echarts4rOutput(ns("rgc_age_chart")))),
        br(),
        #fluidRow(column(12, dataTableOutput(ns("age_table")))),
        #br(),
        tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B01001"),
        hr(style = "border-top: 1px solid #000000;")
        
        
      )
    }) 
  })  # end moduleServer
}
