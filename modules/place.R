
place_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("place"))
  )
}

place_server <- function(id, place_type) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Place based inputs
    place_list <- place_shape |> st_drop_geometry() |> filter(geography_type == place_type)  |> select("name") |> distinct() |> pull() |> sort()
    random_place <- place_list[[sample(1:length(place_list), 1)]]
    if (place_type == rgc_title) {all_places <- "All RGCs"} else {all_places <- "All Schools"}
    
    # Text
    output$description <- renderText(pull_place_information(place_name=input$place_name, place_type=place_type, place_info = "information"))
    
    # Charts & Maps
    output$map <- renderLeaflet(create_place_map(place_name=input$place_name, place_type=place_type))
    
    output$race_chart <- renderEcharts4r({
      echart_multi_bar_chart(df = race_data |> 
                               filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total") |>
                               arrange(desc(grouping)),
                             x = "grouping", y = "share", fill="geography", tog = "year",
                             dec = 0, esttype = "percent", color = "jewel", left_align = '20%')})
    
    output$age_chart <- renderEcharts4r({
      echart_multi_column_chart(df = age_data |>
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$education_chart <- renderEcharts4r({
      echart_multi_bar_chart(df = education_data |>
                               filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                             x = "grouping", y = "share", fill="geography", tog = "year", 
                             dec = 0, esttype = "percent", color = "jewel", left_align = '20%')})
    
    

    # Tab layout
    output$place <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("place_name"), label="Select Geography:", choices=place_list, selected = random_place))),
        fluidRow(column(6, leafletOutput(ns("map"))),
                 column(6, strong("Description:"),
                        br(),
                        textOutput(ns("description")))),
        hr(style = "border-top: 1px solid #000000;"),
        
        tabsetPanel(type = "pills",
                    tabPanel("People", 
                             
                             # Race
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Population by Race & Ethnicity")),
                             fluidRow(column(12, echarts4rOutput(ns("race_chart"), height=500))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B03002"),
                             br(),
                             
                             # Age Group
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Population by Age Group")),
                             fluidRow(column(12, echarts4rOutput(ns("age_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B01001"),
                             br(),
                             
                             # Educational Attainment
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Population 25+ by Educational Attainment")),
                             fluidRow(column(12, echarts4rOutput(ns("education_chart"), height=500))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B15002"),
                             br(),
                             
                             ),
                    
                    tabPanel("Jobs", "Coming Soon"),
                    
                    tabPanel("Transportation", "Coming Soon")),
        
        
        hr(style = "border-top: 1px solid #000000;")
        
        
      )
    }) 
  })  # end moduleServer
}
