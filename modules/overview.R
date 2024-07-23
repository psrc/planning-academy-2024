
overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("overview"))
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$overview1_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Overview1", page_info = "description")})
    output$overview2_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Overview2", page_info = "description")})
    
    # Overview UI
    output$overview <- renderUI({
      tagList(
        br(),
        textOutput(ns("overview1_text")),
        br(),
        
        fluidRow(column(6, align="center",
                        div(a(class = "source_url left-panel-url", 
                              href = "https://psrc-psregcncl.hub.arcgis.com/", 
                              icon("square-poll-vertical"), 
                              target="_blank"),
                            class = "focus",
                            tabindex="0")),
                 
                 column(6, align="center",
                        div(a(class = "source_url left-panel-url", 
                              href = "https://experience.arcgis.com/experience/a587d27d1c444a6e891fe1b58508622d", 
                              icon("book-atlas"), 
                              target="_blank"),
                            class = "focus",
                            tabindex="0"))),
        
        fluidRow(column(6, align="center", strong(tags$div(class="icon_text","PSRC Data Portal"))),
                 column(6, align="center", strong(tags$div(class="icon_text","PSRC Transportation Visualization Tool")))),
        
        fluidRow(column(6, align="center",
                        div(a(class = "source_url left-panel-url", 
                              href = "https://psrcwa.shinyapps.io/planning-academy-centers-2024/", 
                              icon("tree-city"), 
                              target="_blank"),
                            class = "focus",
                            tabindex="0")),
                 
                 column(6, align="center",
                        div(a(class = "source_url left-panel-url", 
                              href = "https://psrcwa.shinyapps.io/planning-academy-schools-2024/", 
                              icon("school"), 
                              target="_blank"),
                            class = "focus",
                            tabindex="0"))),
        
        fluidRow(column(6, align="center", strong(tags$div(class="icon_text","Regional Growth Centers"))),
                 column(6, align="center", strong(tags$div(class="icon_text","Optional School Data")))),
        
        br(),
        textOutput(ns("overview2_text")),
        br(),
        
      )
    })
  })  # end moduleServer
}
