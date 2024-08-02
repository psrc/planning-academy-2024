
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
    
    output$session_overview_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Session Overview", page_info = "description")})
    
    output$session1_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Session 1", page_info = "description")})
    output$session2_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Session 2", page_info = "description")})
    output$session3_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Session 3", page_info = "description")})
    output$session4_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Session 4", page_info = "description")})
    
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
                              href = "https://psrcwa.shinyapps.io/planning-academy-centers-2024/", 
                              icon("tree-city"), 
                              target="_blank"),
                            class = "focus",
                            tabindex="0"))),
        
        fluidRow(column(6, align="center", strong(tags$div(class="icon_text","PSRC Data Portal"))),
                 column(6, align="center", strong(tags$div(class="icon_text","Regional Growth Centers")))),
        
        br(),
        
        fluidRow(column(6, align="center",
                        div(a(class = "source_url left-panel-url", 
                              href = "https://experience.arcgis.com/experience/a587d27d1c444a6e891fe1b58508622d", 
                              icon("book-atlas"), 
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
        
        fluidRow(column(6, align="center", strong(tags$div(class="icon_text","PSRC Transportation Visualization Tool"))),
                 column(6, align="center", strong(tags$div(class="icon_text","Optional School Data")))),
        
        br(),
        textOutput(ns("overview2_text")),
        br(),
        
        # h2("Additional Session Resources"),
        # 
        # textOutput(ns("session_overview_text")),
        # br(),
        # 
        # # 
        # 
        # tabsetPanel(type = "pills",
        #             tabPanel("Session #1", 
        #                      br(),
        #                      textOutput(ns("session1_text")),
        #                      br(),
        #                      tags$a(class = "links", href = "https://www.psrc.org/media/7768", "Equity Impact Assessment", tabindex="0", target = "_blank"),
        #                      br(),
        #                      tags$a(class = "links", href = "https://www.psrc.org/media/5123", "Vision 2050 Handout", tabindex="0", target = "_blank")
        #                      ),
        #             tabPanel("Session #2", 
        #                      textOutput(ns("session2_text"))
        #             ),
        #             tabPanel("Session #3", 
        #                      textOutput(ns("session3_text"))
        #             ),
        #             tabPanel("Session #4", 
        #                      textOutput(ns("session4_text"))
        #             )),
        
      )
    })
  })  # end moduleServer
}
