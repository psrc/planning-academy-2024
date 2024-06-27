# Display Source Data

glossary_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('glossary'))
  )
  
}

glossary_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Text
    output$glossary_text <- renderText({page_information(tbl=page_text, page_name="Glossary", page_section = "Overview", page_info = "description")})
    
    # Tables
    output$glossary_table <- renderDataTable(create_source_table(d=glossary_text))
    
    # Tab layout
    output$glossary <- renderUI({
      
      tagList(
        
        # Source Data
        fluidRow(column(12, tags$div(class="page_goals", "Glossary of terms"))),
        textOutput(ns("glossary_text")),
        br(),
        fluidRow(column(12, dataTableOutput(ns("glossary_table")))),
        br()
      )
      
    })
    
  }) # end moduleServer
  
}
