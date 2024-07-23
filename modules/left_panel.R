# Create the left panel

# https://chsamii.medium.com/how-to-focus-on-a-non-input-element-96db50b36e97
# https://codepen.io/shiroshiro/pen/Rwmgqxv

left_panel_ui <- function(id) {
  ns <- NS(id)
  
  cdf <- left_panel_info |> 
    select(starts_with('contact'))
  
  transit_links_withtags <- withTags(
    map2(transit_links[1:3], names(transit_links)[1:3], 
         ~div(class = "links-container", tags$a(class = "links", href = .x, .y, tabindex="0", target = "_blank")))
  )
  
  equity_links_withtags <- withTags(
    map2(equity_links[1:3], names(equity_links)[1:3], 
         ~div(class = "links-container", tags$a(class = "links", href = .x, .y, tabindex="0", target = "_blank")))
  )

  tagList(
    
    div("Resources",
        class = "m-menu__title"),
    
    bsCollapse(id = "equity-collapse", 
               open = NULL,
               bsCollapsePanel("Equity Resources",
                               equity_links_withtags
               )
    ),
    
    bsCollapse(id = "transit-collapse", 
               open = NULL,
               bsCollapsePanel("Transit Resources",
                               transit_links_withtags
                               )
               ),
    
    # Contact ----
    contact_container_ui('contact-data',
                         name = cdf$contact_name, 
                         title = cdf$contact_title, 
                         email = cdf$contact_email, 
                         phone = cdf$contact_phone)
    
  ) # end taglist
}

left_panel_server <- function(id, page_nm) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns

    
  }) # end moduleServer
  
}
