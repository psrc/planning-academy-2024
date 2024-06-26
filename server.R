# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  overview_server('OVERVIEW')
  
  # Regional NTD metrics
  place_server('RGC', place_type = rgc_title)
  
  # Regional NTD metrics by Mode
  place_server('SPA', place_type = school_title)
  
})    
