# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  overview_server('OVERVIEW')
  
  # Center Metrics
  place_server('RGC', place_type = rgc_title)
  
  # School Metrics
  place_server('SPA', place_type = school_title)
  
  # Glossary
  glossary_server('GLOSSARY')
  
})    
