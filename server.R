# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  overview_server('OVERVIEW')
  
  # Regional NTD metrics
  rgc_server('RGC')
  
  # Regional NTD metrics by Mode
  spa_server('SPA')
  
})    
