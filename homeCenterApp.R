
## app.R ##
library(shiny)
library(shinydashboard)
library(googleway)
library(DT)
library(leaflet)
library(datasets)
library(dplyr)
library(magrittr)

key <- "AIzaSyAOUAfjByh7eSEVrv2ygMRrZo6MHLUB5og"
set_key(key = key)
google_keys()

centers <- read.csv('test2_copy.csv')



ui <- navbarPage(
  
  title = 'Home care centers', id = 'x0',
  
  tabPanel('Search',
    
    
           leafletOutput("sitemap"),
           
    DT::dataTableOutput("table")
   
    
   
   
    

    
    
  
      
   

  ),
  tabPanel('Show all', 
           
           leafletOutput("allMap")
           
           
)
)







server <- function(input, output) {
  
#
  output$table <- DT::renderDataTable(centers, 
                                            selection='single',
                                            server = FALSE,
                                            rownames=FALSE
                          
                                     
  )
  

 #
  observeEvent(input$table_rows_selected,{
    centersfiltered <- centers %>% filter(No == input$table_rows_selected)
    
    map = leaflet(data=centersfiltered ) %>% 
      addTiles() %>% setView(151.2093,-33.8688,zoom = 3)%>%  
      addMarkers(~longitude,~latitude,popup=~as.character(OUTLET_NAME))
    output$sitemap = renderLeaflet(map)
    
  })
  #
  
  
  
  #
  
  mapAll = leaflet(data=centers ) %>% 
    addTiles() %>% setView(151.2093,-33.8688,zoom = 8)%>%  
    addMarkers(~longitude,~latitude,popup=~as.character(OUTLET_NAME))
  output$allMap = renderLeaflet(mapAll)
  
  
  selectedRow <- eventReactive(input$table_rows_selected,{
    row.names(centers)[c(input$table_rows_selected)]
  })
  
  #
  observeEvent(input$allMap_marker_click, { 
    p <- input$allMap_marker_click  # typo was on this line
    print(p)
  })
  #
  
  
  
  

}

shinyApp(ui, server)

