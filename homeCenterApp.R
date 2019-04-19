
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
  datatable = datatable(centers, 
            selection='single',
            extensions = c('Responsive','Buttons','FixedHeader','Scroller'),
            
        
            rownames=FALSE,
            #options = list(searchHighlight = TRUE)
            options = list(searchHighlight = TRUE,dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),fixedHeader = TRUE,deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}")
            )
           ) %>%formatStyle('No', backgroundColor = 'lightblue', fontWeight = 'bold') %>%
    formatStyle('Name', backgroundColor = 'white', fontWeight = 'bold')%>%
    formatStyle('Address', backgroundColor = 'lightblue', fontWeight = 'bold') %>%
    formatStyle('Culture', backgroundColor = 'white', fontWeight = 'bold')%>%
    
    formatStyle('Religion', backgroundColor = 'lightblue', fontWeight = 'bold')%>%
    formatStyle('Language', backgroundColor = 'white', fontWeight = 'bold') %>%
    formatStyle('Services', backgroundColor = 'lightblue', fontWeight = 'bold')%>%
    formatStyle('State', backgroundColor = 'white', fontWeight = 'bold') %>%
    formatStyle('Postcode', backgroundColor = 'lightblue', fontWeight = 'bold')%>%
    formatStyle('Latitude', backgroundColor = 'white', fontWeight = 'bold') %>%
    formatStyle('Longitude', backgroundColor = 'lightblue', fontWeight = 'bold')%>%
    formatStyle('Phone', backgroundColor = 'white', fontWeight = 'bold') %>%
    formatStyle('Website', backgroundColor = 'lightblue', fontWeight = 'bold')
  
  output$table <- renderDataTable(datatable) 
  #%>%formatStyle(colnames(centers)[1:ncol(centers)], backgroundColor = 'lightyellow', fontWeight = 'bold')
  
 #
  observeEvent(input$table_rows_selected,{
      if(!is.null(input$table_rows_selected))
      {
      centersfiltered <- centers %>% filter(No == input$table_rows_selected)
      
      map = leaflet(data=centersfiltered ) %>% 
        addTiles() %>%  
        addMarkers(~Longitude,~Latitude,popup=~as.character(Name))
      output$sitemap = renderLeaflet(map)
      }
    else
    {
      mapAll = leaflet(data=centers ) %>% 
        addTiles() %>% setView(151.2093,-33.8688,zoom = 12)%>%  
        addMarkers(~Longitude,~Latitude,popup=~as.character(Name))
      output$sitemap = renderLeaflet(mapAll)
      
    }
      
  },ignoreNULL = FALSE)
  #
  
  #
  
  mapAll = leaflet(data=centers ) %>% 
    addTiles() %>% setView(151.2093,-33.8688,zoom = 8)%>%  
    addMarkers(~Longitude,~Latitude,popup=~as.character(Name))
  output$allMap = renderLeaflet(mapAll)
  
  
  selectedRow <- eventReactive(input$table_rows_selected,{
    row.names(centers)[c(input$table_rows_selected)]
  })
  
 

}

shinyApp(ui, server)

