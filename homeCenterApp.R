

## app.R ##
library(shiny)
library(shinydashboard)
library(googleway)
library(DT)
library(leaflet)
library(datasets)
library(dplyr)
library(magrittr)
#library(readr)

#install.packages('stringr')
#txt.tmp <- str_replace_all(conteudo_do_tweet,"[^[:graph:]]", " ") 

key <- "AIzaSyAOUAfjByh7eSEVrv2ygMRrZo6MHLUB5og"
set_key(key = key)
google_keys()

#readr::read_csv
centers <- read.csv('GeocodedHomeCare3.csv', encoding="UTF-8", stringsAsFactors=FALSE)



ui <- fluidPage(tags$head(tags$style(
  HTML('
       #sidebar {
       background-color: white;
       }
       
       body, label, input, button, select { 
       font-family: "Arial";
       }'))),
  tags$head(tags$style(
    type="text/css",
    "#img img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  
  sidebarLayout(
    
    
    sidebarPanel(id="sidebar",
                 
                 img(id="img",src = "LogoBlue3.png", height = 300, width=400),
                 
                 #h3( em("Home Care Centers in Australia"), align = "center")
                 h3( em("Tips:") , align = "left",style = "color:Black",font="Times New Roman"),
                 h4("1: Select or enter your postcode.",align = "left",style = "color:navy",font="Times New Roman"),
                
                 h4("2: Click + to see the detailed information.",align = "left",style = "color:navy",font="Times New Roman"),
                 
                 
                 h4("3:You can search by culture, language, religion, and services. Just type in the search box (case sensitive). Examples: ",align = "left",style = "color:navy",font="Times New Roman"),
                 
                 h5("- Italian",align = "left",style = "color:navy",font="Times New Roman"),
                 h5("- Catholic",align = "left",style = "color:navy",font="Times New Roman"),
      
                 h5("- Dementia",align = "left",style = "color:navy",font="Times New Roman"),
                 h5("- Dementia Spanish",align = "left",style = "color:navy",font="Times New Roman"),
                
                 
                 h4("4: You can export your search results by choosing Copy, CSV, Excel, PDF, Print.",align = "left",style = "color:navy",font="Times New Roman")),
    
    
    
    
    
    
    
    mainPanel(
      
      leafletOutput("sitemap"),
      fluidRow(
        
        column(4,
               selectInput("Postcode",
                           "Postcode:",
                           c(
                             sort(unique(as.character(centers$Postcode)))),selected =2000)
        )),
      
      
      
      DT::dataTableOutput("table")
      # verbatimTextOutput("selectedCells")
      
      
      
    )  
    
    
    
    
  )
)

server <- function(input, output) {
  
  
  
  #
  
  
  output$table <- renderDataTable({
    filteredPostcode <- centers %>%
      filter(
        Postcode == input$Postcode)
    
    
    
    
    datatable(
      filteredPostcode, 
      selection='single',
      extensions = c('Responsive','Buttons','FixedHeader','Scroller','KeyTable','FixedColumns'),
      
      
      rownames=FALSE,
      #fixedHeader = TRUE,keys = TRUE,
      #options = list(searchHighlight = TRUE)
      #filter = 'top',
      #dom = 'Bfrtip',
      #searchHighlight = TRUE
      
      options = list(
        columnDefs = list(list(targets = c(0,1,2,9,10,11,12,13), searchable = FALSE)),
        scrollX = TRUE,
        dom = 'Bfrtip',
        
        
        pageLength=5,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        
        search = list( caseInsensitive = FALSE),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}")
      )
    ) %>%formatStyle('No', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Name', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Address', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Culture', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      
      formatStyle('Religion', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Language', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Services', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('State', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Postcode', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Latitude', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Longitude', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Phone', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Website', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
  })
  
  
  
  
  
  output$selectedCells <- renderPrint(input$table_rows_selected)
  #%>%formatStyle(colnames(centers)[1:ncol(centers)], backgroundColor = 'lightyellow', fontWeight = 'bold')
  
  #
  
  #
  
  
  #centersfiltered <- centers %>% filter(No == input$table_rows_all)
  #
  
  
  #
  output$sitemap = renderLeaflet(
    {
      filteredPostcode2 <- centers %>%
        filter(
          Postcode == input$Postcode)
      
      #centersfiltered <- filteredPostcode2  %>% filter(No == input$table_rows_selected)
      
      leaflet(data=filteredPostcode2 ) %>% 
        addTiles() %>%  
        addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
  
  
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


