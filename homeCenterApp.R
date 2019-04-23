

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

centers <- read.csv('GeocodedHomeCare3.csv', encoding="UTF-8", stringsAsFactors=FALSE)
centers <- as.data.frame(centers)

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
                 
               #  img(id="img",src = "LogoBlue3.png", height = 300, width=400),
              
                 
                 
               
               radioButtons("rbLanguage", "Culture & Language:",
                            choiceNames = list(
                              img(id="img9",src = "English.png", height = 40, width=80),
                              img(id="img10",src = "Italian.png", height = 40, width=80),
                              img(id="img11",src = "Chinese.png", height = 40, width=80),
                              img(id="img12",src = "Indian.png", height = 40, width=80),
                              img(id="img13",src = "Russian.png", height = 40, width=80),
                              img(id="img14",src = "German.png", height = 40, width=80),
                              img(id="img15",src = "Greek.png", height = 40, width=80),
                              img(id="img16",src = "Vietnam.png", height = 40, width=80)
                              
                            ),
                            
                            
                            choiceValues = list(
                              "English", "Italian", "Chinese","Indian", "Russian", "German","Greek","Vietnamese"
                            ),selected="English"
               ),
               radioButtons("rbreligion", "Religion:",
                            choiceNames = list(
                              img(id="img26",src = "allReligion.png", height = 40, width=80),
                              img(id="img17",src = "Catholic.png", height = 40, width=80),
                              img(id="img18",src = "Buddism.png", height = 40, width=80),
                              img(id="img19",src = "Islam.png", height = 40, width=80),
                              img(id="img20",src = "Judaism.png", height = 40, width=80),
                              img(id="img21",src = "Hinduism.png", height = 40, width=80),
                              img(id="img22",src = "Eastern_orthodox.png", height = 40, width=80),
                              #img(id="img23",src = "UnitingChurch", height = 40, width=80),
                              #img(id="img24",src = "Anglican", height = 40, width=80),
                              img(id="img25",src = "Lutheran.png", height = 40, width=80)
                             
                             
                              
                            ),
                            
                            
                            choiceValues = list(
                              "allReligion","Catholic", "Buddism", "Islam","Judaism", "Hinduism", "Eastern Orthodox","Lutheran"
                            ),selected="allReligion"
               )
               
                 
                

),
          
                 
                 #h3( em("Home Care Centers in Australia"), align = "center")
                # h3( em("Tips:") , align = "left",style = "color:Black",font="Times New Roman")
                # h4("1: Select or enter your postcode.",align = "left",style = "color:navy",font="Times New Roman"),
                
                # h4("Click + to see the detailed information.",align = "left",style = "color:navy",font="Times New Roman"),
                 
                 
                # h4("3: You can search by culture, language, religion, and services. Just type in the search box (case sensitive). Examples: ",align = "left",style = "color:navy",font="Times New Roman"),
                 
               #  h5("- Italian",align = "left",style = "color:navy",font="Times New Roman"),
                # h5("- Catholic",align = "left",style = "color:navy",font="Times New Roman"),
      
               #  h5("- Dementia",align = "left",style = "color:navy",font="Times New Roman"),
               #  h5("- Dementia Spanish",align = "left",style = "color:navy",font="Times New Roman"),
                
                 
               #  h4("4: You can export your search results by choosing Copy, CSV, Excel, PDF, Print.",align = "left",style = "color:navy",font="Times New Roman")),
    
    
    mainPanel(
      
      fluidRow(
     
      column(4,
             selectInput("Postcode",
                         "Postcode:",
                         c(
                           sort(unique(as.character(centers$Postcode)))),selected =2000),
             align='center'),align='center'),
      
      leafletOutput("sitemap"),
      
      
      DT::dataTableOutput("table")
      # verbatimTextOutput("selectedCells")
      
    )  
    
  )
)

server <- function(input, output) {
  
  
  
  #
  #postcode click, datatable
  observe({
    
    req(input$Postcode)
     
    
  
  output$table <- DT::renderDataTable({
    
    filteredPostcode <- centers %>%
      filter(
        Postcode == input$Postcode)
    filteredPostcode <- as.data.frame(filteredPostcode)
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
        columnDefs = list(list(targets = c(0,1,6,8,9,10,11,12,13), searchable = FALSE)),
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
    ) %>%formatStyle('Name', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Address', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Culture', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Religion', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Language', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Services', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('State', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Postcode', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Latitude', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Longitude', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
      formatStyle('Phone', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
      formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
    formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
    
    
    observe({
      
      req(input$rbLanguage)
      
      
      output$table <- DT::renderDataTable({
        
        if ( input$rbLanguage == "English")
        {
          filteredClickLan <- filteredPostcode
        }
        else
        {
          
          filteredClickLan <- filteredPostcode %>%
            filter( grepl(input$rbLanguage, Language) || grepl(input$rbLanguage, Culture)
            )
        }
        
        filteredClickLan <- as.data.frame(filteredClickLan)
        datatable(
          filteredClickLan, 
          selection='single',
          extensions = c('Responsive','Buttons','FixedHeader','Scroller','KeyTable','FixedColumns'),
          rownames=FALSE,
          #fixedHeader = TRUE,keys = TRUE,
          #options = list(searchHighlight = TRUE)
          #filter = 'top',
          #dom = 'Bfrtip',
          #searchHighlight = TRUE
          
          options = list(
            columnDefs = list(list(targets = c(0,1,6,8,9,10,11,12,13), searchable = FALSE)),
            scrollX = TRUE,
            dom = 'Bfrtip',
            pageLength=5,
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),deferRender = TRUE,
            scrollY = 150,
            scroller = TRUE,
            search = list( caseInsensitive = FALSE),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}")
          )
        ) %>%formatStyle('Name', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Address', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Culture', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Religion', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Language', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Services', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('State', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Postcode', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Latitude', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Longitude', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Phone', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
      })
      
      
    })
    
    #religion dt
    
    observe({
      
      req(input$rbreligion)
      
      
      output$table <- DT::renderDataTable({
        
        if ( input$rbreligion == "allReligion")
        {
          filteredClickreli <- filteredPostcode
        }
        else
        {
          
          filteredClickreli <- filteredPostcode %>%
            filter( grepl(input$rbreligion, Religion) 
            )
        }
        
        filteredClickreli <- as.data.frame(filteredClickreli)
        datatable(
          filteredClickreli, 
          selection='single',
          extensions = c('Responsive','Buttons','FixedHeader','Scroller','KeyTable','FixedColumns'),
          rownames=FALSE,
          #fixedHeader = TRUE,keys = TRUE,
          #options = list(searchHighlight = TRUE)
          #filter = 'top',
          #dom = 'Bfrtip',
          #searchHighlight = TRUE
          
          options = list(
            columnDefs = list(list(targets = c(0,1,6,8,9,10,11,12,13), searchable = FALSE)),
            scrollX = TRUE,
            dom = 'Bfrtip',
            pageLength=5,
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),deferRender = TRUE,
            scrollY = 150,
            scroller = TRUE,
            search = list( caseInsensitive = FALSE),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}")
          )
        ) %>%formatStyle('Name', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Address', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Culture', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Religion', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Language', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Services', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('State', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Postcode', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Latitude', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Longitude', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('Phone', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
          formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
          formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
      })
      
  })
  

  
  
  })
  
  
 # output$selectedCells <- renderPrint(input$table_rows_selected)
  #%>%formatStyle(colnames(centers)[1:ncol(centers)], backgroundColor = 'lightyellow', fontWeight = 'bold')
  
  #
  
  #
  
  #centersfiltered <- centers %>% filter(No == input$table_rows_all)
  #
  
  #postcode leaflet
  
   observe({
     
     req(input$Postcode)
     
  output$sitemap = renderLeaflet(
    {
      filteredPostcode2 <- centers %>%
        filter(
          Postcode == input$Postcode)
     
     # file <- "LogoBlue3.png"
      #centersfiltered <- filteredPostcode2  %>% filter(No == input$table_rows_selected)
     # file <- 'https://geo2.ggpht.com/maps/photothumb/fd/v1?bpb=ChAKDnNlYXJjaC5UQUNUSUxFEloKTAlnqWnCPq4SaxFWAZtg-PjYbRo4CxDThbhCGi8aLQoWChQKEglnqWnCPq4SaxEnZndM2yWJJRITU3VpdGUgMTkwNCBMZXZlbCAxOQwqCg0AAAAAFQAAAAAaBgjwARCYAw&gl=AU'
      
      filteredPostcode2 <- as.data.frame(filteredPostcode2)
      leaflet(data=filteredPostcode2) %>% 
        addTiles() %>%  
        addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
        #addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", file, ">"))%>%
        #addMarkers(~Longitude,~Latitude,popup = paste0("<img src = " ">"))%>%
        
       
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
      
      
      #lan
      observe({
        # click <- input$rb,input$Postcode,
        req(input$rbLanguage)
       
        
        #print(click)
        
        output$sitemap = renderLeaflet(
          {
            
            if ( input$rbLanguage == "English")
            {
              filteredClickLan <- filteredPostcode2
            }
            else
            {
              
              filteredClickLan <- filteredPostcode2 %>%
                filter( grepl(input$rbLanguage, Language) || grepl(input$rbLanguage, Culture)
                )
            }
            filteredClickLan <- as.data.frame(filteredClickLan)
             # file <- "LogoBlue3.png"
             # file <- 'https://geo2.ggpht.com/maps/photothumb/fd/v1?bpb=ChAKDnNlYXJjaC5UQUNUSUxFEloKTAlnqWnCPq4SaxFWAZtg-PjYbRo4CxDThbhCGi8aLQoWChQKEglnqWnCPq4SaxEnZndM2yWJJRITU3VpdGUgMTkwNCBMZXZlbCAxOQwqCg0AAAAAFQAAAAAaBgjwARCYAw&gl=AU'
              
            #centersfiltered <- filteredPostcode2  %>% filter(No == input$table_rows_selected)
            
            leaflet(data=filteredClickLan) %>% 
              addTiles() %>%  
              addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
             #  addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", file, ">"))%>%
              
              addEasyButton(easyButton(
                icon="fa-crosshairs", title="Locate Me",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
          })
        
        
      })
      
      # religion leaflet
      observe({
        # click <- input$rb,input$Postcode,
        req(input$rbreligion)
        
        
        #print(click)
        
        output$sitemap = renderLeaflet(
          {
            
            if ( input$rbreligion == "allReligion")
            {
              filteredClickreli2 <- filteredPostcode2
            }
            else
            {
              
              filteredClickreli2 <- filteredPostcode2 %>%
                filter( grepl(input$rbreligion, Religion))
                
            }
            filteredClickreli2 <- as.data.frame( filteredClickreli2)
            # file <- "LogoBlue3.png"
            # file <- 'https://geo2.ggpht.com/maps/photothumb/fd/v1?bpb=ChAKDnNlYXJjaC5UQUNUSUxFEloKTAlnqWnCPq4SaxFWAZtg-PjYbRo4CxDThbhCGi8aLQoWChQKEglnqWnCPq4SaxEnZndM2yWJJRITU3VpdGUgMTkwNCBMZXZlbCAxOQwqCg0AAAAAFQAAAAAaBgjwARCYAw&gl=AU'
            
            #centersfiltered <- filteredPostcode2  %>% filter(No == input$table_rows_selected)
            
            leaflet(data= filteredClickreli2) %>% 
              addTiles() %>%  
              addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
              #  addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", file, ">"))%>%
              
              addEasyButton(easyButton(
                icon="fa-crosshairs", title="Locate Me",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
          })
        
        
      })
    })
    })
   })
   
  #click marker
  observe({
   
    req(input$sitemap_marker_click)
    
    
    output$table <- DT::renderDataTable({
      filteredClick <- centers %>%
        filter(
          Latitude == input$sitemap_marker_click$lat & Longitude == input$sitemap_marker_click$lng)
      
      filteredClick <- as.data.frame(filteredClick)
      
      datatable(
        filteredClick, 
        selection='single',
        extensions = c('Responsive','Buttons','FixedHeader','Scroller','KeyTable','FixedColumns'),
        rownames=FALSE,
        #fixedHeader = TRUE,keys = TRUE,
        #options = list(searchHighlight = TRUE)
        #filter = 'top',
        #dom = 'Bfrtip',
        #searchHighlight = TRUE
        
        options = list(
          columnDefs = list(list(targets = c(0,1,6,8,9,10,11,12,13), searchable = FALSE)),
          scrollX = TRUE,
          dom = 'Bfrtip',
          pageLength=5,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),deferRender = TRUE,
          scrollY = 150,
          scroller = TRUE,
          search = list( caseInsensitive = FALSE),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")
        )
      ) %>%formatStyle('Name', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
        formatStyle('Address', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('Culture', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
        formatStyle('Religion', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('Language', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
        formatStyle('Services', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('State', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
        formatStyle('Postcode', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('Latitude', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
        formatStyle('Longitude', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('Phone', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman") %>%
        formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
    })
    
    #religion 
  }) 
  



}
      
      #language
      
      
        # map$clearPopups()
        # map$showPopup(click$latitude, click$longtitude, text)
  
    

    
      
      # language
    
      
  


shinyApp(ui, server)



