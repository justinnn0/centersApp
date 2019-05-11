
## app.R ##
library(shiny)
library(shinydashboard)
library(googleway)
library(DT)
library(leaflet)
library(datasets)
library(dplyr)
library(magrittr)
library(shinyWidgets)


#library(readr)

#install.packages('stringr')
#txt.tmp <- str_replace_all(conteudo_do_tweet,"[^[:graph:]]", " ") 

key <- "AIzaSyAOUAfjByh7eSEVrv2ygMRrZo6MHLUB5og"
set_key(key = key)
google_keys()

centers <- read.csv('GeocodedHomeCare3.csv', encoding="UTF-8", stringsAsFactors=FALSE)
centers <- as.data.frame(centers)

ui <- fluidPage(
  
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #222222;
         }
         
         body, label, input, button, select { 
         font-family: "Arial";
         }'))),
  tags$head(tags$style(
    type="text/css",
    "#img img {max-width: 100%; width: 100%; height: auto}"
    )),
  
  
  sidebarLayout(
    
    #tags$head(tags$style(HTML('background-color: #222222'))),
    #setBackgroundColor("#222222")
    
    sidebarPanel(id="sidebar",setBackgroundColor("#222222"),
                 fluidRow(
                   column(12,
                          selectInput("Postcode",
                                      HTML("<h3 style='color:white;'>Enter or select a postcode</h3>"),
                                      c(
                                        sort(unique(as.character(centers$Postcode)))),selected =2000),
                          align='left'),align='left'),
            
                 
                 
                 selectInput("rbLanguage", HTML("<h3 style='color:white;'>Culture & Language the care center provides:</h3>"), 
                             c("English" = "English", 
                                "中文" = "Chinese" ,
                               "Italiano" = "Italian",
                               "हिन्दी " = "Hindi",
                               "Русский" = "Russian",
                               "Deutsche"="German", 
                               "Ελληνικά" = "Greek",
                               "Tiếng Việt"="Vietnamese",
                               "日本語" =  "Japanese",
                               "한국어" = "Korean",
                               "Español" = "Spanish", 
                               "français" = "French",
                               " عربى" = "Arabic" 
                               
                               ),selected = "English"
                 ),
                
                 
                 radioButtons("rbreligion",  HTML("<h3 style='color:white;'>Religion:</h3>"),
                              choiceNames = list(
                                HTML('<img id="img26" src="allReligion.png" height=40 px width=80 px"><label style="color:white">All</label></img>'),
                                HTML('<img id="img26" src="Catholic.png" height=40 px width=80 px"><label style="color:white">Catholic</label></img>'),
                                HTML('<img id="img26" src="Buddism.png" height=40 px width=80 px"><label style="color:white">Buddism</label></img>'),
                                HTML('<img id="img26" src="Islam.png" height=40 px width=80 px"><label style="color:white">Islam</label></img>'),
                                HTML('<img id="img26" src="Judaism.png" height=40 px width=80 px"><label style="color:white">Judaism</label></img>'),
                                HTML('<img id="img26" src="Hinduism.png" height=40 px width=80 px"><label style="color:white">Hinduism</label></img>'),
                                HTML('<img id="img26" src="Eastern_orthodox.png" height=40 px width=80 px"><label style="color:white">Eastern orthodox</label></img>'),
                                HTML('<img id="img26" src="Lutheran.png" height=40 px width=80 px"><label style="color:white">Lutheran</label></img>')
                                
                              ),
                              
                              
                              choiceValues = list(
                                "allReligion","Catholic", "Buddism", "Islam","Judaism", "Hinduism", "Eastern Orthodox","Lutheran"
                              ),selected="allReligion"
                 ),
                 br(),
                 
                 h4( em("Tips: If no data matches your selection criteria, please select 'Englsih' for culture & language and 'All' for religion. Then contact the home care centers for detailed information.") , align = "left",style = "color:lightBlue",font="Times New Roman")
                 
                 
                 
                 
    ),
    
    
    #h3( em("Home Care Centers in Australia"), align = "center")
    # h4("1: Select or enter your postcode.",align = "left",style = "color:navy",font="Times New Roman"),
    
    # h4("1: Click + to see the detailed information.",align = "left",style = "color:navy",font="Times New Roman"),
    # h4("2: To search for other culture and religion just type in the search box below the the map",align = "left",style = "color:navy",font="Times New Roman"),
    
    
    
    # h4("3: You can search by culture, language, religion, and services. Just type in the search box (case sensitive). Examples: ",align = "left",style = "color:navy",font="Times New Roman"),
    
    #  h5("- Italian",align = "left",style = "color:navy",font="Times New Roman"),
    # h5("- Catholic",align = "left",style = "color:navy",font="Times New Roman"),
    
    #  h5("- Dementia",align = "left",style = "color:navy",font="Times New Roman"),
    #  h5("- Dementia Spanish",align = "left",style = "color:navy",font="Times New Roman"),
    
    
    #  h4("4: You can export your search results by choosing Copy, CSV, Excel, PDF, Print.",align = "left",style = "color:navy",font="Times New Roman")),
    
    
    mainPanel(
      
      br(),
     
      leafletOutput("sitemap"),
      
      DT::dataTableOutput("table"),
      tags$head(tags$style("#table {background-color: white; }", media="screen", type="text/css")),
      h4(em("Click and select a name or address from the above table to search on Google", align = "left",style = "color:lightBlue",font="Times New Roman")),
      # uiOutput(outputId = "ggoogle"),
      # uiOutput("tab"),
      fluidRow( 
        column(6, uiOutput("tab"))
        #column(6,uiOutput("tabggmap"))
        )
      # uiOutput(outputId = "ggmap"),
      #uiOutput("tabggmap")
      #verbatimTextOutput("selectedCells")
      #box(google_mapOutput("myGMap") )
      
      
    )  
    
  )
  )

server <- function(input, output) {
  

  
  output$ggmap <- renderUI({
    
    tags$img(HTML('<img id="img35" src="ggmap.png" height=150 px width=150 px"><label style="color:white"></label></img>'))
    
  })
  
  output$ggoogle <- renderUI({
    tags$img(HTML('<img id="img36" src="ggoogle.png" height=150 px width=150 px"><label style="color:white"></label></img>'))
    
  })
  
  
  
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
          searching=FALSE,
          
          # columnDefs = list(list(targets = c(0,1,8,9,10,11,12), searchable = FALSE)),
          columnDefs = list(list(visible=FALSE, targets=c(10,11,12))),
          scrollX = TRUE,
          dom = 'Bfrtp',
          pageLength=5,
          buttons = c('copy','pdf', 'print'),deferRender = TRUE,
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
              filter( grepl(input$rbLanguage, Language ) | grepl(input$rbLanguage, Culture ))              
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
              searching= FALSE,
              #columnDefs = list(list(targets = c(0,1,8,9,10,11,12), searchable = FALSE)),
              columnDefs = list(list(visible=FALSE, targets=c(10,11,12))),
              scrollX = TRUE,
              dom = 'Bfrtp',
              pageLength=5,
              buttons = c('copy', 'pdf', 'print'),deferRender = TRUE,
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
            
            formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
            formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
        })
        
        # google map
        #
        
        
        
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
              searching= FALSE,
              #columnDefs = list(list(targets = c(0,1,8,9,10,11,12), searchable = FALSE)),
              columnDefs = list(list(visible=FALSE, targets=c(10,11,12))),
              scrollX = TRUE,
              dom = 'Bfrtp',
              pageLength=5,
              buttons = c('copy', 'pdf', 'print'),deferRender = TRUE,
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
            
            formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
            formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
        })
        
      })
      
      # dt cell - google map 
      
      observe({
        req(input$table_cell_clicked)
        
        # sstring <- "https://www.google.com/search?cr=countryAU&q="+input$table_cell_clicked
        #https://www.google.com/search?cr=countryAU&q=St Louis Home Care
        #https://www.google.com/maps/place/
        output$selectedCells <- renderPrint(input$table_cell_clicked$value)
        sstring <- paste("https://www.google.com/search?cr=countryAU&q=", as.character(input$table_cell_clicked$value))
        
       url <- a(as.character(input$table_cell_clicked$value), href= sstring,target="_blank")
        
        output$tab <- renderUI({
          tagList( HTML("<h3 style='color:white; align ='center'>Search on Google</h3>"), url)
        }) # HTML("<p style='color:blue;'>English</p>"),
        
        sstring2 <- paste("https://www.google.com/maps/place/", as.character(input$table_cell_clicked$value))
       # url2 <- a(as.character(input$table_cell_clicked$value), href= sstring2)
        url2 <- a(as.character(input$table_cell_clicked$value), href= sstring2, target="_blank")
        output$tabggmap <- renderUI({
          tagList(HTML("<h3 style='color:white; align ='center'>Search on Google Map</h3>"), url2)
        })
        
        
        
        # https://www.google.com/search?cr=countryAU&q=monash
        
        # centersfiltered <- filteredPostcode %>% filter(No == input$table_cell_clicked$value)
        
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
          #URL <- 'https://geo2.ggpht.com/maps/photothumb/fd/v1?bpb=ChAKDnNlYXJjaC5UQUNUSUxFEloKTAlnqWnCPq4SaxFWAZtg-PjYbRo4CxDThbhCGi8aLQoWChQKEglnqWnCPq4SaxEnZndM2yWJJRITU3VpdGUgMTkwNCBMZXZlbCAxOQwqCg0AAAAAFQAAAAAaBgjwARCYAw&gl=AU'
          #  URL <- 'https://geo3.ggpht.com/cbk?panoid=roUHjoCgoaEWmTA0xFoNQA&output=thumbnail&cb_client=search.TACTILE.gps&thumb=2&w=408&h=240&yaw=278.6541&pitch=0&thumbfov=100'
          
          filteredPostcode2 <- as.data.frame(filteredPostcode2)
          leaflet(data=filteredPostcode2) %>% 
            addTiles() %>%  
            addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
            #addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", file, ">"))%>%
            #addMarkers(~Longitude,~Latitude,popup = paste0("<img src = " ">"))%>%
            # addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", ~URL, ">"))%>%
            
            
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
                    filter( grepl(input$rbLanguage, Language ) | grepl(input$rbLanguage, Culture ))
                    #)
                }
                filteredClickLan <- as.data.frame(filteredClickLan)
                # file <- "LogoBlue3.png"
                # file <- 'https://geo2.ggpht.com/maps/photothumb/fd/v1?bpb=ChAKDnNlYXJjaC5UQUNUSUxFEloKTAlnqWnCPq4SaxFWAZtg-PjYbRo4CxDThbhCGi8aLQoWChQKEglnqWnCPq4SaxEnZndM2yWJJRITU3VpdGUgMTkwNCBMZXZlbCAxOQwqCg0AAAAAFQAAAAAaBgjwARCYAw&gl=AU'
                #URL <- 'https://geo3.ggpht.com/cbk?panoid=roUHjoCgoaEWmTA0xFoNQA&output=thumbnail&cb_client=search.TACTILE.gps&thumb=2&w=408&h=240&yaw=278.6541&pitch=0&thumbfov=100'
                
                #centersfiltered <- filteredPostcode2  %>% filter(No == input$table_rows_selected)
                # img(id="img26",src = "allReligion.png", height = 40, width=80),
                
                leaflet(data=filteredClickLan) %>% 
                  addTiles() %>%  
                  addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
                  #  addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", file, ">"))%>%
                  # addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", ~URL ,">"))%>%
                  
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
                #URL <- 'https://geo3.ggpht.com/cbk?panoid=roUHjoCgoaEWmTA0xFoNQA&output=thumbnail&cb_client=search.TACTILE.gps&thumb=2&w=408&h=240&yaw=278.6541&pitch=0&thumbfov=100'
                #centersfiltered <- filteredPostcode2  %>% filter(No == input$table_rows_selected)
                #src = "allReligion.png", height = 40, width=80
                #input$sitemap_marker_click$lat & Longitude == input$sitemap_marker_click$lng
                ### hear to show pic of place####
                leaflet(data= filteredClickreli2) %>% addTiles() %>% 
                  addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
                  #addTiles() %>%  addMarkers(~Longitude,~Latitude,popup =~paste0("<img src = ", URL, ">"))
                  
                  addEasyButton(easyButton(
                    icon="fa-crosshairs", title="Locate Me",
                    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
                
                #addMarkers(lng = ~Longitude, ~Latitude, popup = 
                #            ~paste0('https://www.google.com/maps/dir/', 
                #                   ~Latitude,",",~Longitude,"/",
                #                  ~Latitude,",", 
                #                 ~Longitude))%>%
                #addCircles(lng =  ~Longitude, lat = ~Latitude, popup = ~Name, label = "current location", color = "red", weight = 15)
              })
            # addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
            #  addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", file, ">"))%>%
            #  addMarkers(~Longitude,~Latitude,popup = paste0("<img src = ", ~URL ,">"))%>%
            
          })
          
          
        })
    })
  })
  #})
  
  #click marker dt
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
          searching= FALSE,
          #columnDefs = list(list(targets = c(0,1,8,9,10,11,12), searchable = FALSE)),
          columnDefs = list(list(visible=FALSE, targets=c(10,11,12))),
          scrollX = TRUE,
          dom = 'Bfrtp',
          pageLength=5,
          buttons = c('copy', 'pdf', 'print'),deferRender = TRUE,
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
        
        formatStyle('Website', backgroundColor = 'white', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")%>%
        formatStyle('ID', backgroundColor = 'lightblue', fontWeight = 'bold',`font-size` = '18px',font="Times New Roman")
    })
    
    #Gmap
    #  output$myGMap <- renderGoogle_map({
    #   google_map(location = c(input$sitemap_marker_click$lat, input$sitemap_marker_click$lng), key = key, search_box = T,zoom = 12, split_view = "pano", street_view_control = TRUE,update_map_view = TRUE)
    #  })
    
    #https://maps.google.com/maps/contrib/105726604263661011813/photos
    
    
    #religion 
  }) 
  
  
  
  
}

#language


# map$clearPopups()
# map$showPopup(click$latitude, click$longtitude, text)





# language





shinyApp(ui, server)



