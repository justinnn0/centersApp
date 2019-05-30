
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
    
    mainPanel(
      
      br(),
     
      leafletOutput("sitemap"),
      
      DT::dataTableOutput("table"),
      tags$head(tags$style("#table {background-color: white; }", media="screen", type="text/css")),
      h4(em("Click and select a name or address from the above table to search on Google", align = "left",style = "color:lightBlue",font="Times New Roman")),
      fluidRow( 
        column(6, uiOutput("tab"))
    )  
    
  )
  ))

server <- function(input, output) {
  
  # Below is the code for datatable.
  # Observe the event of postcode being selected or entered, then filter the data according to the selection
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
        options = list(
          searching=FALSE,
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
        )   # Formatting each column.
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
      
      # Observe the event of language being selected or entered, then filter the data according to the selection.
      observe({
        
        req(input$rbLanguage)
        
        
        output$table <- DT::renderDataTable({
          
          if ( input$rbLanguage == "English") # No need to filter if English is selected 
          {
            filteredClickLan <- filteredPostcode  
          }
          else
          {
            # Filter by language and culture based on the data filtered by postcode
            filteredClickLan <- filteredPostcode %>%
              filter( grepl(input$rbLanguage, Language ) | grepl(input$rbLanguage, Culture ))              
          }
          
          filteredClickLan <- as.data.frame(filteredClickLan)
          datatable(
            filteredClickLan, 
            selection='single',
            extensions = c('Responsive','Buttons','FixedHeader','Scroller','KeyTable','FixedColumns'),
            rownames=FALSE,
            options = list(
              searching= FALSE,
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
      
      # Observe the event of religion being selected or entered, then filter the data according to the selection.
      
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
            options = list(
              searching= FALSE,
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
      
 # Observe the event of a cell in the datatable being selected, then pass the value in the cell to google search
      observe({
        req(input$table_cell_clicked)
        output$selectedCells <- renderPrint(input$table_cell_clicked$value)
        sstring <- paste("https://www.google.com/search?cr=countryAU&q=", as.character(input$table_cell_clicked$value))
        
       url <- a(as.character(input$table_cell_clicked$value), href= sstring,target="_blank")
        
        output$tab <- renderUI({
          tagList( HTML("<h3 style='color:white; align ='center'>Search on Google</h3>"), url)
        }) 
        
        sstring2 <- paste("https://www.google.com/maps/place/", as.character(input$table_cell_clicked$value))
        url2 <- a(as.character(input$table_cell_clicked$value), href= sstring2, target="_blank")
        output$tabggmap <- renderUI({
          tagList(HTML("<h3 style='color:white; align ='center'>Search on Google Map</h3>"), url2)
        })
        
      })
      
    })
    # Below is the code for the map, the same filtering approaches as above.
    
    # Observe the event of postcode being selected, then filter the data accordingly.
    
    observe({
      
      req(input$Postcode)
      
      output$sitemap = renderLeaflet(
        {
          filteredPostcode2 <- centers %>%
            filter(
              Postcode == input$Postcode)
          filteredPostcode2 <- as.data.frame(filteredPostcode2)
          leaflet(data=filteredPostcode2) %>% 
            addTiles() %>%  
            addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
            addEasyButton(easyButton(
              icon="fa-crosshairs", title="Locate Me",
              onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
          
          
  # Observe the event of language being selected, then filter the data accordingly.

          observe({
            req(input$rbLanguage)
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
                }
                filteredClickLan <- as.data.frame(filteredClickLan)
                
                leaflet(data=filteredClickLan) %>% 
                  addTiles() %>%  
                  addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
                  
                  addEasyButton(easyButton(
                    icon="fa-crosshairs", title="Locate Me",
                    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
              })
            
            
          })
          
# Observe the event of religion being selected, then filter the data accordingly.
          
          observe({
            req(input$rbreligion)
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
                leaflet(data= filteredClickreli2) %>% addTiles() %>% 
                  addMarkers(~Longitude,~Latitude,popup=~as.character(Name))%>%
                  addEasyButton(easyButton(
                    icon="fa-crosshairs", title="Locate Me",
                    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
              })
          })
          
          
        })
    })
  })
  
  # Observe the event of a maker on the map being clicked, then show the data on the datatable.
  
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
        options = list(
          searching= FALSE,
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
  
  
  
  
}

shinyApp(ui, server)



