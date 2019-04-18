
## app.R ##
library(shiny)
library(shinydashboard)
library(googleway)
library(DT)

key <- "AIzaSyAOUAfjByh7eSEVrv2ygMRrZo6MHLUB5og"
set_key(key = key)
google_keys()

centers <- read.csv('test2_copy.csv')
mtcars2 = mtcars[, c(1:5, 9)]
mtcars2$am = factor(mtcars$am, c(0, 1), c('automatic', 'manual'))


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    
    fluidRow(
      column(4,
             selectInput("postCode",
                         "postcode:",
                         c("All",
                           unique(as.character(centers$postCode))))
      ),
      column(4,
             selectInput("Religion",
                         "Religion:",
                         c("All",
                           unique(as.character(centers$RELIGION))))
      ),
      column(4,
             selectInput("Culture",
                         "Culture:",
                         c("All",
                           unique(as.character(centers$CULTURE))))
      )
      ,
      column(4,
             selectInput("Address",
                         "Address:",
                         c("All",
                           unique(as.character(centers$Address))))
      )
    ),
    DT::dataTableOutput("table"),
    column(3, verbatimTextOutput('x4')),
    
   
    textOutput("celltext"),
    
    
   
  
    div(
      textInput(inputId = "my_address", label = "")    
      ,textOutput(outputId = "copy_of_address")
      ,HTML(paste0("
                   <script>
                   function initAutocomplete() {
                   new google.maps.places.Autocomplete(
                   (document.getElementById('my_address')),
                   {types: ['geocode']}
                   );
                   }
                   </script>
                   <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete'
                   async defer></script>
                   "))
      ,google_mapOutput(outputId = "my_map")
      ),
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
      )
      )







server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  my_address <- reactive({
    #input$my_address
    #centers[['Address']][1]
    #input$Address
    #output$celltext
    centers[input$table_cells_selected]
  })
  
  output$copy_of_address <- renderText({
    my_address()
  })
  
#  output$table <- DT::renderDataTable(DT::datatable({
 #   data <- centers
  #  if (input$postCode != "All") {
   #   data <- data[data$postCode == input$postCode,]
  #  }
    
   # data
#  }))
  
  #
  output$table <- DT::renderDataTable(centers, 
                                            selection=list(mode="single",target="cell"),
                                            server = FALSE,
                                            rownames=FALSE
  )
  
  #output$selectedCells <- renderPrint(input$table_cells_selected)
  output$celltext <- renderText({
    #cell <- input$table_cells_selected
    centers <- centers[input$table_cells_selected]
  })
  
  #
  
  selectedRow <- eventReactive(input$table_rows_selected,{
    row.names(centers)[c(input$table_rows_selected)]
  })
  
  output$selected <- renderText({ 
    selectedRow()
  })
  
  output$address <- renderText({ 
    table['Address']
  })
  output$x4 = renderPrint({
    s = input$table_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s['Address'])
      cat(s, sep = ', ')
      
    }
  })
  
  output$my_map <- renderGoogle_map({
    my_address <- my_address()
    validate(
      need(my_address, "Address not available")
    )
    
    df <- google_geocode(address = my_address)
    my_coords <- geocode_coordinates(df)
    my_coords <- c(my_coords$lat[1], my_coords$lng[1])
    
    google_map(
      location = my_coords,
      zoom = 12,
      map_type_control = FALSE,
      zoom_control = FALSE,
      street_view_control = FALSE
      
     
    ) 
  })
}

shinyApp(ui, server)

