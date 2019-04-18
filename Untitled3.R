library(shiny)
library(DT)
data("mtcars")

centers <- read.csv('test2_copy.csv')

ui <- shinyUI(
  fluidRow(
    DT::dataTableOutput("myDatatable"),
    verbatimTextOutput("selectedCells"),
    textOutput("celltext")
  )
)

server <- shinyServer(function(input, output, session) {
  output$myDatatable <- DT::renderDataTable(centers, 
                                            selection=list(mode="single"),
                                            server = FALSE,
                                            rownames=FALSE
                                            )
  
  output$selectedCells <- renderPrint(input$myDatatable_rows_selected)
  output$celltext <- renderText({
    row <- input$myDatatable_rows_selected
    centers <- centers[row,3]
  })
})

shinyApp(ui, server)