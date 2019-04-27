library(gapminder)
library(shiny)
library(shinydashboard)
library(googleway) 
library(DT)
library(leaflet)
library(datasets)
library(dplyr)
library(magrittr)

dementiaGrowth <- read.csv("dementiaGrowth.csv",encoding="UTF-8")

dementiaGrowth$Number <- as.numeric(dementiaGrowth$Number)
ui <- fluidPage(
  plotOutput("coolplot"),
  "ddhhd"
  
)

server <- function(input, output) {
  
  output$coolplot <- renderPlot({
ggplot(dementiaGrowth, aes(Year, Number, size = Number, colour = State)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  #facet_wrap(~State) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Year', y = 'Number of dementia patients') +
  transition_time(Year) +
  ease_aes('linear')
    
  })
  
}

shinyApp(ui = ui, server = server)

