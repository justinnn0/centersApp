library(ggplot2)
library(maps)
library(ggthemes)
library(readr)
library(dplyr)
library(animation)
library(gganimate)


#url_csv <- 'https://raw.githubusercontent.com/d4tagirl/R-Ladies-growth-maps/master/rladies.csv'
rladies <- read.csv("dementiaGrowth.csv",encoding="UTF-8") 

rladies$Number <- as.numeric(rladies$Number)

library(DT)

datatable(rladies, rownames = FALSE,
          options = list(pageLength = 5))

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = Lon, y = Lat, size = Number),
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 5), 
                        breaks = c(25000, 50000, 75000, 100000)) +
  labs(size = 'Number')  +
  transition_time(Year) 
map



library(tibble)
library(lubridate)

ghost_points_ini <- tibble(
  created_at = as.Date('2011-09-01'),
  followers = 0, lon = 0, lat = 0)

ghost_points_fin <- tibble(
  created_at = seq(as.Date('2017-05-16'),
                   as.Date('2017-05-30'),
                   by = 'days'),
  followers = 0, lon = 0, lat = 0)

map




ani.options(interval = 0.2)
gganimate(map)



map <- world +
  geom_point(aes(x = lon, y = lat, size = followers, 
                 
                 cumulative = TRUE) + 
               transition_manual(created_at),
             data = rladies, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat, size = followers, # this is the init transparent frame
                 
                 cumulative = TRUE)+transition_manual(created_at),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = lon, y = lat, size = followers, # this is the final transparent frames
                 
                 cumulative = TRUE)+transition_manual(created_at),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers') 

