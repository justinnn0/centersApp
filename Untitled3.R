library(ggplot2)
library(maps)
library(ggthemes)
library(readr)
library(dplyr)

url_csv <- 'https://raw.githubusercontent.com/d4tagirl/R-Ladies-growth-maps/master/rladies.csv'
rladies <- read_csv(url(url_csv)) %>% 
  select(-1)

library(DT)

datatable(rladies, rownames = FALSE,
          options = list(pageLength = 5))

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = lon, y = lat, size = followers),
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = lon, y = lat, size = followers),
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')


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

map <- world +
  geom_point(aes(x = lon, y = lat, size = followers, 
                 frame = created_at,
                 cumulative = TRUE),
             data = rladies, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat, size = followers, # this is the init transparent frame
                 frame = created_at,
                 cumulative = TRUE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = lon, y = lat, size = followers, # this is the final transparent frames
                 frame = created_at,
                 cumulative = TRUE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers') 

library(gganimate)
ani.options(interval = 0.2)
gganimate(map)
