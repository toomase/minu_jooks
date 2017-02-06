library(tidyverse)
library(lubridate)
library(sf)
library(sp)

load("~/Dropbox/DataScience/R/sport/output/track.rdata")
load("~/Dropbox/DataScience/R/sport/output/trackpoints.rdata")

glimpse(trackpoints)

install.packages("padr")

trackpoints %>% 
  filter(id %in% c("177954775", "420051278")) %>% 
  
  
  



  filter(sport == "Running") %>% 
  sample_n(2)
  
  distinct(sport, id) %>% 
  count(sport)

df <- tr %>% na.omit()
data(meuse)
coordinates(df) <- ~long+lat
proj4string(df) <- CRS("+proj=longlat")
df_sf <- st_as_sf(df)
class(meuse)   
tr <-  trackpoints %>% 
  mutate(jrk = as.character(row_number())) %>% 
  filter(stringr::str_detect(jrk, "0$")) %>% 
  
  gganimate::
  tr <- tr %>% 
  filter(id == "473625933")  
library(gganimate)   
p <- tr %>% 
  filter(id == "473625933") %>% 
  # head(100) %>% 
  ggplot(aes(x = lat, y = long, frame = jrk)) +
  geom_path(aes(cumulative = TRUE, group = id))

p <- ggmap(tallinna_kaart) +
  geom_path(data = tr, aes(x = long, y = lat, frame = jrk, cumulative = TRUE, 
                           group = id), color = "red")
animation::ani.options(interval = 1/20)

gganimate(p, "output.mp4", ani.width = 800, 
          ani.height = 800)

library(ggmap)
tallinn <- c(left = 24.558454, bottom = 59.342081, right = 24.953275, 
             top = 59.511817)

# Tallinna aluskaart
tallinna_kaart <- get_map("tallinn", zoom = 12, maptype = "toner-lite")
?get_map
ggmap(tallinna_kaart) +  # alusaakrt tumedat tooni 
  geom_path(data = gps, aes(x = lng, y = lat, group = factor(id)), 
            colour = "#FFFF33", alpha = 0.02) 

# Animate with gganimate
p <- ggplot(data=tf, aes(x=x, y=y)) + 
  geom_text(aes(label = label, frame = .frame), data=tweenlogo, size = 13) + 
  geom_point(aes(frame = .frame, size=size, alpha = alpha, colour = colour)) + 
  scale_colour_identity() + 
  scale_alpha(range = c(0, 1), guide = 'none') +
  scale_size(range = c(4, 60), guide = 'none') + 
  expand_limits(x=c(-0.36, 1.36), y=c(-0.36, 1.36)) + 
  theme_bw()
animation::ani.options(interval = 1/50)
gg_animate(p, "dancing ball.gif", title_frame = F, ani.width = 400, 
           ani.height = 400) 

p3 <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
  geom_path(aes(cumulative = TRUE, group = country)) +
  scale_x_log10() +
  facet_wrap(~continent)

gganimate(p3, "output.gif")
?gganimate
trackpoints$jrk <- seq(1:5)
install.packages("magick")

getwd()

df_sf %>% 
  filter(id == "309804596") %>% 
  # fortify() %>%
  # as.data.frame() %>% 
  # class()
  as("Spatial") %>% 
  # df %>% 
  # head(1000) %>%
  mapview::mapview()
mapview(df_sf, native.crs = TRUE)
class(df_sf)
class(meuse_sf)

install.packages("gganimate")
devtools::install_github("dgrtwo/gganimate")









glimpse(trackpoints)
glimpse(track)

track %>%
    ungroup() %>%
    filter(sport %in% c("Running", "Race cycling")) %>%
    group_by(sport, aasta = year(start_date)) %>%
    summarise(distants = sum(distance)/1000) %>%
    ungroup() %>%
    ggplot(aes(x = aasta, y = distants)) + geom_bar(stat = "identity") +
    facet_grid(sport ~ .)

track %>%
    ungroup() %>%
    filter(sport == "Running") %>%
    group_by(aasta = year(start_date), kuu = month(start_date)) %>%
    summarise(distants = sum(distance)/1000, arv = n()) %>%
    ungroup() %>%
    ggplot(aes(x = factor(kuu), y = arv)) + geom_bar(stat = "identity") +
    facet_grid(aasta ~ .) + geom_text(aes(label = arv), colour = "white", 
                                      vjust = 1, size = 3) 


# Basic points plot
trackpoints %>%
        filter(sport == "Running", year(date) == 2015) %>%
        plot(trackpoints$lat, trackpoints$long, type = "n")
        ggplot(aes(x = lat, y = long)) + 
        geom_point(size = 1) +
        xlim(59.43, 59.47) + ylim(24.565, 24.85)
        
        trackpoints %>% 
          mapview::mapview()
        ?mapview
        
        
        library(sf)
        library(sp)
        library(dplyr)
        df <- tr %>% na.omit()
        data(meuse)
        coordinates(df) <- ~long+lat
        proj4string(df) <- CRS("+proj=longlat")
        df_sf <- st_as_sf(df)
 class(meuse)   
tr <-  trackpoints %>% 
   mutate(jrk = as.character(row_number())) %>% 
   filter(stringr::str_detect(jrk, "0$")) %>% 
  
  gganimate::
tr <- tr %>% 
  filter(id == "473625933")  
library(gganimate)   
p <- tr %>% 
  filter(id == "473625933") %>% 
  # head(100) %>% 
  ggplot(aes(x = lat, y = long, frame = jrk)) +
  geom_path(aes(cumulative = TRUE, group = id))

p <- ggmap(tallinna_kaart) +
  geom_path(data = tr, aes(x = long, y = lat, frame = jrk, cumulative = TRUE, 
                           group = id), color = "red")
animation::ani.options(interval = 1/20)

gganimate(p, "output.mp4", ani.width = 800, 
          ani.height = 800)

library(ggmap)
tallinn <- c(left = 24.558454, bottom = 59.342081, right = 24.953275, 
             top = 59.511817)

# Tallinna aluskaart
tallinna_kaart <- get_map("tallinn", zoom = 12, maptype = "toner-lite")
?get_map
ggmap(tallinna_kaart) +  # alusaakrt tumedat tooni 
  geom_path(data = gps, aes(x = lng, y = lat, group = factor(id)), 
            colour = "#FFFF33", alpha = 0.02) 
 
 # Animate with gganimate
 p <- ggplot(data=tf, aes(x=x, y=y)) + 
   geom_text(aes(label = label, frame = .frame), data=tweenlogo, size = 13) + 
   geom_point(aes(frame = .frame, size=size, alpha = alpha, colour = colour)) + 
   scale_colour_identity() + 
   scale_alpha(range = c(0, 1), guide = 'none') +
   scale_size(range = c(4, 60), guide = 'none') + 
   expand_limits(x=c(-0.36, 1.36), y=c(-0.36, 1.36)) + 
   theme_bw()
 animation::ani.options(interval = 1/50)
 gg_animate(p, "dancing ball.gif", title_frame = F, ani.width = 400, 
            ani.height = 400) 
 
p3 <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
  geom_path(aes(cumulative = TRUE, group = country)) +
  scale_x_log10() +
  facet_wrap(~continent)

gganimate(p3, "output.gif")
?gganimate
 trackpoints$jrk <- seq(1:5)
 install.packages("magick")
 
 getwd()
 
 df_sf %>% 
   filter(id == "309804596") %>% 
   # fortify() %>%
   # as.data.frame() %>% 
   # class()
   as("Spatial") %>% 
   # df %>% 
   # head(1000) %>%
   mapview::mapview()
        mapview(df_sf, native.crs = TRUE)
        class(df_sf)
        class(meuse_sf)
        
        install.packages("gganimate")
        devtools::install_github("dgrtwo/gganimate")
        
        
plot(routes$latitude, routes$longitude, type = "n")
points(routes$latitude, routes$longitude, pch = 20, cex = 0.3, col = "#000000")

track %>%
        group_by(sport) %>%
        summarise(arv = n()) %>%
        arrange(desc(arv))

track %>%
        filter(sport == "Running") %>%
        ggplot(aes(x = total_time, y = distance)) + 
        geom_point() +
        xlim(0, 1500) +
        geom_smooth(method = lm)
        
        group_by(sport) %>%
        summarise(mean(avghr))
        summarise(kalorid = sum(calories))
        