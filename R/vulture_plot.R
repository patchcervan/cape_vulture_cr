# 23-09-2020

# This script is used for plotting vulture tracks

library(tidyverse)
library(ggmap)
library(gganimate)

rm(list = ls())

# Load vulture database
bird_db <- read_csv("data/working/bird_db.csv")


# Load bird track
id_sel <- "mb01"

trk <- read_csv(paste0("data/working/bird_tracks/", id_sel, ".csv"))

bird_db %>% 
    filter(bird_id == id_sel)

# Set background map
mapbbox <- c(min(trk$lon)-0.2, min(trk$lat)-0.2, max(trk$lon)+0.2, max(trk$lat)+0.2)

basemap <- get_stamenmap(bbox = mapbbox, zoom = 7)


# Plot
vulplot <- ggmap(basemap) +
    geom_point(data = trk, aes(x = lon, y = lat, colour = datetime), size = 1, alpha = 0.2, inherit.aes = FALSE) +
    # ggtitle(unique(trk$bird_id))
    ggtitle(bird_db %>% filter(bird_id == id_sel) %>% pull(name))

ggsave(vulplot, file = paste0("figures/vul_plots/", id_sel, ".png"), dpi = 500)



# Animated plot
vulplot <- ggmap(basemap) +
    geom_point(data = trk, aes(x = lon, y = lat), inherit.aes = FALSE) +
    transition_time(trk$datetime) +
    shadow_wake(wake_length = 0.1, size = 1.5, wrap = F) +
    ggtitle("Date and time: {frame_time}")