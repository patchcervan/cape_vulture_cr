# 15-09-2020

# In this script we locate which colony accumulated the most activity
# in each year

rm(list = ls())

library(tidyverse)
library(sf)
library(furrr)



# Load data ---------------------------------------------------------------

# Load colony template
colony_db <- read_rds("data/working/colony_db_template.rds")

# Load basemap
source("R/functions/load_basemap.R")

# Ignore warnings because these are auxiliary maps - precision is not important now.

# Read in colony data
colonies <- read_csv("data/raw/CV_Colonies_20200601.csv") %>% 
    # Make a spatial object
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, dim = "XY", remove = FALSE)

# Read in bird tracking files
bird_files <- list.files("data/working/bird_tracks", pattern = ".csv")


# Select one bird ---------------------------------------------------------

for(i in 3:length(bird_files)){
    
    # Load one bird
    bird <- read_csv(paste0("data/working/bird_tracks/", bird_files[i]))
    
    bird_id <- unique(bird$bird_id)
    
    print(bird_id)
    
    # Tracking period
    print(range(bird$datetime))
    
    # Tracking years
    bird <- bird %>% 
        mutate(year = lubridate::year(datetime))
    
    print(
        bird %>% 
            group_by(year) %>% 
            summarize(n = n()/nrow(bird))
    )
    
    # Plot by year
    # ggplot() +
    #     geom_sf(data = sa_map) +
    #     geom_point(data = bird, aes(x = lon, y = lat), alpha = 0.2) +
    #     facet_wrap(~year)
    
    
    # Find highest density points for each tracking year ----------------------
    
    # Nest spatial object by year
    bird <- bird %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE) %>%
        nest(data = -year)
    
    # Locate colony for each year
    source("R/functions/findColony.R")
    
    bird <- bird %>% 
        mutate(colony = future_map(bird$data, ~findColony(.x, bw = 0.1, plotkde = F)))
    
    bird_col <- unnest(bird, cols = c(year, colony)) %>% 
        dplyr::select(year, geometry) %>% 
        st_as_sf()
    
    # Plot by year
    # ggplot() +
    #     geom_sf(data = sa_map) +
    #     geom_point(data = unnest(bird, cols = c(year, data)), aes(x = lon, y = lat), alpha = 0.2) +
    #     geom_sf(data = bird_col, colour = "red", size = 2) +
    #     ggtitle(paste0(bird_id, " colonies")) +
    #     facet_wrap(~year)
    
    
    # Find closest known colonies ---------------------------------------------
    
    # Compute distance between empirical colonies and known colonies
    dist_col <- st_distance(bird_col, colonies, by_element = F)
    
    bird_col <- bird_col %>% 
        mutate(min_dist = apply(dist_col, 1, min))
    
    known_col <- colonies %>% 
        slice(apply(dist_col, 1, which.min)) %>% 
        mutate(year = bird_col$year,
               lon = st_coordinates(.)[,1],
               lat = st_coordinates(.)[,2],)
    
    # Unnest bird data frame
    bird <- bird %>% 
        unnest(cols = c(year, data))
    
    plotdata <- tibble(
        year = c(slice(bird, 1) %>% pull(year),
                 pull(bird_col, year),
                 pull(known_col, year)),
        x = c(slice(bird, 1) %>% pull(lon),
              st_coordinates(bird_col)[,1],
              pull(known_col, lon)),
        y = c(slice(bird, 1) %>% pull(lat),
              st_coordinates(bird_col)[,2],
              pull(known_col, lat)),
        type = c("release", rep("observed", nrow(bird_col)), rep("closest", nrow(bird_col)))
    )
                       
    
    # Save plot
    colPerYear <- ggplot(plotdata) +
        geom_sf(data = sa_map) +
        geom_point(data = bird, aes(x = lon, y = lat), size = 1, alpha = 0.2) +
        geom_point(data = dplyr::select(colonies, -year),
                   aes(x = longitude, y = latitude),
                   shape = 4, col = "green", size = 0.5, alpha = 1) +
        geom_point(aes(x = x, y = y, shape = type, colour = type), size = 4) +
        scale_shape_manual(values = c(0,1,2)) +
        scale_colour_manual(values = c("green", "red", "blue")) +
        ggtitle(paste0(bird_id, " colonies")) +
        facet_wrap(~year)
    
    
    ggsave(colPerYear, filename = paste0("output/colonies/",bird_id , "_colonies.png"), dpi = 700)
    
    
    # Fill in database --------------------------------------------------------
    
    new_entry <- tibble(
        bird_id = bird_id,
        year = unique(bird$year),
        dens_lon = st_coordinates(bird_col)[,1],
        dens_lat = st_coordinates(bird_col)[,2],
        known_lon = st_coordinates(known_col)[,1],
        known_lat = st_coordinates(known_col)[,2],
        dist = bird_col$min_dist/1000,
        known_id = known_col$ID_key
    )
    
    # add new entry to data base
    colony_db <- rbind(colony_db, new_entry)
    
}

write_csv(colony_db, path = "data/working/colony_db.csv")
