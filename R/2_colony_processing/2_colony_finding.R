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
colonies <- read_csv("data/working/colony_data_join.csv") %>% 
    # Make a spatial object
    st_as_sf(coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE)

# Read in bird tracking files
trk_files <- list.files("data/working/bird_tracks/in_process/", pattern = ".rds")


# Select one track ---------------------------------------------------------

for(i in 1:length(trk_files)){
    
    # Load one bird
    trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i]))
    
    id_sel <- unique(trk$bird_id)
    
    print(id_sel)
    
    # Check if track has been processed or it was excluded early on
    if(length(attr(trk, "fine")) == 0){
        print("This bird was excluded")
        next
    }
    
    # Tracking period
    print(range(trk$datetime))
    
    # Tracking years
    trk <- trk %>% 
        mutate(year = lubridate::year(datetime))
    
    print(
        trk %>% 
            group_by(year) %>% 
            summarize(n = n()/nrow(trk))
    )
    
    # Plot by year
    # ggplot() +
    #     geom_sf(data = sa_map) +
    #     geom_point(data = trk, aes(x = lon, y = lat), alpha = 0.2) +
    #     facet_wrap(~year)
    
    
    # Find highest density points for each tracking year ----------------------
    
    # Nest spatial object by year
    trk <- trk %>% 
        st_as_sf(coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE) %>%
        nest(data = -year)
    
    # Locate colony for each year based on kernel density
    source("R/functions/findColony.R")
    
    trk <- trk %>% 
        mutate(colony = future_map(trk$data, ~findColony(.x, coords = c("lon", "lat"),
                                                         timevar = "datetime", bw = 0.1, plotkde = F)))
    
    trk_col <- unnest(trk, cols = c(year, colony)) %>% 
        dplyr::select(year, geometry) %>% 
        st_as_sf()
    
    # Plot by year
    # ggplot() +
    #     geom_sf(data = sa_map) +
    #     geom_point(data = unnest(trk, cols = c(year, data)), aes(x = x_, y = y_), alpha = 0.2) +
    #     geom_sf(data = trk_col, colour = "red", size = 2) +
    #     ggtitle(paste0(id_sel, " colonies")) +
    #     facet_wrap(~year)
    
    
    # Find closest known colonies ---------------------------------------------
    
    # Compute distance between empirical colonies and known colonies
    dist_col <- st_distance(trk_col, colonies, by_element = F)
    
    trk_col <- trk_col %>% 
        mutate(min_dist = apply(dist_col, 1, min))
    
    known_col <- colonies %>% 
        slice(apply(dist_col, 1, which.min)) %>% 
        mutate(year = trk_col$year,
               lon = st_coordinates(.)[,1],
               lat = st_coordinates(.)[,2],)
    
    # Unnest trk data frame
    trk <- trk %>% 
        unnest(cols = c(year, data))
    
    plotdata <- tibble(
        year = c(slice(trk, 1) %>% pull(year),
                 pull(trk_col, year),
                 pull(known_col, year)),
        x = c(slice(trk, 1) %>% pull(lon),
              st_coordinates(trk_col)[,1],
              pull(known_col, lon)),
        y = c(slice(trk, 1) %>% pull(lat),
              st_coordinates(trk_col)[,2],
              pull(known_col, lat)),
        type = c("release", rep("observed", nrow(trk_col)), rep("closest", nrow(trk_col)))
    )
                       
    
    # Save plot
    colPerYear <- ggplot(plotdata) +
        geom_sf(data = sa_map) +
        geom_point(data = trk, aes(x = lon, y = lat), size = 1, alpha = 0.2) +
        geom_point(data = colonies,
                   aes(x = lon, y = lat),
                   shape = 4, col = "green", size = 0.5, alpha = 1) +
        geom_point(aes(x = x, y = y, shape = type, colour = type), size = 4) +
        scale_shape_manual(values = c(0,1,2)) +
        scale_colour_manual(values = c("green", "red", "blue")) +
        ggtitle(paste0(id_sel, " colonies")) +
        facet_wrap(~year)
    
    
    ggsave(colPerYear, filename = paste0("output/colonies/",id_sel , "_colonies.png"), dpi = 700)
    
    
    # Fill in database --------------------------------------------------------
    
    new_entry <- tibble(
        bird_id = id_sel,
        year = unique(trk$year),
        dens_lon = st_coordinates(trk_col)[,1],
        dens_lat = st_coordinates(trk_col)[,2],
        known_lon = st_coordinates(known_col)[,1],
        known_lat = st_coordinates(known_col)[,2],
        dist = trk_col$min_dist/1000,
        known_id = known_col$id
    )
    
    # add new entry to data base
    colony_db <- rbind(colony_db, new_entry)
    
}

write_csv(colony_db, file = "data/working/colony_db.csv")
