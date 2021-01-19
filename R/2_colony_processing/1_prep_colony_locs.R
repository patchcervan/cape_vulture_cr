# 30-10-2020

# In this script we explore, prepare and combine colony locations
# obtained from different sources

rm(list = ls())

library(tidyverse)
library(sf)


# Read in data ------------------------------------------------------------

col1 <- read_csv("data/raw/colony_data/ewt_cv_colonies_20200601.csv")
col2 <- read_csv("data/raw/colony_data/ez_cv_nests_roosts_Oct2020.csv")


# Explore both data sets --------------------------------------------------

# Remove extra columns in the second data set
col2 <- col2[1:6]

# Plot both data sets
plot(col1$longitude, col1$latitude)
plot(col2$X_COORD, col2$Y_COORD)

# Seems like some of the colonies in the second data set are really far away
col2[col2$X_COORD < 0,]

# Perhaps the coordinates were flipped?
col2 <- col2 %>% 
      mutate(X_COORDp = if_else(X_COORD < 0, Y_COORD, X_COORD),
             Y_COORDp = if_else(X_COORD < 0, X_COORD, Y_COORD))

plot(col2$X_COORDp, col2$Y_COORDp)

# There is still one colony that appear to be in the Northern hemisphere
# Perhaps change the sign of the Y coordinate
col2$Y_COORDp[which(col2$Y_COORDp > 0)] <- -1*col2$Y_COORDp[which(col2$Y_COORDp > 0)]

plot(col2$X_COORDp, col2$Y_COORDp)

# Plot both data sets in the same plot
plot(col1$longitude, col1$latitude)
points(col2$X_COORDp, col2$Y_COORDp, pch = 19, col = "red")

# Mark the coordinates that have been changed and
# save data with new coordinates
col2 <- col2 %>% 
      mutate(changed = if_else(Y_COORD == Y_COORDp, 0, 1))

write_csv(col2, "data/working/ez_colony_fixed.csv")

# Are there any names in common?
col1$Location
col2$Site

col1$Location[col1$Location %in% col2$Site]


# Check if any of the colonies overlap in space ---------------------------

# Create an ID for the colonies in the second dataset
col2$id <- paste0("col_", 1:nrow(col2))

# Make spatial objects
col1 <- st_as_sf(col1, coords = c("longitude", "latitude"), crs = 4326, remove = F)
col2 <- st_as_sf(col2, coords = c("X_COORDp", "Y_COORDp"), crs = 4326, remove = F)

# Make buffer of 500 m around colonies in first data set
col1buf500 <- st_buffer(col1, dist = 0.005)

# Find colonies in the second data set that overlap the 1km buffer
col2_int <- st_intersection(col2, col1buf500)

col2_int %>% 
      select(Site, X_COORDp, Y_COORDp, Location.1, longitude, latitude) %>% 
      print(n = Inf)

# The amount of colonies that overlap depends greatly on the buffer distance
# Because it is difficult to establish whether two colonies are actually the same
# we keep all of them for distance calculations. For predicting we will use only
# those colonies that have reliable counts (this is prepared in a different script)


# Join both datasets for distance calculations ----------------------------

col_join <- rbind(col1 %>% 
                        dplyr::select(id = ID_key, name = Location, lon = longitude, lat = latitude, type = ColonyType),
                  col2 %>% 
                        dplyr::select(id, name = Site, lon = X_COORDp, lat = Y_COORDp, type = status))

write_csv(col_join, "data/working/colony_data_join.csv")
