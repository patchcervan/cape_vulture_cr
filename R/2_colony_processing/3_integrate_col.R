# 17-01-2021

rm(list = ls())

library(tidyverse)
library(sf)

# In this script we will integrate all colony data with the new updates in a single file


# Load original data ------------------------------------------------------

# Read in count data (location and raw counts of colonies that have ever been counted)
col_counts <- read_csv("data/working/col_counts_locs.csv")

# Location data (location of all colonies and roosts from original data sets)
col_locs_all <- read_csv("data/working/colony_data_join.csv")

# Count summary (location and avg historic counts for colonies that have been counted)
count_summ <- read_csv("data/working/col_count_summary.csv")


# CORRECTIONS -------------------------------------------------------------

# We have noticed that there is a roost called Nooitgedacht just as the colony 
# next to Pretoria. The breeding count was allocated to this roost rather than to
# the colony.
col_counts %>% 
   filter(name_new == "Nooitgedacht") %>% 
   print(n = Inf)

# Correct coordinates
cc <- col_locs_all %>% 
   filter(name == "Nooitgedacht" & type == "Breeding") %>%
   dplyr::select(lon, lat)

col_counts <- mutate(col_counts,
                     lon = if_else(name_new == "Nooitgedacht", cc[[1,1]], lon),
                     lat = if_else(name_new == "Nooitgedacht", cc[[1,2]], lat),
                     type = if_else(name_new == "Nooitgedacht", "breed", type))

# Mark roost as not ever counted
col_locs_all <- mutate(col_locs_all,
                       counted = if_else(name == "Nooitgedacht" & type == "Roost", 0, counted))

# The coordinates of Robert's farm are wrong on our data base
col_counts %>% 
   filter(name_new == "Robert's Farm") %>% 
   print(n = Inf)

# The correct coordinates we extract them from Vulpro layer using QGIS
col_counts <- mutate(col_counts,
                     lon = if_else(name_new == "Robert's Farm", 27.307515, lon),
                     lat = if_else(name_new == "Robert's Farm", -25.832808, lat))

# Mzimkulu ----------------------------------------------------------------

# Andy Ruffle mentions that the last accurate count at the Umzimkulu/Oribi colony
# was done on 7th July 2017 using  a drone. We recorded 94 nests,
# indicating at least 188 adults
sort(unique(col_counts$name_new))

col_counts %>% 
   filter(name_new == "Mzimkhulu/Oribi") %>% 
   print(n = Inf)
# We seem to have this record

# What is the avg for this colony?
sort(unique(count_summ$name_new))

count_summ %>%
   filter(name_new == "Mzimkhulu/Oribi")
# The average is about 81 birds over the years and the calculation seems correct

# From Dana, Andy and Mike
# the location of Topia Waterfall corresponds to Mzimkhulu/Oribi, whereas there is
# no known colony at the current location of Mzimkhulu/Oribi

count_summ %>% 
   filter(name_new %in% c("Mzimkhulu/Oribi", "Topia Waterfall"))

# Correct coordinates
cc <- count_summ %>% 
   filter(name_new == c("Topia Waterfall")) %>% 
   dplyr::select(lon, lat)

# Change coordinates at Mzimkhulu (FIX ONLY THE SUMMARY TABLE)
count_summ <- mutate(count_summ,
                     lon = if_else(name_new == "Mzimkhulu/Oribi", cc[[1,1]], lon),
                     lat = if_else(name_new == "Mzimkhulu/Oribi", cc[[1,2]], lat))

# Remove Topia Waterfall
count_summ <- filter(count_summ,
                     name_new != "Topia Waterfall")

# There is another colony called Mzimkhuku close to Durban which is a bit suspicious
# check with Sonja.


# Botswana ----------------------------------------------------------------

# Glyn Maude sent recent counts for Bonwalenong and Manong Yeng.
col_counts %>% 
   filter(name_new == "Bonwalenong")

# We don't have counts for Bonwalenong, so it must be added
cc <- col_locs_all %>% filter(name == "Bonwalenong") %>% dplyr::select(lon, lat)

col_counts <- rbind(col_counts,
                    c(2017,"Bonwalenong",NA,NA,378,NA,NA,NA,NA,NA,1,"Bonwalenong",NA,cc[[1,1]],cc[[1,2]],"breed", 378, NA),
                    c(2018,"Bonwalenong",NA,NA,572,NA,NA,NA,NA,NA,1,"Bonwalenong",NA,cc[[1,1]],cc[[1,2]],"breed", 572, NA))

# How about Manong Yeng
col_counts %>% 
   filter(name_new == "Manong Yeng")
# We have counts but not the recent ones

cc <- col_counts %>% filter(name_new == "Manong Yeng") %>% dplyr::select(lon, lat) %>% slice(1)

col_counts <- rbind(col_counts,
                    c(2017,"Manong Yeng",NA,NA,144,NA,NA,NA,NA,NA,1,"Manong Yeng",NA,cc[[1,1]],cc[[1,2]],"breed", 144, NA),
                    c(2018,"Manong Yeng",NA,NA,152,NA,NA,NA,NA,NA,1,"Manong Yeng",NA,cc[[1,1]],cc[[1,2]],"breed", 152, NA))


# Namibia -----------------------------------------------------------------

# Based on comments by John Mendelsohn we should allocate a testimonial number of
# vultures to the Waterberg colony to account for the fact that some individuals
# still roost and possibly breed at the site
col_counts %>% 
   filter(name_new == "Waterberg")

# We have no counts for the Waterberg colony. Find coordinates
cc <- col_locs_all %>% 
   filter(name == "Waterberg") %>%
   dplyr::select(lon, lat)

col_counts <- rbind(col_counts,
                    c(NA,"Waterberg",NA,NA,10,NA,NA,NA,NA,NA,1,"Waterberg",NA,cc[[1,1]],cc[[1,2]],"breed", 10, NA))


# Vulpro updates ----------------------------------------------------------

# Vulpro sent updates for the following colonies
col_counts %>% 
   filter(name_new %in% c("Kransberg", "Mannyelong", "Manoutsa", "Moletjie", "Soutpansberg", "Nooitgedacht", "Skeerpoort", "Robert's Farm")) %>% 
   print(n = Inf)

# We've got all colonies, just need to update numbers
cc <- col_counts %>% 
   filter(name_new %in% c("Kransberg", "Mannyelong", "Manoutsa", "Moletjie", "Soutpansberg", "Nooitgedacht", "Skeerpoort", "Robert's Farm")) %>% 
   dplyr::select(name_new, lon, lat) %>% 
   distinct()

col_counts <- rbind(col_counts,
                    c(2020,"Kransberg",NA,NA,1598,NA,NA,NA,NA,NA,1,"Kransberg",NA,cc[[1,2]],cc[[1,3]],"breed", 1598, NA),
                    c(2019,"Mannyelong",NA,NA,178,NA,NA,NA,NA,NA,1,"Mannyelong",NA,cc[[2,2]],cc[[2,3]],"breed", 178, NA),
                    c(2020,"Manoutsa",NA,NA,1460,NA,NA,NA,NA,NA,1,"Manoutsa",NA,cc[[3,2]],cc[[3,3]],"breed", 1460, NA),
                    c(2020,"Moletjie",NA,NA,10,NA,NA,NA,NA,NA,1,"Moletjie",NA,cc[[4,2]],cc[[4,3]],"breed", 10, NA),
                    c(2020,"Nooitgedacht",NA,NA,354,NA,NA,NA,NA,NA,1,"Nooitgedacht",NA,cc[[5,2]],cc[[5,3]],"breed", 354, NA),
                    c(2019,"Robert's Farm",NA,NA,0,NA,NA,NA,NA,NA,1,"Robert's Farm",NA,cc[[6,2]],cc[[6,3]],"breed", 0, NA),
                    c(2020,"Skeerpoort",NA,NA,720,NA,NA,NA,NA,NA,1,"Skeerpoort",NA,cc[[7,2]],cc[[7,3]],"breed", 720, NA),
                    c(2019,"Soutpansberg",NA,NA,450,NA,NA,NA,NA,NA,1,"Soutpansberg",NA,cc[[8,2]],cc[[8,3]],"breed", 450, NA))


# Update count summary ----------------------------------------------------

# Recover column types
col_counts <- type_convert(col_counts, col_types = spec(col_counts))

col_counts <- col_counts %>% 
   mutate(juv_p = ifelse(is.na(juv), round(0.66*ad_p), juv))

# In some colonies only total counts are available, we must divide this
# into adults and juveniles
col_counts <- col_counts %>% 
   mutate(ad_p = if_else(is.na(ad_p), round(0.53*total), ad_p),
          juv_p = if_else(is.na(juv_p), round(0.32*total), juv_p))

# Calculate average number of adults and juveniles per colony
count_summ <- col_counts %>% 
   group_by(name_new, lon, lat, type) %>% 
   summarize(avg_ad = mean(ad_p, na.rm = T),
             avg_juv = mean(juv_p, na.rm = T),
             names_old = paste(unique(name), collapse = ",")) %>% 
   ungroup()


# Join counts with other locations data -----------------------------------

# Remove counts without coordinates
count_summ <- count_summ %>% 
   filter(!is.na(lon))

# Remove locations that have been counted from the locs data 
# (already present in the count data)
col_locs <- col_locs_all %>% 
   filter(counted == 0)

# Join both locations and locations with counts
col_join <- rbind(
   col_locs %>% 
      mutate(avg_ad = NA, avg_juv = NA, names_old = NA, counted = 0,
             type = if_else(type %in% c("Roost", "Roosting"), "roost", "breed")) %>% 
      dplyr::select(name, lon, lat, avg_ad, avg_juv, names_old, counted, type),
   count_summ %>% 
      dplyr::select(name = name_new, lon, lat, avg_ad, avg_juv, names_old, type) %>% 
      mutate(counted = 1)
)

# Replace any non UTF-8 in colony names by '#'
col_join <- col_join %>% 
   mutate(name = iconv(name, "UTF-8", "UTF-8", sub = '#'))

# Save data temporarily to visualize in QGIS
write_csv(col_join, "output/temp_col_data.csv")


# Include Vulpro roost and colony data ------------------------------------

# Load Vulpro colony data
col_v <- st_read("data/raw/colony_data/colony_updates/vulpro_feedback/All Colonies.kml")

# Load Vulpro roost data
roost_v <- st_read("data/raw/colony_data/colony_updates/vulpro_feedback/CV Roosts.kmz")

# Join these two layers
total_v <- rbind(col_v %>% 
                    transmute(name = "unnamed",
                              type = "breed",
                              lon = st_coordinates(.)[,1],
                              lat = st_coordinates(.)[,2]) %>% 
                    st_drop_geometry(),
                 roost_v %>% 
                    transmute(name = "unnamed",
                              type = "roost",
                              lon = st_coordinates(.)[,1],
                              lat = st_coordinates(.)[,2]) %>% 
                    st_drop_geometry())

# Join the resulting layer with the existing roost and colony layer:
# First add the necessary columns to the Vulpro data
total_v <- total_v %>% 
   mutate(avg_ad = NA,
          avg_juv = NA,
          names_old = NA,
          counted = 0) %>% 
   dplyr::select(names(col_join))

# Then create a spatial object and find the intersection with a 200m buffer from
# existing sites (pre-Vulpro)
source("R/functions/meterToDegree.R")

join_buff200 <- st_as_sf(col_join, coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
   st_union() %>% 
   st_buffer(dist = meterToDegree(meters = 300))
# Warning is fine we don't want precision

int <- st_as_sf(total_v, coords = c("lon", "lat"), crs = 4326, remove = F) %>%
   st_intersects(join_buff200)

# Combine existing colonies with the new non-overlapping colonies
col_join <- rbind(col_join,
                  filter(total_v, sapply(int, is_empty)))

# Create an id for the different sites
col_join <- mutate(col_join,
                   id = paste0("cvcol", case_when(row_number()<10 ~ paste0("00", row_number()),
                                                  row_number()>99 ~ as.character(row_number()),
                                                  TRUE ~ paste0("0", row_number())))
                   )

# Save data temporarily to visualize in QGIS
write_csv(col_join, "output/temp_col_data.csv")

# Save the final version
write_csv(col_join, "data/working/colony_data_all_upt.csv")
