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
                       counted = if_else(name == "Nooitgedacht" & type == "Roost", 0, counted),
                       name = if_else(name == "Nooitgedacht" & type == "Roost", "Nooitgedacht_roost", name))

# The coordinates of Robert's farm are wrong on our data base
col_counts %>% 
   filter(name_new == "Robert's Farm") %>% 
   print(n = Inf)

# The correct coordinates we extract them from Vulpro layer using QGIS
col_counts <- mutate(col_counts,
                     lon = if_else(name_new == "Robert's Farm", 27.307515, lon),
                     lat = if_else(name_new == "Robert's Farm", -25.832808, lat))

# We have also noticed that some locations have the same name and only approx.
# the same coordinates
col_locs_all %>% 
   group_by(name) %>% 
   mutate(n = n()) %>% 
   ungroup() %>% 
   filter(n > 1,
          name != "Unnamed") %>% 
   arrange(name) %>% 
   print(n = Inf)

# We will keep the first location only, giving priority to breeding
# Don't do this for "Unnamed" colonies
# First mark all colonies as either breed or roost
col_locs_all <- col_locs_all %>% 
   mutate(type = if_else(type %in% c("Roost", "Roosting"), "roost", "breed"))

temp <- col_locs_all %>% 
   filter(name != "Unnamed") %>% 
   arrange(name, type) %>% 
   group_by(name) %>% 
   slice(1) %>% 
   ungroup()

# Put back unnamed colonies
col_locs_all <- rbind(temp,
                      col_locs_all %>% 
                         filter(name == "Unnamed"))

# colony 34 has been identified and can be removed
col_locs_all <- col_locs_all %>% 
   filter(name != "34")

# Fix some names
col_counts <- col_counts %>% 
   mutate(name_new = if_else(name_new == "Oragn Pipes", "Organ Pipes", name_new))

# Mount Erskine(A) seems to be wrong and has very small count so delete
col_counts <- col_counts %>% 
   filter(name_new != "Mount Erskine(A)")


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

# Change coordinates at Mzimkhulu (fix at counts data frame)
col_counts <- mutate(col_counts,
                     lon = if_else(name_new == "Mzimkhulu/Oribi", cc[[1,1]], lon),
                     lat = if_else(name_new == "Mzimkhulu/Oribi", cc[[1,2]], lat))

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
                    c(2020,"Kransberg",NA,NA,NA,NA,NA,799,NA,NA,1,"Kransberg",NA,cc[[1,2]],cc[[1,3]],"breed", 1598, NA),
                    c(2019,"Mannyelong",NA,NA,NA,NA,NA,89,NA,NA,1,"Mannyelong",NA,cc[[2,2]],cc[[2,3]],"breed", 178, NA),
                    c(2020,"Manoutsa",NA,NA,NA,NA,NA,730,NA,NA,1,"Manoutsa",NA,cc[[3,2]],cc[[3,3]],"breed", 1460, NA),
                    c(2020,"Moletjie",NA,NA,NA,NA,NA,5,NA,NA,1,"Moletjie",NA,cc[[4,2]],cc[[4,3]],"breed", 10, NA),
                    c(2020,"Nooitgedacht",NA,NA,NA,NA,NA,177,NA,NA,1,"Nooitgedacht",NA,cc[[5,2]],cc[[5,3]],"breed", 354, NA),
                    c(2019,"Robert's Farm",NA,NA,NA,NA,NA,0,NA,NA,1,"Robert's Farm",NA,cc[[6,2]],cc[[6,3]],"breed", 0, NA),
                    c(2020,"Skeerpoort",NA,NA,NA,NA,NA,360,NA,NA,1,"Skeerpoort",NA,cc[[7,2]],cc[[7,3]],"breed", 720, NA),
                    c(2019,"Soutpansberg",NA,NA,NA,NA,NA,225,NA,NA,1,"Soutpansberg",NA,cc[[8,2]],cc[[8,3]],"breed", 450, NA))


# KZN updates -------------------------------------------------------------

# Load error file
err_kzn <- readxl::read_xlsx("data/raw/colony_data/colony_updates/errors_Sonja_KZN.xlsx")[-1,]

# Detect names that appear in err_kzn and also in col_counts (fuzzy match)
match <- vector()
for(i in seq_along(err_kzn$name)){
   match <- c(match, agrep(err_kzn$name[i], col_counts$name_new, ignore.case = T, value = T))
}

# Make corrections at colony counts
change <- unique(match)

# First extract coordinates and order same as "change"
cc <- col_counts %>% 
   filter(name_new %in% change) %>% 
   dplyr::select(name_new, lon, lat) %>% 
   distinct()
cc <- cc %>% 
   slice(match(change, cc$name_new))

# Help to locate changes
i=1
col_counts %>% 
   filter(name_new == change[i])

# Fix count data
col_counts <- col_counts %>% 
   mutate(name_new = case_when(name_new == change[1] ~ "Isiwa Samange",
                               name_new == change[3] ~ "Mzimkulu",
                               name_new == change[4] ~ "Thaba Phatsoa",
                               name_new == change[7] ~ "Elephant",
                               str_detect(name_new, "Ndedema") ~ "Ndedema",
                               TRUE ~ name_new),
          lon = case_when(name_new == change[2] ~ "28.081365",
                          name_new == change[6] ~ "29.22",
                          name_new == change[7] ~ "29.12654",
                          name_new == change[13] ~ as.character(col_locs_all$lon[col_locs_all$name == "Mount Lebanon2"]),
                          name_new == "Elephant" ~ as.character(col_counts$lon[col_counts$name == "Elephant Rock"]),
                          name_new == "Ndedema" ~ as.character(col_counts$lon[col_counts$name == "Ndedema Dome"]),
                          TRUE ~ lon),
          lat = case_when(name_new == change[2] ~ "-30.344861",
                          name_new == change[6] ~ "-28.95",
                          name_new == change[7] ~ "-28.97478",
                          name_new == change[13] ~ as.character(col_locs_all$lat[col_locs_all$name == "Mount Lebanon2"]),
                          name_new == "Elephant" ~ as.character(col_counts$lat[col_counts$name == "Elephant Rock"]),
                          name_new == "Ndedema" ~ as.character(col_counts$lat[col_counts$name == "Ndedema Dome"]),
                          TRUE ~ lat),
          type = case_when(name_new == change[10] ~ "breed",
                           TRUE ~ type))

# Add counts if necessary
col_counts <- rbind(col_counts,
                    c(NA,"Isiwa Samange",NA,NA,NA,NA,NA,4,NA,NA,1,"Isiwa Samange",NA, cc[[1,2]],cc[[1,3]],"breed", 8, NA),
                    c(NA,change[2],NA,NA,NA,NA,NA,10,NA,NA,1,change[2],NA,28.081365,-30.344861,"breed", 20, NA),
                    c(NA,change[10],NA,NA,NA,NA,NA,22,NA,NA,1,change[10],NA,cc[[10,2]],cc[[10,3]],"breed", 44, NA))

# Remove counts if necessary
col_counts <- col_counts %>% 
   filter(!(name %in% c("Amphitheater - Ribbon Falls", "Elephant Rock")))

# Long wall might have not been picked up
col_counts %>% 
   filter(name_new %in% c("Long Wall", "Long wall"))

cc <- col_counts %>% 
   filter(name_new == "Long Wall") %>% 
   dplyr::select(lon, lat)

col_counts <- col_counts %>% 
   mutate(lon = if_else(name_new == "Long wall", cc[[1,1]], lon),
          lat = if_else(name_new == "Long wall", cc[[1,2]], lat),
          name_new = if_else(name_new == "Long wall", "Long Wall", name_new))


# REPEAT THE PROCESS FOR THE LOCATIONS WITHOUT COUNTS

# Detect names that appear in err_kzn and also in col_locs (fuzzy match)
match <- vector()
for(i in seq_along(err_kzn$name)){
   match <- c(match, agrep(err_kzn$name[i], col_locs_all$name[col_locs_all$counted == 0],
                           ignore.case = T, value = T))
}

# Make corrections at colony counts
change <- unique(match)

# First extract coordinates and order same as "change"
cc <- col_locs_all %>% 
   filter(name %in% change) %>% 
   dplyr::select(name, lon, lat) %>% 
   distinct() %>% 
   print(n = Inf)
cc <- cc %>% 
   slice(match(change, cc$name))

# Help to locate changes
i=28
col_locs_all %>% 
   filter(name == change[i])

# Remove colonies (Nelson and Fangs will be re-entered later)
col_locs_all <- col_locs_all %>% 
   filter(name != "Babangibone") %>% 
   filter(!str_detect(name, "Nelson")) %>% 
   filter(!str_detect(name, "Fangs")) %>% 
   filter(!(name %in% change[c(4, 7, 10)]))

# Fix location data
col_locs_all <- col_locs_all %>% 
   mutate(name = case_when(name == change[2] ~ "Ingwe",
                           name == change[8] ~ "Injesuthi Triplets",
                           name == change[22] ~ "Lichecheng",
                           name == "Babangibona" ~ "Babangibone",
                           str_detect(name, "Ndedema") ~ "Ndedema",
                           name == "23" ~ "Mphosong",
                           TRUE ~ name),
          counted = case_when(name == change[1] ~ 1,
                              name == change[9] ~ 1,
                              str_detect(name, "ebanon") ~ 1,
                              name == change[8] ~ 1,
                              name == change[11] ~ 1,
                              name == change[12] ~ 1,
                              name == change[13] ~ 1,
                              name == change[14] ~ 1,
                              name == change[15] ~ 1,
                              name == change[16] ~ 1,
                              name == change[17] ~ 1,
                              name == change[19] ~ 1,
                              name == change[20] ~ 1,
                              name == change[22] ~ 1,
                              name == change[25] ~ 1,
                              name == "Babangibone" ~ 1,
                              name == "Ndedema" ~ 1,
                              name == "Mphosong" ~ 1,
                              name == "Elephant" ~ 0,
                              name == "Injesuthi Triplets" ~ 1,
                              name == "Lichecheng" ~ 1,
                              TRUE ~ counted),
          type = case_when(name == change[3] ~ "roost",
                           name == change[19] ~ "breed",
                           name == change[25] ~ "roost",
                           name == "Mphosong" ~ "breed",
                           TRUE ~ type),
          lon = case_when(name == "Mphosong" ~ 28.38803,
                          TRUE ~ lon),
          lat = case_when(name == "Mphosong" ~ -29.07087,
                          TRUE ~ lat))

# Add counts if necessary
col_counts <- rbind(col_counts,
                    c(NA,change[1],NA,NA,NA,NA,NA,500,NA,NA,1,change[1],NA, cc[[1,2]],cc[[1,3]],"breed", 1000, NA),
                    c(NA,"Injesuthi Triplets",NA,NA,NA,NA,NA,10,NA,NA,1,"Injesuthi Triplets",NA, cc[[8,2]],cc[[8,3]],"breed", 20, NA),
                    c(NA,"Mount Erskine",NA,60,NA,NA,NA,NA,NA,NA,1,"Mount Erskine",NA, cc[[9,2]],cc[[9,3]],"roost", NA, NA),
                    c(NA,change[12],NA,NA,NA,NA,NA,30,NA,NA,1,change[12],NA, cc[[12,2]],cc[[12,3]],"breed", 60, NA),
                    c(NA,change[13],NA,NA,NA,NA,NA,2,NA,NA,1,change[13],NA, cc[[13,2]],cc[[13,3]],"breed", 4, NA),
                    c(NA,change[14],NA,NA,NA,NA,NA,10,NA,NA,1,change[14],NA, cc[[14,2]],cc[[14,3]],"breed", 20, NA),
                    c(NA,change[15],NA,NA,NA,NA,NA,5,NA,NA,1,change[15],NA, cc[[15,2]],cc[[15,3]],"breed", 10, NA),
                    c(NA,change[16],NA,NA,NA,NA,NA,5,NA,NA,1,change[16],NA, cc[[16,2]],cc[[16,3]],"breed", 10, NA),
                    c(NA,change[17],NA,NA,NA,NA,NA,2,NA,NA,1,change[17],NA, cc[[17,2]],cc[[17,3]],"breed", 4, NA),
                    c(NA,change[19],NA,NA,NA,NA,NA,18,NA,NA,1,change[19],NA, cc[[19,2]],cc[[19,3]],"breed", 36, NA),
                    c(NA,change[20],NA,NA,NA,NA,NA,1,NA,NA,1,change[20],NA, cc[[20,2]],cc[[20,3]],"breed", 2, NA),
                    c(NA,"Lichecheng",NA,NA,NA,NA,NA,8,NA,NA,1,"Lichecheng",NA, cc[[22,2]],cc[[22,3]],"breed", 16, NA),
                    c(NA,change[25],NA,60,NA,NA,NA,NA,NA,NA,1,change[25],NA, cc[[25,2]],cc[[25,3]],"roost", NA, NA),
                    c(NA,"Babangibone",NA,75,NA,NA,NA,NA,NA,NA,1,"Babangibone",NA,
                      col_locs_all$lon[col_locs_all$name == "Babangibone"],col_locs_all$lat[col_locs_all$name == "Babangibone"],"roost", NA, NA),
                    c(NA,"Mphosong",NA,NA,NA,NA,NA,12,NA,NA,1,"Mphosong",NA, 28.38803,-29.07087,"breed", 12, NA))

# Next I need to add the new records provided by KZN
col_counts <- rbind(col_counts,
                    c(NA,"Nelson's Kop",NA,150,NA,NA,NA,NA,NA,NA,1,"Nelson's Kop",NA, 29.4356,-28.2317,"roost", NA, NA),
                    c(NA,"Fangs",NA,NA,NA,NA,NA,2,NA,NA,1,"Fangs",NA, 28.9452,-28.8607, "breed", 4, NA),
                    c(NA,"Red Wall",NA,62,NA,NA,NA,NA,NA,NA,1,"Red Wall",NA, 29.368543,-29.188185, "breed", NA, NA),
                    c(NA,"Gladstone's Nose",NA,150,NA,NA,NA,NA,NA,NA,1,"Gladstone's Nose",NA, 29.685,-29.398, "roost", NA, NA),
                    c(NA,"Stimela",NA,NA,NA,NA,NA,1,NA,NA,1,"Stimela",NA, 28.946972,-28.826248, "breed", 2, NA))

# Special fixes for counts (we will fix the adult and juv estimates later)
fix <- col_counts %>% 
   filter(str_detect(name, "Ntabamhlope")) %>% 
   mutate(name_new = "Ntabamhlope",
          type = "breed",
          lon = 29.65937,
          lat = -29.13054) %>% 
   group_by(name_new, year) %>% 
   mutate(across(.cols = c("n_nests", "total", "ad", "juv"), .fns = as.numeric)) %>% 
   mutate(across(.cols = c("n_nests", "total", "ad", "juv"), .fns = sum)) %>% 
   slice(1) %>% 
   ungroup()

col_counts <- rbind(col_counts %>% 
                       filter(!str_detect(name_new, "Ntabamhlope")),
                    fix)

col_counts <- col_counts %>% 
   mutate(name_new = case_when(str_detect(name_new, "Gladstone") ~ "Gladstone's Nose",
                               name_new == "Game pass" ~ "Game Pass",
                               TRUE ~name_new),
          lon = case_when(str_detect(name_new, "Gladstone") ~ "29.685",
                          name_new == "Game Pass" ~ "29.637",
                          name_new == "Snowdon" ~ "27.516685",
                          TRUE ~ lon),
          lat = case_when(str_detect(name_new, "Gladstone") ~ "-29.398",
                          name_new == "Game Pass" ~ "-29.382",
                          name_new == "Snowdon" ~ "-30.695596",
                          TRUE ~ lat),
          ad = case_when(name_new == "Game Pass" ~ "60",
                         name_new == "Snowdon" ~ "25",
                         TRUE ~ ad),
          total = case_when(name_new == "Game Pass" ~ NA_character_,
                            name_new == "Snowdon" ~  NA_character_,
                            TRUE ~ total))

# Fix location data
col_locs_all <- col_locs_all %>% 
   mutate(lon = if_else(str_detect(name, "Ntabamhlope"), 29.65937, lon),
          lat = if_else(str_detect(name, "Ntabamhlope"), -29.13054, lat),
          type = if_else(str_detect(name, "Ntabamhlope"), "breed", type))


# Update count summary ----------------------------------------------------

# Recover column types
col_counts <- type_convert(col_counts, col_types = spec(col_counts))

# Transform pair or nest counts to adults. If adult counted, then ad_p = ad,
# if not, ad_p = pairs * 2, if no adult or pair count, then ad_p = n_nests * 2
col_counts <- col_counts %>% 
   mutate(ad_p = ifelse(is.na(ad),
                        if_else(is.na(pairs), n_nests*2, pairs*2),
                        ad))

# According to our very basic life-history model, in a steady state,
# there should be 70% of adults and 30% juveniles (ages 1 to 4).
# Therefore the number of juvs = 0.30/0.70 * ad = 0.43 * ad
# This seems to be consistent with the observations, but are juveniles
# present at the breeding colonies or they are rather at roosts?
col_counts <- col_counts %>% 
   mutate(juv_p = ifelse(is.na(juv), round(0.43*ad_p), juv))

# In some colonies only total counts are available, we must divide this
# into adults and juveniles
col_counts <- col_counts %>% 
   mutate(ad_p = if_else(is.na(ad_p), round(0.70*total), ad_p),
          juv_p = if_else(is.na(juv_p), round(0.30*total), juv_p))

# Calculate average number of adults and juveniles per colony
count_summ <- col_counts %>% 
   filter(!is.na(ad_p)) %>% 
   group_by(name_new, lon, lat, type) %>% 
   summarize(avg_ad = round(mean(ad_p, na.rm = T)),
             avg_juv = round(mean(juv_p, na.rm = T)),
             ncounts = n(),
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
      mutate(avg_ad = NA, avg_juv = NA, ncounts = 0, names_old = NA, counted = 0) %>% 
      dplyr::select(name, lon, lat, avg_ad, avg_juv, ncounts, names_old, counted, type),
   count_summ %>%
      mutate(counted = 1) %>% 
      dplyr::select(name = name_new, lon, lat, avg_ad, avg_juv, ncounts, names_old, counted, type)
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
          ncounts = 0,
          names_old = NA,
          counted = 0) %>% 
   dplyr::select(names(col_join))

# Then create a spatial object and find the intersection with a 600m buffer from
# existing sites (pre-Vulpro)
source("R/functions/meterToDegree.R")

join_buff600 <- st_as_sf(col_join, coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
   st_union() %>% 
   st_buffer(dist = meterToDegree(meters = 600))
# Warning is fine we don't want precision

int <- st_as_sf(total_v, coords = c("lon", "lat"), crs = 4326, remove = F) %>%
   st_intersects(join_buff600)

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
