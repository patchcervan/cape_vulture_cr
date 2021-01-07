# 30-10-2020

# In this script we explore, prepare and combine colony counts
# obtained from different sources

rm(list = ls())

library(tidyverse)
library(sf)


# Read in data ------------------------------------------------------------

# Read in count data
col_counts <- read_csv("data/raw/colony_data/colonies_EWT/Cape Vulture Numbers_20180501.csv")

# Read in location data
col_locs <- read_csv("data/working/colony_data_join.csv")

summary(col_counts)

# Rename columns
col_counts <- col_counts %>% 
      rename_with(tolower) %>% 
      rename(name = colony, n_nests = 'no. active nests', total = 'total bird', 
             ad = adults, juv = juveniles, undet = unknown, pairs = 'breeding pairs',
             chick = 'nestlings/chick', fled = fledglings)

# Fix numeric columns. Detect letters
col_counts$total[str_which(col_counts$total, "[:alpha:]")]
col_counts$pairs[str_which(col_counts$pairs, "[:alpha:]")]

col_counts$pairs[str_which(col_counts$pairs, "[:alpha:]")] <- c(125, 9, 8)

# Fix numeric columns. Detect hyphens
col_counts$total[str_which(col_counts$total, "-")]
col_counts$pairs[str_which(col_counts$pairs, "-")]

col_counts$pairs[str_which(col_counts$pairs, "-")] <- c(51, 33, 60)

# Fix numeric columns. Detect special characters
col_counts$total[str_which(col_counts$total, "\\+")]
col_counts$pairs[str_which(col_counts$pairs, "\\+")]

col_counts$total[str_which(col_counts$total, "\\+")] <- 45


# Fix numeric columns
col_counts <- col_counts %>% 
      mutate(total = as.numeric(total),
             pairs = as.numeric(pairs))

# Seems like some counts show -9999. Make these counts NA
col_counts <- col_counts %>% 
   mutate(chick = ifelse(chick < 0, NA, chick),
          fled = ifelse(fled < 0, NA, fled))


# Identify those colonies with counts ----------------------------------

# First separate those colonies that have been counted recently
col_counts <- col_counts %>% 
   filter(year > 2009)

# Calculate the mean count at each of these colonies
col_counts <- col_counts %>% 
      dplyr::select(1,2,4:11)

col_counts %>% 
      print(n = Inf)

# Calculate row sums to identify colonies without counts
col_counts <- col_counts %>% 
   mutate(ever = if_else(rowSums(col_counts[,3:10], na.rm = T) == 0, 0, 1))

# Keep those that have ever been counted
col_counts <- col_counts %>% 
   filter(ever == 1)


# Identify and match colony names -----------------------------------------

sort(unique(col_counts$name))
sort(unique(col_locs$name))


# Join counts and locations -----------------------------------------------

col_data <- left_join(col_counts,
                      dplyr::select(col_locs, id, name, lon, lat, type),
                      by = "name")

col_data %>% 
      print(n = Inf)

# How many colonies with counts don't have location
summary(col_data$lat)

# Names of colonies with no location
sort(col_data$name[is.na(col_data$lat)])

# Names of colonies with location
sort(col_locs$name)

# Try to find approximate matches
names_target <- col_counts$name
names_match <- col_locs$name

matches <- cbind(names_target, rep(NA, length(names_target)))

for(i in 1:nrow(matches)){
   match <- agrep(names_target[i], names_match, ignore.case = T, value = T)
   matches[i,2] <- if(length(match) == 0) NA else match[1]
}

matches

# change names in count data
col_counts <- col_counts %>% 
   mutate(name_new = if_else(!is.na(matches[,2]), matches[,2], name))

print(col_counts, n = Inf)


# Fix non-matching entries manually ---------------------------------------

# what names still need to find
matches[is.na(matches[,2]),1]


col_counts <- col_counts %>% 
   mutate(name_new = case_when(name_new == "Amphitheater - Ribbon Falls" ~ "Ganabu",
                               name_new == "Barkley East/ Lady Grey Nestling Site" ~ "Karnmelkspruit",
                               name_new == "Black Mountain" ~ "Sehonghong River",
                               # name_new == "Cathedral Peak" ~ "",
                               name_new == "Botswana" ~ "Mannyelong",
                               name_new == "Botswana - Moremi Gorge" ~ "Manong Yeng",
                               name_new == "Cathedral peak (Cockade and Elephant)" ~ "Cockade and Elephant",
                               name_new == "Cathedral Peak (Pampering)" ~ "PamperingA",
                               # name_new == "Cleft Peak" ~ "",
                               # name_new == "Column Pyramid Pass" ~ "",
                               name_new == "Culfargie (Grays Pass)" ~ "Gray's Pass",
                               name_new == "Dungu" ~ "189",
                               name_new == "Elephant Rock" ~ "Elephant",
                               name_new == "Ganabu (Ladysmith District/Cathedral Peak)" ~ "Ganabu",
                               # name_new == "Ha Mosele Phororone ~ "",
                               name_new == "Isiwa Samanqe (Greytown)" ~ "Episweni Mtn",
                               name_new == "Jumbla" ~ "205",
                               # name_new == "Killmore" ~ "",
                               # name_new == "Kylemore" ~ "",
                               # name_new == "Langalebalele (D)" ~ "",
                               name_new == "Long Wall (A)" ~ "Long wall",
                               name_new == "Magaliesberg (Nooitgedacht)" ~ "Nooitgedacht",
                               name_new == "Magaliesberg (Robert's Farm)" ~ "Robert's Farm",
                               name_new == "Magaliesberg (Skeerpoort)" ~ "Skeerpoort",
                               name_new == "Mkhambathi Nature Reserve" ~ "Mtentu",
                               name_new == "Mlengana" ~ "Ku-Yeneni",
                               # name_new == "Morristone A" ~ "",
                               name_new == "Motsitseng" ~ "Lower Moremoholo",
                               name_new == "Mzimkhulu Colony" ~ "Mzimkhulu/Oribi",
                               name_new == "Ndedema Dome" ~ "Ndedema River",
                               name_new == "Ndedema Twin Peak A&B" ~ "Ndedema Buttress",
                               name_new == "Nguza Pass" ~ "Nguza",
                               # name_new == "Ntabamhlope E facing cliff" ~ "",
                               # name_new == "Ntabamhlope East" ~ "",
                               # name_new == "Ntabamhlope W facing cliff" ~ "",
                               # name_new == "Ntabamhlope West" ~ "",
                               name_new == "Port St Johns/ Mlengana" ~ "Ku-Yeneni",
                               # name_new == "Red Wall B&C" ~ "",
                               # name_new == "Rockliff" ~ "",
                               # name_new == "Rockliffe" ~ "",
                               name_new == "Selomo se Putsoa"  ~ "Pitso e Moena",
                               # name_new == "Singleton"  ~ "",
                               # name_new == "Snowdon"  ~ "",
                               name_new == "Sphinx's nest (Sugar Loaf Kop)"  ~ "The Sphinx",
                               name_new == "Thaba Phatsoa" ~ "Vulture's Peak",
                               # name_new == "The Giant" ~ "",
                               # name_new == "The Thumb A" ~ "",
                               # name_new == "Thlanyaku Selomo se Putsoa" ~ "" ,
                               name_new == "Thomas River" ~ "Stonehenge",
                               name_new == "Vultures Retreat (Culfargie/Monks Cowl)" ~ "Vulture's Retreat",
                               name_new == "Witteberg (Longwall & Giants Pass)" ~ "Long Wall",
                               TRUE ~ name_new))

matches <- cbind(col_counts$name_new, rep(NA, nrow(col_counts)))

for(i in 1:nrow(matches)){
   match <- agrep(matches[i,1], names_match, ignore.case = T, value = T)
   matches[i,2] <- if(length(match) == 0) NA else match[1]
}

matches

# what names still need to find
matches[is.na(matches[,2]),1]

col_counts %>% 
   arrange(name, year) %>% 
   print(n = Inf)

# add coordinates and ids from colony location data
col_counts <- left_join(col_counts,
                        dplyr::select(col_locs, id, name, lon, lat, type),
                        by = c("name_new" = "name"))

# Save new count data set
# write.csv(col_counts, "data/working/col_counts_locs.csv", row.names = F)


# Explore this new data set -----------------------------------------------

# col_counts <- as_tibble(read.csv("data/working/col_counts_locs.csv"))

# Remove rows that are classified as roost
col_counts <- col_counts %>% 
   filter(type != "Roost")

# Which records have repeated name and year
col_counts <- col_counts %>% 
   add_count(year, n_nests, total, ad, juv, undet, pairs, chick, fled, name_new, name = "n_recs")

# Remove rows for which year, name_new
col_counts <- col_counts %>% 
   arrange(desc(id)) %>% 
   distinct(year, n_nests, total, ad, juv, undet, pairs, chick, fled, name_new, .keep_all = T)

# Order by year and name, remove obsolete columns and save data set
col_counts %>% 
   arrange(name_new, year) %>% 
   dplyr::select(-c(ever, n_recs)) %>% 
   write.csv("data/working/col_counts_locs.csv", row.names = F)





# For later:
# Calculate the mean count at each of these colonies
col_counts <- col_counts %>% 
   summarise_at(.vars = c("n_nests", "total", "ad", "juv", "undet", "chick", "fled"),
                .funs = ~mean(., na.rm = TRUE))