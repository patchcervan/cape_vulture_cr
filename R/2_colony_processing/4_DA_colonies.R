# 16-06-2021

# In this script we integrate the colony data contributed by DA with the rest of
# the database. A lot of the work is done by comparing data in QGIS.

library(tidyverse)
library(sf)

rm(list = ls())

# Read in new colonies
col <- readxl::read_excel("data/raw/colony_data/CV_COLONIES_MASTER_allan.xlsx")

# Read in old colonies
col_all <- read.csv("data/working/colony_data_all_upt.csv")

# Transform degree-minute-second to decimal degrees
DMStoDecDeg <- function(D, M, S){
   D + (M/60) + (S/3600)
}

names(col) <- make.names(names(col))

col <- col %>% 
   mutate(Co.ords.S = str_remove_all(Co.ords.S, " "),
          Co.ords.E = str_remove_all(Co.ords.E, " "),
          Dlat = as.numeric(str_sub(Co.ords.S, 1, 2)),
          Mlat = as.numeric(str_sub(Co.ords.S, 3, 4)),
          Slat = if_else(nchar(Co.ords.S) == 6, as.numeric(str_sub(Co.ords.S, 5, 6)), 0),
          Dlon = as.numeric(str_sub(Co.ords.E, 1, 2)),
          Mlon = as.numeric(str_sub(Co.ords.E, 3, 4)),
          Slon = if_else(nchar(Co.ords.E) == 6, as.numeric(str_sub(Co.ords.E, 5, 6)), 0),
          lat = DMStoDecDeg(Dlat, Mlat, Slat) * -1,
          lon = DMStoDecDeg(Dlon, Mlon, Slon)) %>% 
   rename(name = Name,
          nbirds = No..birds,
          npairs = No..prs.act.cnt,
          nestm = No..prs.best.est)



# Fix counts --------------------------------------------------------------


# Fix numeric columns. Detect hyphens
tofix <- col$nbirds[str_which(col$nbirds, "-")]
tofix <- str_remove(tofix, ".*-")
tofix <- str_remove(tofix, "\\+")

col$nbirds[str_which(col$nbirds, "-")] <- tofix

# Fix numeric columns. Detect letters
tofix <- col$nbirds[str_which(col$nbirds, "[:alpha:]")]
tofix <- str_remove(tofix, ".* ")
tofix <- str_remove(tofix, "ca")
col$nbirds[str_which(col$nbirds, "[:alpha:]")] <- tofix

tofix <- col$nbirds[str_which(col$nbirds, "[:alpha:]")]
tofix <- c("200", rep(NA, 6))
col$nbirds[str_which(col$nbirds, "[:alpha:]")] <- tofix

# Fix numeric columns. Detect special characters
tofix <- col$nbirds[str_which(col$nbirds, "\\+")]
tofix <- str_remove(tofix, "\\+")
col$nbirds[str_which(col$nbirds, "\\+")] <- tofix

# Fix numeric columns. Detect hyphens
tofix <- col$npairs[str_which(col$npairs, "-")]
tofix <- str_remove(tofix, ".*-")
tofix <- str_remove(tofix, "\\?")

col$npairs[str_which(col$npairs, "-")] <- tofix

# Fix numeric columns. Detect letters
tofix <- col$npairs[str_which(col$npairs, "[:alpha:]")]
tofix <- str_remove(tofix, ".* ")
tofix <- str_remove(tofix, "c")
tofix <- str_remove(tofix, "\\?")
col$npairs[str_which(col$npairs, "[:alpha:]")] <- tofix

tofix <- col$npairs[str_which(col$npairs, "[:alpha:]")]
tofix <- rep(NA, 4)
col$npairs[str_which(col$npairs, "[:alpha:]")] <- tofix

# Fix numeric columns. Detect special characters
tofix <- col$npairs[str_which(col$npairs, "\\+")]
tofix <- str_remove(tofix, "\\+")
col$npairs[str_which(col$npairs, "\\+")] <- tofix

# Fix numeric columns. Detect hyphens
tofix <- col$Year[str_which(col$Year, "-")]
tofix <- str_remove(tofix, ".*-")
tofix <- str_remove(tofix, ".* ")
tofix[tofix=="83"] <- "1983"

col$Year[str_which(col$Year, "-")] <- tofix

# Fix numeric columns. Detect letters
tofix <- col$Year[str_which(col$Year, "[:alpha:]")]
tofix <- str_remove(tofix, ".* ")
tofix <- str_remove(tofix, "s")
col$Year[str_which(col$Year, "[:alpha:]")] <- tofix


# Select columns ----------------------------------------------------------

col <- col %>% 
   mutate(year = as.numeric(Year),
          npairs = as.numeric(npairs),
          nbirds = as.numeric(nbirds)) %>%   
   dplyr::select(name, lon, lat, year, nbirds, npairs, Breed, Ref)


# Fix some colonies -------------------------------------------------------

withletters <- col %>% 
   filter(str_detect(name, "Mabeli .")) %>% 
   pull(npairs) %>% 
   sum()

# Modify one of the records with letters and keep
col <- col %>% 
   mutate(npairs = if_else(str_detect(name, "Mabeli A"), withletters, npairs),
          name = if_else(str_detect(name, "Mabeli A"), "Upper Quthing Valley/Mojoana Mabeli", name)) %>% 
   filter(!str_detect(name, "Mabeli ."))

col %>% 
   dplyr::select(-Ref) %>% 
   filter(str_detect(name, "Mahone"))


# Calculate average counts ------------------------------------------------

names(col_all)

# Calculate number of years with counts for each colony
navg_ad <- col %>% 
   group_by(name, lat, lon, year) %>% 
   summarize(avg_ad = mean(npairs, na.rm = T)*2,
             avg_cnt = mean(nbirds, na.rm = T)) %>% 
   filter(!is.nan(avg_ad)) %>% 
   summarize(ncounts = n()) %>% 
   ungroup()

navg_cnt <- col %>% 
   group_by(name, lat, lon, year) %>% 
   summarize(avg_ad = mean(npairs, na.rm = T)*2,
             avg_cnt = mean(nbirds, na.rm = T)) %>% 
   filter(!is.nan(avg_cnt)) %>% 
   summarize(ncounts = n()) %>% 
   ungroup()


# Calculate actual average counts
col <- col %>% 
   group_by(name, lat, lon, year) %>% 
   mutate(avg_ad = mean(npairs, na.rm = T)*2,
          avg_cnt = mean(nbirds, na.rm = T)) %>% 
   ungroup() %>% 
   group_by(name, lat, lon) %>% 
   mutate(avg_ad = mean(avg_ad, na.rm = T),
          avg_cnt = mean(avg_cnt, na.rm = T),
          lastdate = max(year, na.rm = T)) %>% 
   mutate(names_old = NA,
          counted = if_else(is.na(avg_ad) & is.na(avg_cnt), 0, 1),
          type = if_else(is.na(avg_ad), "roost", "breed")) %>% 
      ungroup()

col_summ <- col %>% 
   group_by(name, lat, lon) %>% 
   summarize(avg_ad = unique(avg_ad),
             avg_cnt = unique(avg_cnt),
             lastdate = unique(lastdate))

col_summ %>% 
   filter(lastdate == -Inf) %>% 
   print(n = Inf)

col %>% 
   filter(name == "Xukula")

col_summ <- col_summ %>% 
   filter(lastdate != -Inf)

# filter(is.nan(avg_ad))
# 
# col_summ <- col_summ %>% 
#    mutate(lastdate = ifelse(lastdate == -Inf, NA, lastdate))

# col %>% 
#    filter(is.na(lon)) %>% 
#    dplyr::select(Co.ords.S, Co.ords.E, Dlat, Mlat, Slat, Dlon, Mlon, Slon, lon, lat,
#                  nbirds, npairs, nestm) %>% 
#    print(n = Inf)

col_summ <- col_summ %>% 
   filter(!is.nan(avg_ad)) %>% 
   filter(!is.na(lon), !is.na(lat))

# Add number of counts
col_summ <- left_join(col_summ,
                      dplyr::select(navg_ad, name, ncounts))

write_csv(col_summ, file = "data/working/DA_col_loc.csv")


# Integrate with other data -----------------------------------------------

col_da <- read.csv("data/working/DA_col_loc.csv")

# Create an id for the new colonies
col_da$id <- paste0("da_", 1:nrow(col_da))


# Intersect both datasets -------------------------------------------------

col_da <- st_as_sf(col_da, coords = c("lon", "lat"), crs = 4326, remove = F)
col_all <- st_as_sf(col_all, coords = c("lon", "lat"), crs = 4326, remove = F)

# Create a 5km buffer around existing colonies
col_all_buff5k <- st_buffer(col_all, 5/111)

# Find the intersection between our current colonies' buffer and the new ones
ints <- st_intersects(col_da, col_all_buff5k)

sapply(ints, length)

toappend <- col_da[sapply(ints, length) == 0,]

toappend <- toappend %>% 
   st_drop_geometry() %>% 
   mutate(names_old = NA,
          counted = if_else(is.na(avg_ad) & is.na(avg_cnt), 0, 1),
          type = if_else(is.na(avg_ad), "roost", "breed"),
          avg_juv = round(0.43*avg_ad))

col_all <- col_all %>% 
   st_drop_geometry()

col_all <-  col_all%>% 
   rbind(toappend %>% 
            dplyr::select(names(col_all)))

write_csv(col_all, "data/working/colony_all_w_da.csv")


# Correct numbers that do not match old records ---------------------------

rm(list = ls())

col_new <- read.csv("data/working/colony_all_w_da.csv")
col_old <- read.csv("data/working/colony_data_all_upt.csv")

col_da <- read.csv("data/working/DA_col_loc.csv")

# Create an id for the new colonies
col_da$id <- paste0("da_", 1:nrow(col_da))

# Create variables needed for new data
col_da <- col_da %>% 
   mutate(avg_juv = NA,
          names_old = NA,
          counted = if_else(is.na(avg_ad) & is.na(avg_cnt), 0, 1),
          type = if_else(!is.na(avg_ad), "breed", "roost")) %>% 
   dplyr::select(names(col_new))


# Records from DA to append -----------------------------------------------

colnames <- c("Kugqebenya","Liwalimdaka","Forest Range","Ntlonze","Maxalanga Peak","Rooiberg","Dudumashe",
"Zastron","Jozana's Hoek","Balloch","Phoqoane/Sinxondo","Ha Frans","Seforong", "Sebapala waterfall",
"Castle Rocks", "Colon Koppe","Mtamvuna","Magusheni","Mzwakazi","Pitseng - Lehaha-la-Molapo",
"Pitseng - Khora-ea-Mokhooa","Pitseng - Bolahla","Moteng Pass","Metebong-ea-Lelingoa","Upper Senqu",
"Monyetleng","Mohlokohloko","Macacaceng","Ha Ramaepho","Fangs North","iTsolwane","Maqukhu","Maseepho",
"Thaba-Maseka","Semonkong","MaraKabei Manong")


col_new <- col_new %>% 
   slice(-unlist(map(colnames[1:5], ~str_which(col_new$name, .x)))) %>% 
   rbind(col_da %>% 
            slice(map_int(colnames[1:5], ~str_which(col_da$name, .x))))

col_new <- col_new %>% 
   filter(!id %in% c("cvcol370", "cvcol234", "cvcol106")) %>% 
   rbind(col_da %>% 
            slice(map_int(colnames[c(6, 11, 12)], ~str_which(col_da$name, .x))))


col_new <- col_new %>% 
   slice(-unlist(map(colnames[c(7:10,13)], ~str_which(col_new$name, .x)))) %>% 
   rbind(col_da %>% 
            slice(map_int(colnames[c(7:10,13)], ~str_which(col_da$name, .x))))

col_da[unlist(map(colnames, ~str_which(col_da$name, .x))),]
col_new[unlist(map(colnames, ~str_which(col_new$name, .x))),]


col_new <- col_new %>% 
   filter(id != "cvcol235") %>% 
   mutate(avg_ad = if_else(id == "cvcol401", 78, avg_ad))

col_new <- col_new %>% 
   filter(!id %in% c("cvcol107", "cvcol130")) %>% 
   filter(!str_detect(name, "Castle Rocks")) %>% 
   rbind(col_da %>% 
            slice(map_int(colnames[c(14,16)], ~str_which(col_da$name, .x))))

col_da %>% 
   filter(str_detect(name, "Mahone")) %>% 
   pull(avg_ad) %>% 
   sum()

col_new <- col_new %>% 
   filter(!id %in% paste0("cvcol", c("306","131","585"))) %>% 
   mutate(avg_ad = if_else(id == "cvcol009", 94, avg_ad),
          avg_ad = if_else(str_detect(name, "Mahon"), NA_real_, avg_ad))

col_new <- col_new %>% 
   filter(!id %in% c("cvcol267","cvcol179","cvcol054","cvcol171","cvcol160","cvcol122","cvcol209","cvcol203","cvcol270",
                     "cvcol208","cvcol206","cvcol083", "cvcol090", "cvcol295")) %>% 
   rbind(col_da %>% 
            slice(map_int(colnames[c(17:28)], ~which(str_detect(col_da$name, .x)))))

col_da %>% 
   filter(str_detect(name, colnames[29])) %>% 
   pull(avg_ad) %>% 
   sum()
   
col_new <- col_new %>% 
   mutate(avg_ad = if_else(str_detect(name, "Ramaepho"), NA_real_, avg_ad)) %>% 
   rbind(col_da %>% 
            filter(str_detect(name, colnames[29])) %>% 
            slice(1) %>% 
            mutate(name = "Ha Ramaepho",
                   avg_ad = 74))


col_new <- col_new %>% 
   filter(!id == "cvcol196") %>% 
   rbind(col_da %>% 
            filter(str_detect(name, "Matebeng Pass")) %>% 
            slice(1) %>% 
            mutate(name = "Matebeng Pass",
                   avg_ad = 40))



col_new <- col_new %>% 
   filter(!id %in% c("cvcol219","cvcol391","cvcol186","cvcol193","cvcol255","cvcol249","cvcol189",
                     "cvcol095","cvcol096","cvcol286","cvcol097","cvcol254")) %>% 
   rbind(col_da %>% 
            slice(str_which(col_da$name, colnames[30])) %>% 
            slice(1)) %>% 
   rbind(col_da %>% 
            slice(map_int(colnames[c(31:36)], ~str_which(col_da$name, .x))))


# Fix juvenile counts -----------------------------------------------------

col_new <- col_new %>% 
   mutate(avg_ad = round(avg_ad),
          avg_juv = round(0.43*avg_ad))

   

# Save new data -----------------------------------------------------------

write_csv(col_new, "data/working/colony_all_w_da.csv")



# OTHER FIXES -------------------------------------------------------------

col_new <- read_csv("data/working/colony_all_w_da.csv")

# Info updates from DA:

# - Segundanine/Goba colony should not have more than 10 pairs
col_new <- col_new %>% 
   mutate(avg_ad = if_else(str_detect(name, "Goba"), 20, avg_ad))

# - Upper Quthing/Mojoana Mabeli should not have more than 100 pairs
col_new <- col_new %>% 
   mutate(avg_ad = if_else(str_detect(name, "Mabeli"), 200, avg_ad))

# Bonwalenong in Botswana is repeated. Remove the one with NA counts
col_new <- col_new %>% 
   filter(!str_detect(name, "Bonwalenong") | !is.na(avg_ad))

# Check
col_new %>% 
   slice(map_int(c("Goba", "Mabeli", "Bonwalenong"), ~str_which(name, .x)))

# Potberg colonies, we will keep the original records with birds only at main Potberg
col_new <- col_new %>% 
   filter(!id %in% c("da_1", "da_165", "da_168", "cvcol462", "cvcol800"))

col_new <- col_new %>% 
   mutate(avg_ad = if_else(id %in% c("da_166", "da_164", "da_162", "da_163", "da_165", "da_167"),  NA_real_, avg_ad))

# Waterberg colonies in Namibia (only one colony)
col_new <- col_new %>% 
   filter(!id %in% c("cvcol273", "cvcol456", "cvcol878"))

# Botswana Mannyelanong
col_new <- col_new %>% 
   filter(!id %in% c("cvcol403", "cvcol183"))

col_new <- col_new %>% 
   mutate(avg_ad = if_else(id %in% c("da_195", "cvcol229", "da_192"),  NA_real_, avg_ad))

# Botwana Moremi Gorge. I'll leave counts for all colonies. Perhaps check with Glyn
col_new <- col_new %>% 
   filter(!id %in% c("cvcol657", "cvcol740", "cvcol251", "cvcol166", "cvcol272"))

# Blouberg colonies. Remove duplicates
col_new <- col_new %>% 
   filter(!id %in% c("cvcol146", "cvcol463", "cvcol600", "da_20"))

col_new <- col_new %>% 
   mutate(lon = if_else(id == "cvcol376",  29.059, lon),
          lat = if_else(id == "cvcol376",  -23.044, lat))

# Southpansberg
col_new <- col_new %>% 
   filter(!id %in% c("cvcol269", "cvcol188", "cvcol838"))

col_new <- col_new %>% 
   mutate(avg_ad = if_else(id %in% c("da_196", "da_197"),  NA_real_, avg_ad))

# Manoutsa
col_new <- col_new %>% 
   filter(!id %in% c("da_95", "cvcol459"))

# Kransberg colony
col_new <- col_new %>% 
   filter(!id %in% c("cvcol164", "cvcol688"))

# Magaliesberg
col_new <- col_new %>% 
   filter(!id %in% c("da_89", "cvcol464", "cvcol784", "da_88", "cvcol245", "cvcol828"))

# Mtzikaba region
col_new <- col_new %>% 
   filter(!id %in% c("da_131", "da_130", "da_128", "cvcol753", "cvcol215", "cvcol888"))

# Mdakeni region
col_new <- col_new %>% 
   filter(!id %in% c("da_133", "cvcol770", "cvcol789"))

# Dungu region
col_new <- col_new %>% 
   filter(!id %in% c("cvcol372", "cvcol067", "da_202", "cvcol061", "cvcol842"))

# Mlengana/Port St Johns region
col_new <- col_new %>% 
   filter(!id %in% c("da_117"))

# Colleywobbles region
col_new <- col_new %>% 
   filter(!id %in% c("da_29", "cvcol309", "cvcol908", "cvcol271",
                     "cvcol357", "cvcol904", "cvcol356"))

# Stonehenge region
col_new <- col_new %>% 
   filter(!id %in% c("cvcol319", "da_82"))

# Aasvolgelsberg/Kammelkspruit region
col_new <- col_new %>% 
   filter(!id %in% c("cvcol599", "cvcol894", "cvcol004", "cvcol392", "cvcol686", "cvcol003"))

# Jozanas region
col_new <- col_new %>% 
   filter(!id %in% c("cvcol002", "cvcol153", "cvcol675"))

# The Castle region
col_new <- col_new %>% 
   filter(!id %in% c("cvcol133", "cvcol631", "cvcol197"))

# The Drakensberg region
col_new <- col_new %>% 
   filter(!id %in% c("da_140", "da_191", "da_149", "da_148", "da_147", "da_146", "da_145", "da_144", "da_143", "da_142", "da_25"))

# Queenstown region
col_new <- col_new %>% 
   filter(!id %in% c("cvcol689", "cvcol312", "cvcol313", "cvcol721"))

# Others
col_new <- col_new %>% 
   filter(!id %in% c("cvcol274", "cvcol712", "cvcol070", "cvcol315", "cvcol887",
                     "cvcol892", "cvcol893")) %>% 
   filter(name != "Ha Ramaepho A")

# Remove duplicate ids
col_new %>% 
   group_by(id) %>% 
   mutate(n = n()) %>% 
   filter(n > 1) %>% 
   arrange(name)

col_new <- col_new %>% 
   distinct(id, .keep_all = TRUE)



# Add preliminary counts to Cookhouse region ------------------------------

# This is an area that is notably undercounted and of interest to the wind energy industry
# therefore, we allocate the mean colony count to those colonies with zero counts

# Find median adult count
median(col_new$avg_ad, na.rm = TRUE)

col_new <- col_new %>% 
   mutate(avg_ad = if_else(id %in% c("da_32", "da_65", "da_201", "da_75", "da_24",
                                     "da_9", "da_200", "da_38", "da_27", "da_239", "da_4",
                                     "cvcol669", "cvco719", "cvcol687", "cvcol589"),  16, avg_ad))


# Fix juvs ----------------------------------------------------------------

col_new <- col_new %>% 
   mutate(avg_juv = round(0.43*avg_ad))

# Save
write_csv(col_new, "data/working/colony_all_w_da.csv")

col_new %>% 
   filter(!is.na(avg_ad)) %>% 
   write_csv("data/working/colony_all_w_da_counted.csv")
