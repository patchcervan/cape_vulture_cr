loginStored <- move::movebankLogin(username=" FCervantes", password="wanditam0v3b6nk")

move::getMovebankStudy(study = "EWT_DRUID_RAPTORS", login = loginStored) # see study-level info



data.move <- move::getMovebankData(study = "EWT_DRUID_RAPTORS", login = loginStored, 
                             removeDuplicatedTimestamps = T, animalName = c("CV_3418", "CV_3802"))

#convert to dataframe
data <- as(data.move, "data.frame")

unique(data$local_identifier)

# THERE SHOULD BE TWO CODES BUT THERE IS ONLY ONE...

# Remove data from before 16th of March
data <- data %>% 
    filter(timestamp > as.POSIXct("2020-03-16 00:00:00", tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

range(data$timestamp)



# Save

write_csv(data, paste0("data/raw/ew/EWT_movebank_", today(), ".csv"))

data <- read_csv(paste0("data/raw/ew/EWT_movebank_", today(), ".csv"))

# Useful code to extract hourly data:
library(tidyverse)
library(lubridate)

df <- as_tibble(data) %>%
    filter(year(timestamp) > 2019) %>% 
    mutate(hour = hour(timestamp)) %>%
    mutate(date = ymd(str_c(year(timestamp),
                            month(timestamp),
                            day(timestamp), 
                            sep = "-"))) %>%
    group_by(date, hour, tag_local_identifier) %>%
    filter(row_number() == 1) %>% 
    ungroup()

df <- df %>% 
    group_by(tag_local_identifier) %>% 
    mutate(dt = as.numeric(lead(timestamp) - timestamp)/3600) %>% 
    ungroup()

df %>% 
    group_by(tag_local_identifier) %>% 
    summarize(mean_dt = mean(dt, na.rm = T),
              sd_dt = sd(dt, na.rm = T))

df %>% 
    filter(tag_local_identifier == "CV_3802") %>% 
    dplyr::select(tag_local_identifier, timestamp, dt) %>% 
    tail()

df %>% 
    filter(tag_local_identifier == "CV_3802") %>% 
    dplyr::select(timestamp, dt) %>% 
    ggplot() +
    geom_point(aes(x = timestamp, y = dt))




range(df$timestamp)

df <- df[order(df$timestamp, df$id),]

df[1:10,]

 

data.hour<-df

ddply(data.hour, c("id"), summarise,n=length(id))