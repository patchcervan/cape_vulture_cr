library(tidyverse)

rm(list = ls())

# Vulture data
vults <- readRDS("data/working/data_height_ready.rds")

ids <- vults %>% 
      filter(height > 3000 | height < -750) %>% 
      pull(bird_id) %>% 
      unique()

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Create an indicator variable to label the records to keep
vults$quality <- 1


# CT birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "ct"),
          height < 5000,
          height > -750) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "ct")){
      
      idsel <- ids[i]
      
      vult_sel <- vults %>% 
            filter(bird_id == idsel)
      
      vult_sel %>% 
            pull(height) %>% 
            plot()
      
      ring <- bird_db %>% 
            filter(bird_id == idsel) %>% 
            pull(ring_id)
      
      dat <- read.csv(paste0("data/raw/vp/VulPro2/Cellulartracking/", ring, ".csv"))
      
      dat <- dat %>% 
            mutate(datetime = lubridate::dmy_hms(paste(GPS_date_YYYY.MM.DD,
                                                       GPS_utc_HH.MM.SS), tz = "GMT"))
      
      vult_sel <- vult_sel %>% 
            left_join(dplyr::select(dat, datetime, data_voltage, fix, cog), by = "datetime")
      
      vult_sel %>% 
            filter(height < -500) %>% 
            pull(fix)
      
      vults <- vults %>% 
         filter(bird_id != idsel | height > -750) %>% 
         filter(bird_id != idsel | height < 3000)
}


# EZ birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "ez"),
          height < 5000,
          height > -750) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "ez")){
   
   idsel <- ids[i]
   
   vult_sel <- vults %>% 
      filter(bird_id == idsel)
   
   vult_sel %>% 
      pull(height) %>% 
      plot()
   
   tag_id <- bird_db %>% 
      filter(bird_id == idsel) %>% 
      pull(tag_id)
   
   sheet <- case_when(idsel == "ez01" ~ 7,
                      idsel == "ez08" ~ 3)
   
   dat <- readxl::read_excel("data/raw/ez/Cape vulture data_Ezemvelo_Feb19_datecorrected.xls", sheet = sheet)
   
   vults <- vults %>% 
      filter(bird_id != idsel | height > -750) %>% 
      filter(bird_id != idsel | height < 3000)
}


# KR birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "kr"),
          height < 5000,
          height > -1000) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "kr")){
      
      idsel <- ids[i]
      
      vult_sel <- vults %>% 
            filter(bird_id == idsel)
      
      vult_sel %>% 
            # filter(bird_id != idsel | height > -500) %>% 
            # filter(bird_id != idsel | height < 3000) %>% 
            pull(height) %>% 
            plot()
      
      tag_id <- bird_db %>% 
            filter(bird_id == idsel) %>% 
            pull(tag_id)
      
      dat <- readxl::read_excel("data/raw/kr/MASTERcape.xlsx")
      
      vults <- vults %>% 
            filter(bird_id != idsel | height > -500) %>% 
            filter(bird_id != idsel | height < 3000)
}



# MA birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "ma"),
          height < 5000,
          height > -750) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "ma")){
   
   idsel <- ids[i]
   
   vult_sel <- vults %>% 
      filter(bird_id == idsel)
   
   vult_sel %>% 
      pull(height) %>% 
      plot()
   
   name_id <- bird_db %>% 
      filter(bird_id == idsel) %>% 
      pull(name)
   
   if(name_id == "Marie"){
      name_id <- ""
   }
   
   dat <- readRDS(paste0("data/working/pre_proc_data/temp_ma/", name_id, "_temp.rds"))
   
   dat <- dat %>% 
      mutate(datetime = str_sub(timestamp, end = -5),
             datetime = as.POSIXct(timestamp, tz = "GMT"))
   
   vult_sel <- vult_sel %>% 
      left_join(dplyr::select(dat, datetime, type=eobs.type.of.fix), by = "datetime")
   
   vult_sel %>% 
      filter(height < -500 | height > 3000) %>% 
      pull(type)
   
   # dates to remove
   keep_dates <- vult_sel %>% 
      filter(type == 3) %>% 
      pull(datetime)
   
   vults <- vults %>% 
      filter(bird_id != idsel | height > -750) %>% 
      filter(bird_id != idsel | height < 3000)
}

vults %>% 
   filter(str_detect(bird_id, "ma")) %>% 
   pull(height) %>% hist()


# MB birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "mb"),
          height < 3000,
          height > -750) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "mb")){
   
   idsel <- ids[i]
   
   vult_sel <- vults %>% 
      filter(bird_id == idsel)
   
   vult_sel %>% 
      pull(height) %>% 
      plot()
   
   ring <- bird_db %>% 
      filter(bird_id == idsel) %>% 
      pull(ring_id)
   
   dat <- read.csv(paste0("data/raw/vp/VulPro2/Movebank/", ring, ".csv"))
   
   dat <- dat %>% 
      mutate(datetime = str_sub(timestamp, end = -5),
             datetime = as.POSIXct(timestamp, tz = "GMT"))
   
   vult_sel <- vult_sel %>% 
      left_join(dplyr::select(dat, datetime, nsats = gps.satellite.count), by = "datetime")
   
   vult_sel %>% 
      filter(height < -1000) %>% 
      pull(nsats)
   
   vults <- vults %>% 
      filter(bird_id != idsel | height > -750) %>% 
      filter(bird_id != idsel | height < 3000)
}


# MT birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "mt"),
          height < 7000,
          height > -1000) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "mt")){
   
   idsel <- ids[i]
   
   vult_sel <- vults %>% 
      filter(bird_id == idsel)
   
   vult_sel %>% 
      pull(height) %>% 
      plot()
   
   ring <- bird_db %>% 
      filter(bird_id == idsel) %>% 
      pull(ring_id)
   
   dat <- readxl::read_excel(paste0("data/raw/vp/VulPro2/MTI/", ring, ".xlsx"))
   
   vults <- vults %>% 
      filter(bird_id != idsel | height > -750) %>% 
      filter(bird_id != idsel | height < 3000)
}


# WT birds ----------------------------------------------------------------

vults %>% 
   filter(str_detect(bird_id, "wt"),
          height < 7000,
          height > -1000) %>% 
   pull(height) %>% hist()

for(i in str_which(ids, "wt")){
   
   idsel <- ids[i]
   
   vult_sel <- vults %>% 
      filter(bird_id == idsel)
   
   vult_sel %>% 
      pull(height) %>% 
      plot()
   
   ring <- bird_db %>% 
      filter(bird_id == idsel) %>% 
      pull(ring_id)
   
   dat <- read_delim(paste0("data/raw/vp/VulPro2/Wildlife Tracking/", ring,".csv"), delim = ";")
   
   vults <- vults %>% 
      filter(bird_id != idsel | height > -750) %>% 
      filter(bird_id != idsel | height < 3000)
}

vults %>% 
   pull(height) %>% 
   hist()

vults %>% 
   filter(height > 3000 | height < -750) %>% 
   pull(bird_id) %>% 
   unique()

# Save data
saveRDS(vults, "data/working/data_height_ready_noextrm.rds")
