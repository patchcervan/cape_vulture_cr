library(tidyverse)
library(raster)

rm(list = ls())


# Load data and model results ---------------------------------------------

# Tracking data
vults <- readRDS("data/working/data_ssf_ready.rds")


# Compare observed vs predicted UD ----------------------------------------

# Load function to calculate quantiles
source("R/functions/calcUDquantile.R")


# Correlation of adult birds


# Load SA UD raster
rud <- raster("output/pred_raster_sims/3_provinces/SA_ad_gamfit.tif")

adsample <- vults %>% 
      filter(case_ & age_fct == "ad")
%>% 
      # group_by(bird_id) %>% 
      # mutate(size = n()) %>% 
      # slice_sample(n = 1000) %>% 
      # ungroup()

counts <- rasterize(adsample %>%  
                          dplyr::select(lon1_, lat1_) %>% 
                          as.matrix(),
                    rud, fun = "count", background = NA)

rdf <- as.data.frame(rud, xy = T) %>% 
      rename(ud = 3) %>% 
      mutate(count = raster::values(counts)) %>% 
      filter(!is.na(count), !is.na(ud))

# Calculate UD quantiles
qq <- calcUDquantile(rdf$ud, seq(0,1,length.out = 10))

Ntotal <- sum(rdf$count)

rdf %>% 
      mutate(ud_fct = cut(ud, breaks = qq, labels = 1:(length(qq)-1), include.lowest = T)) %>% 
      group_by(ud_fct) %>% 
      summarize(freq = sum(count)/Ntotal) %>% 
      # slice(-c(8:9)) %>% 
      summarize(spcor = cor(rev(as.numeric(ud_fct)), freq, method = "spearman")) %>% 
      pull(spcor)


# Correlation of juvenile birds

# Load SA UD raster
rud <- raster("output/pred_raster_sims/3_provinces/SA_juv_gamfit.tif")

juvsample <- vults %>% 
      filter(case_ & age_fct == "juv")
%>% 
      # group_by(bird_id) %>% 
      # mutate(size = n()) %>% 
      # slice_sample(n = 1000) %>% 
      # ungroup()
      
      counts <- rasterize(juvsample %>%  
                                dplyr::select(lon1_, lat1_) %>% 
                                as.matrix(),
                          rud, fun = "count", background = NA)

plot(log(counts))

rdf <- as.data.frame(rud, xy = T) %>% 
      rename(ud = 3) %>% 
      mutate(count = raster::values(counts)) %>% 
      filter(!is.na(count), !is.na(ud))

# Calculate UD quantiles
qq <- calcUDquantile(rdf$ud, seq(0,1,length.out = 10))

Ntotal <- sum(rdf$count)

rdf %>% 
      mutate(ud_fct = cut(ud, breaks = qq, labels = 1:(length(qq)-1), include.lowest = T)) %>% 
      group_by(ud_fct) %>% 
      summarize(freq = sum(count)/Ntotal) %>% 
      # slice(-c(8:9)) %>% 
      summarize(spcor = cor(rev(as.numeric(ud_fct)), freq, method = "spearman")) %>% 
      pull(spcor)



# Extract distribution of trip durations ----------------------------------

# Read in adult simulated data
adsimfiles <- list.files("output/pred_raster_sims/1_pred_map_col/", pattern = "_ad_")

simdur <- vector()

for(i in seq_along(adsimfiles)){
      sims <- readRDS(paste0("output/pred_raster_sims/1_pred_map_col/", adsimfiles[i]))
      dur <- sims %>% 
            group_by(trip) %>% 
            summarize(dur = max(dur)) %>% 
            pull(dur)
      simdur <- c(simdur, dur)
}

# Plot observed trip durations
trip_dur <- vults %>% 
      group_by(age_fct, trip) %>% 
      summarize(dur = max(days_away))

trip_dur %>% 
      filter(dur < 15) %>%
      filter(age_fct == "ad") %>% 
      pull(dur) %>% 
      hist(freq = F, nclass = 5)

hist(simdur, freq = F, nclass = 5)


# Read in juv simulated data
juvsimfiles <- list.files("output/pred_raster_sims/1_pred_map_col/", pattern = "_juv_")

simdur <- vector()

for(i in seq_along(adsimfiles)){
      sims <- readRDS(paste0("output/pred_raster_sims/1_pred_map_col/", adsimfiles[i]))
      dur <- sims %>% 
            group_by(trip) %>% 
            summarize(dur = max(dur)) %>% 
            pull(dur)
      simdur <- c(simdur, dur)
}

trip_dur %>% 
      filter(dur < 15) %>%
      filter(age_fct == "juv") %>% 
      pull(dur) %>% 
      hist(freq = F, nclass = 5)

hist(simdur, freq = F, nclass = 5)


# Extract maximum distance from colony ------------------------------------

max_dist <- vults %>% 
      group_by(age_fct, trip) %>% 
      summarize(dist = max(dist_col))

max_dist %>%
   ggplot() +
   geom_histogram(aes(x = dist,y = stat(..density..))) +
   facet_wrap("age_fct")

# Define distance function
calcDist <- function(s, x, y){
      sqrt((s[1] - x)^2 + (s[2] - y)^2)
}


# Read in adult simulated data
adsimfiles <- list.files("output/pred_raster_sims/1_pred_map_col/", pattern = "_ad_")

simdist <- vector()

for(i in seq_along(adsimfiles)){
      sims <- readRDS(paste0("output/pred_raster_sims/1_pred_map_col/", adsimfiles[i]))
      cc <- sims[1, c("")]
      dist <- sims %>% 
            mutate(dist_col = )
            group_by(trip) %>% 
            summarize(dur = max(dur)) %>% 
            pull(dur)
      simdur <- c(simdur, dur)
}

# Plot observed trip durations
trip_dur <- vults %>% 
      group_by(age_fct, trip) %>% 
      summarize(dur = max(days_away))

trip_dur %>% 
      filter(dur < 15) %>%
      filter(age_fct == "ad") %>% 
      pull(dur) %>% 
      hist(freq = F, nclass = 5)

hist(simdur, freq = F, nclass = 5)
