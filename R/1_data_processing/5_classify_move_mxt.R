# 21-12-2020

# In this script we explore vulture speed data to identify those locations
# when birds are not moving, and possibly roosting

rm(list = ls())

library(tidyverse)
library(sf)
library(moveHMM)
library(mixtools)
library(lubridate)
library(furrr)

future::plan("sequential") # change if multiple cores and excess RAM are available


# Read in data ------------------------------------------------------------

# We want to use tracks annotated with distance to colonies and SFSs

# List files
trkfiles <- list.files("data/working/bird_tracks/keep/", pattern = "col.rds")


# Fit mixture or HMM in batch ---------------------------------------------

# Read files
trks <- map(trkfiles, ~readRDS(paste0("data/bird_tracks/keep/", .x)))

# Load function to fit batch of mixtures
source("functions/fitMixBatch.R")

# Fit batch
future_map(trks, ~fitMixBatch(.x, fig_path = "output/mixture_plots", 
                              file_path = "data/working/bird_tracks/keep"))

future::plan("sequential")


# Explore state proportions -----------------------------------------------

# List files annotated with states
trkfiles <- list.files("data/working/bird_tracks/keep/", pattern = "mxt.rds")

# Make a data frame to store the results
states_df <- data.frame()

for(i in 1:length(trkfiles)){
   
   # Read in original files from these birds
   trk <- readRDS(paste0("data/working/bird_tracks/keep/", trkfiles[i]))
   
   # Idenfify bird
   id_sel <- unique(trk$bird_id)
   
   # Find state proportions
   trk_states <- trk %>% 
      group_by(bird_id, state) %>% 
      summarize(n = n())
   
   # Calculate proportions
   trk_states <- trk_states %>% 
      mutate(total = sum(n),
             prop = n / total) %>% 
      ungroup()
   
   # Add to general data frame
   states_df <- rbind(states_df,
                      trk_states)
}

# Plot sample sizes
states_df %>% 
   filter(state == 2) %>% 
   ggplot() +
   geom_histogram(aes(x = total))

# Plot proportions in moving state (2)
states_df %>% 
   filter(state == 2) %>% 
   ggplot() +
   geom_histogram(aes(x = prop))

# Find 90% lower quantile of total number of locations
states_df %>% 
   pull(total) %>% 
   unique() %>% 
   quantile(., 0.1)

# Find 90% lower quantile of proportions in state 2
states_df %>% 
   filter(state == 2) %>% 
   pull(prop) %>% 
   quantile(., 0.1)

# Find 90% lower quantile of birds with more than 200 locations in state 2
states_df %>% 
   filter(state == 2, total > 650) %>% 
   pull(prop) %>% 
   quantile(., 0.1)

# Plots proportion of state 2
states_df %>% 
   filter(state == 2) %>% 
   mutate(size = case_when(n <= 200 ~ 1,
                           n > 200 & n <= 1e3 ~ 2,
                           n > 1e3 & n <= 5e3 ~ 3,
                           n > 5e3 ~ 4)) %>% 
   ggplot() +
   geom_point(aes(x = prop, y = bird_id, size = size, col = factor(size)), alpha = 0.7) +
   scale_size_continuous(name = "size", range = c(2,9), labels = c("<200", "200-1e3", "1e3-5e3", ">5e3")) +
   theme(axis.text.y = element_text(size = 8))

# Based on the proportion of locations with moving state 2 of those birds with more
# than 650 locations in total (90%), I will remove those birds with less than 15%
states_df <- states_df %>% 
   filter(state == 2) %>% 
   mutate(keep = if_else(prop < 0.15, 0, 1))

# Plots proportion of state 2
stt_plot <- states_df %>% 
   mutate(size = case_when(n <= 300 ~ 1,
                           n > 300 & n <= 1e3 ~ 2,
                           n > 1e3 & n <= 5e3 ~ 3,
                           n > 5e3 ~ 4)) %>% 
   ggplot() +
   geom_point(aes(x = prop, y = bird_id, size = size, fill = size), alpha = 0.7, pch = 21) +
   geom_point(data = filter(states_df, keep == 0), aes(x = prop, y = bird_id), shape = 4, col = "red", size = 5) +
   scale_size_continuous(name = "n_locs", range = c(2, 9), breaks = c(1, 2, 3, 4), labels = c("<300", "300-1e3", "1e3-5e3", ">5e3")) +
   scale_fill_viridis_c(name = "n_locs", option = "C", limits = c(1, 4), breaks = c(1, 2, 3, 4), labels = c("<300", "300-1e3", "1e3-5e3", ">5e3")) +
   guides(fill = guide_legend(), size = guide_legend()) +
   xlab("Proportion of locations in moving state") +
   theme(axis.text.y = element_text(size = 8))

ggsave(filename = "output/prop_state2.png", plot = stt_plot, dpi = 300)

# Save data frame with problem birds
states_df %>% 
   dplyr::select(bird_id, prop, keep) %>% 
   write_csv("data/working/birds_mix_exclude.csv")


# Explore zeros in state 2 ------------------------------------------------

# ez02 and wt03 are good examples
trk <- readRDS("data/working/bird_tracks/keep/ez02_mix.rds")

# Plot lon/lat sequence with state allocation
trk %>% 
   ggplot() +
   geom_point(aes(x = lon, y = lat, col = factor(state)))

trk %>% 
   dplyr::select(datetime, lon, lat, state, spd_h) %>% 
   gather(dim, coord, -c(datetime, state)) %>% 
   group_by(dim) %>% 
   slice(1:50) %>% 
   ggplot() +
   geom_line(aes(x = datetime, y = coord), linetype = 2) +
   geom_point(aes(x = datetime, y = coord, col = factor(state))) +
   facet_wrap("dim", ncol = 1, scales = "free")

# There might be a problem with the resolution
dplyr::select(trk, datetime, dt)

head(difftime(trk$datetime, lag(trk$datetime), units = "hours"))

# remove night-time locations
daylight <- trk %>% 
   dplyr::select(datetime, lat, lon) %>% 
   mutate(date = date(datetime)) %>% 
   slice(1:100) %>% 
   suncalc::getSunlightTimes(data = ., keep = c("sunrise", "sunset"))

trk %>% 
   dplyr::select(datetime, lat, lon) %>% 
   slice(1:100) %>% 
   mutate(day = if_else(datetime > daylight$sunrise & datetime < daylight$sunset, 1, 0))
