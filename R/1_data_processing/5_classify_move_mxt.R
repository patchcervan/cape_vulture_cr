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
trkfiles <- list.files("data/working/bird_tracks/in_process/", pattern = "col.rds")


# Fit mixture or HMM in batch ---------------------------------------------

# Read files
trks <- map(trkfiles, ~readRDS(paste0("data/working/bird_tracks/in_process/", .x)))

# Load function to fit batch of mixtures
source("R/functions/fitMixBatch.R")

# Fit batch
future_map(trks, ~fitMixBatch(.x, fig_path = "output/mixture_plots", 
                              file_path = "data/working/bird_tracks/in_process"))

future::plan("sequential")


# Explore state proportions -----------------------------------------------

# List files annotated with states
trkfiles <- list.files("data/working/bird_tracks/in_process/", pattern = "mxt.rds")

# Make a data frame to store the results
states_df <- data.frame()

for(i in 1:length(trkfiles)){
   
   # Read in original files from these birds
   trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trkfiles[i]))
   
   # Idenfify bird
   id_sel <- unique(trk$bird_id)
   
   # Calculate state proportions
   trk_states <- trk %>% 
      group_by(bird_id, state) %>% 
      summarize(n = n()) %>% 
      mutate(total = sum(n),
             prop = n / total) %>% 
      ungroup()
   
   # Add to general data frame
   states_df <- rbind(states_df,
                      trk_states)
}

# Proportions of ma07 should be NA because we don't know the states
states_df <- states_df %>% 
   mutate(prop = ifelse(bird_id == "ma07", NA, prop))

# Plot sample sizes
states_df %>% 
   filter(!is.na(prop)) %>% 
   filter(state == 2) %>% 
   ggplot() +
   geom_histogram(aes(x = total))

# Plot proportions in moving state (2)
states_df %>% 
   filter(!is.na(prop)) %>% 
   filter(state == 2) %>% 
   ggplot() +
   geom_histogram(aes(x = prop))

# Find 90% lower quantile of total number of locations
states_df %>% 
   filter(!is.na(prop)) %>% 
   pull(total) %>% 
   unique() %>% 
   quantile(., probs = 0.1, type = 1)

# Find 90% lower quantile of proportions in state 2
states_df %>% 
   filter(!is.na(prop)) %>% 
   filter(state == 2) %>% 
   pull(prop) %>% 
   quantile(., 0.1)

# With the 90% birds with more locations, find 90% lower quantile of locations in state 2
states_df %>% 
   filter(!is.na(prop)) %>% 
   filter(state == 2, total >= 590) %>% 
   pull(prop) %>% 
   quantile(., 0.1)


# Based on the proportion of locations with moving state 2 of those birds with 590
# locations or more (91%), I will remove those birds with less than 14%
states_df <- states_df %>% 
   filter(state == 2 |is.na(state)) %>% 
   mutate(keep = if_else(prop > 0.145, 1, 0))

# Plots proportion of state 2
stt_plot <- states_df %>% 
   mutate(size = case_when(n <= 300 ~ 1,
                           n > 300 & n <= 1e3 ~ 2,
                           n > 1e3 & n <= 5e3 ~ 3,
                           n > 5e3 ~ 4)) %>% 
   ggplot() +
   geom_point(aes(x = prop, y = bird_id, size = size, fill = size), alpha = 0.7, pch = 21) +
   geom_point(data = filter(states_df, keep == 0), aes(x = prop, y = bird_id), shape = 4, col = "red", size = 5) +
   geom_vline(aes(xintercept = 0.14), linetype = 2) +
   scale_size_continuous(name = "n_locs", range = c(2, 9), breaks = c(1, 2, 3, 4), labels = c("<300", "300-1e3", "1e3-5e3", ">5e3")) +
   scale_fill_viridis_c(name = "n_locs", option = "C", limits = c(1, 4), breaks = c(1, 2, 3, 4), labels = c("<300", "300-1e3", "1e3-5e3", ">5e3")) +
   guides(fill = guide_legend(), size = guide_legend()) +
   xlab("Proportion of locations in moving state") +
   theme(axis.text.y = element_text(size = 8))

ggsave(filename = "output/prop_state2.png", plot = stt_plot, dpi = 300)

# Save data frame with problem birds
states_df %>% 
   dplyr::select(bird_id, prop, keep) %>% 
   write_csv("data/working/birds_mxt_exclude.csv")


# Explore zeros in state 2 ------------------------------------------------

# Explore mt02
trk <- readRDS("data/working/bird_tracks/in_process/mt02_mxt.rds")

# Plot lon/lat sequence with state allocation
trk %>% 
   ggplot() +
   geom_point(aes(x = lon, y = lat, col = factor(state)))

trk %>% 
   dplyr::select(datetime, lon, lat, state, spd_h) %>% 
   gather(dim, coord, -c(datetime, state)) %>% 
   group_by(dim) %>% 
   slice(250:350) %>% 
   ggplot() +
   geom_line(aes(x = datetime, y = coord), linetype = 2) +
   geom_point(aes(x = datetime, y = coord, col = factor(state))) +
   facet_wrap("dim", ncol = 1, scales = "free")

# There might be a problem with the transitions between states
# they might be wrongly classified


# Explore speed units of ct birds -----------------------------------------

# Explore ct08
trk <- readRDS("data/working/bird_tracks/in_process/ct08_col.rds")

hist(trk$spd_h)
hist(trk$avg_spd)

# there seems to be very little movement no matter what variable we use