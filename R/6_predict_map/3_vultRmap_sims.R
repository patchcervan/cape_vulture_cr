# 24-06-2021

# In this script we simulate vulture activity around the colonies based on 
# the fitted SSF model.

# Simulations can be very slow and it is adviced to run them in a machine with
# multiple cores. Preferably a high performance cluster or similar. Running
# 120k steps for each colony takes 2 days in a machine with 40 cores.

library(vultRmap)

# Set future maxsize to 650MB
options(future.globals.maxSize = 850*1024^2)


# Simulate for one colony -------------------------------------------------

# Load colony data to find a colony
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

system.time(
      sims <- vultRmap::simOneColony(age = "juv",
                                     totalsteps = 1000,
                                     ncores = 5,
                                     col_sel = unlist(col_all[303, c("lon", "lat")]),
                                     set_seed = round(runif(1, 1, 1e5)),
                                     dist_lim = 1000,
                                     sample_coefs = 5,
                                     data_dir = "../vultRmap_data_aux")
)


# Simulate for all colonies -----------------------------------------------

# Define minimum size of colony (number of adults) and
# roosts (total number of birds)
min_size_col <- 1
min_size_roost <- 50

# We will need to calculate distance to other colonies
col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")

# Subset colonies to we have counts for
col_to_pred <- col_all %>%
      dplyr::filter(!is.na(avg_ad)) %>%
      dplyr::filter((type == "breed" & avg_ad >= min_size_col) |
                          (type == "roost" & (avg_ad + avg_juv) >= min_size_roost))

# In case we want to exclude some colonies
# col_to_pred <- col_to_pred[-(1:167),]

vultRmap::simAllColonies(col_to_pred = col_to_pred,
                         age = "ad",
                         totalsteps = 120000,
                         ncores = 40,
                         set_seed = 6548,
                         dist_lim = 1200,
                         sample_coefs = 40,
                         out_dir = "/home/crvfra001/vults/output/vultRmap/sims",
                         data_dir = "/home/crvfra001/vults/data/vultRmap_data_aux")
