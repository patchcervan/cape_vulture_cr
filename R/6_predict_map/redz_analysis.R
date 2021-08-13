library(tidyverse)
library(raster)
library(sf)

rm(list = ls())


# Define regions --------------------------------------------------

SA <- readRDS("data/working/gadm36_ZAF_1_sp.rds")
LST <- readRDS("data/working/gadm36_LSO_1_sp.rds")
SWZ <- readRDS("data/working/gadm36_SWZ_1_sp.rds")
BWA <- readRDS("data/working/gadm36_BWA_1_sp.rds")
NAM <- readRDS("data/working/gadm36_NAM_1_sp.rds")
ZWE <- readRDS("data/working/gadm36_ZWE_1_sp.rds")
MOZ <- readRDS("data/working/gadm36_MOZ_1_sp.rds")

# For Lesotho and Swaziland we will predict for the whole countries rather than
# provinces
LST$NAME_1 <- LST$NAME_0
SWZ$NAME_1 <- SWZ$NAME_0
BWA$NAME_1 <- BWA$NAME_0
NAM$NAME_1 <- NAM$NAME_0
ZWE$NAME_1 <- ZWE$NAME_0
MOZ$NAME_1 <- MOZ$NAME_0

# South Africa gets plotted with Lesotho and Swaziland
SA_total <- rbind(SA,LST,SWZ) %>% 
      st_as_sf() %>% 
      dplyr::select(NAME_1)

SA_total <- SA_total %>% 
      group_by(NAME_1) %>% 
      summarise(m = unique(NAME_1)) %>% 
      st_cast()

range_total <- rbind(SA,LST,SWZ, BWA, NAM, ZWE, MOZ) %>% 
      st_as_sf() %>% 
      dplyr::select(NAME_1)

# Load data ---------------------------------------------------------------

# Load risk
risk_hgt <- raster("../vultRmap_data_aux/risk_maps/risk_hgt_total.tif")
risk <- raster("../vultRmap_data_aux/risk_maps/risk_total.tif")

# Load REDZ
redz1 <- st_read("data/working/Phase1_REDZs/REDZs.shp")
redz2 <- st_read("data/working/Phase2_REDZs/PHASE 2_REDZs.shp")

# Get both shapes in the same coordinate system and combine
redz1 <- st_transform(redz1, st_crs(redz2))

redz <- rbind(dplyr::select(redz1, Name),
              dplyr::select(redz2, Name))

plot(redz)

# Reclassify risk raster
nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)
cutpt <- rev(vultRmap::calcUDquantile(raster::values(risk), udlevels))
reclassM <- matrix(NA, nrow = length(cutpt)-1, ncol = 3)
for(i in seq_len(nrow(reclassM))){
      reclassM[i,] <- c(cutpt[i], cutpt[i+1], i)
}

risk_hgt_UD <- reclassify(risk_hgt, reclassM)
risk_UD <- reclassify(risk, reclassM)


# Analyze risk per province -----------------------------------------------

# Extract provincial risk
plot(risk_UD)
plot(st_geometry(SA_total), add = TRUE)

region_risk <- exactextractr::exact_extract(risk, range_total)

region_risk <- map(region_risk, ~filter(.x, coverage_fraction > 0.5))

range_total <- range_total %>% 
      mutate(risk_total = map_dbl(region_risk, ~sum(.x$value, na.rm = TRUE)),
             risk_avg = map_dbl(region_risk, ~mean(.x$value, na.rm = TRUE)),
             risk_sd = map_dbl(region_risk, ~sd(.x$value, na.rm = TRUE)),
             risk_frac = 100 * risk_total / sum(raster::values(risk), na.rm = TRUE))

range_total

sum(range_total$risk_total)
sum(values(risk), na.rm = T)
sum(values(risk_hgt), na.rm = T)

sum(values(risk), na.rm = T)/1.5


# Analyze risk per REDZ ---------------------------------------------------

# Extract risk values for each REDZ
redz_risk <- exactextractr::exact_extract(risk, redz)
redz_risk <- map(redz_risk, ~filter(.x, coverage_fraction > 0.5))

redz <- redz %>% 
      mutate(risk_total = map_dbl(redz_risk, ~sum(.x$value, na.rm = TRUE)),
             risk_avg = map_dbl(redz_risk, ~mean(.x$value, na.rm = TRUE)),
             risk_sd = map_dbl(redz_risk, ~sd(.x$value, na.rm = TRUE)),
             risk_frac = 100 * risk_total / sum(raster::values(risk), na.rm = TRUE))

# Extract risk hgt values for each REDZ
redz_hgt_risk <- exactextractr::exact_extract(risk_hgt, redz)
redz_hgt_risk <- map(redz_hgt_risk, ~filter(.x, coverage_fraction > 0.5))

redz <- redz %>% 
   mutate(risk_hgt_total = map_dbl(redz_hgt_risk, ~sum(.x$value, na.rm = TRUE)),
          risk_hgt_avg = map_dbl(redz_hgt_risk, ~mean(.x$value, na.rm = TRUE)),
          risk_hgt_sd = map_dbl(redz_hgt_risk, ~sd(.x$value, na.rm = TRUE)),
          risk_hgt_frac = 100 * risk_hgt_total / sum(raster::values(risk_hgt), na.rm = TRUE))

# Order redz by increasing avg hgt risk
redz$Name <- factor(redz$Name, 
                    levels = unique(redz$Name)[order(redz$risk_hgt_avg, decreasing = T)])

arrange(redz, desc(risk_hgt_avg))


# Plot REDZ risk ----------------------------------------------------------

# Relate hgt risk to REDZs
redz_hgt_risk <- map2_dfr(redz$Name, redz_hgt_risk, ~tibble(name = .x, risk = .y$value))

# Clip to South Africa for plotting
risk_df <- as.data.frame(crop(risk_hgt_UD, SA_total), xy = TRUE)

redz_bars <- redz %>% 
   ggplot() +
   geom_pointrange(aes(x = risk_hgt_avg, 
                       xmin = pmax(0, risk_hgt_avg - risk_hgt_sd),
                       xmax = risk_hgt_avg + risk_hgt_sd,
                       y = Name, colour = Name), fatten = 0.5, size = 0.5) +
   scale_y_discrete(limits=rev) +
   scale_colour_viridis_d(option = "D", direction = 1) +
   ylab("") + xlab("risk") +
   theme_bw() +
   theme(axis.text = element_text(size = 10),
         axis.text.y = element_blank(),
         axis.title = element_text(size = 10),
         legend.position = "none")

redz_violplot <- redz_hgt_risk %>% 
      ggplot() +
      geom_violin(aes(x = log(risk), y = name, fill = name)) +
      scale_y_discrete(limits=rev) +
      scale_fill_viridis_d(option = "D", direction = 1) +
      ylab("") +
      theme_bw() +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none")

redz_riskmap <- ggplot() +
   geom_raster(data = risk_df, aes(x = x, y = y, fill = risk_hgt_total),
               alpha = 1, show.legend = FALSE) +
   geom_sf(data = SA_total, fill = NA, linetype = 2, size = 0.3) +
   geom_sf(data = redz, aes(colour = Name), fill = NA) +
   scale_fill_gradient(low = "white", high = "#CC089B") +
   # scale_fill_viridis_c(option = "C", direction = -1) +
   scale_colour_viridis_d(option = "D", direction = 1, name = "") +
   scale_x_continuous(expand = c(0, 0)) +
   scale_y_continuous(expand = c(0, 0)) +
   theme_classic() +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.line = element_blank(),
         legend.text = element_text(size = 10),
         panel.border = element_rect(fill = NA))

redzplot <- gridExtra::arrangeGrob(redz_violplot, redz_bars, redz_riskmap,
                                   nrow = 5, ncol = 3,
                                   layout_matrix = cbind(c(1,1,3,3,3),
                                                         c(1,1,3,3,3),
                                                         c(1,1,3,3,3),
                                                         c(2,2,3,3,3),
                                                         c(2,2,3,3,3)))

ggsave(filename = "text/paper/figures/redz_risk.png", redzplot)
