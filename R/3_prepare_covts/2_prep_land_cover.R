# 2-11-2020

# In this script we will create some custom codes for the Copernicus
# habitat classes

rm(list = ls())

library(tidyverse)

hab <- read_csv(file = "data/raw/copernicus_codes.csv")

# Reclassify to simplify
hab <- hab %>% 
      rename(orig_class = 'Land Cover Class') %>% 
      mutate(custom_code = case_when(str_detect(orig_class, "Closed forest") ~ "forest_c",
                                     str_detect(orig_class, "Open forest") ~ "forest_o",
                                     str_detect(orig_class, "Shrubs") ~ "shrub",
                                     str_detect(orig_class, "Herbaceous") ~ "grass",
                                     str_detect(orig_class, "Moss and lichen") ~ "bare",
                                     str_detect(orig_class, "Bare") ~ "bare",
                                     str_detect(orig_class, "Cultivated") ~ "crops",
                                     str_detect(orig_class, "Urban") ~ "urban",
                                     str_detect(orig_class, "Snow") ~ "snow",
                                     str_detect(orig_class, "water") ~ "water",
                                     str_detect(orig_class, "sea") ~ "water"))

# Add a class category to simplify even further
hab <- hab %>% 
      mutate(class_code = case_when(str_detect(orig_class, "forest") ~ "closed",
                                     str_detect(orig_class, "Shrubs") ~ "open",
                                     str_detect(orig_class, "Herbaceous") ~ "open",
                                     str_detect(orig_class, "Moss and lichen") ~ "open",
                                     str_detect(orig_class, "Bare") ~ "open",
                                     str_detect(orig_class, "Cultivated") ~ "crops",
                                     str_detect(orig_class, "Urban") ~ "urban",
                                     str_detect(orig_class, "Snow") ~ "open",
                                     str_detect(orig_class, "water") ~ "water",
                                     str_detect(orig_class, "sea") ~ "sea"))

hab %>% print(n = Inf)

write_csv(hab, "data/working/copernicus_codes.csv")
             