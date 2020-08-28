# 06-08-2020

# In this script we explore the Cape Vulture telemetry database

rm(list = ls())

library(tidyverse)


# Read in database --------------------------------------------------------

db <- read_csv("data/working/bird_db.csv")

# db <- db %>% 
#     mutate(date_start = as.POSIXct(date_start, tz = "GMT", format = "%m/%d/%y"),
#            date_end = as.POSIXct(strftime(db$date_end, format = "%m/%d/%y"), tz = "GMT", format = "%m/%d/%y"))



# Explore temporal coverage -----------------------------------------------

tl <- db %>% 
    ggplot() + 
    geom_segment(aes(x = date_start, xend = date_end, 
                     y = bird_id, yend = bird_id, col = age), size = 2) + 
    geom_vline(xintercept = lubridate::mdy(paste0("01/01/", 2003:2020)), 
               colour = "grey") +
    xlab("") + ggtitle("Timeline of tracked birds") +
    # scale_x_date(breaks = lubridate::y(paste0("01/01/", 2003:2020))) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 8))


ggsave(tl, filename = "figures/timeline.png", dpi = 500)



# Explore tracking resolution ---------------------------------------------

ttl <- db %>% 
    ggplot() +
    geom_pointrange(aes(x = avg_dt, y = bird_id,
                        xmin = 0,
                        xmax = avg_dt + sd_dt,
                        col = age)) +
    xlab("Time to location (hours)") +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 8))
    
ggsave(ttl, filename = "figures/timetolocation.png", dpi = 500)

ttl2 <- db %>% 
    ggplot() +
    geom_pointrange(aes(x = avg_dt, y = bird_id,
                        xmin = 0,
                        xmax = avg_dt + sd_dt,
                        col = age)) +
    xlab("Time to location (hours)") +
    coord_cartesian(xlim = c(0, 50)) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 8))

ggsave(ttl2, filename = "figures/timetolocation2.png", dpi = 500)


# Resolution histogram
tres <- db %>% 
    ggplot() +
    geom_histogram(aes(x = avg_dt), binwidth = 1) +
    stat_bin(aes(x = avg_dt), binwidth = 1, col = "white") +
    stat_bin(aes(x = avg_dt, y = 0.8*(..count..), label = ..count..), binwidth = 1,
             col = "white", geom = "text", ) +
    ggtitle("Time resolution of the different birds")

ggsave(tres, filename = "figures/timeresolution.png", dpi = 500)


# Birds with good duration and resolution ---------------------------------

# Filter birds
db_sel <- db %>% 
    mutate(dur = date_end - date_start) %>% 
    filter(dur > 300, avg_dt < 2) %>% 
    print(n = Inf)

# Explore timeline
tl_sel <- db_sel %>% 
    ggplot() + 
    geom_segment(aes(x = date_start, xend = date_end, 
                     y = bird_id, yend = bird_id, col = age), size = 2) + 
    geom_vline(xintercept = lubridate::mdy(paste0("01/01/", 2003:2020)), 
               colour = "grey") +
    xlab("") + ggtitle("Birds with good time coverage (> 300 days) and resolution (< 2 hours avg)") +
    # scale_x_date(breaks = lubridate::y(paste0("01/01/", 2003:2020))) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 8))

ggsave(tl_sel, filename = "figures/timeline_sel.png", dpi = 500)


# Create a PDF with all the plots -----------------------------------------

pdf(file = "output/db_exploration.pdf")

tl

ttl
ttl2
tres

tl_sel

dev.off()