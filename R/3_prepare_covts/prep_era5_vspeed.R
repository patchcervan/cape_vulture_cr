# 18-11-2020

# In this script we download and explore vertical air speed using
# ERA5 pressure dataset.

rm(list = ls())

library(ecmwfr)
library(raster)
library(ncdf4)

# Define years and months to analyse
years <- as.character(c(2016))
months <- paste0("0", c(1:5))

for(i in seq_along(years)){
      for(j in seq_along(months)){
            
            filename <- paste("era5_vspeed", years[i], months[j], sep = "_")
            
            request <- list("dataset_short_name" = 'reanalysis-era5-pressure-levels-monthly-means',
                            'product_type' = 'monthly_averaged_reanalysis',
                            'variable' = 'vertical_velocity',
                            'pressure_level' = '1000',
                            'year' = years[i],
                            'month' = months[j],
                            'time' = '00:00',
                            'area' = "-14/ 11/ -35/39",
                            'format' = 'netcdf',
                            "target" = paste0(filename, ".nc"))
            
            wf_request(user     = "67260",   # user ID (for authentification)
                       request  = request,  # the request
                       transfer = TRUE,     # download the file
                       path     = "data/working/covts_rasters/era5") 
            
      }
}

rfiles <- as.list(paste0("data/working/covts_rasters/era5/era5_vspeed_", paste(years, months, sep = "_"), ".nc"))
r <- brick(rfiles)

r <- brick("data/working/covts_rasters/era5/era5_vspeed_march_900.nc")

plot(r)

rsd <- calc(r, sd)
plot(rsd)

rmean <- calc(r, mean)
plot(rmean)
