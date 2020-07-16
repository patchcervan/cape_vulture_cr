processLandSpeed <- function(bird, country_map, proj, cut_speed){
    
    # proj <- 32735
    # bird <- new_trk
    # country_map <- sa_map
    # cut_speed <- 150
    
    # Extract bird id
    bird_id <- unique(bird$bird_id)
    
    warning("We assume location data comes originally in lat-lon WGS84!")
    
    # Create name for pdf export file
    filename <- paste("output/data_process/land_speed_", unique(bird$bird_id), ".pdf", sep = "")
    
    # Open pdf device
    pdf(file = filename)
    
    # Print info
    print(paste("Bird: ", bird_id, ", with tag: ", unique(bird$tag_id), sep = ""))
    print(paste("cut speed >", cut_speed, "km/h", sep = " "))
    
    # Plot geographical location
    print(
        ggplot() +
            geom_sf(data = country_map) +
            geom_point(data = bird, aes(x = lon, y = lat), alpha = 0.5) +
            ggtitle(paste(bird_id, ": Location before processing", sep = ""))
    )
    
    # Eliminate locations out of SA land ----------------------------------------
    
    # create a buffer around SA land (0.5 degrees should be about 5 km)
    sa_buffer <- st_buffer(country_map, 0.5) %>% 
        st_union()
    
    # Make sf object
    bird <- st_as_sf(bird, coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE)
    
    # Remove records that are not in SA land
    bird <- st_intersection(bird, st_geometry(sa_buffer))
    
    # Restore order by datetime and recompute dt
    bird <- bird %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")))
    
    
    # Remove locations with speeds that are too high --------------------------
    
    # Project data to UTM
    bird <- bird %>% 
        st_transform(crs = proj) %>% 
        mutate(x = as.double(st_coordinates(.)[,1]),
               y = as.double(st_coordinates(.)[,2]))
    
    # Calculate travelling speed in km/h
    bird_aux <- bird %>% 
        mutate(dx = lead(x) - x,
               dy = lead(y) - y,
               dist = sqrt(dx^2 + dy^2),
               speed = dist / (1000 * dt))
    
    # Plot speed distribution before removing locations
    summary(bird_aux$speed)
    
    print(
        ggplot(bird_aux) +
            geom_histogram(aes(x = speed)) +
            ggtitle(paste(bird_id, ": Speed distribution before processing", sep = ""))
    )
    
    # # Uncomment in case we want to investigate    
    # which(bird_aux$speed > cut_speed)
    # bird_aux[2:6,c("datetime", "dx", "dy", "dt", "speed")]
    # ggplot(bird_aux[3:6,]) +
    #     geom_point(aes(x = x, y = y, col = factor(3:6)))
    
    
    # Remove records with speed greater than cut_speed
    bird_aux <- bird_aux %>% 
        filter(speed < cut_speed)
    
    # Recalculate dt and speed
    bird_aux <- bird_aux %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               dx = lead(x) - x,
               dy = lead(y) - y,
               dist = sqrt(dx^2 + dy^2),
               speed = dist / (1000 * dt))
    
    # Plot speed distribution before removing locations
    summary(bird_aux$speed)
    
    print(
        ggplot(bird_aux) +
            geom_histogram(aes(x = speed)) +
            ggtitle(paste(bird_id, ": Speed distribution after processing - cut speed = ", cut_speed, sep = ""))
    )
    # Geographical location
    print(
        ggplot() +
            geom_sf(data = sa_map) +
            geom_point(data = bird, aes(x = lon, y = lat, col = datetime), alpha = 0.5) +
            ggtitle(paste(bird_id, ": Location after processing - cut speed = ", cut_speed, sep = ""))
    )
    
    dev.off()
    
    return(
        bird_aux %>% 
            select(colnames(bird)) %>% 
            st_drop_geometry()
    )
    
}

