removeSpeedOutliers <- function(bird, country_map, cut_speed, filepath){
    
    # bird <- new_trk
    # country_map <- sa_map
    # cut_speed <- 200
    # filepath <- "output/data_process/"
    
    # save original variables
    var_orig <- colnames(bird)
    
    # Extract bird id
    bird_id <- unique(bird$bird_id)
    
    warning("We assume location data is projected to UTM!")
    
    # Print info
    print(paste("bird: ", bird_id, ", with tag: ", unique(bird$tag_id), sep = ""))
    print(paste("cut speed >", cut_speed, "km/h", sep = " "))
    
    # Plot geographical location
    geopre <- ggplot() +
        geom_sf(data = country_map) +
        geom_point(data = bird, aes(x = lon, y = lat), alpha = 0.5) +
        ggtitle(paste(bird_id, ": Location before processing", sep = ""))
    
    
    # Remove locations with speeds that are too high --------------------------
    
    # Calculate travelling speed in km/h (speed to reach location not to depart from location!)
    bird <- bird %>% 
        mutate(dx = x - lag(x),
               dy = y - lag(y),
               dist = sqrt(dx^2 + dy^2),
               speed = dist / (1000 * lag(dt)))
    
    
    # Plot speed distribution before removing locations
    summary(bird$speed)
    
    locspre <- bird %>% 
        st_drop_geometry() %>% 
        select(datetime, x, y) %>% 
        gather(coord, loc, -1) %>% 
        ggplot() +
        geom_point(aes(x = datetime, y = loc, group = coord)) +
        geom_line(aes(x = datetime, y = loc, group = coord)) +
        facet_grid(coord ~ ., scales = "free") +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        xlab("Time") + ylab("UTM coordinate") +
        ggtitle(paste(bird_id, ": Locations before processing", sep = ""))
    
    velpre <- bird %>% 
        ggplot() +
        geom_point(aes(x = datetime, y = speed)) +
        geom_line(aes(x = datetime, y = speed)) +
        ggtitle(paste(bird_id, ": Speed before processing (km/h)", sep = ""))
    
    histpre <- ggplot(bird) +
        geom_histogram(aes(x = speed)) +
        ggtitle(paste(bird_id, ": Speed distribution before processing (km/h)", sep = ""))
    
    mpre <- gridExtra::arrangeGrob(
        grobs = list(locspre, velpre, histpre),
        ncol = 1, nrow = 3
    )
        
    
    # Remove records with speed greater than cut_speed.
    # I include a correction to account for the fact that it is more unlikely that 
    # a bird sustains high speeds for long periods of time.
    
    # First create an auxliary effective speed variable (corrected by dt).
    # If dt is greater than 1 hour the auxiliary speed gets penalized (increased)
    bird <- bird %>% 
        mutate(speed_aux = if_else(lag(dt) > 1, speed * (1 + log(lag(dt))), speed))
    
    while(any(bird$speed_aux > cut_speed, na.rm = TRUE)){
        bird <- bird %>% 
            filter(speed_aux < cut_speed) %>% 
            # Recalculate dt and speed
            arrange(datetime) %>%       # Sort data by date before computing dt
            mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
                   dx = x - lag(x),
                   dy = y - lag(y),
                   dist = sqrt(dx^2 + dy^2),
                   speed = dist / (1000 * lag(dt)),
                   speed_aux = if_else(lag(dt) > 1, speed * (1 + log(lag(dt))), speed))
    }
    
    # Geographical location after removing outlying speeds
    geopost <- ggplot() +
            geom_sf(data = sa_map) +
            geom_point(data = bird, aes(x = lon, y = lat, col = datetime), alpha = 0.5) +
            ggtitle(paste(bird_id, ": Location after processing - cut speed = ", cut_speed, sep = ""))
    
    # Combine with geographical location before removing
    mgeo <- gridExtra::arrangeGrob(
        grobs = list(geopre, geopost),
        ncol = 1, nrow = 2
    )

    # Plot speed distribution after removing locations
    locspost <- bird %>% 
        st_drop_geometry() %>% 
        select(datetime, x, y) %>% 
        gather(coord, loc, -1) %>% 
        ggplot() +
        geom_point(aes(x = datetime, y = loc, group = coord)) +
        geom_line(aes(x = datetime, y = loc, group = coord)) +
        facet_grid(coord ~ ., scales = "free") +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        xlab("Time") + ylab("UTM coordinate") +
        ggtitle(paste0(bird_id, ": Locations after processing", " - cut speed ", cut_speed, " (km/h)"))
    
    velpost <- bird %>% 
        ggplot() +
        geom_point(aes(x = datetime, y = speed)) +
        geom_line(aes(x = datetime, y = speed)) +
        ggtitle(paste0(bird_id, ": Speed after processing", " - cut speed ", cut_speed, " (km/h)"))
    
    histpost <- ggplot(bird) +
        geom_histogram(aes(x = speed)) +
        ggtitle(paste0(bird_id, ": Distribution of speed after processing", " - cut speed ", cut_speed, " (km/h)"))
    
    mpost <- gridExtra::arrangeGrob(
        grobs = list(locspost, velpost, histpost),
        ncol = 1, nrow = 3
    )
    

    
    plots <- list(mgeo, mpre, mpost)
    
    for(i in 1:3){
        ggsave(plots[[i]], file = paste0(filepath, bird_id, "_plot",i , ".png"))
    }

    
    # Return a data frame with the original variables
    return(
        bird %>% 
            select(all_of(var_orig)) %>% 
            st_drop_geometry()
    )
    
}

