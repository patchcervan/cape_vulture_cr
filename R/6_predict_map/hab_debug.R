.hab <- .hab %>% 
      mutate(predh1 = X[[1]],
             predh5 = X[[5]],
             predh10 = X[[10]],
             predh15 = X[[15]])

.hab %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = predh10)) +
      scale_fill_viridis_c()

.hab %>% 
      filter(predh10 < -7) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = exp(predh10))) +
      scale_fill_viridis_c()

.ssf_coef

sl_coef <- .ssf_coef[str_detect(names(.ssf_coef), "dist_col")]
sl_coef <- sl_coef[!str_detect(names(sl_coef), "any")]
sl_coef <- sl_coef[!str_detect(names(sl_coef), "log")]

dist_col <- 0.1
ttnoon <- -7:7

sl_coef

eff <- vector(length = 15)
for(i in 1:15){
      i=15
      eff[i] <- sum(c(1, 1, ttnoon[i], ttnoon[i]^2, ttnoon[i], ttnoon[i]^2)*sl_coef) 
}

eff

log_sl_coef <- .ssf_coef[str_detect(names(.ssf_coef), "log_dist_col")]

dist_col <- 0.01
ttnoon <- -7:7

log_sl_coef

eff_log <- sum(c(log(dist_col), log(dist_col)) * log_sl_coef)


stephab %>% 
      ggplot() +
      geom_point(aes(x = .sls, y=wgamma))

stephab$.sls[stephab$wgamma > quantile(stephab$wgamma, 0.99)]

stephab %>% 
      ggplot() +
      geom_point(aes(x = .sls, y=predh))

stephab %>% 
      ggplot() +
      geom_point(aes(x = .sls, y=w))

stephab %>% 
      ggplot() +
      geom_point(aes(x = dist_col_sc, y=w))

stephab %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = w)) +
      scale_fill_viridis_c()

stephab %>% 
      mutate(wgammap = if_else(wgamma > quantile(wgamma, 0.99), quantile(wgamma, 0.99), wgamma),
             wp = wgammap*predh) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = wp)) +
      scale_fill_viridis_c()



stephab %>% 
      mutate(wgamma = wgamma/(0.0065*.sls),
             w = w/(0.0065*.sls)) %>% 
      ggplot() +
      geom_point(aes(x = .sls, y=w))

quantile(stephab$w, 0.99)

stephab %>% 
      mutate(wp = if_else(w > quantile(w, 0.99), quantile(w, 0.99), w)) %>% 
      mutate(predhp = if_else(predh > quantile(predh, 0.99), quantile(predh, 0.99), predh)) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = wp)) +
      scale_fill_viridis_c()

stephab %>% 
      mutate(wgamma = wgamma/(0.0065*.sls),
             wgammap = if_else(wgamma > quantile(wgamma, 0.99), quantile(wgamma, 0.99), wgamma),
             wp = predh*wgammap) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = wp)) +
      scale_fill_viridis_c()

stephab %>% 
      mutate(wgamma = wgamma/(0.0065*.sls),
             wgammap = if_else(wgamma > quantile(wgamma, 0.99), quantile(wgamma, 0.99), wgamma),
             wp = predh*wgammap) %>% 
      ggplot() +
      geom_point(aes(x = .sls, y = wp))
