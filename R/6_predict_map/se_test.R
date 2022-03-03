# In this script we estimate the uncertainty in step-selection coefficients

library(vultRmap)
library(dplyr)
library(ggplot2)

# Set future maxsize to 650MB
options(future.globals.maxSize = 850*1024^2)

# Define minimum size of colony (number of adults) and
# roosts (total number of birds)
min_size_col <- 1
min_size_roost <- 50

# We will need to calculate distance to other colonies
col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")
sfs <- utils::read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

# For debugging
# col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")

# Subset colonies to we have counts for
col_to_pred <- col_all %>%
  dplyr::filter(!is.na(avg_ad)) %>%
  dplyr::filter((type == "breed" & avg_ad >= min_size_col) |
                  (type == "roost" & (avg_ad + avg_juv) >= min_size_roost))

# In case we want to exclude some colonies
# col_to_pred <- col_to_pred[-(1:3),]

# Define ages
ages <- c("ad", "juv")
age <- ages[2]

# And output suffixes
suff <- c("", "_v2")

# Define simulation parameters --------------------------------------------

# Sample random coefficients if necessary
ssf_coef <- vultRmap::sampleSsfCoef(nind = 1)[[1]]

# Loop through colonies ---------------------------------------------------

col_sel <- col_to_pred[1,]

# Prepare habitat for simulations

# This may take some minutes if max_range is large (>500)
hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon","lat")]), max_range = 1200,
                            col_all = col_all, sfs = sfs, scale = "ssf")

hab <- vultRmap::completeDataFrame(hab, names(ssf_coef), 0)

# Simulate activity

# # FIXES
# .age <- "juv"
# .hab <- hab
# .ssf_coef <- ssf_coef[[1]]
# 
# 
# # Transform maxdist to meters
# .maxdist <- 1200

# Correct age related predictors
if(age != "juv"){
  hab <- hab %>%
    dplyr::mutate(dplyr::across(dplyr::contains("juv"), ~.x*0))
}

# Predict from habitat
X <- vector("list", 15)
SE <- vector("list", 15)

Rcpp::sourceCpp("R/eigenMatMult.cpp")

deltam <- function(X, Q){
  
  M <- eigenMapMatMult(X, Q)
  M2 <- eigenMapMatMult(M, t(X))
  
  gc()
  
  out <- diag(M2)
  
  return(out)
}


future::plan("multisession", workers = 2)

for(t in 1:15){
  ttnoon <- -8 + t

  predt <- hab %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("ttnoon"), ~.x*ttnoon),
                  dplyr::across(.cols = dplyr::ends_with("ttnoon_sq"), ~.x*ttnoon^2)) %>%
    dplyr::select(dplyr::all_of(names(ssf_coef)))

  X[[t]] <- as.matrix(predt) %*% ssf_coef

  n <- 50
  nr <- nrow(predt)
  mm <- split(predt, rep(1:n, each = ceiling(nr/n), length.out = nr))
  
  mm <- lapply(mm, as.matrix)

  SE[[t]] <- furrr::future_map(mm, ~deltam(.x, ssf_fit_summary$vcov$cond))
  SE[[t]] <- do.call("c", SE[[t]])
  
}

future::plan("sequential")

SE <- readRDS("hpc/output/ssf_se_test.rds")

SE_mean <- Reduce("+", SE)/15

testse <- do.call("c", SE[[1]])

out_df <- hab %>% 
  dplyr::select(lon, lat) %>% 
  mutate(se = SE)

out_df %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = se))

paste0("x", 1:25)


x <- 1:100
y <- rnorm(100, 4*x, 5)

lmdat <- data.frame(int = 1,
                    x = x,
                    y = y)

toy.lm <- lm(y ~ -1 + int + x, data = lmdat)
estmean <- coef(toy.lm)
estvar <- summary(toy.lm)$cov.unscaled * summary(toy.lm)$sigma^2

preds <- predict(toy.lm, newdata = lmdat, se.fit = T)
preds$se.fit

f <- function(a, estvar){

  out <- msm::deltamethod(~ x1 + x2, a, estvar)

  return(as.numeric(out))

}

msm::deltamethod(~ x1 + x2, mean = c(0,0), estvar)

f(a = c(1,10), estvar)

as.matrix(lmdat[,-3]) %*% estvar %*% t(as.matrix(lmdat[,-3]))

sqrt(diag(as.matrix(lmdat[,-3]) %*% estvar %*% t(as.matrix(lmdat[,-3]))))



x <- 1:100
y <- rpois(100, 4*x)

glmdat <- data.frame(int = 1,
                     x = x,
                     y = y)

toy.glm <- glm(y ~ -1 + int + x, data = glmdat, family = "poisson")
estmean <- coef(toy.lm)
estvar <- summary(toy.lm)$cov.unscaled * summary(toy.lm)$sigma^2

preds <- predict(toy.glm, newdata = glmdat, se.fit = T, type = "link")
preds$se.fit

exp(preds$fit)
exp(preds$se.fit)
sqrt(exp(preds$fit)^2 * preds$se.fit^2) # This is good

preds_resp <- predict(toy.glm, newdata = glmdat, se.fit = T, type = "response")
preds_resp$fit
preds_resp$se.fit
