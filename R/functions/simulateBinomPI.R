# Modified from ciTools::add_pi.glm
# https://github.com/jthaman/ciTools/blob/master/R/add_pi_glm.R

simulateBinomPi <- function(df, fit, alpha = 0.05, names = NULL, yhatName = "pred",
                        nSims = 2000){
    
    if (is.null(names)) {
        names[1] <- paste0("plb", 100-alpha*100)
        names[2] <- paste0("pub", 100-alpha*100)
    }
    
    if ((names[1] %in% colnames(df)))
        warning ("These PIs may have already been appended to your dataframe. Overwriting.")
    
    if(max(fit$prior.weights) == 1)
            stop("Prediction intervals for Bernoulli response variables aren't useful")
    else {
        warning("Treating weights as indicating the number of trials for a binomial regression where the response is the proportion of successes")
        warning("The response variable is not continuous so Prediction Intervals are approximate")
    }
    
    out <- predict(fit, newdata = df, type = "response")
    
    nPreds <- NROW(df)
    modmat <- model.matrix(fit, data = df)
    response_distr <- fit$family$family
    inverselink <- fit$family$linkinv
    overdisp <- summary(fit)$dispersion
    sims <- arm::sim(fit, n.sims = nSims) # what does this do?
    sim_response <- matrix(NA, ncol = nSims, nrow = nPreds)
    
    for (i in 1:nSims){
        
        yhat <- inverselink(modmat %*% sims@coef[i,]) * df$weights
        sim_response[,i] <- rbinom(n = nPreds,
                                   size = df$weights,
                                   prob = yhat / df$weights)
        
    }

    lwr <- apply(sim_response, 1, FUN = quantile, probs = alpha/2, type = 1)
    upr <- apply(sim_response, 1, FUN = quantile, probs = 1 - alpha / 2, type = 1)
    
    if(is.null(df[[yhatName]]))
        df[[yhatName]] <- out
    df[[names[1]]] <- lwr / df$weights
    df[[names[2]]] <- upr / df$weights
    data.frame(df)
}
