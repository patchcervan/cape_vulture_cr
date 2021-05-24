hessianmethod <- function(funfcn, pars, y, delta, print = TRUE){
      
      # funfcn <- Obj$fn
      # pars <- Opt$par
      # delta <- 0.00001
      # y <- data.bundle$z
      
      source("R/functions/calcHessian.R")
      
      # Applies the Hessian method
      # funfcn - function which return negative loglikelihood
      # pars - parameter values at which hessian is evaluated
      # y - data to be passed into likelihood
      # delta - error used in calculating Hessian and cutt-off
      # suggested value delta = 0.00001
      
      # cut-off used delta*p, where p is no. pars
      cutoff <- delta*length(pars)
      
      # Finds hessian matrix
      h <- do.call("hessian",list(funfcn, pars, y, delta))
      
      # Calculates eigenvalues
      E <- eigen(h)
      
      # find standardised eigenvalues
      standeigenvalues <- abs(E$values)/max(abs(E$values))
      
      # find number of estimable parameters
      # number of parameters with eigenvalues below the cutoff
      noestpars <- 0
      
      for (i in 1:length(pars)) {
            if (standeigenvalues[i] > cutoff) {
                  noestpars <- noestpars + 1
            }
      }
      
      if (print) {
            # Prints whether model is parameter redundant or not
            # Then prints smallest eigenvalue and
            # number of estimable parameters
            if (min(standeigenvalues) < cutoff) {
                  cat("model is non-identifiable or parameter redundant")
            }
            else {
                  cat("model is identifiable or not parameter redundant")
            }
            cat("\n")
            cat('smallest standardized eigenvalue', min(standeigenvalues))
            cat("\n")
            cat('number of estimable parameters', noestpars)
      }
      
      result <- list(standeigenvalues = standeigenvalues, noestpars = noestpars)
      
      return(result)
}