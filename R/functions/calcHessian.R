hessian <- function(funfcn, x, y, delta){
      # funfcn <- Obj$fn
      # x <- Opt$par
      # delta <- 0.00001
      # y <- data.bundle$z
      
      
      
      # function to calculate the hessian of funfcn at x
      t <- length(x)
      h <- matrix(0, t, t)
      Dx <- delta*diag(t)
      for (i in 1:t) {
            for (j in 1:i) {
                  h[i,j] <- (do.call("funfcn", list(x + Dx[i,] + Dx[j,], y)) # LL with
                             - do.call("funfcn", list(x + Dx[i,] - Dx[j,], y))
                             - do.call("funfcn", list(x - Dx[i,] + Dx[j,], y))
                             + do.call("funfcn",list(x - Dx[i,] - Dx[j,], y)))/(4*delta^2)
                  h[j,i] <- h[i,j]
            }
      }
      return(h)
}