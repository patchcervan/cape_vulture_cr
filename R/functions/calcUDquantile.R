# This function calculates the CDF of a utilization distribution, passed on as
# a vector of densities/probabilities (p). It returns the highest value of p that
# captures the desired level of activity proportion, as specified in probs. probs
# can be a vector.

calcUDquantile <- function(ud, probs){
      
      # Calculate the distribution function
      ud <- sort(ud)
      cumud <- cumsum(ud)/sum(ud)
      
      # Calculate the minimum value that captures at least a UD  proportion of probs
      qs <- sapply(1-probs, function(x) min(ud[cumud >= x]))
      
      return(qs)
      
}