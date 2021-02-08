# This function APPROXIMATES the transformation from meters to degrees and back
meterToDegree <- function(meters = NULL, degrees = NULL){
      if(is.null(degrees)) return(360*meters/(2*pi*6371000))
      if(is.null(meters)) return(2*pi*6371000*degrees/360)
}
