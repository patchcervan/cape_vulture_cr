# This function takes the attributes of scaled variables and
# uses them to bring them back to the original scale

# If y is supplied the attributes of y are used to scale x (not center!)

backScale <- function(x, y = NULL){
      if(is.null(y)){
            x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
      } else {
            x * attr(y, 'scaled:scale')
      }
}