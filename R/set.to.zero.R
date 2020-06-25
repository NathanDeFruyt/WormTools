#' Set a series of values to 0 (or a threshold value)
#' 
#' This function will set any value to an arbitrary value if below a threshold value (for now). If you enter a column in a dataset, mind to indeed enter the column!
#' @param value The values you want to set to a threshold if 
#' @param output Choose the output value you'd like.
#' @param threshold Choose the threshold
#' @keywords
#' @export
#' @examples
#' set.to.zero

set.to.zero <- function(value, output = 0, threshold=0){
  values <- c()
  for (i in 1:length(value)){
    if (value[i] <= threshold){
      values[i] <- output
    }
    else{
      values[i] <- value[i]
    }
  }
  return(values)
}
