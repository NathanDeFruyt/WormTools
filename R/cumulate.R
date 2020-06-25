#' Cumulate
#'  
#' This function cumulates ordered stuff
#' @param values The values you'd like to cumulate 
#' @keywords
#' @export
#' @examples
#' cumulate


cumulate <- function(values){
  ll = c()
  previous = 0
  for (i in 1:length(values)){
    current = previous + values[i]
    ll = c(ll, current)
    previous = current
  }
  return(ll)
}
