#' Acknowledge
#'  
#' Highlight your results with, what else: asterisks!
#' @param value or values. The values you'd like to acknowledge
#' @param list a list with the values of interest
#' @keywords
#' @export
#' @examples
#' acknowledge

acknowledge <- function(value, listie){
  if (length(value) > 1){
    ll = c()
    for (i in c(1:length(value))){
      if (value[i] %in% listie){
        ll = c(ll, '*')
      }
      else{
        ll = c(ll, ' ')
      } # else
    } # for value
    return(ll)
  } # if not singular
  else{
    if (value %in% listie){
      return('*')
    }
    else{
      return(' ')
    }
  }
  }