#' Replacing NA values by a constant. 
#' 
#' This function allows you to replace any NA that occur by a value. Chose the value wisely so that it will not interfere with your data, but will allow you to e.g. divide it or execute logical statements.
#' @param data Here goes the data
#' @param replacement Here goes your replacement
#' @keywords
#' @export
#' @examples
#' na.replace

na.replace <- function(data, replacement){
  if (class(data) == 'data.frame'){
    for (i in 1:nrow(data)){
      for (j in 1:ncol(data)){
        if (data[i, j] == NA){
          data[i,j] <- replacement
        } # if
      } # for j
    } # for i
  }

} #function