#' A4 page measures
#' 
#' @param metric = c('cm', 'inch')
#' @keywords
#' @export
#' @examples
#' 
#' A4.measures

A4.measures <- function(metric = c('cm', 'inch')){
  library(WormTools)
  width = 19.05
  length = 27.517
  measures = list()
  if (metric == 'cm'){
    measures$width <- width
    measures$length <- length
  }
  if (metric == 'inch'){
    width = cm2inch(width)
    length = cm2inch(length)
    measures$width <- width
    measures$length <- length
  }
  return(measures)

}
