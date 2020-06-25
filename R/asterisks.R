#' Symbolizing significance by asterisks
#' 
#' Adapted from prof. Tom Wenseleers:
#' This function uses given threshold values (cutpoints) to code these in symbols (symbols)
#' By default, it will annote asterisks (*) to p.value values. 
#' 
#' @param value Here goes your data.
#' @param cutpoints Here goes a vector of cutpoints, usually one longer than the vector of thresholds
#' @param symbols Here goes a vector of symbols, one shorter than the vector of cutpoints
#' @keywords
#' @export
#' @examples
#' asterisks

asterisks <- function(value, cutpoints =  c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c('***', '**', '*', '.', '')){
  return(as.vector(symnum(value, corr = FALSE, cutpoints = cutpoints, symbols= symbols)))
} 