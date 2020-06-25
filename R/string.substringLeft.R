#' Generate a head of a string!
#'
#' Generate a substring composed of the first n characters of your string in your string of interest
#' @param x A string you'd like to slice
#' @param n the number of characters your string should contain.
#' @keywords
#' @export
#' @examples
#' string.substringLeft

string.substringLeft <- function(x, n){
  substr(x, 1, nchar(x)-n)
}