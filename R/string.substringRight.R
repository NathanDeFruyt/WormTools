#' Generate a tail of a string!
#'
#' Generate a substring composed of the final n characters of your string in your string of interest
#' @param x A string you'd like to slice
#' @param n the number of characters your string should contain.
#' @keywords
#' @export
#' @examples
#' string.substringRight


string.substringRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}