#' Convert cm to inches
#' 
#' Convert cm to inches. What else to explain? 
#' @param value The value you'd like to convert to inches
#' @keywords
#' @export
#' @examples
#' cm2inch

cm2inch <- function(value){
  output = value*0.393700787
  return(output)
}