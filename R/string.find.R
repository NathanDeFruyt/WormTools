#' find a substring in a string
#'
#' Similar to the str.find command in Python: find the index of a substring in a character string. 
#' @param substring The substring you want to find
#' @param string The string you want to find it in
#' @param mode = c('first', 'all'): only the first occurence or all occurences?
#' @keywords
#' @export
#' @examples
#' string.find

string.find <- function(string, substring, mode = 'first'){
  if (mode == 'first'){
    index = gregexpr(pattern = substring, string)[[1]][1]
    return(index)
  }
  else{
    indices = gregexpr(pattern = substring, string)[[1]]
    indies = c()
    for (i in indices){
      indies = c(indies, i)
    }
    return(indies)
  }

}
