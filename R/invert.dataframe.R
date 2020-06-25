#' Invert your data frame
#'  
#' This function inverts data.frames
#' @param dataframe a data.frame which you'd like to see inverted. 
#' @keywords
#' @export
#' @examples
#' invert.dataframe


invert.dataframe <- function(dataframe){
  output = dataframe[nrow(dataframe),]
  for (i in 1:(nrow(dataframe-1))){
    newline <- dataframe[nrow(dataframe)-i, ]
    output <- rbind(output, newline)
  }
  return(output)
}