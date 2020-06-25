#' Omitting NA values (or rows) for a specific column of your dataset
#' 
#' Removing NA values from a specific column of your dataset: If it's a data.frame, the row is omitted, if it is a 
#' @param data Here goes your data. Accepted datatypes e.g. data.frame, lists, ...
#' @param column data$column: Here goes the column you'd like to check. 
#' @keywords
#' @export
#' @examples
#' NaN.omit

na.col.omit <- function(data, column){
  library(crayon)
  omitted <- 0
  omitted.rows <- c()
  for (i in 1:nrow(data)){
    if(is.na(column[i])){
      data <- data[-(i-omitted),]
      omitted.rows <- c(omitted.rows, i)
      omitted = (omitted+1)
    } # if is.na
  } # for i
  cat(blue(paste0('Omitted ', length(omitted.rows), ' row(s) [')))
  for (i in 1:length(omitted.rows)){
    cat(blue(omitted.rows[i]))
    cat(blue(', '))
  } # for i
  cat(blue(']\n'))
  return(data)
} # function
