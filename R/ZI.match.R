#' Matching two datafiles to eachother. 
#' 
#' If you have two datafiles with overlapping information, this function allows you to select the columns in which the identifier resides for both dataframes and produces a vector in which you put the values of either of the data.frame which you'd like to convert to the other data.frame. Therefore, this method also works if the nrow() differs for both data.frames. 
#' @param data1 A vector or data.frame column in which the identifier of data.frame 1 is.
#' @param data2 A vector or data.frame column in which the identifier of data.frame 2 is.
#' @param value A vector or data.frame column in which you can find the values you'd like to return in an output vector. 
#' @param type Default: numeric, other options: 'character', make sure to select the option character if it is a character. 
#' @keywords
#' @export
#' @examples
#' ZI.match

ZI.match <- function(data1, data2, value, type = 'numeric'){
  library(crayon)
  cat(blue(paste0('Screening: \n0%   10%  20%  30%  40%  50%  60%  70%  80%  90%  100%\n|----|----|----|----|----|----|----|----|----|----|\n|')))
  
  output <- c()
  
  if (type != 'character'){
    for (i in 1:length(data1)){
      for (j in 1:length(data2)){
        if (data1[i] == data2[j]){
          output[i] <- value[j]
        } # if data1 == data2
      } # for j
      divi = i/nrow(data)
      r <- 1:50
      r <- r*2
      r <- r/100
      if (i == length(data1)){
        cat(blue(paste0('|')))
        cat(blue(paste0('Finished screening ', nrow(data), ' rows.')))
      } # if i = last
      else if (divi %in% r){
        cat(blue(paste0('=')))
      }
    } # for i
  }
  else if (type == 'character'){
    for (i in 1:length(data1)){
      for (j in 1:length(data2)){
        if (data1[i] == data2[j]){
          output[i] <- as.character(value[j])
        } # if data1 == data2
      } # for j
      divi = i/nrow(data)
      r <- 1:50
      r <- r*2
      r <- r/100
      if (i == length(data1)){
        cat(blue(paste0('|')))
        cat(blue(paste0('Finished screening ', nrow(data), ' rows.')))
      } # if i = last
      else if (divi %in% r){
        cat(blue(paste0('=')))
      }
    } # for i
  } # else
  return(output)
} # function