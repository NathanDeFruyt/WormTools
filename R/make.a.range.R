#' Make a range 
#' 
#' Make a range out of a vector of elements. This allows you to visually make it easier to find gropus of subsequent numbers. 
#' @param values the vector of values in which you see several ranges
#' @keywords
#' @export
#' @examples
#' make.a.range

make.a.range <- function(values){
  ranges = list()
  range = c()
  current.rangestart = values[1] - 1
  previous = values[1]
  range = values[1]
  NrOfRanges = 0
  
  for (i in 1:length(values)){
    current = values[i]
    ## if the current value is not following the previous value:
    if (current != previous+1){
      ## we set a range start
      NrOfRanges = NrOfRanges + 1
      range = c(range, previous)
      if (length(ranges) == 0){
        ranges[[1]] = range
      } # if
      else{
        ranges[[length(ranges)+1]] = range
      } # else
      range = c(current)
    }
    else{
      if (i == length(values) && length(range) == 1){
        range = c(range, current)
        if (length(ranges) == 0){
          ranges[[1]] = range
        }
        else{
          ranges[[length(ranges)+1]] = range
        } # else
      } # if final value
    } # else
    previous = current
  } # for
  ranges <- ranges[-1]
  rangeString <- c()
  cat(red(paste0('Detected ', length(ranges), ' ranges:\n')))
  rangecounter = 0
  for (range in ranges){
    rangecounter = rangecounter + 1
    rangeString <- c(rangeString, paste0(range[1], '-', range[2]))
    cat(blue(paste0('Range nr ', rangecounter, ': ', range[1], '-', range[2], '\n')))
  }
  return(rangeString)
} # function