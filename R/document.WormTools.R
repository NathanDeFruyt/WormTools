#' Document and install your newly written code in one line! 
#' 
#' This simple function documents your newly written function in one line to the WormTools directory. 
#' @keywords
#' @export
#' @examples
#' document.WormTools

document.WormTools <- function(){
  setwd('C:/Program Files/R/R-3.6.0/library/WormTools')
  library(devtools)
  library(roxygen2)
  library(crayon)
  document()
  cat(blue('Updated the package\n'))
  setwd('C:/Program Files/R/R-3.6.0/library')
  install("WormTools")
  .rs.restartR()
  library(WormTools)
  cat(blue('Installed the updated package'))
}


