#' Update the WormTools package
#' 
#' This simple function updates the WormTools package in one line
#' @keywords
#' @param version Enter the version number of R you are currently running. version 3.6.0 should be mentioned as 3.6.0 for example (so not v3.6.0 ...)
#' @export
#' @examples
#' update.WormTools

update.WormTools <- function(version = "3.6.1"){
  prevwd <- getwd()
  pat <- paste0('C:/Program Files/R/R-', version, '/library')
  setwd(pat)
  library(devtools)
  library(crayon)
  install("WormTools")
  cat(blue('Updated the WormTools package\n\n'))
  library(WormTools)
  cat(blue('Reloaded the package from library \n\n'))
  setwd(prevwd)
  cat(blue(paste0('Reset the wd to current wd [', getwd(), '] \n')))
  .rs.restartR()
  cat(blue('Reloaded WormTools to working space\n'))
  library(WormTools)
}


