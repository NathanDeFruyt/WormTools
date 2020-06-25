#' Loading the plate layout. 
#' 
#' This function loads the plate layout and adds the relevant columns to the dataset. Load your .csv file as a data.frame with a first column in which the y-values (A, B, ..., H) are and the next few columns show the peptide per x-value (1, 2, ..., 12). All plates are pasted underneath eachother. 
#' @param filename The name of the file.
#' @param nr.plates Default == 5: the number of plates you screened. 
#' @keywords
#' @export
#' @examples
#' load.layout

load.layout <- function(filename, nr.plates = 5){
  ## load the .csv file
  layout <- read.table(filename, header = T, sep = ',', dec = '.')
  ## add a column for the plate
  reppie <- c()
  for (i in 1:nr.plates){
    reppie <- c(reppie, rep(i, 8))
  }
  layout$P <- reppie
  ## stack them all in one column
  attach(layout)
  for (i in 1:12){
    if (i == 1){
      newd <- data.frame(plate = P, x = rep(i, nrow(layout)), y = layout$y, peptide = layout[,(i+1)])
    }
    else{
      subbie <- data.frame(plate = P, x = rep(i, nrow(layout)), y = layout$y, peptide = layout[,(i+1)])
      newd <- rbind(newd, subbie)
    }
  }
  detach(layout)
  newd$pos <- interaction(newd$plate, newd$y, newd$x)
  return(newd)
}

