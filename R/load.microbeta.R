#' Loading the data from the microbeta machine. 
#' 
#' This function loads your .csv file from the microbeta machine with an option to immediately read in the peptides from the plate layout file. 
#' @param data The data from your receptor screen
#' @param layout Optional argument: The plate layout. 
#' @param nr.plates The number of plates you screened. 
#' @param nr.series The number of seconds in the peptide addition and triton addition measurements. Default: 30 (on microbeta)
#' @param matching == c('manual', 'automatic') Default: 'automatic' Optional argument  that by default loads in the plate layout as well. THIS CAN TAKE A WHILE.
#' @param filename The name you'd like to give to the .csv file that you'll write.
#' @param write.csv == c(T, F), Default: T. Optional argument to write a .csv file with your data.  
#' @keywords
#' @export
#' @examples
#' load.microbeta

load.microbeta <- function(data, layout, nr.plates = 5, nr.series = 30, matching = 'automatic', filename = 'microbeta.compiled.data.csv', write.csv = T){
  ## load the screening data
  data <- read.table(file = data, header= T, sep=',', dec = '.')
  
  ## add a column for the position in x and the position in y to produce a consistent position
  reppie <- c() # plate nr
  xs <- c() # 1, 2, ..., 12 (for a 96-well plate)
  ys <- c() # A, B, ..., H (for a 96-well plate)
  for (i in 1:nr.plates){
    reppie <- c(reppie, rep(i, nrow(data)/nr.plates))
    for (k in 1:8){
      ys <- c(ys, rep(alphabet[k], 720))
      for (j in 1:12){
        xs <- c(xs, rep(j, 60))
      }
    }
  }
  data$plate <- reppie
  data$x <- xs
  data$y <- ys
  data$POS <- interaction(data$plate, data$y, data$x)
  
  ## load the plate layout and match the peptides from the layout to your screening data this can take a while.
  if (matching == 'automatic'){
    layout <- load.layout(layout, nr.plates = nr.plates)
    peptides <- ZI.match(data$POS, layout$pos, value = layout$peptide, type = "character")
    data$peptide <- peptides
  }
  
  series <- c()
  for (i in 1:length(levels(data$POS))){
    series <- c(series, rep('peptide', nr.series), rep('triton', nr.series))
  }
  data$series <- series
  
  if (write.csv == T){
    write.csv(data, file = filename)
  }
  
  return(data)
  #View(data)
}