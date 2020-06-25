#' Produce data for a levelplot for your receptor screen data 
#' 
#' Returns a data.frame for receptor data to produce a levelplot after you processed them with process.microbeta.data
#' @param data This is the data you loaded with load.microbeta() and processed with process.microbeta.data()
#' @param pos.control Default: 'ATP', this is the character string coding for the positive control (I wasn't entirely sure whether this is always ATP)
#' @param baseline Default: 'BSA'. Idem, but for teh baseline values, i.e. the solvent in which you diluted your peptides. 
#' @param norm.over.ATP Default: True, this is an optional argument that you can tick if you'd like to normalize over the positive control. 
#' @keywords
#' @export
#' @examples
#' levelplot.data.microbeta

levelplot.data.microbeta <- function(data, nr.plates = 5, plate.type = 96, nr.x = 12, nr.y = 8){
  library(crayon)
  for (i in 1:nr.plates){
    if (i == 1){
      plates <- rep(i, plate.type)
    }
    else{
      plates <- c(plates, rep(i, plate.type))
    }
  }
  
  if (plate.type == 96){
    nr.y = 8
    nr.x = 12
  }
  
  for (i in 1:nr.y){
    if (i == 1){
      y <- rep(alphabet[i], nr.x)
    }
    else{
      y <- c(y, rep(alphabet[i], nr.x))
    }
  }
  
  y <- rep(y, nr.plates)
  x <- rep(1:nr.x, (nr.y*nr.plates))
  dimmies <- data.frame(plate = plates, y = y, x = x)
  dimmies$POS <- interaction(dimmies$plate, dimmies$y, dimmies$x)
  
  heatdimmies <- data.frame(POS = dimmies$POS)
  
  testx <- seq(1,10, length.out=nr.x)
  testy <- seq(10,1, length.out=(nr.y*nr.plates))
  testdata <- expand.grid(X=testx, Y=testy)
  heatdimmies <- data.frame(POS = dimmies$POS, X = testdata$X, Y = testdata$Y)
  
  ## match z.scores and peptide names with the positions in the plates
  heat.names <- c()
  heat.z.scores <- c()
  
  for (i in 1:nrow(heatdimmies)){
    for (j in 1:nrow(data)){
      if (heatdimmies$POS[i]==data$POS[j]){
        heat.names[i] <- as.character(data$peptide[j])
        heat.z.scores[i] <- data$z.score[j]
      } # if
    } # for rows
    if (i == 1){
      cat(blue(paste0('Started screening ', nrow(heatdimmies), ' datasets\n')))
      cat(blue(paste0('Screening: \n0%   50%  100%\n|----|----|\n|')))
    }
    divi = i/nrow(heatdimmies)
    r <- 1:50
    r <- r*2
    r <- r/100
    
    if (i == nrow(heatdimmies)){
      cat(blue(paste0('|')))
      cat(blue(paste0('Finished screening ', nrow(heatdimmies), ' rows.')))
    } # if i = last
    else if (divi %in% r){
      cat(blue(paste0('=')))
    }
  } # for lines
  
  heatdata <- heatdimmies
  heatdata$peptide <- heat.names
  heatdata$z.score <- heat.z.scores
  
  return(heatdata)
}
