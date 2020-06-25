#' Analyse MSA data 
#' 
#' Analyse MSA data which you first fed into the python script "analyse_alignment_on_similarity_in_speciesv2.py"
#' @param folder folder in which you saved the python output
#' @param destination folder in which you'd like to save the output files
#' @param writeCsv if you want to save the output files in a csv. 
#' @keywords
#' @export
#' @examples
#' analyse.MSA


analyse.MSA <- function(folder = getwd(), destination= getwd(), writeCsv = T){
  library(doBy)
  library(crayon)
  oldWd <- getwd()
  subfolders = check.folder()
  tags = folderTag()
  specialRegions = list()
  output = list()
  
  for (j in 1:length(subfolders)){
    setwd(subfolders[j])
    cat(blue(paste0('Processing folder ', j, '\n')))
    data = read.table('main file.csv', header = T, sep = ',', dec = '.')
    
    ## now let's check all files and find out special regions in our sequences
    filies <- folderTag()
    indexMainFile <- match('main file.csv', filies)
    indexGapFile <- match('gaps.csv', filies)
    filies <- filies[-c(indexMainFile, indexGapFile)]
    
    newColIndices <- c()
    newColNamen <- c()
    regionNamen <- c()
    for (file in filies){
      tempData <- read.table(file, header = T, sep = ',', dec = '.')$Positions
      name <- string.substringLeft(file, 4)
      newColNamen <- c(newColNamen, name)
      regionNamen <- c(regionNamen, name)
      namen <- c(names(data), name)
      data$newcol <- acknowledge(data$Position, tempData)
      names(data) <- namen
      newColIndices <- c(newColIndices, ncol(data))
    }
    
    gap.POS = read.table('gaps.csv', header = T, sep = ',', dec = '.')$Position
    data$gaps <- acknowledge(data$Position, gap.POS)
    
    gps <- c()
    for (i in 1:nrow(data)){
      if (data$Residue[i] == '-'){
        gps <- c(gps, 1)
      }
      else{
        gps <- c(gps, 0)
      }
    }
    
    data$gaps <- gps
    data$cum.gaps <- cumulate(data$gaps)
    data$adj.pos <- data$Position - data$cum.gaps
    data$species <- rep(tags[j], nrow(data))
    
    if (j == 1){
      CombiData <- data
    }
    else{
      CombiData <- rbind(CombiData, data)
    }
    
    ## now we'll first calculate some descriptive stats:
    nrGaps = length(gap.POS)
    nrChar = nrow(data)
    nrRes = nrChar - nrGaps
    
    ## now we'll create a summary file
    data.sum <- summaryBy(Position~Coverage, data = data, FUN = length)
    names(data.sum) <- c('coverage', 'abundance')
    
    ## add a column for the relative percentage within each region
    lastColIndices <- newColIndices
    lastColNamen <- newColNamen
    newColNamen <- c()
    newColIndices <- c()
    abundance.indices = c()
    for (index in c(1:length(lastColIndices))){
      col <- lastColIndices[index]
      subdata = subset(data, data[,col] == '*')
      subdata.sum <- summaryBy(Position~Coverage, data = subdata, FUN = length)
      names(subdata.sum) <- c('coverage', 'abundance')
      
      sub.abundance = c()
      for (i in 1:nrow(data.sum)){
        if (data.sum$coverage[i] %in% subdata.sum$coverage){
          ab = subdata.sum$abundance[which(subdata.sum$coverage == i-1)]
          sub.abundance = c(sub.abundance, ab)
        }
        else{
          sub.abundance = c(sub.abundance, 0)
        }
      }
      colNaam <- paste0('abund.', lastColNamen[index])
      namen <- c(names(data.sum), colNaam)
      data.sum$newcol <- sub.abundance
      names(data.sum) <- namen
      newColNamen <- c(newColNamen, colNaam)
      newColIndices <- c(newColIndices, ncol(data.sum))
      abundance.indices <- c(abundance.indices, ncol(data.sum))
    }
    
    ## now we go on with summarising
    data.sum <- invert.dataframe(data.sum)
    
    ### what is the cumulative abundance for coverage? 
    data.sum$cum.abundance <- cumulate(data.sum$abundance)
    
    regional.cum.abundances <- list()
    lastColIndices <- newColIndices
    lastColNamen <- newColNamen
    newColNamen <- c()
    newColIndices <- c()
    for (index in c(1:length(lastColIndices))){
      col <- lastColIndices[index]
      colNaam <- paste0('cum.', lastColNamen[index])
      namen <- c(names(data.sum), colNaam) 
      cum.abundance <- cumulate(data.sum[,col])
      
      data.sum$newcol <- cum.abundance
      regional.cum.abundances[[index]] <- cum.abundance
      names(data.sum) <- namen
      newColNamen <- c(newColNamen, colNaam)
      newColIndices <- c(newColIndices, ncol(data.sum))
    }
    
    ### what is the percentage of 'conservation' represented by each group? 
    data.sum$perc.cover <- data.sum$cum.abundance/nrRes
    
    lastColNamen <- newColNamen
    lastColIndices <- newColIndices
    newColNamen <- c()
    newColIndices <- c()
    for (index in c(1:length(lastColIndices))){
      col <- lastColIndices[index]
      colNaam <- paste0('perc.cover.', regionNamen[index])
      namen <- c(names(data.sum), colNaam)
      noemerIndex <- abundance.indices[index]
      data.sum$newcol <- data.sum[,col]/sum(data.sum[, noemerIndex])
      names(data.sum) <- namen
      newColNamen <- c(newColNamen, colNaam)
      newColIndices <- c(newColIndices, ncol(data.sum))
    }
    
    ### what is the percentage of each group within total conserved residues?
    lastColNamen <- newColNamen
    lastColIndices <- newColIndices
    newColNamen <- c()
    newColIndices <- c()
    for (index in c(1:length(lastColIndices))){
      col <- lastColIndices[index]
      colNaam <- paste0('abs.cover.perc.', regionNamen[index])
      namen <- c(names(data.sum), colNaam)
      data.sum$newcol <- regional.cum.abundances[[index]]/nrRes
      names(data.sum) <- namen
    }
    data.sum$species <- rep(tags[j], nrow(data.sum))
    
    if (writeCsv == T){
      folder = destination
      output.filename = paste0(folder, '/', tags[j], ' R output.csv') 
      write.csv(data.sum, file = output.filename)
      cat(red(paste0('Printed file ', j, ' to ', output.filename, '\n\n')))
    }
    
    if (j == 1){
      combiSumOutput <- data.sum
    }
    else{
      combiSumOutput <- rbind(combiSumOutput, data.sum)
    }
  }
  output$data <- CombiData
  output$sum.data <- combiSumOutput
  setwd(oldWd)
  return(output)
}
