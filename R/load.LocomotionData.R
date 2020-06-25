#' Loading data from the Locomotion Assay
#'
#' This function allows you to quickly load all data from one mother folder in which you saved data for several experiments in subfolders. E.g. the main folder contains your data, the subfolders contain .csv files for each time a different strain in a different condition.
#' @param path Enter the path to the main folder.
#' @param folders Enter all subfolders you constituted. I'll try to find a way to do this more efficiently.
#' @param strains Enter a list of the strains you used. These should be ordered in the way you ordered the respective folders.
#' @param conditions Enter a list of again respective conditions for all folders. Mind that the order does matter a lot!
#' @keywords
#' @export
#' @examples
#' load.LocomotionData

load.LocomotionData <- function(path = getwd(), folders, strains, conditions){
  for (i in 1:length(folders)){
    cat(paste0("Loading folder ", i, " of ", length(folders), "\n"))
    
    foldername <- folders[i]
    folder <- paste(path, foldername, sep = "/")
    
    files <- list.files(path=folder, pattern="*.csv", full.names=TRUE, recursive=FALSE)
    
    for (j in 1:length(files)){
      if (j==1 && i==1){
        data <- read.table(files[j], header=T, sep=",", dec=".")
        data$strain <- rep(strains[i], nrow(data))
        data$condition <- rep(conditions[i], nrow(data))
        data$replicate <- rep(paste(i, j, sep='.'), nrow(data))
        data$replicate <- as.factor(data$replicate)
        cat(paste0("    dataset ", j, " of ", length(files), "\n"))
      } ## if
      else{
        subdata <- read.table(files[j], header=T, sep= ",", dec=".")
        subdata$strain <- rep(strains[i], nrow(subdata))
        subdata$condition <- rep(conditions[i], nrow(subdata))
        subdata$replicate <- rep(paste(i, j, sep='.'), nrow(subdata))
        subdata$replicate <- as.factor(subdata$replicate)
        data <- rbind(data, subdata)
        cat(paste0("    dataset ", j, " of ", length(files), "\n"))
      } ## else
    } ## for files
  } ## for folders
  return(data)
  cat(paste("Loaded all files in all ", length(folders), " folders", "\n"))
} ## function
