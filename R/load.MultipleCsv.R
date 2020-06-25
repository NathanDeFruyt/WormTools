#' Loading multiple .csv files from a common folder. 
#'
#' This function allows you to quickly load all data from one mother folder in which you saved data as .csv files.
#' @param NONE Be sure to set your working directory to the folder of which you want to access the data. Your data should be formatted as follows: One column in which there is the data for the DA spot, one column in which there is the data for the EtOH spot and another column in which there is the data for all other worms. Next to this, the function requires a column that indicates the strain and a column that indicates the condition.
#' @keywords
#' @export
#' @examples
#' load.MultipleCsv

# load the data
load.MultipleCsv <- function(add.replicate = F){
  path <- getwd()
  files <- list.files(path=path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  for (j in 1:length(files)){
    if (j==1){
      data <- read.table(files[j], header=T, sep=",", dec=".")
      if (add.replicate == T){
        data$replicate <- rep(j, nrow(data))
        
      }
      cat(paste0("Loaded dataset ", j, " of ", length(files), "\n"))
    } ## if 
    else{
      subdata <- read.table(files[j], header=T, sep= ",", dec=".")
      if (add.replicate == T){
        subdata$replicate <- rep(j, nrow(subdata))
      }
      data <- rbind(data, subdata)
      cat(paste0("       dataset ", j, " of ", length(files), "\n"))
    } ## else
  } ## for files
  cat(paste("Loaded all ", length(files), " files \n"))
  return(data)
} ## function
