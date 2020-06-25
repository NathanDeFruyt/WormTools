#' Loading data from the DA assay: adding a column indicating the replicate for different files from a common folder. 
#'
#' This function allows you to quickly load all data from one mother folder in which you saved data as .csv files and afterwards still identify what data came from which file. 
#' @param NONE Be sure to set your working directory to the folder of which you want to access the data. Your data should be formatted as follows: One column in which there is the data for the DA spot, one column in which there is the data for the EtOH spot and another column in which there is the data for all other worms. Next to this, the function requires a column that indicates the strain and a column that indicates the condition.
#' @param na.col.omit Default = T; would you like to delete row from the CI column that contain NaN values? Usually these are rows for which there were no data entered. 
#' @param check.learning.data Default = T; would you like to check whether the DA assay worked for wildtype (N2) worms and only use those datasets for whom it did?
#' @param path Default: wd; choose the path in which the function will find your learningData.
#' @param strain Default = "N2"; Optional if check.learning.data = T. what strain would you like to check up on? Default: N2
#' @param threshold Default = 0.7; Optional if check.learning.data = T. The threshold CI value for which the assay went well. For naive: CI > threshold is good, whereas for conditioned CI < threshold is better 
#' @param learning.phenotype Default = T; Optional if check.learning.data = T. Is the strain learning (true) or shouldn't it be (false)? Default: True (learning)
#' @param learning.cutpoints The cutpoints you used to determine the significance of the learning phenotype
#' @param nonlearning.cutpoints The cutpoints you used to determine the significance of the non-learning phenotype
#' @keywords
#' @export
#' @examples
#' load.learningData

# load the data
load.learningData <- function(check.learning.data = T, na.col.omit = T, path = getwd(), strain = "N2", learning.phenotype = T, threshold = 0.7, learning.cutpoints = c(-1, (threshold - 0.2), (threshold - 0.1), (threshold - 0.05), (threshold), 1), non.learning.cutpoints = c(-1, (threshold - 0.15), (threshold),(threshold + 0.1), (threshold + 0.2), 1)){
  path = path
  files <- list.files(path=path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
  for (j in 1:length(files)){
    if (j==1){
      data <- read.table(files[j], header=T, sep=",", dec=".")
      data$replicate <- rep(j, nrow(data))
      data$name <- 
      cat(paste0("Loaded dataset ", j, " of ", length(files), "\n"))
    } ## if 
    else{
      subdata <- read.table(files[j], header=T, sep= ",", dec=".")
      subdata$replicate <- rep(j, nrow(subdata))
      data <- rbind(data, subdata)
      cat(paste0("       dataset ", j, " of ", length(files), "\n"))
    } ## else
  } ## for files
  data$CI <- chemoindex(data$DA, data$EtOH, data$Others)
  if (na.col.omit == T){
    data <- na.col.omit(data, data$CI)
  }
  
  if (check.learning.data == T){
    checkup <- check.learningData(data, data$CI, strain = strain, learning.phenotype = learning.phenotype, threshold = threshold, learning.cutpoints = learning.cutpoints, non.learning.cutpoints = non.learning.cutpoints) # check for the quality of the data
    checkup.summary <- data.frame(replicate = checkup$replicate, signN2 = checkup$significance)
    
    names(data)[1] <- 'condition'
    data$condition = factor(data$condition,levels=c("naive", "D+S", "D", "S"))
    data$strain = factor(data$strain, levels = c("N2", "npr-6", "ZX2038", "flp-26"))
    data$class <- interaction(data$condition, data$strain)  
    data$class <- factor(data$class, levels = c('naive.N2', 'D+S.N2', 'D.N2', 'S.N2', 'naive.npr-6', 'D+S.npr-6', 'D.npr-6', 'S.npr-6', 'naive.ZX2038', 'D+S.ZX2038', 'D.ZX2038', 'S.ZX2038', 'naive.flp-26', 'D+S.flp-26') , labels = c('naive N2', 'conditioned N2', 'habituated N2', 'starved N2', 'naive npr-6', 'conditioned npr-6', 'habituated npr-6', 'starved npr-6', 'naive rescue', 'conditioned rescue', 'habituated rescue', 'starved rescue', 'naive flp-26', 'conditioned flp-26'))
    data$total <- data$DA + data$EtOH + data$Others
    
    good.replicates <- checkup.summary$replicate[which(checkup.summary$signN2 %in% c('***', '**', '*', '.'))]
    bad.replicates <- checkup.summary$replicate[-which(checkup.summary$replicate %in% good.replicates)]
    data <- data[which(data$replicate %in% good.replicates),]
    
    names(data)[1] <- 'condition'
    data$condition=factor(data$condition,levels=c("naive", "D+S", "D", "S"))
    data$strain = factor(data$strain, levels = c("N2", "npr-6", "ZX2038", "flp-26"))
    data$class <- interaction(data$condition, data$strain)  
    data$class <- factor(data$class, levels = c('naive.N2', 'D+S.N2', 'D.N2', 'S.N2', 'naive.npr-6', 'D+S.npr-6', 'D.npr-6', 'S.npr-6', 'naive.ZX2038', 'D+S.ZX2038', 'D.ZX2038', 'S.ZX2038', 'naive.flp-26', 'D+S.flp-26') , labels = c('naive N2', 'conditioned N2', 'habituated N2', 'starved N2', 'naive npr-6', 'conditioned npr-6', 'habituated npr-6', 'starved npr-6', 'naive rescue', 'conditioned rescue', 'habituated rescue', 'starved rescue', 'naive flp-26', 'conditioned flp-26'))
  }
  cat(paste("Loaded all ", length(files), " files \n"))
  if(check.learning.data == T){
    cat(red(paste0("Removed ", length(bad.replicates), " failed experiments.\n")))
  }
  return(data)
} ## function
