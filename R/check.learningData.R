#' Checking your learning data whether it worked or not.
#' 
#' This function creates a data.frame in which you can check the quality of your learning assay.
#' @param data Here goes your data, loaded by load.learningdata()
#' @param column data$column: Here goes the column you'd like to check: data$CI.
#' @param strain what strain would you like to check up on? Default: N2
#' @param threshold The threshold CI value for which the assay went well. For naive: CI > threshold is good, whereas for conditioned CI < threshold is better 
#' @param learning.phenotype Is the strain learning (true) or shouldn't it be (false)? Default: True (learning)
#' @param learning.cutpoints The cutpoints you used to determine the significance of the learning phenotype
#' @param nonlearning.cutpoints The cutpoints you used to determine the significance of the non-learning phenotype
#' @keywords
#' @export
#' @examples
#' check.learningData

check.learningData <- function(data, column, strain = 'N2', learning.phenotype = T, threshold=0.7,
                               learning.cutpoints = c(-1, (threshold-0.2), (threshold-0.1), (threshold-0.05), (threshold), 1),
                               non.learning.cutpoints = c(-1, (threshold-0.15), (threshold), (threshold+0.1), (threshold+0.2), 1)
                               ){
  # naive
  evaluationsNaive <- c()
  evaluationsConditioned <- c()
  
  averageNaiveCIs <- c()
  averageConditionedCIs <- c()
  noconditioneds <- c()
  
  replicates <- as.numeric(levels(as.factor(data$replicate)))
  
  naiveasterisks = function(CI) as.vector(symnum(CI, corr = FALSE, cutpoints = c(-1, (threshold-0.05), (threshold), (threshold+0.1), (threshold+0.2), 1), symbols = c("-", ".", "*", "**", "***")))
  if (learning.phenotype == T){
    conditionedasterisks = function(CI) as.vector(symnum((CI), corr = FALSE, cutpoints = learning.cutpoints, 
                                                         symbols = c("***", "**", "*", ".", "-")))
  } else{
    conditionedasterisks = function(CI) as.vector(symnum(CI, corr = FALSE, cutpoints = non.learning.cutpoints, symbols = c("-", ".", "*", "**", "***")))
  }
  
  cat(blue(paste0('Checking dataset \n')))
  
  for (i in 1:length(replicates)){
    cat(blue(paste0(i, ' ')))
    subdata <- subset(data, data$replicate == i)
    subdata <- subdata[which(subdata$strain == strain),]
    
    if ('D+S' %in% subdata$Condition){
      naive <- subset(subdata, subdata$Condition == 'naive')
      conditioned <- subset(subdata, subdata$Condition == 'D+S')
      
      averageNaiveCI <- sum(naive$CI)/nrow(naive)
      averageNaiveCIs[i] <- averageNaiveCI
      
      averageConditionedCI <- sum(conditioned$CI)/nrow(conditioned)
      averageConditionedCIs[i] <- averageConditionedCI
      
      replicates[i] <- i
      
      evaluationsNaive[i] <- naiveasterisks(averageNaiveCI)
      evaluationsConditioned[i] <- conditionedasterisks(averageConditionedCI)
      
      
    } # if
    else{
      noconditioneds <- c(noconditioneds, i)
    }
  } # for i
  
  output <- data.frame('replicate' = replicates, 'avgNaiveCI' = averageNaiveCIs, 'avgCondCI' = averageConditionedCIs, 'signNaive' = evaluationsNaive, 'signCond' = evaluationsConditioned)
  output
  significances <- c()
  for (i in 1:(nrow(output))){
    if (i %in% noconditioneds){
      significances[i] <- NA
    }
    else{
      if (output$signNaive[i] == '***' || output$signCond[i] == '***'){
        significances[i] <- '***'
      } # if 
      if (output$signNaive[i] == '**' || output$signCond[i] == '**'){
        significances[i] <- '**'
      } # if
      if (output$signNaive[i] == '*' || output$signCond[i] == '*'){
        significances[i] <- '*'
      } # if
      if (output$signNaive[i] == '.' || output$signCond[i] == '.'){
        significances[i] <- '.'
      }
      if (output$signNaive[i] == '-' || output$signCond[i] == '-'){
        significances[i] <- '-'
      } # if
    } # for i
  }
  
  output$significance <- significances
  
  cat(red(paste0(' \n Dataset(s) ', noconditioneds, ' has/have no D+S condition\n')))
  return(output)
  
} # function