#' Check for hits in your screening data. 
#' 
#' Returns a data.frame to identify hits: If you ran and loaded your peptide data with load.microbeta(), this function allows you to screen for hits. For now, hits are defined when the z-score is 2 standarddeviations (*) away from the mean of the population of hits for the peptide data only (excluding ATP and baseline BSA levels). However, for now, I haven't found a distribution that fits this plot. 
#' @param data This is the data you loaded with load.microbeta()
#' @param pos.control Default: 'ATP', this is the character string coding for the positive control (I wasn't entirely sure whether this is always ATP)
#' @param baseline Default: 'BSA'. Idem, but for teh baseline values, i.e. the solvent in which you diluted your peptides. 
#' @param norm.over.ATP Default: True, this is an optional argument that you can tick if you'd like to normalize over the positive control. 
#' @param pool.plates Default: True, this is an optional argument that allows you to run the analysis on each plate separately if you'd like to.
#' @param return.list Default: False, this is an optional argument that allows you to return a list if you chose to run the analysis on separate plates. If not, remember to keep plates separate throughout your graphs. 
#' @param set.to.zero Default: False, this is an optional argument which you can select if you'd like to put negative values to zero. 
#' @keywords
#' @export
#' @examples
#' process.microbeta.data

process.microbeta.data <- function(data, pos.control = 'ATP', baseline = 'BSA', norm.over.ATP = T, pool.plates = T, return.list = F, set.to.zero = F){
  library(crayon)
  ### 1. Normalise peptide response over triton peak. 
  for (i in levels(data$POS)){
    subdata <- subset(data, data$POS == i)
    peptide.data <- subset(subdata, subdata$series == 'peptide')
    triton.data <- subset(subdata, subdata$series == 'triton')
    
    peptide.response <- sum(peptide.data$CCPS1)
    triton.response <- sum(triton.data$CCPS1)
    
    standardised.PR <- peptide.response/triton.response
    if (i == levels(data$POS)[1]){
      sumdata <- data.frame(POS = subdata$POS[1], x = subdata$x[1], y = subdata$y[1], plate = subdata$plate[1], peptide = subdata$peptide[1], RLU = standardised.PR)
    } # if
    else{
      subsumdata <- data.frame(POS = subdata$POS[1], x = subdata$x[1], y = subdata$y[1], plate = subdata$plate[1], peptide = subdata$peptide[1], RLU = standardised.PR)
      sumdata <- rbind(sumdata, subsumdata)
    } # else
  } # for POS
  
  ### 2. Calculate z.scores
  
  if (pool.plates == T){
    #### 2.1 Standardise all data over baseline levels and positive control
    ## 2.1.a) the median BSA (BSA RLUs are not correlated with the plate)
    median.BSA.RLU <- median(sumdata$RLU[which(sumdata$peptide == baseline)])
    sumdata$RLU <- sumdata$RLU - median.BSA.RLU
    
    ## 2.1.b) proportional RLU compared to positive control
    for (i in as.numeric(levels(as.factor(sumdata$plate)))){
      subdata <- subset(sumdata, sumdata$plate == i)
      if (norm.over.ATP == T){
        median.ATP.RLU <- median(subdata$RLU[which(subdata$peptide == pos.control)])
      } # if norm.over.ATP
      else{
        median.ATP.RLU <- 1
      }
      subdata$prop.RLU <- subdata$RLU/median.ATP.RLU
      if (i == 1){
        summiedata <- subdata
      } # if i == 1
      else{
        summiedata <- rbind(summiedata, subdata)
      } # else
    } # for 
    
    sumdata <- summiedata
    
    if (set.to.zero == T){
      values <- c()
      for (i in 1:length(sumdata$prop.RLU)){
        if (sumdata$prop.RLU[i] < 0){
          values <- c(values, 0)
        }
        else{
          values <- c(values, sumdata$prop.RLU[i])
        }
      }
      sumdata$prop.RLU <- values
    }
    
    #### 2.2 calculate z-scores for all peptides (and ATP)
    
    ##### 2.2.a) function for z.score
    
    z.median.score <- function(value, mean, sd){
      score <- (value - mean)/sd
      return(score)
    }
    
    ##### 2.2.b) calculate it for peptide data
    ## exclude all but ligand data
    peptide.data <- sumdata[-which(sumdata$peptide %in% c(pos.control, baseline)),]
    
    ## calculate median and sd
    median.peptide.RLU <- median(peptide.data$prop.RLU)
    sd.peptide.RLU <- sd(peptide.data$prop.RLU)
    
    ## return again pos.control (ATP) and baseline (BSA) data
    peptide.data <- rbind(peptide.data, sumdata[which(sumdata$peptide %in% c(pos.control, baseline)),])
    
    ## calculate z.scores
    peptide.data$z.score <- z.median.score(peptide.data$prop.RLU, median.peptide.RLU, sd.peptide.RLU)
    
    ## add a hit symbol
    median.z.score <- median(peptide.data$z.score)
    sd.z.score <- sd(peptide.data$z.score)
    
    cuts <- c(-1000)
    for (i in 1:4){
      v <- median.z.score + i*sd.z.score
      cuts <- c(cuts, v)
    }
    cuts <- c(cuts, 1000)
    symbs <- c("", ".", "*", "**", "***")
    peptide.data$symbol <- asterisks(peptide.data$z.score, cutpoints = cuts, symbols = symbs)
    cat(blue("    You had a threshold value of ", 2*sd.z.score, " [two standard deviations from the mean for all peptides]"))
    
    return(peptide.data)
  } # if pool.plates == T
  else{ # if pool.plates == F
    sumdata.buffer <- sumdata
    for (q in 1:length(unique(sumdata.buffer$plate))){
      sumdata <- subset(sumdata.buffer, plate == q)
      #### 2.1 Standardise all data over baseline levels and positive control
      ## 2.1.a) the median BSA (BSA RLUs are not correlated with the plate)
      median.BSA.RLU <- median(sumdata$RLU[which(sumdata$peptide == baseline)])
      sumdata$RLU <- sumdata$RLU - median.BSA.RLU
      
      ## 2.1.b) proportional RLU compared to positive control
      subdata <- sumdata
      if (norm.over.ATP == T){
        median.ATP.RLU <- median(subdata$RLU[which(subdata$peptide == pos.control)])
      } # if norm.over.ATP
      else{
        median.ATP.RLU <- 1
      }
      subdata$prop.RLU <- subdata$RLU/median.ATP.RLU
      
      sumdata <- subdata
      
      if (set.to.zero == T){
        values <- c()
        for (i in 1:length(sumdata$prop.RLU)){
          if (sumdata$prop.RLU[i] < 0){
            values <- c(values, 0)
          }
          else{
            values <- c(values, sumdata$prop.RLU[i])
          }
        }
        sumdata$prop.RLU <- values
      }
      #### 2.2 calculate z-scores for all peptides (and ATP)
      
      ##### 2.2.a) function for z.score
      
      z.median.score <- function(value, mean, sd){
        score <- (value - mean)/sd
        return(score)
      }
      
      ##### 2.2.b) calculate it for peptide data
      ## exclude all but ligand data
      peptide.data <- sumdata[-which(sumdata$peptide %in% c(pos.control, baseline)),]
      
      ## calculate median and sd
      median.peptide.RLU <- median(peptide.data$prop.RLU)
      sd.peptide.RLU <- sd(peptide.data$prop.RLU)
      
      ## return again pos.control (ATP) and baseline (BSA) data
      peptide.data <- rbind(peptide.data, sumdata[which(sumdata$peptide %in% c(pos.control, baseline)),])
      
      ## calculate z.scores
      peptide.data$z.score <- z.median.score(peptide.data$prop.RLU, median.peptide.RLU, sd.peptide.RLU)
      
      ## add a hit symbol
      median.z.score <- median(peptide.data$z.score)
      sd.z.score <- sd(peptide.data$z.score)
      
      cuts <- c(-1000)
      for (i in 1:4){
        v <- median.z.score + i*sd.z.score
        cuts <- c(cuts, v)
      } # for i in 1:4
      cuts <- c(cuts, 1000)
      symbs <- c("", ".", "*", "**", "***")
      peptide.data$symbol <- asterisks(peptide.data$z.score, cutpoints = cuts, symbols = symbs)
      if (i == 1){
        cat(blue("Plate    threshold\n"))
      }
      cat(blue(paste0("    ", i, "    ", sd.z.score, "\n")))
      if (return.list == T){
        if (q == 1){
          output <- list(peptide.data)
        } # if
        else{
          output <- c(output, list(peptide.data))
        } # else
      } # if return.list == T
      else{
        if (q == 1){
          output <-  peptide.data
        } # if
        else{
          output <- rbind(output, peptide.data)
        } # else
      }
    } # for q (each plate)
    if (return.list == T){
      cat(blue(paste0("Pooled plates == F; returned a list with ", length(unique(sumdata.buffer$plate)), " data.frames")))
    }
    if (pool.plates == F){
      cat(blue(paste0("Pooled plates == F; returned a data.frame for ", length(unique(sumdata.buffer$plate)), " plates pooled. \n")))
      cat(red(paste0(" ### Remember to keep your plates separate when making graphs!")))
    }
    return(output)
  } # if pool.plates == F
}


