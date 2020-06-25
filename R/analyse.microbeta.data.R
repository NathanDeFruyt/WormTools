#' Analyse your micro.beta data. 
#' 
#' Using this function, you can elegantly and rather quickly scout your microbeta screen for potential hits. The outcome will be a list of several elements, including a density plot, the potential hits and a levelplot. 
#' @param data Data that were loaded using load.microbeta and subsequently summarised using process.microbeta.data()
#' @param screen.type Default= wide screen. 
#' @param pool.plates would you like to pool the plates in your assay? IF not, you'll get one plot as output, otherwise it produces a list of several plots. 
#' @param pos.control = "ATP", your positive control. 
#' @param baseline = "BSA", the solution in which peptides were diluted.
#' @param norm.over.ATP Default = F: Would you like to divide your values by the ATP value per plate or not? 
#' @param set.to.zero Default = T: would you like to set negative z.scores to 0?
#' @param nr.plates The number of plates you screened. 
#' @param plate.type The type of plates you used (i.e. e.g. 96 wells)
#' @param plot.levelplots If T: levelplots are plotted, this can take a while
#' @param graph.levelplots Default: F; If T: writes a levelplot graph to the specified location in path
#' @param path If you opted to write graphs to ppt: where should the file go? 
#' @param levelplot.file.names Default: 'Levelplot Plate 1', 2, 3, ... If graph2ppt: how should you like to call your files? 
#' @param estimate.p.values Default = F: Would you like to estimate p.values? 
#' @param p.value.distribution No Default: what distribution do you think you'r data has?
#' @param alpha What alpha value would you like to identify hits on? 
#' @param plot.distr.estimate Default = F; Would you like to plot the estimated distribution of your data?
#' @param graph.density Default = F; Woudl you like to write the density curve to a file? 
#' @keywords
#' @export
#' @examples
#' 
#' analyse.microbeta.data

analyse.microbeta.data <- function(data, pool.plates = T, screen.type = 'hit', pos.control = 'ATP', 
                                   baseline = 'BSA', norm.over.ATP = T, 
                                   set.to.zero = F,
                                   nr.plates = 5, plate.type = 96, 
                                   graph.levelplots = F,
                                   path = path, levelplot.file.names = 'Default',
                                   estimate.p.values = F,
                                   plot.levelplots = F,
                                   p.value.distribution = c('chisq', 'gamma', 'exp', 'normal'),
                                   alpha = 0.05, 
                                   plot.distr.estimate = F,
                                   graph.density = F){
  ## load required libraries
  library(car)
  library(ggplot2)
  library(ggthemes)
  library(export)
  library(crayon)
  
  if (screen.type == 'hit'){
  
    ## create an output file
    output <- list()
    
    ## summarise the data
    summary.data <- process.microbeta.data(data, pos.control = pos.control, baseline = baseline, pool.plates = pool.plates, 
                                           norm.over.ATP = norm.over.ATP, set.to.zero = set.to.zero, return.list = F)
    output$summary.data <- summary.data
    
    ## check and analyse the date for relevant ligands only
    peptide.data <- summary.data[-which(summary.data$peptide == baseline),]
    peptide.data$adj.z.score <- set.to.zero(peptide.data$z.score)
    
    if (estimate.p.values == T){
      cat(red(paste0('\nRemember to enter a p-value distribution\n')))
      ### add p.values
      if (p.value.distribution == 'chisq'){
        peptide.data$p.value <- 1- pchisq(peptide.data$adj.z.score, df = 1)
        
      }
      if (p.value.distribution == 'gamma'){
        peptide.data$p.value <- 1- pgamma(peptide.data$adj.z.score, shape = 1)
      }
      if (p.value.distribution == 'exp'){
        peptide.data$p.value <- 1- pexp(peptide.data$adj.z.score)
      }
      if (p.value.distribution == 'normal'){
        peptide.data$p.value <- 1- pnorm(peptide.data$adj.z.score)
      }
      
      peptide.data$p.symbol <- asterisks(peptide.data$p.value)
    } ## if estimate.p.value == T
    
    output$peptide.data <- peptide.data
    
    ## check for hits and bundle the hits in a separate file. 
    hits <- peptide.data[which(peptide.data$symbol %in% c('.', '*', '**', '***')),]
    hit.table <- data.frame(peptide = hits$peptide, plate = hits$plate, z.score = hits$z.score, symbol = hits$symbol)
    
    hits.on.threshold <- list()
    hits.on.threshold$table <- hit.table
    
    hitties <- unique(hits$peptide)
    hits.on.threshold$names <- hitties
    
    output$hits.on.threshold <- hits.on.threshold
    if (estimate.p.values == T){
      hitsP <- peptide.data[which(peptide.data$p.value < alpha),]
      hitsP.table <- data.frame(peptide = hitsP$peptide, plate = hitsP$plate, z.score = hitsP$z.score, symbol = hitsP$p.symbol)
      
      hits.on.p.value <- list()
      hits.on.p.value$table <- hitsP.table
      
      hittiesP <- unique(hitsP$peptide)
      hits.on.p.value$names <- hittiesP
      
      output$hits.on.p.value <- hits.on.p.value
    } ## if estimate.p.values == T
    
    
    ## plot the density of your data
    if (plot.distr.estimate == T){
      y2 <- seq(-1, 20, 0.1)
      if (p.value.distribution == 'chisq'){
        distr <- data.frame(x = y2, y = dchisq(y2, df= 1))
      }
      if (p.value.distribution == 'gamma'){
        distr <- data.frame(x = y2, y = dgamma(y2, shape = 1))
      }
      if (p.value.distribution == 'exp'){
        distr <- data.frame(x = y2, y = dexp(y2))
      }
      if (p.value.distribution == 'norm'){
        distr <- data.frame(x = y2, y = dnorm(y2))
      }
      
      if (pool.plates == T){
        peptide.density <- ggplot()+
          geom_density(data = peptide.data, aes(z.score, fill = 'sienna'), alpha=0.7)+
          theme_classic()+
          geom_line(data = distr, size = 1, aes(x, y, colour = "red"))+
          theme(legend.position="none") +
          labs(y = 'density', x = 'z.score')+
          scale_y_continuous(expand = c(0, 0))
      } ## if pool.plates == T
      else{
        peptide.density <- ggplot()+
          geom_density(data = peptide.data, aes(z.score, fill = plate, group = plate), alpha=0.7)+
          theme_classic()+
          geom_line(data = distr, size = 1, aes(x, y, colour = "red"))+
          theme(legend.position="none") +
          labs(y = 'density', x = 'z.score')+
          scale_y_continuous(expand = c(0, 0))
      } ## if pool.plates == F
    } ## if plot.distr.estimate == T
    
    else{
      if (pool.plates == T){
        peptide.density <- ggplot()+
          geom_density(data = peptide.data, aes(z.score, fill = 'sienna'), alpha=0.7)+
          theme_classic()+
          theme(legend.position="none") +
          labs(y = 'density', x = 'z.score')+
          scale_y_continuous(expand = c(0, 0))
      }
      else{
        peptide.density <- ggplot()+
          geom_density(data = peptide.data, aes(z.score, fill = plate, group = plate), alpha=0.7)+
          theme_classic()+
          theme(legend.position="none") +
          labs(y = 'density', x = 'z.score')+
          scale_y_continuous(expand = c(0, 0))
      } ## if pool.plates == F
    } ## if plot.distr.estimate == F
    
    output$density.plot <- peptide.density
    
    if (pool.plates == F){
      facetted.density.plot <- peptide.density + facet_wrap(~plate)
      output$facetted.density.plot <- facetted.density.plot
    }
    
    if (graph.density == T){
      graph2ppt(peptide.density, file = paste0(path, '/', 'Density Plot.ptt'))
      if (pool.plates == F){
        graph2ppt(facetted.density.plot, file = paste0(path, '/', 'Density Plot per Plate.ppt'))
      }
    }
    
    ## identify hit bulbs visually
    ### create a new data.frame with the hits only
    hitplot.text <- hits
    hitplot.text$X <- hitplot.text$plate+0.01
    hitplot.text$y.text <- rep(1, nrow(hitplot.text))
    hitplot.text$y.symbol <- rep(0.5, nrow(hitplot.text))
    
    annotated.peptide.density <- peptide.density+
      annotate("text", x = hitplot.text$z.score, y = hitplot.text$y.text, label = hitplot.text$peptide, angle = 45)+
      annotate("text", x = hitplot.text$z.score, y = hitplot.text$y.symbol, label = hitplot.text$symbol, angle = 45)
    
    output$annotated.peptide.density <- peptide.density
    
    if (estimate.p.values == T){
      hitplotP.text <- hitsP
      hitplotP.text$X <- hitplotP.text$plate+0.01
      hitplotP.text$y.text <- rep(1, nrow(hitplotP.text))
      hitplotP.text$y.symbol <- rep(0.5, nrow(hitplotP.text))
      
      annotated.peptide.density.P <- peptide.density + 
        annotate("text", x = hitplotP.text$z.score, y = hitplotP.text$y.text, label = hitplotP.text$peptide, angle = 45)+
        annotate("text", x = hitplotP.text$z.score, y = hitplotP.text$y.symbol, label = hitplotP.text$symbol, angle = 45)
      
      output$annotated.peptide.density.p.values <- annotated.peptide.density.P
    }
    
    if (plot.levelplots == T){
      levelplot.pooled <- levelplot.microbeta(data = summary.data, pool.plates = T, graph2ppt = graph.levelplots, graph.names = c('Levelplot Pooled'), nr.plate = nr.plates, plate.type = plate.type, path = path)
      output$levelplot.pooled <- levelplot.pooled
      
      if (pool.plates == F){
        levelplots.per.plate <- levelplot.microbeta(data = summary.data, pool.plates = F, graph2ppt = graph.levelplots, graph.names = levelplot.file.names, path = path)
        output$levelplots.per.plate <- levelplots.per.plate
      }
    }
    
    
    cat(blue(paste0('\nFinished Analysis of your Microbeta Data. Enjoy Interpreting.\n')))
    
    return(output)
  }
}
