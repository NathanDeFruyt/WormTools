#' Make a levelplot (i.e. a heatmap without clustering) for your microbeta data
#' 
#' This function will allow you to plot your microbeta data to visually see whether you have potential hits. 
#' @param data Data that were loaded using load.microbeta and subsequently summarised using process.microbeta.data()
#' @param pool.plates would you like to pool the plates in your assay? IF not, you'll get one plot as output, otherwise it produces a list of several plots. 
#' @param nr.plates The number of plates you screened. 
#' @param plate.type The type of plates you used (i.e. e.g. 96 wells)
#' @param graph2ppt Default: F; If T: writes a graph to the specified location in path
#' @param path If graph2ppt: where should the file go? 
#' @param file.names Default: 'Levelplot Plate 1', 2, 3, ... If graph2ppt: how should you like to call your files? 
#' @keywords
#' @export
#' @examples
#' levelplot.microbeta

levelplot.microbeta <- function(data, pool.plates = F, nr.plates = 5, plate.type = 96, graph2ppt = F, graph.names = 'Default', path = getwd()){
  # load the data according the microbeta data loading function
  if(graph2ppt == T){
    if (graph.names != 'Default'){
      library(crayon)
      cat(blue(paste0("Do not forget to add a vector containing the names of your ", nr.plates, " graphs.\n")))
    }
    if (graph.names == 'Default'){
      graph.names <- c()
      for (i in 1:nr.plates){
        graph.names <- c(graph.names, paste0("Levelplot Plate ", i, ".ppt"))
      }
    }
  }
  
  library(crayon)
  heatdata <- levelplot.data.microbeta(data) 
  
  # if you do want to pool data, put an extra column that pretends as if there was only one plate. 
  plates <- rep(1, nrow(heatdata))
  
  # if you don't, add a column that says in which plate you are. 
  if (pool.plates == F){
    plates <- c()
    for (i in 1:nr.plates){
      plates <- c(plates, rep(i, 96))
    } ## for plates
  } ## if pool.plates == F
  heatdata$plate <- plates
  
  # 
  figures <- list()
  cat(blue(paste0("\n Writing the requested ", nr.plates, " graphs to ppt \n")))
  for (i in 1:length(unique(plates))){
    subdata <- subset(heatdata, heatdata$plate == i)
    levelplot <- ggplot(subdata, aes(X, Y, z= z.score))+
      geom_tile(aes(fill = z.score))+
      theme_few()+
      scale_y_continuous(expand = c(0, 0))+
      scale_x_continuous(expand = c(0, 0))+
      labs(y = '', x = '')+
      scale_fill_gradient(low="goldenrod1", high="sienna")+
      annotate("text", x = subdata$X, y = subdata$Y, label = subdata$peptide)+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(legend.position = "top")
    
    figures[[i]] <- levelplot
    if (graph2ppt == T){
      cat(blue(paste0('Graphing file ', i, ': ', graph.names[i], '\n')))
      graph2ppt(levelplot, file = paste0(path, '/', graph.names[i]))
    }
  } ## for plates
  
  cat(blue(paste0("\n Succesfully graphed the requested ", nr.plates, " levelplots. \n")))
  
  
  if (pool.plates == F){
    return(figures)
  } ## if 
  else{
    return(figures[[1]])
  } ## else
  
} ## function


