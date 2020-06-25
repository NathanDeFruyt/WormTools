#' Plot in Nathan's favourite style (hint: There are options!).
#' 
#' This function elegantly puts together a graph using ggplot. 
#' Be sure ggplot2 and ggthemes are installed.
#' @param data Here goes your data as a data-frame. 
#' @param x x-values (format: either data$column or column)
#' @param y y-values (format: idem)
#' @param group How you'd like to group your variables. 
#' @param facet_wrap (T or F) Would you like to have several panels?
#' @param facet Optional: If you chose for several panels, here you indicate what the panels should include. 
#' @param ylab Labels the y-axis
#' @param xlab Labels the x-axis
#' @param fill How you'd like to fill the plots. Different from colour.
#' @param colour Default == fill. How you'd like to colour the outlines of your plotted elements (e.g. outline of boxes in the boxplot)
#' @param filling 'manual' or 'automatic' (Default: automatic) How would you like to fill the plots? Manually or automatically? 
#' @param colour How you'd like to colour your 
#' @param colours If you opted filling = 'manual', provide a vector of the colours you'd like. This should have the same length as the number of objects you graph. 
#' @param type 'Boxplot' or 'violin' for now. 
#' @keywords
#' @export
#' @examples
#' ZI.plot

ZI.plot <- function(data, x, y, group, fill, colour=fill, facet_wrap = F, facet = '', ylab = 'y', xlab = 'x',  filling = 'automatic', colours, type = 'Boxplot'){
  library(crayon)
  library(ggthemes)
  library(ggplot2)
  attach(data)
  if (type == 'Boxplot'){
    if (filling == 'automatic'){
      plot <- ggplot(NULL)+
        geom_boxplot(data = data, aes(x=x, y=y, group=group, fill = fill, colour = colour), alpha= 0.7, width = 0.7) +
        geom_jitter(data = data, stat="identity", width = 0.2, aes(x=x, y=y, fill=fill, group=group, colour = colour), alpha = 0.5, cex=1, pch=16)+
        theme_few(base_size=14) +
        theme(legend.position="none") +
        labs(y = ylab, x = xlab)+
        theme(axis.text.x = element_text(angle=45, hjust = 1))
    } # if automatic
    if (filling == 'manual'){
      cat(red(paste0('Do not forget to add a vector of your ', length(levels(as.factor(fill))), ' favourite colours')))
      plot <- ggplot(NULL)+
        geom_boxplot(data = data, aes(x=x, y=y, group=group, fill = fill, colour = colour), alpha= 0.7, width = 0.7) +
        geom_jitter(data = data, stat="identity", width = 0.2, aes(x=x, y=y, fill=fill, group=group, colour = colour), alpha = 0.5, cex=1, pch=16)+
        theme_few(base_size=14) +
        scale_fill_manual(values=colours)+
        scale_colour_manual(values = colours)+
        theme(legend.position="none") +
        labs(y = ylab, x = xlab)+
        theme(axis.text.x = element_text(angle=45, hjust = 1))
    } # if manual
  } # if boxplot
  
  if (type == 'violin'){
    if (filling == 'automatic'){
      plot <- ggplot(NULL)+
        geom_violin(data = data, aes(x=x, y=y, fill = fill , group = group, colour = colour), draw_quantiles= TRUE, alpha=0.5)+
        geom_jitter(data = data, width = 0.2, stat="identity", aes(x=x, y=y, fill=fill, group=group, colour = colour), alpha = 0.8, cex=1.5, pch=21)+
        theme_few(base_size=14) +
        theme(legend.position="none") +
        labs(y = ylab, x = xlab)+
        theme(axis.text.x = element_text(angle=45, hjust = 1))
    } # if fill == automatic
    if (filling == 'manual'){
      cat(red(paste0('Do not forget to add a vector of your ', length(levels(as.factor(fill))), ' colours')))
      plot <- ggplot(NULL)+
        geom_violin(data = data, aes(x=x, y=y, fill = fill , group = group, colour = colour), draw_quantiles= TRUE, alpha=0.7)+
        geom_jitter(data = data, stat="identity", width = 0.2, aes(x=x, y=y, fill=fill, group=group, colour = colour), alpha = 0.5, cex=1.5, pch=21)+
        theme_few(base_size=14) +
        theme(legend.position="none") +
        scale_fill_manual(values=colours)+
        scale_colour_manual(values = colours)+
        labs(y = ylab, x = xlab)+
        theme(axis.text.x = element_text(angle=45, hjust = 1))
    } # if fill == manual
  } # if type == violin
  return(plot)
}
