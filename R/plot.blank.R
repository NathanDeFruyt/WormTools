#' Plot a blank screen
#' 
#' Sometimes the plotting window does a little onorthodox things and you might want to plot a blank screen in between. 
#' @keywords
#' @export
#' @examples
#' plot.blank

plot.blank <- function(){
  ggplot(NULL)+
  theme_classic()
}