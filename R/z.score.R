#' Calculate the z-score. 
#' 
#' Calculate the z-score of a value by manually indicating the mean and the sd/var.
#' @param value The value of which you'd like to calculate the z-score.
#' @param mean The mean value
#' @param sd The sd of the distribution of your value. 
#' @param var The variance of the distribution. By default, the sd is used, but if you like you can also put the variance in; 
#' @param method=='sd' By default you put the sd in, however, the variance can also work, if you indicate that you're using the variance; 
#' @keywords
#' @export
#' @examples
#' z.score

z.score <- function(value, mean, sd, var, method = 'sd'){
  if (method == 'sd'){
    z = ((value - mean)^2)/sd
  }
  if (method == 'var'){
    z = ((value - mean)^2)/sqrt(var)
  }
  return(z)
}