#' Replace all NA values by a constant. 
#'
#' This function enables you to replace all NA values by a constant. 
#' @param data Here you put the data for which you like to replace it by bins.
#' @param binrange Here you put the range that the bins should span. E.g. if you want to put bins for years, you can chose 5 years and that'll give you bins of five years wide. 
#' @param set_bins Optinoal argument: 'automatic', or 'manual'; Default: 'automatic'. If you like to set bins manually, select manual.
#' @param manual_bins Here goes a vector with the bins you'd like. If you already selected automatic, this argument is optional. 
#' @keywords
#' @export
#' @examples
#' binning

# load the data
binning <- function(data, binrange, set_bins = 'automatic', manual_bins = c()){
  if (set_bins == 'automatic'){
    bins <- c()
    BinSpectrum <- as.numeric(levels(as.factor(data)))
    for (i in 1:round(length(BinSpectrum)/binrange)){
      bins[i] <- BinSpectrum[i*binrange]
    } # for
    bins[length(bins)] <- max(BinSpectrum)
    bins <- c(-1*10^31, bins, 1*10^31)
  }
  
  if (set_bins == 'manual'){
    bins <- manual_bins
    bins <- c(-1*10^31, bins, 1*10^31)
  }
  
  bins_output <- c()
  for (i in 1:length(data)){
    for (j in 1:length(bins)){
      if ((data[i] <= bins[j]) && (data[i] > bins[j-1])){
        bins_output[i] <- bins[j]
      } #if
    } # for
  }
  
  return(bins_output)
  
} # function
