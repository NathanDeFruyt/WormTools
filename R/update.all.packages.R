#' This function will update/install a concise list of packages in R
#' 
#' @keywords
#' @export
#' @examples
#' update.packages()

update.packages <- function(){
  packs <- c(
    'crayon',
    'devtools',
    'roxygen2',
    'car',
    'rockchalk',
    'lmtest',
    'doBy',
    'multcomp',
    'tidyr',
    'dplyr',
    'broom',
    'lsmeans',
    'MASS',
    'ggplot2',
    'ggthemes',
    'afex',
    'effects',
    'lattice',
    'latticeExtra',
    'glmulti',
    'gmodels',
    'epitools',
    'vcd',
    'vcdExtra',
    'devtools',
    'lme4',
    'nnet',
    'AER',
    'ordinal',
    'colorRamps',
    'export',
    'Matrix',
    'nlme',
    'nlstools',
    'nlsMicrobio',
    'investr',
    'bbmle',
    'optimx',
    'locfit',
    'splines',
    'mgcv',
    'scales',
    'survival',
    'ggfortify',
    'KMsurv',
    'BGPhazard',
    'frailtySurv',
    'flexsurv',
    'Rmisc',
    'psych',
    'ggbiplot',
    'vegan',
    'ade4',
    'gclus',
    'cluster',
    'RColorBrewer',
    'MVN',
    'scatterplot3d',
    'installr')
  
  
  install.packages('crayon')
  library(crayon)
  head(packs)
  
  for (i in 1:length(packs)){
    install.packages(packs[i])
    cat(blue(paste0('Installed package ', i, ' of ', length(packs), ' (', i/length(packs)*100, '%)\n')))
  }
  source("https://www.dropbox.com/s/ojjhu0s743fbgwl/3dplots.R?dl=1")
  source("http://www.bioconductor.org/biocLite.R")
  biocLite("GEOquery")
  
  
  cat(paste0('Installed all ', length(packs), ' packages'))
}
