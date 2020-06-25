#' Show all files in a folder
#'
#' Check what files are in a folder. 
#' @param folder Default = wd. What folder do you want to check the files of?
#' @keywords
#' @export
#' @examples
#' check.folder


check.folder <- function(folder = getwd()){
  path = folder
  files <- list.files(path=path, full.names=TRUE, recursive=FALSE)
  return(files)
}
