#' Produce a tag for every subfolder in a folder
#'
#' As the title states: this function will go over all subfolders in a folder and produce an identifier for each folder
#' @param folder This can be any folder. By default the function will check all subfolers the wd
#' @keywords
#' @export
#' @examples
#' folderTag

folderTag <- function(folder = getwd(), suffix = ''){
  files = check.folder(folder)
  tags = c()
  for (file in files){
    slashPositions = gregexpr(pattern= '/', file)[[1]]
    finalPosition = slashPositions[length(slashPositions)]
    if (suffix != ''){
      suffixPosition = gregexpr(pattern = suffix, file)[[1]]
    }
    
    lastChar = nchar(file) - finalPosition
    queryTag = string.substringRight(file, lastChar)
    tags = c(tags, queryTag)
  }
  return(tags)
}