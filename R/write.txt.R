#' Write output to a .txt file
#'
#' Write your output to a .txt file
#' @param data The data you'd like to output. Make sure this is a character string. 
#' @param file The name you want your file to carry
#' @param suffix By default: .txt .If you'd like to rather safe it as another file than .txt, take in mind the spacer then.
#' @param folder The destination folder, by default: working directory
#' @keywords
#' @export
#' @examples
#' write.txt

write.txt <- function(data, file, suffix = '.txt', folder= getwd()){
  setwd(folder)
  filename = paste0(file, suffix)
  fileConn<-file(filename)
  writeLines(data, fileConn)
  close(fileConn)
}

