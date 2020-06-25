#' Restart your R session. 
#' 
#' Restarts your R session. 
#' @keywords
#' @export
#' @examples
#' restart R

restart.wormtools <- function(){
  require(crayon)
  .rs.restartR()
  cat(red("Night night, see you again in a couple of seconds."))
}