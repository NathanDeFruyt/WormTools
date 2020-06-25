#' Calculcating the Chemotaxis index
#'
#' This function calculates a chemotaxis index
#' @param DA is the number of worms in the diacetyl spot
#' @param EtOH is the number of worms in the ethanol spot
#' @param Others is the remainder of worms on the plate
#' @keywords
#' @export
#' @examples
#' load.LocomotionData

chemoindex <- function(DA, EtOH, Others){
  CI <- (DA - EtOH)/(DA+EtOH + Others)
  return(CI)
} ## function
