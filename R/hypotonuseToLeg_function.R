#' hypotonuseToLeg
#'
#' This function returns calculate the length of one leg of a 45° 45° 90° triangle from its hypotenuse
#' @param x The length of a 45-45-90 triangle's leg
#' @keywords hypotonuseToLeg
#' @export
#' @examples
#' hypotonuseToLeg()
hypotonuseToLeg = function(x){
  return(x / sqrt(2))
}
