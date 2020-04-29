#' Enable imported functions
#' 
#' @title External imported functions widely used
#' @description imported functions
#' @details pipes
#' @importFrom dplyr `%>%` 
#' @export
`%>%` = magrittr::`%>%`

#' Default separator
#'
#' @return default separator string for the package
#' @export
default_sep = function() '::'

#' md5 is fine for a collision-resistant identifier
#'
#' Some day we should add checking and bump the hash
#' complexity on collision (?)
#'
#' @param x thing to hash
#' @return a hash string
#' @export
default_hash = function(x) {
  hash = openssl::md5(x)
  hash = as.character(hash)
  return(hash)
}
