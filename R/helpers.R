
#' Check if object has a specific name
#'
#' @param x object to check on 
#' @param name name to check for
#' @return TRUE iff `name` is a name in `x`
#' @export
has_name = function(x, name) isTRUE(name %in% names(x))

#' Count unique values in x
#'
#' @param x object to check
#' @return count of unique values in x
#' @export
count_unique = function(x) x %>% unique %>% length

#' Length of longest character string in x
#'
#' @param x object to check
#' @return length of longest character string in x
#' @export
max_nchar = function(x) x %>% nchar %>% max

#' Find a short version of an identifier that maintains
#' uniqueness within the vector
#'
#' @param x object
#' @return truncate all elements to shortest possible string 
#'         without loosing uniqueness
#' @export
short_form = function(x) {
  n_unique = count_unique(x)
  max_nchar = max_nchar(x)
  for (i in 1:max_nchar) {
    sub_x = substr(x, 1, i)
    if (count_unique(sub_x) == n_unique)
      break
  }
  return(sub_x)
}
  

