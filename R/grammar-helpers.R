
#' @param .data NodeSet
#' @param ... filtering expressions
#' @return NodeSet
#' @export
filter = function(.data, ...) .data$filter(...)

#' @param .data NodeSet
#' @param ... mutating expressions
#' @return NodeSet
#' @export
mutate = function(.data, ...) .data$mutate(..., .which = 'data')

#' @param .data NodeSet
#' @return single remaining node or error
#' @export
pop = function(.data) .data$pop()
