#' R6 class representing a single transition
#'
#' The transition has a process name, conditions it occurs under, and
#' transformations that it applies to a node
#'
#' The condition ("match") are specified as in `dplyr::filter` using the 
#' node attributes to match against.  The transformations follow the 
#' same approach as `dplyr::mutate` where a data mask basd on the attribute
#' names is searched prior to checking for varaible names in the quosure
#' environments.
Transition = R6::R6Class("Transition",
  public = list(

    #' @description Used as Transition$new(...) create a new transition
    #'
    #' @param process name of the process described by the transformation
    #' @param match list of conditions that the process applies to 
    #' @param transformation list of transformations to use on the process
    #' @return a 'Transition' object
    initialize = function(process, match, transformation) {
      match = rlang::enquo(match)
      match = rlang::as_quosures(x = match[[2]], env = rlang::quo_get_env(match))
      transformation = rlang::enquo(transformation)
      transformation = rlang::as_quosures(x = transformation[[2]], 
                                          env = rlang::quo_get_env(transformation))
      private$process_ = process
      private$match_ = match[-1] 
      private$transformation_ = transformation[-1]
    },

    #' @description Apply this transition to a node if the node matches
    #'
    #' @param x a 'Node' object
    #' @return a child 'Node' object created by applying the transition, if applicable
    do = function(x, mutate = FALSE) {
      stopifnot("Node" %in% class(x))
      match = x$matches(!!!private$match_)
      if (match) {
        if (mutate) {
          x_child = purrr::lift_dl(x$modify)(private$transformation_)
        } else {
          x_child = purrr::lift_dl(x$spawn)(private$transformation_)
        }
      } else {
        x_child = NULL
      }
      return(x_child)
    }
  ),
  private = list(
    process_ = character(),
    match_ = rlang::enquos(),
    transformation_ = rlang::enquos()
  ),
  active = list(
    #' @field id of the transition
    id = function() {
      return(digest::digest(self))
    },

    #' @field process process name stored by the transition
    process = function() {
      return(private$process_)
    },

    #' @field match process match stored by the transition
    match = function() {
      return(private$match_)
    },

    #' @field transformation process transformation stored by the transition
    transformation = function() {
      return(private$transformation_)
    }
  )
)

