
#' R6 class representing a process (a list of transitions
#'
#' Not much here but a fancy list...
Process = R6::R6Class("Process",
  public = list(

    #' @description Used as Process$new(...) to create a new process from a set of
    #' transtions
    #'
    #' @param ... transitions combined to create the process
    #' @return a 'Process' object
    initialize = function(...) {
      transitions = list(...)
      for (p in transitions) {
        self$insert(p)
      }
    },

    #' @description insert a new transition into the process.
    #'
    #' @param x transition to insert
    #' @return invisible NULL
    insert = function(x) {
      private$transitions_ = c(private$transitions_, list(x))
      return(invisible(NULL))
    },

    #' @description retrieve a transition by index
    #'
    #' @param i index of the transition to retrieve
    #' @return a transition
    get_transition = function(i) { 
      return(private$transitions_[[i]])
    }
  ),
  private = list(
    transitions_ = list()
  ),
  active = list(
    #' @field size the number of transitions in the process
    size = function() {
      n = length(private$transitions_)
      return(n)
    }
  )
)
