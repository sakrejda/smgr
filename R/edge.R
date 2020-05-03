Node = R6::R6Class("Node", lock_class=FALSE)
Transition = R6::R6Class("Transition", lock_class=FALSE)

#' R6 class representing the directed edge between two nodes
#'
#' Edges in graph building are implicit (stored as child nodes for 
#' each node) so these edges are used primarily for implementing 
#' simulation.  They store references to the parent nodes and 
#' they can modfiy both nodes.
DirectedEdge = R6::R6Class("DirectedEdge", 
  public = list(
    #' @description Used as DirectedEdge$new(...) create a new edge
    #'   between two nodes.  
    #'
    #' @param from (directed) edge originates at this node
    #' @param tail effect of edge on the ('from') node it originates
    #'   from as a list of `dplyr::mutate`-style expressions.  The 
    #'   expressions can access the special objects `.from` and `.to` 
    #'   which are used to access the nodes at the head or tail of 
    #'   the edge.  They are used as `.from$foo` to access attribute or
    #'   or data `foo` in the tail node.
    #' @param to (directed) edge goes to this node
    #' @param head parallel to `tail`, but for the `to` node.
    #' @return a 'DirectedEdge' object
    initialize = function(from, tail, to, head) {
      stopifnot(is(from, 'Node'))
      stopifnot(is(to, 'Node'))
      private$source_ = from
      private$tail_ = tail
      private$target_ = to
      private$head_ = head
    },
    transfer = function() {
      e_from = rlang::child_env(self$base)
      e_to = rlang::child_env(self$base)
      e_exec = rlang::child_env(self$base)
      rlang::env_bind(.env = e_exec, .from = e_from, .to = e_to)
      env_copy(from = self$source$attributes, 
        to = e_from, nms = ls(self$source$attributes))
      env_copy(from = self$source$data, 
        to = e_from, nms = ls(self$source$data))
      env_copy(from = self$target$attributes, 
        to = e_to, nms = ls(self$target$attributes))
      env_copy(from = self$target$data, 
        to = e_to, nms = ls(self$target$data))
      dm = rlang::new_data_mask(
        bottom = e_exec,
        top = self$base)
      match_tail = purrr::lift_dl(private$source_$matches)(private$tail_$match)
      if (match_tail) {
        for (i in 1:length(private$tail_$transformation)) {
          modified = purrr::map(private$tail_$transformation[i], rlang::eval_tidy, data = dm)
          rlang::env_bind(.env = e_exec, !!!modified)
          rlang::env_bind(.env = private$source_$data, !!!modified)
        }
      } 
      match_head = purrr::lift_dl(private$target_$matches)(private$head_$match)
      if (match_head) {
        for (i in 1:length(private$head_$transformation)) {
          modified = purrr::map(private$head_$transformation[i], rlang::eval_tidy, data = dm)
          rlang::env_bind(.env = e_exec, !!!modified)
          rlang::env_bind(.env = private$target_$data, !!!modified)
        }
      } 
      return(self)
    }
  ),
  private = list(
    source_ = Node$new(),
    target_ = Node$new(),
    head_ = Transition$new(),
    tail_ = Transition$new(),
    base_ = rlang::env()
  ),
  active = list(
    source = function() private$source_,
    target = function() private$target_,
    head = function() private$head_,
    tail = function() private$tail_,
    base = function() private$base_
  )
)
