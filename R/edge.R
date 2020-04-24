Node = R6::R6Class("Node", lock_class=FALSE)

#' R6 class representing the edge between two nodes
#'
Edge = R6::R6Class("Edge", 
  public = list(
    initialize = function(from, tail, to, head) {
      private$source_ = from
      private$target_ = to
      head = rlang::enquo(head)
      head = rlang::as_quosures(x = head[[2]],
                                    env = rlang::quo_get_env(head))
      private$head_ = head[-1]
      tail = rlang::enquo(tail)
      tail = rlang::as_quosures(x = tail[[2]],
                                    env = rlang::quo_get_env(tail))
      private$tail_ = tail[-1]
    },
    do = function(..., .which = 'target') {
      e_from = rlang::child_env(self$base)
      e_to = rlang::child_env(self$base)
      e_exec = rlang::child_env(self$base)
      rlang::env_bind(.env = e_exec, .from = e_from, .to = e_to)
      env_copy(from = self$source$attr, 
        to = e_from, nms = ls(self$source$attr))
      env_copy(from = self$source$data, 
        to = e_from, nms = ls(self$source$data))
      env_copy(from = self$target$attr, 
        to = e_to, nms = ls(self$target$attr))
      env_copy(from = self$target$data, 
        to = e_to, nms = ls(self$target$data))
      dm = rlang::new_data_mask(
        bottom = e_exec,
        top = self$base)
      modified = purrr::map(rlang::enquos(...), rlang::eval_tidy, data = dm)
      if (isTRUE(.which == 'target')) {
        rlang::env_bind(.env = private$target_$data, !!!modified)
      } else {
        rlang::env_bind(.env = private$source_$data, !!!modified)
      }
      return(self)
    }, 
    transfer = function() {
      self$do(!!!private$head_, .which = 'target')
      self$do(!!!private$tail_, .which = 'source')
      return(self)
    }
  ),
  private = list(
    source_ = Node$new(),
    target_ = Node$new(),
    head_ = rlang::enquos(),
    tail_ = rlang::enquos(),
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
