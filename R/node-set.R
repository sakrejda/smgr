
#' R6 class representing a set of nodes (defined by 'tag', see helpers).
#'
#' The set is composed of nodes, nodes are inserted and then tracked by id.
#' Identical nodes (name and attributes) have their children merged on 
#' insert.
NodeSet = R6::R6Class("NodeSet",
  public = list(

    #' @description Used as NodeSet$new(...) create a new node set
    #'
    #' @param ... nodes to be inserted into the set
    #' @return a 'NodeSet' object.
    initialize = function(...) {
      nodes = list(...) 
      for (n in nodes) {
        self$insert(n)
      }
    },

    #' @description insert a new node into the set, if it is identical
    #' (name and attributes) as a current member, merge it into that member
    #' so that all children are preserved.
    #'
    #' @param x node to insert
    #' @return TRUE iff the new node was not identical to any other node (set
    #' size increases).
    insert = function(x) {
      if (!("Node" %in% class(x))) {
        return(FALSE)
      }
      id = x$id
      if (id %in% names(private$nodes_)) {
        private$nodes_[[id]]$merge(x)
        return(FALSE)
      }
      size = self$size
      private$nodes_[[id]] = x
      if (self$size != size)
        return(TRUE)
      else
        return(FALSE)
    },

    #' @description clear the current nodes from the set
    clear = function() {
      private$nodes_ = list()
      return(invisible(NULL))
    }, 

    #' @description get a specific node from the set
    #'
    #' @param id hash of the node
    #' @return node with the 'id', or NULL if missing
    get = function(id) {
      return(private$nodes_[[id]])
    },

    #' @description return a subset node set with only a subset of
    #'              nodes.
    filter = function(...) {
      o = NodeSet$new() 
      ids = self$ids
      for (id in ids) {
        if (self$get(id)$matches(...))
          o$insert(self$get(id))
      }
      return(o)
    },

    #' @description apply the given transition to the node with the matching
    #' id
    #'
    #' @param id id of the node to apply transition to
    #' @param transition transition to apply
    #' @return TRUE iff the transition created a new node.
    transition = function(id, transition, mutate = FALSE) {
      return(private$nodes_[[id]]$transition(transition, mutate))
    },

    #' @description apply a process (a list of transitions) to all nodes
    #' in the set until no new nodes are created.
    #'
    #' @param process the process to apply
    #' @return the final count of nodes
    build = function(process) {
      n_trz = process$size
      modified = TRUE
      while(isTRUE(modified)) {
        modified = FALSE
        for (i in 1:n_trz) {
          trz = process$get_transition(i)
          ids = self$ids
          for (id in ids) {
            new_node = self$transition(id, trz)
            modified = modified || self$insert(new_node)
          }
        }
      }
      return(self$size)
    },

    #' @description apply a process (a list of transitions) to all nodes
    #' in the set once.  Does not create new nodes.
    #'
    #' @param process the process to apply
    #' @return the final count of nodes
    modify = function(process) {
      for (i in 1:process$size) {
        trz = process$get_transition(i)
        ids = self$ids
        for (id in ids) {
          self$transition(id, trz, mutate = TRUE)
          names(private$nodes_) = self$ids
        }
      }
      return(self)
    }
  ),
  private = list(
    nodes_ = list()
  ),
  active = list(

    #' @field size number of distinct nodes in the set
    size = function() {
      return(length(private$nodes_))
    },

    #' @field ids of all nodes in the set.
    ids = function() {
      return(purrr::map_chr(private$nodes_, ~ .$id))
    },

    #' @field child_ids list of ids of all node children
    child_ids = function(x) purrr::map(private$nodes_, ~ .$child_ids),

    #' @field attributes list of attributes of all nodes
    attributes = function(x) purrr::map(private$nodes_, ~ .$attributes),

    #' @field contents list of attributes of all nodes in the set.
    contents = function(x) {
      values = purrr::map2(private$nodes_, self$attributes, function(.x, .y) {
        node = .x
        v = purrr::map(.y, ~ node$get(!!.)) %>% purrr::set_names(.y)
        return(v)
      })
      return(values)
    }
  )
)


