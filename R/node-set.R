
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

    #' @description insert a new edge into the set, if it is identical
    #' to a current link, merge it into that link
    #'
    #' @param from id of originating node
    #' @param to id of target node
    #' @param f transfer function implementing the edge
    #' @return id of edge
    link = function(from, to, f = NULL) {
      if (!(from %in% names(private$edges_))) {
        private$edges_[[from]] = list()
      }
      if (!(to %in% names(private$edges_[[from]]))) {
        private$edges_[[from]][[to]] = list()
      }
      if (!is.null(f)) {
          private$edges_[[from]][[to]] = c(
            private$edges_[[from]][[to]], f)
      }
      private$edge_list_ = c(
        private$edge_list_, c(from = from, to = to))
      return(private$edge_list_)
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
    #' @param ... expression (matches `dplyr::filter` used to subset
    #' @return subset of nodes
    filter = function(...) {
      o = NodeSet$new() 
      ids = self$ids
      for (id in ids) {
        if (self$get(id)$matches(...))
          o$insert(self$get(id))
      }
      return(o)
    },

    #' @description modify nodes returns modified nodes *also* modifies
    #'              original
    #' @param ... expression (matches `dplyr::mutate`) used to modify
    #' @return modified nodes
    mutate = function(..., .which = 'data') {
      ids = self$ids
      for (id in ids) {
        self$get(id)$mutate(..., .which = .which)
        names(private$nodes_) = self$ids
      }
      return(self)
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
            new_node = self$get(id)$transform(trz)
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
          self$get(id)$modify(trz)
          names(private$nodes_) = self$ids
        }
      }
      return(self)
    }
  ),
  private = list(
    nodes_ = list(),
    edges_ = list(),
    edge_list_ = list()
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
    attributes = function(x) {
      attr = purrr::map(private$nodes_, ~ as.list(.$attributes))
      names(attr) = self$ids
      return(attr)
    },

    #' @field data list of data of all nodes
    data = function(x) {
      data = purrr::map(private$nodes_, ~ as.list(.$data))
      names(data) = self$ids
      return(data)
    },

    #' @field dump all data/attributes from all nodes as lists
    dump = function() {
      o = purrr::map(private$nodes_, ~ .$dump)
      return(o)
    }
  )
)


