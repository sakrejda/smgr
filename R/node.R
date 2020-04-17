#' R6 class representing a single node, with links to child nodes
#'
#' The node has a name, an environment that contains its attributes, 
#' and a NodeSet that contains its children, along with accessors for
#' required functionality (determined by downstrea).  
Node = R6::R6Class("Node",
  public = list(
    
    #' @description Used as Node$new(...) create a new node
    #'
    #' @param name name of the node (arbitrary label, not checked).
    #' @param ... an attributes to record (in the environment).
    #' @param .children optoinally a list of child nodes (as NodeSet)
    #' @return a 'Node' object
    initialize = function(name, ..., .children = NodeSet$new()) {
      private$name_ <<- name
      private$attributes_ <<- rlang::env()
      att = list(...)
      rlang::env_bind(private$attributes_, !!!att)
      private$children_ <<- .children
    },
    
    #' @description
    #' Check whether this node matches conditions specified in dots
    #' @param ... interpreted as in `dplyr::filter` against the node
    #'   attributes.
    #' @return TRUE if the tests pass
    matches = function(...) {
      test = rlang::enquos(...)
      dm = rlang::new_data_mask(
        bottom = private$attributes_,
        top = private$attributes_)
      modified = purrr::map_lgl(test, rlang::eval_tidy, data = dm)
      return(all(modified))
    },
    
    #' @description
    #' Modify node attributes as specified in dots
    #' @param ... interpreted as in `dplyr::mutate` against the node
    #'   attributes using a data mask and quosures.
    #' @return modified version of self
    modify = function(...) {
      dm = rlang::new_data_mask(
        bottom = private$attributes_,
        top = private$attributes_)
      modified = purrr::map(list(...), rlang::eval_tidy, data = dm)
      rlang::env_bind(.env = private$attributes_, !!!modified)
      return(self)
    },
    
    #' @description
    #' Deep-clone the node and modify node attributes as specified in dots
    #' @param ... interpreted as in `dplyr::mutate` against the node
    #'   attributes using a data mask and quosures.
    #' @return modified version of the cloned node
    spawn = function(...) {
      clone = self$clone(deep = TRUE)
      clone$modify(...)
      clone$disown()
      private$add_child(clone)
      return(clone)
    }, 
    
    #' @description
    #' Merge another node's child id's into this node.  Only makes sense on
    #' a node with identical id's... should test that,
    #' @param x another Node object.
    #' @return modified self
    merge = function(x) {
      other_ids = x$child_ids
      other_children = x$children
      for (i in seq_along(other_ids)) {
        private$children_$insert(other_children$get(other_ids[i]))
      }
      return(self)
    },
    
    #' @description
    #' Clear record of child nodes, internal use
    #' @return self, without children
    disown = function() {
      private$children_$clear()
      return(self)
    },
    
    #' @description 
    #' Apply transformations described in a Transition object
    #' @param x transition object
    #' @return a *child* object created by applying the transition
    transition = function(x, mutate = FALSE) {
      stopifnot("Transition" %in% class(x))
      child = x$do(self, mutate)
      return(child)
    },
    
    #' @description
    #' Retrieve an attribute value by symbol (or character string).
    #' @param x bare symbol or character string
    #' @return value of x in node attributes
    get = function(x) {
      x = rlang::ensym(x)
      x = rlang::expr_text(x)
      if (missing (x))
        stop(msg_method_requires_arg('get', 'x'))
      attr = rlang::env_get(private$attributes_, x, default = NULL)
      return(attr)
    },

    #' @description
    #' Check for presence of attribute by symbol (or character string).
    #' @param x bare symbol or character string
    #' @return TRUE iff x exists as an attribute
    has = function(x) {
      x = rlang::ensym(x)
      x = rlang::expr_text(x)
      return(private$has_attribute(x))
    }
  ),
  private = list(
    name_ = "",
    attributes_ = rlang::env(),
    children_ = NodeSet$new(),
    deep_clone = function(name, value) {
      if (name == 'attributes_')
        return(rlang::env_clone(private$attributes_))
      if (name == 'children_')
        return(private$children_$clone(deep = TRUE))
      return(value)
    },
    add_child = function(child) {
      private$children_$insert(child)
    },
    has_attribute = function(x) {
      if (missing (x))
        stop(msg_method_requires_arg('has_attribute', 'x'))
      return(has_name(private$attributes_, x))
    },
    has_value = function(x) {
      if (missing (x))
        stop(msg_method_requires_arg('has_value', 'x'))
      return(isTRUE(!is.null(rlang::env_get(private$attributes_, x, default = NULL))))
    }
  ),
  active = list(

    #' @field id node id as string
    id = function() {
      id_ = compose_id('node', private$attributes_)
      return(id_)
    },

    #' @field children NodeSet of child nodes
    children = function() {
      children_ = private$children_
      return(children_)
    },

    #' @field child_ids character vector of child ids
    child_ids = function() {
      ids_ = sort(private$children_$ids)
      return(ids_)
    },

    #' @field attributes node attribute names as a string
    attributes = function() {
      attr = names(private$attributes_)
      return(attr)
    },

    #' @field json node attribute contents as JSON
    json = function() {
      attr_list = as.list(private$attributes_)
      json = jsonlite::toJSON(attr_list)
      return(json)
    },

    #' @field edges lists (one per edge) of list(from = <FROM_ID> to = <TO_ID>)
    edges = function() as_edges(self)
  )
)

