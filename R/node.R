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
      private$data_ <<- rlang::child_env(private$attributes_)
      att = list(...)
      rlang::env_bind(private$attributes_, !!!att)
      private$children_ <<- .children
    },
    
    #' @description
    #' Check whether this node matches conditions specified in dots
    #' @param ... interpreted as in `dplyr::filter` against the node
    #'   attributes.
    #' @return TRUE iff the tests pass
    matches = function(...) {
      test = rlang::enquos(...)
      dm = rlang::new_data_mask(
        bottom = self$data,
        top = self$attributes)
      matched = purrr::map_lgl(test, rlang::eval_tidy, data = dm)
      return(isTRUE(all(matched)))
    },
    
    #' @description
    #' Modify node attributes as specified in dots
    #' @param ... interpreted as in `dplyr::mutate` against the node
    #'   attributes using a data mask and quosures.
    #' @param .which either 'attributes' in which case the static node
    #'   attributes are modified and a new node ID will be generated 
    #'   or 'data' (anything else really) and the mutable data in the 
    #'   node will be modified.
    #' @return modified version of self
    mutate = function(..., .which = 'attributes') {
      dm = rlang::new_data_mask(
        bottom = self$data,
        top = self$attributes)
      modified = purrr::map(list(...), rlang::eval_tidy, data = dm)
      if (isTRUE(.which == 'attributes')) {
        rlang::env_bind(.env = self$attributes, !!!modified)
      } else {
        rlang::env_bind(.env = self$data, !!!modified)
      }
      return(self)
    },
    
    #' @description
    #' Deep-clone the node and mutate node attributes as specified in dots
    #' @param ... interpreted as in `dplyr::mutate` against the node
    #'   attributes using a data mask and quosures.
    #' @return modified version of the cloned node
    spawn = function(...) {
      clone = self$clone(deep = TRUE)
      clone$mutate(...)
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
      if (self$id != x$id)
        stop("Can not merge nodes with different attributes.")
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
    #' Create a new node using transformations described in a Transition object
    #' @param x transition object
    #' @return a *child* object created by applying the transition
    transform = function(x) {
      stopifnot("Transition" %in% class(x))
      match = purrr::lift_dl(self$matches)(x$match)
      if (match) {
        child = purrr::lift_dl(self$spawn)(x$transformation)
      } else {
        child = NULL
      }
      return(child)
    },

    #' @description 
    #' Modify the current node using transformations described in a Transition object
    #' @param x transition object
    #' @return self object created by applying the transition
    modify = function(x) {
      stopifnot("Transition" %in% class(x))
      match = purrr::lift_dl(self$matches)(x$match)
      if (match) {
        child = purrr::lift_dl(self$mutate)(x$transformation)
      } else {
        child = NULL
      }
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
      attr = rlang::env_get(self$attributes, x, default = NULL)
      if (is.null(attr)) {
        attr = rlang::env_get(self$data, x, default = NULL)
      }
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
    data_ = rlang::env(),
    children_ = NodeSet$new(),
    deep_clone = function(name, value) {
      if (name == 'attributes_')
        return(rlang::env_clone(private$attributes_))
      if (name == 'data_')
        return(rlang::child_env(private$attributes_))
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

    #' @field attributes node attributes
    attributes = function() {
      return(private$attributes_)
    },

    #' @field data node data
    data = function() {
      if (!identical(rlang::env_parent(private$data_), private$attributes_)) {
        data_e = rlang::child_env(private$attributes_)
        for (nm in rlang::env_names(private$data_)) {
          rlang::env_bind(data_e, !!nm := rlang::env_get(private$data_, nm))
        }
        private$data_ = data_e
      }
      return(private$data_)
    },

    #' @field dump contents as a list
    dump = function() c(as.list(self$attributes), as.list(self$data)),

    #' @field json node contents as JSON
    json = function() {
      json = jsonlite::toJSON(self$dump)
      return(json)
    }

  )
)

