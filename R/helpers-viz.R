
#' Take a node list and return a list of from/to edges identified by id 
#'
#' A helper function, returns a structured list
#'
#' @param x node list
#'
#' @return a list (one per node) of lists (one per edge) of
#'   list(from = <FROM_ID> to = <TO_ID>)
#'
#' @export 
as_edges = function(x) { 
  from = names(x$child_ids)
  to = x$child_ids
  n_children = purrr::map_int(to, length)
  edges = list()
  for (i in seq_along(from)) {
    if (n_children[i] == 0)
      next
    edges[[from[i]]] = list()
    for (j in seq_along(to[[i]])) {
      A = from[i]
      B = to[[i]][[j]]
      transition = openssl::sha512(x = paste0(A, ":", B))
      edges[[from[i]]][[transition]] = c(from = A, to = B)
    }
  }
  return(edges)
}

#' Turn the node list into a graph object
#'
#' Currently uses igraph
#'
#' @param x node list
#'
#' @return igraph graph representing the node-list and parent-child
#'   relationships.
#' @export
as_graph = function(x) {
  edges = as_edges(x) 
  edges_df = edges %>% 
    purrr::set_names(rep('', length(.))) %>% 
    purrr::map_depth(2,  ~ dplyr::bind_rows(.)) %>% 
    purrr::lift_dl(dplyr::bind_rows)()
  if (!requireNamespace('igraph'))
    return(edges_df)
  gr = igraph::graph_from_edgelist(el = as.matrix(edges_df), directed = TRUE)
  node_ids = igraph::get.vertex.attribute(gr)$name
  short_ids = short_form(node_ids) 
  gr = igraph::set.vertex.attribute(gr, 'label', value = short_ids)
  for (id in node_ids) {
    node_json = x$get(id)$json
    gr = igraph::set.vertex.attribute(gr, 'json', id, value = node_json)
    attributes = x$get(id)$attributes
    for (attr in attributes) {
      gr = igraph::set.vertex.attribute(gr, attr, id, value = x$get(id)$get(!!attr))
    }
  }
  return(gr) 
}


