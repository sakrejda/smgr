library(dplyr)

identify_node = function(node) {
  node = node[!(names(node) %in% c('children', 'id'))]
  id = openssl::sha512(x = do.call(paste, args = c(node, list(sep = '::'))))
  return(id)
}

df_to_nodes = function(x) {
  x_list = list()
  for (i in 1:nrow(x)) {
    x_list[[i]] =  as.list(x[i,])
    x_list[[i]][['id']] = identify_node(x_list[[i]])
  }
  names(x_list) = sapply(x_list, `[[`, 'id')
  return(x_list)
}

match_tag = function(x, tag, match) isTRUE(x[[tag]] == match[[tag]])

match_all_tags = function(x, match) {
  tags = names(match)
  for (tag in tags) {
    if (!match_tag(x, tag, match))
      return(FALSE)
  }
  return(TRUE)
}

match_any_tags = function(x, match) {
  tags = names(match)
  for (tag in tags) {
    if (match_tag(x, tag, match))
      return(TRUE)
  }
  return(FALSE)
}

match_list_tags = function(x, match, filter = match_any_tags) {
  keepers = list()
  for (name in names(x)) {
    if (filter(x[[name]], match)) {
      keepers[[name]] = x[[name]]
    }
  }
  return(keepers)
}

create_node = function(x) {
  if (!is.list(x))
    stop("Node can only be created from a list of attributes.")
  node = x
  node[['id']] = identify_node(node)
  node[['children']] = character()
  if (!('node' %in% class(node))) 
    class(node) = c('node', class(node))
  return(node)
}

add_child_id = function(parent, child) {
  if (is.null(parent[['children']]))
    parent[['children']] = character()
  parent[['children']] = c(parent[['children']], child[['id']])
  return(parent)
}

add_descendent_ids = function(x, descendents) {
  for (child in descendents) {
    x = add_child_id(x, child)
  }
  return(x)
}

create_transition = function(process, match, to) {
  o = list(process = process, match = match, to = to)
  o[['matches']] = function(node) match_all_tags(node, o[['match']])
  o[['do']] = function(node) {
    for (to_name in names(to)) {
      if (to_name %in% names(node)) {
        node[[to_name]] = o[['to']][[to_name]]
        node = create_node(node)
      } else {
        msg = paste0("Target '", to_name, "', does not exist for node '", node[['id']], "'.")
        stop(msg)
      }
      return(node)
    }
  }
  class(o) = c('transition', class(o))
  return(o)
}

create_states = function(...) {
  o = list(...)
  for (state_name in names(o)) {
    o[[state_name]] = create_state(state_name, o[[state_name]])
  }
  return(o)
}

create_state = function(name, set) {
  o = list(name = name, set = set)
  class(o) = c('state', class(o))
  return(o)
}
      
create_process = function(states, transitions) {
  o = list(
    states = states, transitions = transitions)
  return(o)
}

do_transition = function(node, transition) {
  if(transition$matches(node)) {
    node = transition$do(node)
  }
  return(node)
}

visited = function(node) isTRUE(node[['visited']])

tag_visited = function(node) {
  node[['visited']] = TRUE
  return(node)
}

add_descendents = function(nodes, process) {
  transitions = process$transitions
  ids = get_ids(nodes)
  for (i in seq_along(nodes)) {
    N = length(nodes)
    if (visited(nodes[[i]]))
      next
    n_ = list()
    for (trz in transitions) {
      n_ = c(n_, list(do_transition(nodes[[i]], trz)))
    }
    nodes[[i]] = tag_visited(nodes[[i]])
    nodes[[i]] = add_descendent_ids(nodes[[i]], n_)
    nodes = c(nodes, n_)
  }
  nodes = merge_nodes(nodes)
  if (length(ids) == length(nodes) && all(sort(ids) == sort(get_ids(nodes))))
    return(nodes)
  else
    return(add_descendents(nodes, process))
}

get_ids = function(nodes) purrr::map_chr(nodes, ~ .$id)

identical_sets_unsorted = function(A, B) {
  ids_A = get_ids(A)
  ids_B = get_ids(B)
  condition = all(sort(ids_A) == sort(ids_B))
  return(condition)
}

get_children = function(nodes) purrr::map(nodes, ~ .$children %>% sort %>% as.character)

shorter_ids = function(ids) {
  N = length(unique(ids))
  N_ = 0
  i = 3
  while (N_ != N) {
    ids_ = substr(ids, 1, i)
    N_ = length(unique(ids_))
    i = i + 1
  }
  return(ids_)
}

shorten_ids = function(nodes) {
  ids = get_ids(nodes)
  id_map = shorter_ids(ids) %>% as.list()
  names(id_map) = ids
  for (i in seq_along(nodes)) {
    nodes[[i]][['id']] = id_map[[nodes[[i]][['id']]]]
    for (j in seq_along(nodes[[i]][['children']])) {
      nodes[[i]][['children']][j] = id_map[[nodes[[i]][['children']][j]]]
    }
  }
  return(nodes)
}

merge_nodes = function(nodes) {
  ids = get_ids(nodes)
  if (length(ids) == length(unique(ids)))
    return(nodes)
  uid_idx = which(!duplicated(ids))
  uids = ids[uid_idx]
  for (u_idx in uid_idx) {
    id = ids[u_idx]
    did_idx = which(ids == id & duplicated(ids))
    for (d_idx in did_idx) {
      nodes[[u_idx]][['children']] = c(
        nodes[[u_idx]][['children']], nodes[[d_idx]][['children']])
    }
    nodes[[u_idx]][['children']] = sort(unique(nodes[[u_idx]][['children']]))
  }
  return(nodes[uid_idx]) 
}



process = create_process(
  states = create_states(
    life = c('alive', 'dead'),
    location = c('mainstem', 'obear', 'mitchell', 'jimmy', 'downstream'),
    capture = c('stream', 'captured'),
    tagging = c('tagged', 'untagged')
  ),
  transitions = list(
    create_transition('death', list(life = 'alive'), list(life = 'death')),
    create_transition('move', list(life = 'alive', location = 'mainstem'), list(location = 'obear')),
    create_transition('move', list(life = 'alive', location = 'mainstem'), list(location = 'mitchell')),
    create_transition('move', list(life = 'alive', location = 'mainstem'), list(location = 'jimmy')),
    create_transition('move', list(life = 'alive', location = 'obear'), list(location = 'mainstem')),
    create_transition('move', list(life = 'alive', location = 'mitchell'), list(location = 'mainstem')),
    create_transition('move', list(life = 'alive', location = 'jimmy'), list(location = 'mainstem')),
    create_transition('emigrate', list(life = 'alive', location = 'mainstem'), list(location = 'downstream')),
    create_transition('first capture', list(life = 'alive', tagging = 'untagged'), 
                                       list(tagging = 'tagged', capture = 'captured')),
    create_transition('recapture, fatal', list(life = 'alive', tagging = 'untagged'), 
                                          list(life = 'dead', capture = 'captured')),
    create_transition('recapture', list(life = 'alive', tagging = 'tagged'),
                                   list(capture = 'captured'))
  )
)

states_to_list = function(x) {
  o = purrr::map(x, function(x) x[['set']])
  names(o) = purrr::map(x, function(x) x[['name']])
  return(o)
}

nodes_to_list = function(x) lapply(x, as.character)

extract_states = function(process) {
  states = process[['states']] %>% 
    states_to_list()
  states_df = do.call(what = expand.grid, 
    args = c(states, list(
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE))) %>%
    apply(1, as.list) %>% 
    lapply(create_node)
  return(states_df)
}

make_node_graph = function(nodes) {
  if (!require(igraph)) {
    install.packages('igraph')
    require(igraph)
  }
  n_nodes = length(nodes)
  ids = get_ids(nodes)
  nodes = purrr::map(nodes, ~ {class(.$children) = "character"; return(.)})
  edges = purrr::map2(.x = get_ids(nodes), .y = get_children(nodes),
    .f = ~ data.frame(from = .x, to = .y, stringsAsFactors = FALSE)) %>%
    purrr::map( ~ as.matrix(.) %>% t() %>% c()) %>% 
    unlist
  g <- graph(edges = edges, directed = TRUE) 
  return(g)
}


full_states = extract_states(process)

initial_states = full_states %>%
  purrr::keep(match_all_tags, match = list(tagging = 'untagged', life = 'alive', capture = 'stream')) %>%
  purrr::discard(match_all_tags, match = list(location = 'downstream'))
  

# What states can we reach from just the first initial states?
set_part = add_descendents(initial_states[1], process)

# What states can we reach from all initial states
set_full = add_descendents(initial_states, process)

# Are the two resulting sets the same?
ids_match = identical_sets_unsorted(set_part, set_full)
cat("ID's of two sets are the same: ", ids_match, "\n")

# ID hashes *exclude* information on descendents, do all nodes
# from the two sets have the same descendents? 
o = matrix(data = NA, nrow = length(set_part), ncol = length(set_full))
children_full = get_children(set_full)
children_part = get_children(set_part)
for (i in 1:nrow(o)) {
  for (j in 1:ncol(o)) {
    if(length(children_part[[i]]) == length(children_full[[j]])) 
      o[i,j] = all(children_part[[i]] == children_full[[j]])
    else o[i,j] = FALSE
  }
}
cat("All nodes in the two sets have the same descendents: ", all(apply(o, 1, sum) == 1), "\n")

set_full_s = shorten_ids(set_full)
g = make_node_graph(set_full_s)
plot(g, edge.arrow.size = .3, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

