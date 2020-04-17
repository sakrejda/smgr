# This state machine example has two sub-states (survival and location) that
# match up with the scenario of brook trout emigrating from a small stream
# through a one-way barrier.  Fish are tagged in the small stream or main stem
# and they can only be observed after tagging.
library(smgr)

nodes = smgr:::NodeSet$new(
  smgr:::Node$new("Tag Brook fish", 
           alive = TRUE, location = "Tag Brook", tagged = FALSE, 
           captured = FALSE, tag_reading = FALSE),
  smgr:::Node$new("main stem fish", 
           alive = TRUE, location = "main stem", tagged = FALSE, 
           captured = FALSE, tag_reading = FALSE)
)

# Fish can a) die; b) be tagged; c) emigrate to the main stem from Tag Brook; and 
# d) be recaptured (if they are already tagged).
process = smgr:::Process$new(
  smgr:::Transition$new("death", 
    match = list(isTRUE(alive), !isTRUE(tag_reading)), 
    transformation = list(alive = FALSE)),
  smgr:::Transition$new("emigration", 
    match = list(isTRUE(alive), location == "Tag Brook", !isTRUE(captured), !isTRUE(tag_reading)),
    transformation = list(location = "main stem")),
  smgr:::Transition$new("capture", 
    match = list(isTRUE(alive), !isTRUE(captured), !isTRUE(tag_reading)),
    transformation = list(captured = TRUE)),
  smgr:::Transition$new("release", 
    match = list(isTRUE(alive), isTRUE(captured), !isTRUE(tag_reading)),
    transformation = list(captured = FALSE)),
  smgr:::Transition$new("insert tag", 
    match = list(isTRUE(alive), location == "Tag Brook", isTRUE(captured), !isTRUE(tagged), !isTRUE(tag_reading)), 
    transformation = list(tagged = TRUE)),
  smgr:::Transition$new("insert tag", 
    match = list(isTRUE(alive), isTRUE(captured), !isTRUE(tagged), !isTRUE(tag_reading)), 
    transformation = list(tagged = TRUE)),
  smgr:::Transition$new("reading tag", 
    match = list(isTRUE(captured), isTRUE(tagged), !isTRUE(tag_reading)), 
    transformation = list(tag_reading = TRUE)),
  smgr:::Transition$new("done_reading tag", 
    match = list(isTRUE(tag_reading)), 
    transformation = list(tag_reading = FALSE))
)

# Build the full graph
nodes$build(process)

# i-graph object
gr = smgr::as_graph(nodes)

# 1) Location is labelled on the vertex
# 2) Grey nodes are *not* captured, light blue *are* captured
# 3) Untaggged fish represented by circles, only squares are available for
#    reading tags
# 4) Red borders indicate a tag being read (it's reversible)
# 5) Small nodes indicate dead fish, you can die in any state (except
#     for *during* tag reading).  
plot(gr, 
  vertex.label = factor(igraph::get.vertex.attribute(gr, 'location')), 
  vertex.shape = dplyr::if_else(igraph::get.vertex.attribute(gr, 'tagged'), 'square', 'circle'),
  vertex.color = dplyr::if_else(igraph::get.vertex.attribute(gr, 'captured'), 'lightblue', 'grey'),
  vertex.size = dplyr::if_else(igraph::get.vertex.attribute(gr, 'alive'), 15, 5),
  vertex.frame.color = dplyr::if_else(igraph::get.vertex.attribute(gr, 'tag_reading'), 'red', 'black')
)

pdf("/tmp/fish-graph.pdf", width = 15, height = 15)
plot(gr, 
  vertex.label = paste(
    paste(
      dplyr::if_else(igraph::get.vertex.attribute(gr, 'alive'), 'alive', 'dead'),
      dplyr::if_else(igraph::get.vertex.attribute(gr, 'tagged'), 'tagged', 'no tag'),
      sep = '/'),
    igraph::get.vertex.attribute(gr, 'location'),
    dplyr::if_else(igraph::get.vertex.attribute(gr, 'captured'), 'captured', 'free'),
    dplyr::if_else(igraph::get.vertex.attribute(gr, 'tag_reading'), 'read tag', '' ),
    sep = '\n'
  ),
  vertex.shape = 'square',
  vertex.color = dplyr::if_else(igraph::get.vertex.attribute(gr, 'captured'), 'rosybrown1', 'lightblue'),
  vertex.size = 15,
  vertex.frame.color = dplyr::if_else(igraph::get.vertex.attribute(gr, 'tag_reading'), 'red', 'black')
); dev.off()

# The next step is generating datasets and using this structure to check for
# correctness.
