# Example 1
#
# Problem statement: we are working with O'Bear data. Fish that leave O'Bear do
# not return and fish can die in O'Bear.  Emigration rates are low. 

# We have four types of observations:

# 1) handling in O'Bear during seasonal electrofishing
# 2) handling outside of O'Bear during seasonal electrofishing
# 3) recorded on PIT antenna at bottom of O'Bear
# 4) recorded on PIT antenna outside of O'Bear (could be recorded on any of
#  several antennas - possibilities are bottom of study area, top of study area,
#  confluence of WB and Jimmy, confluence of WB and Mitchell, middle of study
#  area (short time only))


library(smgr)

# Node attribute definition:
#
# alive : TRUE iff fish is alive
# location : One of 1) "O'Bear"; 2) "O'Bear antenna"; 3) "outside"
# tagged : TRUE iff fish has been PIT-tagged
# handling : TRUE iff fish is in a bucket or being handled (at whatever location)
# tag_record : 
#   1) "no" if the node represents a fish; 
#   2) "hand scanner" if the node represents data produced scanning by hand
#   3) "antenna" if the node represents data produced from an antenna hit
states = smgr:::NodeSet$new(
  smgr:::Node$new("O'bear fish", 
    alive = TRUE, 
    location = "O'Bear", 
    tagged = FALSE, 
    handling = FALSE, 
    tag_record = "no")
)

process = smgr:::Process$new(
  # Things fish can do
  smgr:::Transition$new("death", 
    match = list(isTRUE(alive), tag_record == "no"), 
    transformation = list(alive = FALSE)),
  smgr:::Transition$new("emigration, approach antenna", 
    match = list(isTRUE(alive), location == "O'Bear", !isTRUE(handling), tag_record == "no"),
    transformation = list(location = "O'Bear antenna")),
  smgr:::Transition$new("un-emigration, move upstream", 
    match = list(isTRUE(alive), location == "O'Bear", !isTRUE(handling), tag_record == "no"),
    transformation = list(location = "O'Bear antenna")),
  smgr:::Transition$new("emigration, pass antenna", 
    match = list(isTRUE(alive), location == "O'Bear antenna", !isTRUE(handling), tag_record == "no"),
    transformation = list(location = "outside")),

  # Captures and recaptures
  smgr:::Transition$new("capture", 
    match = list(isTRUE(alive), !isTRUE(handling), tag_record == "no"),
    transformation = list(handling = TRUE)),
  smgr:::Transition$new("insert tag", 
    match = list(isTRUE(alive), isTRUE(handling), !isTRUE(tagged)), 
    transformation = list(tagged = TRUE)),
  smgr:::Transition$new("record tag", 
    match = list(isTRUE(handling), isTRUE(tagged), tag_record == "no"), 
    transformation = list(tag_record = "hand scanner")),
  smgr:::Transition$new("release", 
    match = list(isTRUE(alive), isTRUE(handling), tag_record == "no"),
    transformation = list(handling = FALSE)),

  # Antenna hits
  smgr:::Transition$new("read tag (antenna)",
    match = list(location %in% c("O'Bear antenna", "outside"), isTRUE(tagged), 
                 !isTRUE(handling), tag_record == "no"),
    transformation = list(tag_record = "antenna"))
)

states$build(process)


attr_string = function(gr, attr) {
  o = paste(attr, ": ", igraph::get.vertex.attribute(gr, attr), sep = "")
  return(o)
}

# A plot of sorts
gr = smgr::as_graph(states)
pdf(file.path(getwd(), "bear-graph.pdf"), width = 50, height = 50)
plot(gr, 
  vertex.label = paste(
    attr_string(gr, "alive"),
    attr_string(gr, "location"),
    attr_string(gr, "tagged"),
    attr_string(gr, "handling"),
    attr_string(gr, "tag_record"),
    sep = "\n"
  ),
  vertex.shape = 'square',
  vertex.color = 'lightyellow',
  vertex.size = 10
); dev.off()

# Plausible data, according to the model 
data_description = smgr:::as_df(states)

# Show plausible data
data_description %>% data.frame %>% 
  dplyr::filter(tag_record %in% c("hand scanner", "antenna")) %>%
  print

# data = data.frame(...)   there's some observed data
#
#impossible_data = dplyr::left_join(
#  x = data, 
#  y = data_description %>% dplyr::filter(tag_record %in% c('hand scanner', 'antenna')),
#  by = c('location', 'alive', 'tag_record', 'handling', 'tagged')
#) %>% dplyr::filter(is.na(id))



