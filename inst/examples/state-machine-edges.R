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


# Initialize simulation parameters
swim_in_tag_brook = nodes %>%
  smgr::filter(alive, location == "Tag Brook", !captured, !tag_reading) %>%
  smgr::mutate(
    birth_rate = 0.2, death_rate = 2.3, 
    emigration_rate = 0.1, 
    capture_proportion = 0.8,
    season = "fall")

swim_in_main_stem = nodes %>%
  smgr::filter(alive, location == "main stem", !captured, !tag_reading) %>%
  smgr::mutate(
    birth_rate = 0.01, death_rate = 3.6, 
    immigration_rate = 0.05, emigration_rate = 0.04,
    capture_proportion = 0.6,
    season = "fall")

captured = nodes %>% 
  smgr::filter(alive, captured, !tag_reading) %>%
  smgr::mutate(death_rate = .05)

# Initialize populations
nodes %>% 
  smgr::filter(alive, !captured, !tagged, !tag_reading,
               location == "Tag Brook") %>%
  smgr::mutate(count = 194)
nodes %>% 
  smgr::filter(alive, !captured, !tagged, !tag_reading,
               location == "main stem") %>%
  smgr::mutate(count = 538)

# Define edges / flows (what's the UX here?)
migration = smgr:::DirectedEdge$new(
  from = nodes$get('999be65e4685'),
  tail = smgr:::Transition$new("emigration",
    transformation = list(
      emigrant_count = rbinom(n = 1, size = .from$count, prob = 1 - exp(-.from$emigration_rate)),
      count = .from$count - emigrant_count
    )),
  to = nodes$get('652b058320'),
  head = smgr:::Transition$new('immigration',
    transformation = list(
      count = .to$count + .from$emigrant_count))
)


