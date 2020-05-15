# Basic disease example with Susceptible, Exposed, Infected, Recovered
# and nodes for counting positives/negatives from PCR and 
# antibody tests.  Also death (from any cause).

devtools::install(); unloadNamespace('smgr'); 
library(smgr)

states = smgr:::NodeSet$new(
  smgr:::Node$new("susceptible", 
    outcome = "alive", disease_status = "naive", 
    oxygen = "normal", shedding = "none",
    pcr_test = "unknown", elisa_test = "unknown")
)

process = smgr:::Process$new(
  smgr:::Transition$new("mortality",
    match = list(outcome == "alive"),
    transformation = list(outcome = "death")),
  smgr:::Transition$new("disease death record",
    match = list(outcome == "death", 
                 pcr_test == "positive" | elisa_test == "positive"),
    transformation = list(outcome = "disease death")),
  smgr:::Transition$new("other death record",
    match = list(outcome == "death", 
                 pcr_test != "positive" & elisa_test != "positive"),
    transformation = list(outcome = "other death")),
  smgr:::Transition$new("infection",
    match = list(outcome == "alive", disease_status == "naive"),
    transformation = list(disease_status = "infected")),
  smgr:::Transition$new("shedding develops",
    match = list(outcome == "alive", disease_status == "infected",
                 shedding == "none"),
    transformation = list(shedding = "infectious")),
  smgr:::Transition$new("shedding abates",
    match = list(outcome == "alive", disease_status == "infected",
                 shedding == "infectious"),
    transformation = list(shedding = "low")),
  smgr:::Transition$new("respiratory problems",
    match = list(outcome == "alive", disease_status == "infected", 
                 oxygen == "normal"),
    transformation = list(oxygen = "low")),
  smgr:::Transition$new("respiratory recovery",
    match = list(outcome == "alive", disease_status == "infected", 
                 oxygen == "low"),
    transformation = list(oxygen = "normal")),
  smgr:::Transition$new("infection clearance",
    match = list(outcome == "alive", disease_status == "infected"),
    transformation = list(disease_status = "recovered", shedding = "none")),
  smgr:::Transition$new("true positive PCR",
    match = list(outcome == "alive", disease_status == "infected"),
    transformation = list(pcr_test = "positive")),
  smgr:::Transition$new("false positive PCR",
    match = list(outcome == "alive", disease_status != "infected"),
    transformation = list(pcr_test = "positive")),
  smgr:::Transition$new("true negative PCR",
    match = list(outcome == "alive", disease_status != "infected"),
    transformation = list(pcr_test = "negative")),
  smgr:::Transition$new("false negative PCR",
    match = list(outcome == "alive", disease_status == "infected"),
    transformation = list(pcr_test = "negative")),
  smgr:::Transition$new("true positive ELISA",
    match = list(outcome == "alive", disease_status != "naive"),
    transformation = list(elisa_test = "positive")),
  smgr:::Transition$new("false positive ELISA",
    match = list(outcome == "alive", disease_status == "naive"),
    transformation = list(elisa_test = "positive")),
  smgr:::Transition$new("true negative ELISA",
    match = list(outcome == "alive", disease_status == "naive"),
    transformation = list(elisa_test = "negative")),
  smgr:::Transition$new("false negative ELISA",
    match = list(outcome == "alive", disease_status != "naive"),
    transformation = list(elisa_test = "negative"))
)

# Build the full graph
states$build(process)

attr_string = function(gr, attr) {
  o = paste(attr, ": ", igraph::get.vertex.attribute(gr, attr), sep = "")
  return(o)
}

# A plot of sorts
gr = smgr::as_graph(states)
pdf("/tmp/sir-graph.pdf", width = 100, height = 100)
plot(gr, 
  vertex.label = paste(
    attr_string(gr, "outcome"),
    attr_string(gr, "disease_status"),
    attr_string(gr, "oxygen"),
    attr_string(gr, "shedding"),
    attr_string(gr, "pcr_test"),
    attr_string(gr, "elisa_test"),
    sep = "\n"
  ),
  vertex.shape = 'square',
  vertex.color = 'lightyellow',
  vertex.size = 7
); dev.off()

# Valid combinations
states$dump %>% dplyr::bind_rows()
