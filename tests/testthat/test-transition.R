test_that('a transition can be created', {
  o = Transition$new("dog", 
    match = list(cat = 3, dog = "beast"), 
    transformation = list(zerg = "best", frolic = 333))
  testthat::expect_true("Transition" %in% class(o))
})


test_that('a node can use a transition.', {
  n1 = smgr:::Node$new("dead fish", alive = FALSE, swimming = FALSE)
  o = smgr:::Transition$new("dog", 
    match = list(!isTRUE(alive)), 
    transformation = list(alive = TRUE))
  n2 = n1$modify(o)
  testthat::expect_true("Node" %in% class(n1))
  testthat::expect_true("Transition" %in% class(o))
  testthat::expect_true(n2$matches(isTRUE(alive)))
}) 


