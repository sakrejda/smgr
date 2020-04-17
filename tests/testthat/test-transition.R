test_that('a transition can be created', {
  o = Transition$new("dog", 
    match = list(cat = 3, dog = "beast"), 
    transformation = list(zerg = "best", frolic = 333))
  testthat::expect_true("Transition" %in% class(o))
})

test_that('a transition can spawn a new node', {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  o = Transition$new("dog", match = list(!isTRUE(alive)), transformation = list(alive = TRUE))
  o_child = o$do(n)
  testthat::expect_true("Node" %in% class(n))
  testthat::expect_true("Transition" %in% class(o))
  testthat::expect_true(o_child$matches(isTRUE(alive)))
}) 

test_that('a transition can modify a node in-place', {
  death_count = 33
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE, N = death_count)
  o = Transition$new("death", match = list(!isTRUE(alive)), 
    transformation = list(N = N + 2))
  testthat::expect_equal(n$get(N), death_count)
  o$do(n, mutate = TRUE)
  o$do(n, mutate = TRUE)
  testthat::expect_true("Node" %in% class(n))
  testthat::expect_true("Transition" %in% class(o))
  testthat::expect_equal(n$get(N), death_count + 2 + 2)
}) 

test_that('a node can use a transition.', {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  o = Transition$new("dog", match = list(!isTRUE(alive)), transformation = list(alive = TRUE))
  n2 = n1$transition(o)
  testthat::expect_true("Node" %in% class(n1))
  testthat::expect_true("Transition" %in% class(o))
  testthat::expect_true(n2$matches(isTRUE(alive)))
}) 


