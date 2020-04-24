test_that("a node can be created", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  testthat::expect_true("Node" %in% class(n))
})

test_that("a node can expose its environments", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  att = ls(n$attr)
  dat = n$data
  testthat::expect_true(all(att %in% c('alive', 'swimming')))
  testthat::expect_true(isTRUE(is.environment(dat)))
})

test_that("a node can return an attribute", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  testthat::expect_true("Node" %in% class(n))
  testthat::expect_false(n$get(alive))
  testthat::expect_false(n$get(swimming))
})

test_that("a node can report the presence of an attribute", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  testthat::expect_true("Node" %in% class(n))
  testthat::expect_true(n$has(alive))
  testthat::expect_true(n$has(swimming))
  testthat::expect_false(n$has(al1ve))
  testthat::expect_false(n$has(swiMming))
})

test_that("a node can report its attribute", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  testthat::expect_true(all(n$attributes %in% c('alive', 'swimming')))
})

test_that("a node has an id over 6 characters", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n_id = n$id
  testthat::expect_true("Node" %in% class(n))
  testthat::expect_true("hash" %in% class(n_id))
  testthat::expect_gt(nchar(n_id), 6)
})

test_that("a new node has no children", {
  n = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n_children = n$children
  testthat::expect_true("Node" %in% class(n))
  testthat::expect_equal(n_children, NodeSet$new())
})

test_that("a new node can be cloned", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n2 = n1$clone()
  testthat::expect_equal(n1$id, n2$id)
})

test_that("a new node can be deep cloned", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n2 = n1$clone(deep = TRUE)
  testthat::expect_equal(n1$id, n2$id)
})

test_that("a new node can be null-modified", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n2 = n1$clone(deep = TRUE)
  n1 = n1$modify()
  testthat::expect_equal(n1$id, n2$id)
})

test_that("new nodes can be modified consistently", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n2 = n1$clone(deep = TRUE)
  n1 = n1$modify(alive = TRUE)
  n2 = n2$modify(alive = TRUE)
  testthat::expect_equal(n1$id, n2$id)
  testthat::expect_equal(n1$matches(isTRUE(alive)), n2$matches(isTRUE(alive)))
})

test_that("new nodes can have their simulation data modified without affecting ID's", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n2 = n1$clone(deep = TRUE)
  n1 = n1$modify(N = 33, .which = 'data')
  n2 = n2$modify(N = 34, .which = 'data')
  testthat::expect_equal(n1$id, n2$id)
  testthat::expect_true(n1$matches(N == 33))
  testthat::expect_true(n2$matches(N == 34))
})

test_that("new nodes can be spawned modified.", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  n2 = n1$spawn(alive = TRUE)
  n1 = n1$modify(alive = TRUE)
  testthat::expect_equal(n1$id, n2$id)
})

test_that("a node can match", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  matches = n1$matches(!isTRUE(alive), !isTRUE(swimming))
  testthat::expect_true(matches)
})

test_that("a node can match and be modified", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  matches = n1$matches(!isTRUE(alive), !isTRUE(swimming))
  n1$modify(alive = TRUE, swimming = 13)
  also_matches = n1$matches(isTRUE(alive), swimming == 13)
  mis_matches = n1$matches(isTRUE(alive), swimming == 14)
  testthat::expect_true(matches)
  testthat::expect_true(also_matches)
  testthat::expect_false(mis_matches)
})

test_that("new nodes can be spawned modified by transitions.", {
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE)
  o = Transition$new("dog", match = list(!isTRUE(alive)), transformation = list(alive = TRUE))
  n2 = n1$transition(o)
  testthat::expect_true("Node" %in% class(n1))
  testthat::expect_true("Node" %in% class(n2))
  testthat::expect_true(n2$id %in% n1$child_ids)
  testthat::expect_true(n2$id != n1$id)
})

test_that("nodes can be modified by transitions.", {
  N = 10
  n1 = Node$new("dead fish", alive = FALSE, swimming = FALSE, N = N)
  o = Transition$new("live fish", match = list(!isTRUE(alive)), transformation = list(alive = TRUE, N = N + 1))
  n2 = n1$transition(o, mutate = TRUE)
  testthat::expect_true("Node" %in% class(n1))
  testthat::expect_true("Node" %in% class(n2))
  testthat::expect_true(n2$id == n1$id)
  testthat::expect_equal(n1$get(N), 11)
  testthat::expect_equal(n2$get(N), 11)
})
