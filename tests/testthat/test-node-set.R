test_that("a node set can be created", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n_list = NodeSet$new(n1)
  testthat::expect_true("NodeSet" %in% class(n_list))
})

test_that("an empty node set can be created", {
  n_list = NodeSet$new()
  testthat::expect_true("NodeSet" %in% class(n_list))
  testthat::expect_equal(n_list$size, 0)
})

test_that("a node can be inserted into an empty node set", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n_list = NodeSet$new()
  testthat::expect_true("NodeSet" %in% class(n_list))
  testthat::expect_equal(n_list$size, 0)
  n_list$insert(n1)
  testthat::expect_equivalent(n_list$ids, purrr::map_chr(list(n1), ~ .$id))
})

test_that("calling insert twice with the same node returns false", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n_list = NodeSet$new()
  testthat::expect_true("NodeSet" %in% class(n_list))
  testthat::expect_equal(n_list$size, 0)
  n_list$insert(n1)
  false = n_list$insert(n1)
  testthat::expect_false(false)
  testthat::expect_equivalent(n_list$ids, purrr::map_chr(list(n1), ~ .$id))
})

test_that("calling insert with a NULL node returns false", {
  n_list = NodeSet$new()
  testthat::expect_true("NodeSet" %in% class(n_list))
  testthat::expect_equal(n_list$size, 0)
  false = n_list$insert(NULL)
  testthat::expect_false(false)
  testthat::expect_equivalent(n_list$size, 0)
})

test_that("a node set can report its size", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n_list = NodeSet$new(n1)
  testthat::expect_equal(n_list$size, 1)
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n2 = Node$new("Mainstem fish", alive = TRUE, location = "Main stem", taggged = FALSE)
  n_list = NodeSet$new(n1, n2)
  testthat::expect_equal(n_list$size, 2)
})

test_that("a node set can return member id's", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n2 = Node$new("Mainstem fish", alive = TRUE, location = "Main stem", taggged = FALSE)
  n_list = NodeSet$new(n1, n2)
  testthat::expect_equivalent(n_list$ids, c(n1$id, n2$id))
})

test_that("a node set can return a member", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n2 = Node$new("Mainstem fish", alive = TRUE, location = "Main stem", taggged = FALSE)
  n_list = NodeSet$new(n1, n2)
  ids = n_list$ids
  testthat::expect_equal(n1, n_list$get(ids[1]))
})

test_that("a node set can return all member contents", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n2 = Node$new("Mainstem fish", alive = TRUE, location = "Main stem", tagged = FALSE)
  n_list = NodeSet$new(n1, n2)
  contents = n_list$contents
  purrr::map2(contents, n_list$attributes, ~ testthat::expect_true(isTRUE(all(names(.x) == .y))))
  testthat::expect_true(contents[[1]]$alive)
  testthat::expect_true(contents[[2]]$alive)
  testthat::expect_false(contents[[1]]$tagged)
  testthat::expect_false(contents[[2]]$tagged)
  testthat::expect_equal(contents[[1]]$location, "Bear brook")
  testthat::expect_equal(contents[[2]]$location, "Main stem")
})

# Creating the state machine description, simplest example.
test_that("a node set can use a process to build itself out", {
  n1 = Node$new("Bear fish", alive = TRUE, location = "Bear brook", tagged = FALSE)
  n2 = Node$new("Mainstem fish", alive = TRUE, location = "Main stem", tagged = FALSE)
  n_list = NodeSet$new(n1, n2)
  trz_list = list(
    Transition$new("die", match = list(isTRUE(alive)), transformation = list(alive = FALSE)),
    Transition$new("tag", match = list(!isTRUE(tagged), isTRUE(alive)), transformation = list(tagged = TRUE)))
  process = do.call(Process$new, trz_list)
  testthat::expect_equivalent(n_list$ids, c(n1$id, n2$id))
  testthat::expect_equal(n_list$size, 2)
  n_list$build(process)
  testthat::expect_equal(n_list$size, 8)
})

# Using the state machine for in-place modification, simplest example.
test_that("a node set can use a process to build itself out", {
  n1 = Node$new("Bear fish", 
    alive = TRUE, location = "Bear brook", tagged = FALSE, N = 100)
  n2 = Node$new("Mainstem fish", 
    alive = TRUE, location = "Main stem", tagged = FALSE, N = 10)
  n_set = NodeSet$new(n1, n2)
  process = Process$new(
    Transition$new("die", 
      match = list(isTRUE(alive)), 
      transformation = list(alive = FALSE, N = 0)),
    Transition$new("tag", ## tag 10 fish to start
      match = list(!isTRUE(tagged), isTRUE(alive)), 
      transformation = list(tagged = TRUE, N = 0)))
  testthat::expect_equivalent(n_set$ids, c(n1$id, n2$id))
  testthat::expect_equal(n_set$size, 2)
  n_set$build(process)
  testthat::expect_equal(n_set$size, 8)
  sim_process = Process$new(
    Transition$new("deaths_removal",
      match = list(isTRUE(alive), location == "Bear brook", !isTRUE(tagged)),
      transformation = list(N = N - 15)
    ),
    Transition$new("deaths_recording",
      match = list(!isTRUE(alive), location == "Bear brook", !isTRUE(tagged)),
      transformation = list(N = N + 15))
  )
  n_set$modify(sim_process)
  testthat::expect_equal(n_set$size, 8)
  testthat::expect_equal(n1$get(N), 85)
  dead_record_id = n_set$filter(
    !isTRUE(alive), location == "Bear brook", !isTRUE(tagged)
  )$ids[1]
  testthat::expect_equal(n_set$get(dead_record_id)$get(N), 15)
})

# Using the state machine for simulation, simplest example.
test_that("a node set can use a process to simulate", {
  n_list = NodeSet$new(
    Node$new("fish", 
      alive = TRUE, tagged = FALSE, N = 100))
  process = Process$new(
    Transition$new("die", 
      match = list(isTRUE(alive)), 
      transformation = list(alive = FALSE)),
    Transition$new("tag", 
      match = list(!isTRUE(tagged), isTRUE(alive)), 
      transformation = list(tagged = TRUE))
  )
  n_list$build(process)
  ## FIXME: in progress!
})
