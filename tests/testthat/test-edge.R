test_that("an edge between two nodes can be created.", {
  N = 10
  n1 = smgr:::Node$new("dead fish", alive = FALSE, swimming = FALSE, id = 1)
  o = smgr:::Transition$new("live fish", 
    match = list(!isTRUE(alive)), 
    transformation = list(alive = TRUE, id = id + 1))
  n2 = n1$transform(o)
  n1$mutate(N = N, .which = 'data')
  n2$mutate(N = N + 5, .which = 'data')
  e1 = smgr:::DirectedEdge$new(
    from = n1, 
    tail = smgr:::Transition$new("emigration",
      transformation = list(aa = 33, z = !alive + !swimming)),
    to = n2, 
    head = smgr:::Transition$new("imigration",
      transformation = list(aa = 33 - 0.5 * aa)))
  testthat::expect_true("DirectedEdge" %in% class(e1))
  #do_output_1 = e1$do(N = .from$N - 5 + .to$N)
  #testthat::expect_equal(do_output_1$N, 20)
  #do_output_2 = e1$do(N = 5, .which = 'source')
  #testthat::expect_equal(do_output_2$N, 5)
})

test_that("an edge can apply its transfers", {
  N = 10
  N_transfer = 7
  n1 = smgr:::Node$new("dead fish", alive = FALSE, swimming = FALSE, id = 1)
  o = smgr:::Transition$new("live fish", 
    match = list(!isTRUE(alive)), 
    transformation = list(alive = TRUE, id = id + 1))
  n2 = n1$transform(o)
  n1$mutate(N = N, .which = 'data')
  n2$mutate(N = N + 5, .which = 'data')
  e1 = smgr:::DirectedEdge$new(
    from = n1, 
    tail = smgr:::Transition$new("emigration", 
      transformation = list(N = .from$N - N_transfer)),
    to = n2,
    head = smgr:::Transition$new("imigration",
      transformation = list(N = .to$N + N_transfer)))
  testthat::expect_true("DirectedEdge" %in% class(e1))
  testthat::expect_equal(n1$get(N), N)
  testthat::expect_equal(n2$get(N), N + 5)
  e1$transfer()
  testthat::expect_equal(n1$get(N), N - N_transfer)
  testthat::expect_equal(n2$get(N), N + 5 + N_transfer)
})
