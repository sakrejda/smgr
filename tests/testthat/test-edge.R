test_that("an edge between two nodes can be created and modify them.", {
  N = 10
  n1 = smgr:::Node$new("dead fish", alive = FALSE, swimming = FALSE, id = 1)
  o = smgr:::Transition$new("live fish", 
    match = list(!isTRUE(alive)), 
    transformation = list(alive = TRUE, id = id + 1))
  n2 = n1$transition(o)
  n1$modify(N = N, .which = 'data')
  n2$modify(N = N + 5, .which = 'data')
  e1 = smgr:::Edge$new(
    from = n1, tail = list(aa = 33, z = !alive + !swimming),
    to = n2, head = list(aa = 33 - 0.5 * aa))
  testthat::expect_true("Edge" %in% class(e1))
  e1$do(N = .from$N - 5 + .to$N)
  testthat::expect_equal(n2$data$N, 20)
  e1$do(N = 5, .which = 'source')
  testthat::expect_equal(n1$data$N, 5)
})

test_that("an edge can apply its transfers", {
  N = 10
  N_transfer = 7
  n1 = smgr:::Node$new("dead fish", alive = FALSE, swimming = FALSE, id = 1)
  o = smgr:::Transition$new("live fish", 
    match = list(!isTRUE(alive)), 
    transformation = list(alive = TRUE, id = id + 1))
  n2 = n1$transition(o)
  n1$modify(N = N, .which = 'data')
  n2$modify(N = N + 5, .which = 'data')
  e1 = smgr:::Edge$new(
    from = n1, tail = list(N = .from$N - N_transfer),
    to = n2, head = list(N = .to$N + N_transfer))
  testthat::expect_true("Edge" %in% class(e1))
  testthat::expect_equal(n1$data$N, N)
  testthat::expect_equal(n2$data$N, N + 5)
  e1$transfer()
  testthat::expect_equal(n1$data$N, N - N_transfer)
  testthat::expect_equal(n2$data$N, N + 5 + N_transfer)
})
