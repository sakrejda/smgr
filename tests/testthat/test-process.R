test_that("a process can be created", {
  process = Process$new(
    Transition$new("die", match = list(isTRUE(alive)), 
      transformation = list(alive = FALSE)),
    Transition$new("tag", match = list(!isTRUE(tagged)), 
      transformation = list(tagged = TRUE)))
  testthat::expect_true("Process" %in% class(process))
  testthat::expect_equal(process$size, 2)
})


test_that("transitions can be retrieved from a process by index", {
  trz = list(
    Transition$new("die", match = list(isTRUE(alive)), transformation = list(alive = FALSE)),
    Transition$new("tag", match = list(!isTRUE(tagged)), transformation = list(tagged = TRUE)))
  process = do.call(Process$new, trz)
  testthat::expect_true("Process" %in% class(process))
  testthat::expect_equal(trz[[1]], process$get_transition(1))
  testthat::expect_equal(trz[[2]], process$get_transition(2))
})


