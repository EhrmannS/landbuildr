library(checkmate)
library(mmand)
context("ls_rescale")


test_that("output has class RasterLayer", {
  input <- rtRasters$continuous

  # test for thresh
  output <- ls_rescale(input, range = c(0, 25))

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("rescaled"))
  expect_equal(object = length(output@feature$rescaled), expected = 3360)
})


test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous
  mat <- as.matrix(input)

  expect_error(ls_rescale("bla"))
  expect_error(ls_rescale("bla", range = 0.5))
})

test_that("history is correct", {
  input <- rtRasters$continuous

  output <- ls_rescale(input, range = c(0, 25))
  history <- output@history
  expect_list(history, len = 2, types = "character")
  expect_true(history[[2]] == "the values have been scaled between 0 and 25")

  input@history <- list("this object has a history")
  output <- ls_rescale(input, range = c(0, 25))
  history <- output@history
  expect_list(history, len = 2, types = "character")
  expect_true(history[[1]] == "this object has a history")
})

