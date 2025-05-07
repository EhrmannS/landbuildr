library(checkmate)
library(testthat)
context("ls_binarise")


test_that("ls_binarise RasterLayer", {
  input <- rtRasters$continuous

  # test for thresh
  output <- ls_binarise(obj = input, thresh = 30)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("binarised"))
  expect_equal(object = length(output@feature$binarised), expected = 3360)
  expect_subset(x = output@feature$binarised, choices = c(0, 1))

  # test for match
  output <- ls_binarise(obj = input, match = 1)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("binarised"))
  expect_equal(object = length(output@feature$binarised), expected = 3360)
  expect_subset(x = output@feature$binarised, choices = c(0, 1))

  # test for several match values
  output <- ls_binarise(obj = input, match = c(1, 2))

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("binarised"))
  expect_equal(object = length(output@feature$binarised), expected = 3360)
  expect_subset(x = output@feature$binarised, choices = c(0, 1))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous

  expect_error(ls_binarise(obj = "bla"))
  expect_error(ls_binarise(obj = input, thresh = "bla"))
  expect_error(ls_binarise(obj = input, thresh = 101))
  expect_error(ls_binarise(obj = input, match = "bla"))
})

test_that("history is correct", {
  input <- rtRasters$continuous

  # test for thresh
  output <- ls_binarise(obj = input, thresh = 30)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "values have been binarised")

  # test for match
  output <- ls_binarise(obj = input, match = 1)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "values have been binarised")
})
