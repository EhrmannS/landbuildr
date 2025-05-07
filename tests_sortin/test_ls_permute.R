library(checkmate)
library(testthat)
context("ls_permute")


test_that("ls_permute RasterLayer", {
  input <- rtRasters$categorical

  # test for default
  output <- ls_permute(obj = input)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("permuted"))
  expect_equal(object = length(output@feature$permuted), expected = 3360)
  expect_identical(unique(output@feature$permuted), c(16, 0, 3, 26, 20, 6, 23, 36, 46))

  # test for default with sorting
  output <- ls_permute(obj = input, type = "descending")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("permuted"))
  expect_equal(object = length(output@feature$permuted), expected = 3360)
  expect_identical(unique(output@feature$permuted), c(47, 44, 41, 31, 27, 24, 21, 11, 1))

  # test for reverted permutation
  output <- ls_permute(obj = input, type = "revert")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("permuted"))
  expect_equal(object = length(output@feature$permuted), expected = 3360)
  expect_identical(unique(output@feature$permuted), c(1, 11, 24, 41, 27, 21, 44, 47, 31))

  # test for cycling values
  output <- ls_permute(obj = input, type = "cycle", by = 10)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("permuted"))
  expect_equal(object = length(output@feature$permuted), expected = 3360)
  expect_identical(unique(output@feature$permuted), c(41, 10, 7, 31, 37, 4, 34, 21, 11))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous

  expect_error(ls_permute(obj = "bla"))
  expect_error(ls_permute(obj = input, type = "bla"))
  expect_error(ls_permute(obj = input, type = 1))
  expect_error(ls_permute(obj = input, type = "cycle", by = "bla"))
  expect_error(ls_permute(obj = input, type = "cycle", by = 2.5))
})

test_that("history is correct", {
  input <- rtRasters$continuous

  output <- ls_permute(obj = input)
  history <- output@history
  expect_list(history, len = 2, types = "character")
  expect_equal(history[[2]], "values have been inverted")
})
