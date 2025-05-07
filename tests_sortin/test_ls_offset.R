library(checkmate)
library(testthat)
context("rOffset")


test_that("ls_categorise RasterLayer", {
  input <- rtRasters$continuous
  binarised <- ls_binarise(input, thresh = 30)

  # test default
  output <- ls_offset(obj = binarised)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("offset"))
  expect_equal(object = length(output@feature$offset), expected = 3360)
  expect_equal(unique(output@feature$offset), c(1, 2))

  # test fun
  output <- ls_offset(obj = binarised, fun = "*", value = 2)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("offset"))
  expect_equal(object = length(output@feature$offset), expected = 3360)
  expect_equal(unique(output@feature$offset), c(0, 2))
})


test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous

  expect_error(rOffset(obj = "bla"))
  expect_error(rOffset(obj = input, value = "bla"))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  binarised <- ls_binarise(input, thresh = 30)

  output <- ls_offset(obj = binarised)
  history <- output@history
  expect_list(history, len = 3, types = "character")
  expect_equal(history[[3]], "the raster values have been offset by 1.")
})
