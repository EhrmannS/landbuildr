library(checkmate)
library(testthat)
library(raster)
context("ls_skeletonise")


test_that("ls_skeletonise RasterLayer", {
  input <- rtRasters$continuous
  binarised <- ls_binarise(input, thresh = 30)

  # test for default
  output <- ls_skeletonise(obj = binarised)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("skeletonised"))
  expect_equal(object = length(output@feature$skeletonised), expected = 3360)
  expect_subset(x = output@feature$skeletonised, choices = c(NA, 1))

  # test specification of struct

})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous
  mat <- raster::as.matrix(input)
  binarised <- ls_binarise(input, thresh = 30)

  expect_error(ls_skeletonise(obj = mat))
  expect_error(ls_skeletonise(obj = "bla"))
  expect_error(ls_skeletonise(obj = binarised, struct = c(1, 2, 3)))
  expect_error(ls_skeletonise(obj = input))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  binarised <- ls_binarise(input, thresh = 30)

  output <- ls_skeletonise(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the morphological skeleton has been determined")

  binarised@history <- list()
  output <- ls_skeletonise(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the morphological skeleton has been determined")
})
