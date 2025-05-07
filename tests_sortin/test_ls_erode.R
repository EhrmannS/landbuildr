library(checkmate)
library(testthat)
context("ls_erode")


test_that("rDilate RasterLayer", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  # test default
  output <- ls_erode(obj = binarised)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("eroded"))
  expect_equal(object = length(output@feature$eroded), expected = 3360)
  expect_subset(x = unique(output@feature$eroded), choices = c(0, 1))
})

test_that("Error if arguments have wrong value", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  expect_error(ls_erode(obj = "bla"))
  expect_error(ls_erode(obj = binarised, struct = c(1, 2, 3)))
})

test_that("history is correct", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  output <- ls_erode(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the raster has been morphologically eroded")

  binarised@history <- list()
  output <- ls_erode(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been morphologically eroded")
})

