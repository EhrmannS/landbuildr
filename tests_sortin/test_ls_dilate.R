library(checkmate)
library(testthat)
context("ls_dilate")


test_that("ls_dilate RasterLayer", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  # test default
  output <- ls_dilate(obj = binarised)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("dilated"))
  expect_equal(object = length(output@feature$dilated), expected = 3360)
  expect_subset(x = unique(output@feature$dilated), choices = c(0, 1))
})

test_that("dilate also non-binary input/kernel", {
  output <- ls_dilate(obj = rtRasters$continuous,
                    struct = setStruct(custom = matrix(c(1, 1, 1, 1, 6, 1, 1, 1, 1), 3, 3)))

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("dilated"))
  expect_equal(object = length(output@feature$dilated), expected = 3360)
  expect_numeric(x = output@feature$dilated, lower = 0, upper = 106)
})

test_that("Error if arguments have wrong value", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  expect_error(ls_dilate(obj = "bla"))
  expect_error(ls_dilate(obj = binarised, struct = c(1, 2, 3)))
})

test_that("history is correct", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  output <- ls_dilate(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the raster has been morphologically dilated")

  binarised@history <- list()
  output <- ls_dilate(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been morphologically dilated")
})
