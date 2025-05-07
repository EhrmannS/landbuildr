library(checkmate)
library(testthat)
context("rFillNA")


test_that("rDilate RasterLayer", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)
  patches <- ls_patches(binarised)

  # test default
  output <- ls_fillNA(obj = patches)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("na_replaced"))
  expect_equal(object = length(output@feature$na_replaced), expected = 3360)
  expect_identical(unique(output@feature$na_replaced), c(0, 27, 13, 4, 26, 9, 10, 25, 3, 16, 18, 12, 22, 24, 19, 20, 17, 5, 21, 6, 15, 1, 14, 11, 2, 7, 8, 28, 23))
})

test_that("Error if arguments have wrong value", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)
  patches <- ls_patches(binarised)

  expect_error(ls_fillNA(obj = "bla"))
  expect_error(ls_fillNA(obj = patches, with = "NA"))
  expect_error(ls_fillNA(obj = patches, with = 1.1))
})

test_that("history is correct", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)
  patches <- ls_patches(binarised)

  output <- ls_fillNA(obj = patches)
  history <- output@history
  expect_list(history, len = 4)
  expect_equal(history[[4]], "NA has been replaced with 0")

  patches@history <- list()
  output <- ls_fillNA(obj = patches)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "NA has been replaced with 0")
})
