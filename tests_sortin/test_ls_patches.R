library(checkmate)
library(testthat)
context("ls_patches")


test_that("ls_patches RasterLayer", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  # test default
  output <- ls_patches(obj = binarised)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("patches"))
  expect_equal(object = length(output@feature$patches), expected = 3360)
  expect_identical(unique(output@feature$patches), c(NA, 27, 13, 4, 26, 9, 10, 25, 3, 16, 18, 12, 22, 24, 19, 20, 17, 5, 21, 6, 15, 1, 14, 11, 2, 7, 8, 28, 23))

  # test background
  output <- ls_patches(obj = binarised, background = 0)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("patches"))
  expect_equal(object = length(output@feature$patches), expected = 3360)
  expect_identical(unique(output@feature$patches), c(0, 27, 13, 4, 26, 9, 10, 25, 3, 16, 18, 12, 22, 24, 19, 20, 17, 5, 21, 6, 15, 1, 14, 11, 2, 7, 8, 28, 23))
})

test_that("Error if arguments have wrong value", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  expect_error(ls_patches(obj = "bla"))
  expect_error(ls_patches(obj = binarised, struct = c(1, 2, 3)))
  expect_error(ls_patches(obj = binarised, struct = "bla"))
  expect_error(ls_patches(obj = binarised, background = 1.1))
})

test_that("history is correct", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  output <- ls_patches(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "patches have been determined")

  binarised@history <- list()
  output <- ls_patches(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "patches have been determined")
})
