library(checkmate)
library(testthat)
context("ls_mask")


test_that("ls_mask RasterLayer", {
  input <- rtRasters$continuous
  mask <- matrix(nrow = 56, ncol = 60, data = 0)
  mask[c(5:25), c(5:50)] <- 1

  # test default
  output <- ls_mask(obj = input, by = mask)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("masked"))
  expect_equal(object = length(output@feature$masked), expected = 3360)

  # test backround
  output <- ls_mask(obj = input, by = mask, background = 0)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("masked"))
  expect_equal(object = length(output@feature$masked), expected = 3360)
  expect_true(all(!is.na(unique(output@feature$masked))))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous
  mask <- matrix(nrow = 56, ncol = 60, data = 0)
  mask[c(5:25), c(5:50)] <- 1
  nonbinary_m <- mask
  nonbinary_m[5, 5] <- 2
  wrongsize_m <- matrix(nrow = 60, ncol = 60, data = 1)

  expect_error(ls_mask(obj = "bla"))
  expect_error(ls_mask(obj = mat))
  expect_error(ls_mask(obj = input, by = "bla"))
  expect_error(ls_mask(obj = input, by = nonbinary_m))
  expect_error(ls_mask(obj = input, by = wrongsize_m))
  expect_error(ls_mask(obj = mat, background = 0))
  expect_error(ls_mask(obj = mat, by = m, background = 1.1))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  mask <- matrix(nrow = 56, ncol = 60, data = 0)
  mask[c(5:25), c(5:50)] <- 1

  output <- ls_mask(obj = input, by = mask)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been masked")
})
