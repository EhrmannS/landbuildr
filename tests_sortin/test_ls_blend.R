library(checkmate)
library(testthat)
library(raster)
context("ls_blend")


test_that("ls_blend RasterLayer", {
  patches <- ls_patches(ls_binarise(obj = rtRasters$continuous, thresh = 30))
  mask <- matrix(nrow = 56, ncol = 60, data = 0)
  mask[c(5:25), c(5:50)] <- 10
  rasMask <- raster(mask, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)
  geomMask <- gc_geom(input = rasMask)

  # test for grid geom overlay
  output <- ls_blend(obj = patches, overlay = geomMask)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("blended"))
  expect_equal(object = length(output@feature$blended), expected = 3360)
  expect_subset(x = unique(output@feature$blended), choices = c(NA, 27, 13, 4, 26, 9, 37, 23, 14, 20, 35, 3, 28, 22, 12, 24, 19, 17, 5, 21, 6, 15, 1, 11, 2, 7, 8))

  # test for RasterLayer overlay
  output <- ls_blend(obj = patches, overlay = rasMask)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("blended"))
  expect_equal(object = length(output@feature$blended), expected = 3360)
  expect_subset(x = unique(output@feature$blended), choices = c(NA, 27, 13, 4, 26, 9, 37, 23, 14, 20, 35, 3, 28, 22, 12, 24, 19, 17, 5, 21, 6, 15, 1, 11, 2, 7, 8))

  # test for matrix overlay
  output <- ls_blend(obj = patches, overlay = mask)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("blended"))
  expect_equal(object = length(output@feature$blended), expected = 3360)
  expect_subset(x = unique(output@feature$blended), choices = c(NA, 27, 13, 4, 26, 9, 37, 23, 14, 20, 35, 3, 28, 22, 12, 24, 19, 17, 5, 21, 6, 15, 1, 11, 2, 7, 8))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous
  mask <- matrix(nrow = 56, ncol = 60, data = 0)
  mask[c(5:25), c(5:50)] <- 10

  expect_error(ls_blend(obj = input))
  expect_error(ls_blend(obj = input, overlay = "bla"))
  expect_error(ls_blend(obj = input, overlay = mask, fun = "sum"))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  mask <- matrix(nrow = 56, ncol = 60, data = 0)
  mask[c(5:25), c(5:50)] <- 10

  output <- ls_blend(obj = input, overlay = mask)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been blended")
})
