library(checkmate)
library(testthat)
context("ls_centroid")


test_that("ls_centroid RasterLayer", {
  patches <- ls_patches(obj = ls_binarise(obj = rtRasters$continuous, thresh = 30))

  # test default
  output <- ls_centroid(obj = patches)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("centroids"))
  expect_equal(object = length(output@feature$centroids), expected = 3360)
  expect_subset(x = unique(output@feature$centroids), choices = c(NA, 26, 9, 13, 4, 10, 27, 25, 16, 18, 3, 22, 24, 19, 20, 17, 5, 21, 15, 6, 14, 12, 1, 11, 7, 28, 2, 8, 23))

  # test with point geom as output
  output <- ls_centroid(obj = patches, gridded = FALSE)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "point")
  expect_names(x = names(output@feature), must.include = c("fid", "gid"))
  expect_subset(x = unique(output@feature$fid), choices = c(NA, 26, 9, 13, 4, 10, 27, 25, 16, 18, 3, 22, 24, 19, 20, 17, 5, 21, 15, 6, 14, 12, 1, 11, 7, 28, 2, 8, 23))

  # test backround
  output <- ls_centroid(obj = patches, background = 0)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("centroids"))
  expect_equal(object = length(output@feature$centroids), expected = 3360)
  expect_true(all(!is.na(unique(output@feature$centroids))))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous
  patches <- ls_patches(ls_binarise(input, thresh = 30))

  expect_error(ls_centroid(obj = "bla"))
  expect_error(ls_centroid(obj = patches, gridded = "bla"))
  expect_error(ls_centroid(obj = patches, background = "bla"))
})

test_that("history is correct", {
  patches <- ls_patches(ls_binarise(rtRasters$continuous, thresh = 30))

  output <- ls_centroid(obj = patches)
  history <- output@history
  expect_list(history, len = 4)
  expect_equal(history[[4]], "the centroids of patches have been determined")

  patches@history <- list()
  output <- ls_centroid(obj = patches)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the centroids of patches have been determined")
})
