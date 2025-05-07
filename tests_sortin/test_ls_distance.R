library(checkmate)
library(testthat)
context("ls_distance")


test_that("ls_distance RasterLayer", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  # test method = default (euclidean)
  output <- ls_distance(obj = binarised)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("distance"))
  expect_equal(object = length(output@feature$distance), expected = 3360)
  expect_subset(x = round(max(output@feature$distance), 5), choices = c(7.21110))
  expect_subset(x = round(min(output@feature$distance), 5), choices = c(0))

  # test method = manhattan
  output <- ls_distance(obj = binarised, method = "manhattan")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("distance"))
  expect_equal(object = length(output@feature$distance), expected = 3360)
  expect_subset(x = round(max(output@feature$distance), 5), choices = c(10))
  expect_subset(x = round(min(output@feature$distance), 5), choices = c(0))

  # test method = chessboard
  output <- ls_distance(obj = binarised, method = "chessboard")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("distance"))
  expect_equal(object = length(output@feature$distance), expected = 3360)
  expect_subset(x = round(max(output@feature$distance), 5), choices = c(6))
  expect_subset(x = round(min(output@feature$distance), 5), choices = c(0))
})

test_that("bibliography item has been created", {
  input <- rtRasters$continuous
  binarised <- ls_binarise(input, thresh = 40)

  output <- ls_distance(binarised)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})


test_that("Error if arguments have wrong value", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  expect_error(ls_distance(obj = "bla"))
  expect_error(ls_distance(obj = rtRasters$categorical))
  expect_error(ls_distance(obj = binarised, method = 1))
  expect_error(ls_distance(binarised, method = "bla"))
})

test_that("history is correct", {
  binarised <- ls_binarise(rtRasters$continuous, thresh = 30)

  output <- ls_distance(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "distance values have been calculated according to the euclidean distance")

  binarised@history <- list()
  output <- ls_distance(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "distance values have been calculated according to the euclidean distance")
})
