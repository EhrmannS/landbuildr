library(checkmate)
library(raster)
context("ls_area")


test_that("output is data.frame", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_area(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- ls_area(obj = input, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 48, any.missing = FALSE)

  output <- ls_area(obj = input, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- ls_area(obj = input, scale = "landscape")
  expect_data_frame(output, ncols = 2, nrows = 1, any.missing = FALSE)
})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_area(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- ls_area(obj = bin, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)
})

test_that("output has the correct columm names", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_area(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "cells"))

  output <- ls_area(obj = input, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "cells"))

  output <- ls_area(obj = input, scale = "class")
  expect_names(names(output), identical.to = c("class", "cells"))

  output <- ls_area(obj = input, scale = "landscape")
  expect_names(names(output), identical.to = c("landscape", "cells"))
})

test_that("output with the correct unit", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_area(obj = bin, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "area"))

  output <- ls_area(obj = input, scale = "patch", layer = "categorical", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "area"))

  output <- ls_area(obj = input, scale = "class", unit = "map")
  expect_names(names(output), identical.to = c("class", "area"))

  output <- ls_area(obj = input, scale = "landscape", unit = "map")
  expect_names(names(output), identical.to = c("landscape", "area"))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$categorical
  mat <- as.matrix(input)

  expect_error(ls_area(obj = mat))
  expect_error(ls_area(obj = input, scale = "bla"))
  expect_error(ls_area(obj = input, unit = "meter"))
  expect_error(ls_area(obj = input, layer = 1))
})

test_that("bibliography item has been created", {
  input <- rtRasters$categorical
  options(bibliography = NULL)

  output <- ls_area(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")

  options(bibliography = NULL)
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)
  disEuc <- rDistance(bin)

  output <- ls_area(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
