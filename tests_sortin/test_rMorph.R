library(checkmate)
library(testthat)
context("rMorph")


test_that("rMorph RasterLayer", {
  # input <- rtRasters$continuous
  # binarised <- rBinarise(input, thresh = 30)
  #
  # output <- rMorph(obj = input, struct = setStruct(custom = matrix(40, 1, 1)),
  #                  blend = "greater", merge = "all", rotate = FALSE, strict = FALSE)
  #
  # expect_class(x = output, classes = "geom")
  # expect_true(object = output@type == "grid")
  # expect_names(x = names(output@feature), must.include = c("morphed"))
  # expect_equal(object = length(output@feature$morphed$values), expected = 3360)
  # expect_subset(x = output@feature$morphed$values, choices = c(0, 1))
  #
  # output <- rMorph(obj = binarised, struct = setStruct(custom = matrix(c(NA, 1, NA, 0, 1, 1, 0, 0, NA), 3, 3)),
  #                  blend = "product", merge = "any", rotate = FALSE, strict = FALSE)
  #
  # expect_class(x = output, classes = "geom")
  # expect_true(object = output@type == "grid")
  # expect_names(x = names(output@feature), must.include = c("morphed"))
  # expect_equal(object = length(output@feature$morphed$values), expected = 3360)
  # expect_subset(x = output@feature$morphed$values, choices = c(0, 1))
})

test_that("Error if arguments have wrong value", {
  # input <- rtRasters$continuous
  #
  # expect_error(rMorph(obj = input, blend = "greater"))
  # expect_error(rMorph(obj = input, blend = "greater", merge = "all", struct = matrix(40, 1, 1)))
  # expect_error(rMorph(obj = input, blend = "greater", merge = "all", struct = list(c(40, 1, 1))))
  # expect_error(rMorph(obj = input, struct = setStruct(matrix(40, 1, 1)), blend = "greater", merge = "all", rotate = "bla"))
  # expect_error(rMorph(obj = input, struct = setStruct(matrix(40, 1, 1)), blend = "greater", merge = "all", rotate = FALSE, strict = "bla"))
  # expect_error(rMorph(obj = input, struct = setStruct(matrix(40, 1, 1)), blend = "greater", merge = "all", rotate = FALSE, background = "bla"))
})

test_that("history is correct", {
  # input <- rtRasters$continuous
  #
  # output <- rMorph(obj = input, blend = "greater", merge = "all", rotate = FALSE, strict = FALSE)
  # history <- output@history
  # expect_list(history, len = 2, types = "character")
  # expect_equal(history[[1]], "the object has been morphologically modified (blend:greater,merge:all)")
})
