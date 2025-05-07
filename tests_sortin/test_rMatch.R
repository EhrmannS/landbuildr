library(checkmate)
library(testthat)
context("ls_match")


test_that("ls_match RasterLayer", {
  # binarised <- ls_binarise(obj = rtRasters$continuous, thresh = 30)
  #
  # # test default
  # output <- ls_match(obj = binarised)
  #
  # expect_class(x = output, classes = "geom")
  # expect_true(object = output@type == "grid")
  # expect_names(x = names(output@feature), must.include = c("morphed"))
  # expect_equal(object = length(output@feature$morphed$values), expected = 3360)
  # expect_subset(x = unique(output@feature$morphed$values), choices = c(NA, 26, 9, 13, 4, 10, 27, 25, 16, 18, 3, 22, 24, 19, 20, 17, 5, 21, 15, 6, 14, 12, 1, 11, 7, 28, 2, 8, 23))
  #
  # # test without rotating the structuring element
  # output <- ls_match(obj = binarised, rotate = FALSE)
  #
  # expect_class(x = output, classes = "geom")
  # expect_true(object = output@type == "grid")
  # expect_names(x = names(output@feature), must.include = c("morphed"))
  # expect_equal(object = length(output@feature$morphed$values), expected = 3360)
  #
  # # test backround
  # output <- ls_match(obj = binarised, background = 0)
  #
  # expect_class(x = output, classes = "geom")
  # expect_true(object = output@type == "grid")
  # expect_names(x = names(output@feature), must.include = c("morphed"))
  # expect_equal(object = length(output@feature$morphed$values), expected = 3360)
  # expect_true(all(!is.na(unique(output@feature$masked$values))))
})

test_that("output has the correct values (0/1) or (NA/1)", {
  # input <- rtRasters$continuous
  # binarised <- ls_binarise(input, thresh = 30)
  # kIso <- setStruct(custom = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3))
  #
  # output <- ls_match(binarised, struct = kIso)
  # vals <- unlist(unique(unique(output@feature[[1]])), use.names = F)
  # expect_true(all(vals %in% c(NA, 1)))
  #
  # output <- ls_match(binarised, struct = kIso, background = 0)
  # vals <- unlist(unique(unique(output@feature[[1]])), use.names = F)
  # expect_true(all(vals %in% c(0, 1)))
})

test_that("Error if arguments have wrong value", {
  # input <- rtRasters$continuous
  # kIso <- setStruct(custom = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3))
  #
  # expect_error(ls_match(obj = "bla"))
  # expect_error(ls_match(obj = mat))
  # expect_error(ls_match(obj = input, struct = c(1, 2, 3)))
  # expect_error(ls_match(obj = input, rotate = "bla"))
  # expect_error(ls_match(obj = input, background = 1.1))
})

test_that("history is correct", {
  # input <- rtRasters$continuous
  # binarised <- ls_binarise(input, thresh = 30)
  # kIso <- setStruct(custom = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3))
  #
  # output <- ls_match(binarised, struct = kIso)
  # history <- output@history
  # expect_list(history, len = 3, types = "character")
  # expect_equal(history[[3]], "cells have been matched with a 3 x 3 structuring element with values 0 0 0 0 1 0 0 0 0")
  #
  # binarised@history <- list()
  # output <- ls_match(binarised, struct = kIso)
  # history <- output@history
  # expect_list(history, len = 2, types = "character")
  # expect_equal(history[[2]], "cells have been matched with a 3 x 3 structuring element with values 0 0 0 0 1 0 0 0 0")
})
