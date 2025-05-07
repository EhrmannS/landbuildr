library(checkmate)
library(testthat)
context("ls_categorise")


test_that("ls_categorise RasterLayer", {
  input <- rtRasters$continuous

  # test breaks
  output <- ls_categorise(obj = input, breaks = c(25, 50, 75, 90))

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("categorised"))
  expect_equal(object = length(output@feature$categorised), expected = 3360)
  expect_subset(x = unique(output@feature$categorised), choices = c(1, 2, 3, 4, 5))

  # test n
  output <- ls_categorise(obj = input, n = 5)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("categorised"))
  expect_equal(object = length(output@feature$categorised), expected = 3360)
  expect_subset(x = unique(output@feature$categorised), choices = c(1, 2, 3, 4, 5))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous

  expect_error(ls_categorise(obj = "bla"))
  expect_error(ls_categorise(obj = input, breaks = "bla"))
  expect_error(ls_categorise(obj = input, n = "bla"))
  expect_error(ls_categorise(obj = input, n = 5.5))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  greater <- ls_select(input, lower = 20, upper = 80)

  output <- ls_categorise(obj = input, n = 5)
  history <- output@history
  expect_list(history, types = "character", len = 2)
  expect_equal(history[[2]], "categories based on the breaks 0/20/40/60/80/100 have been defined")

  output <- ls_categorise(obj = greater, n = 5)
  history <- output@history
  expect_list(history, types = "character", len = 3)
  expect_equal(history[[3]], "categories based on the breaks 20/31.8/43.6/55.4/67.2/79 have been defined")
})
