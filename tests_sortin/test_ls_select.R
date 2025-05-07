library(checkmate)
library(testthat)
library(raster)
context("ls_select")


test_that("ls_select RasterLayer", {
  input <- rtRasters$continuous

  # test both lower and upper
  output <- ls_select(obj = input, lower = 40, upper = 80)

  expect_names(x = names(output@feature), identical.to = "selected")
  expect_true(object = min(output@feature$selected, na.rm = T) > 40)
  expect_true(object = max(output@feature$selected, na.rm = T) < 80)

  # test only lower
  output <- ls_select(obj = input, lower = 40)
  expect_names(x = names(output@feature), identical.to = "selected")
  expect_true(object = min(output@feature$selected, na.rm = T) > 40)
  expect_true(object = max(output@feature$selected, na.rm = T) == 100)

  # test only upper
  output <- ls_select(obj = input, upper = 80)
  expect_names(x = names(output@feature), identical.to = "selected")
  expect_true(object = min(output@feature$selected, na.rm = T) == 1)
  expect_true(object = max(output@feature$selected, na.rm = T) < 80)

  # test background
  output <- ls_select(obj = input, upper = 80, background = 0)

  expect_names(x = names(output@feature), identical.to = "selected")
  expect_true(object = all(!is.na(output@feature$selected)))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous

  expect_error(ls_select(obj = input, lower = 40, upper = 101))
  expect_error(ls_select(obj = "bla", lower = 40, upper = 80))
  expect_error(ls_select(obj = input, lower = "bla"))
  expect_error(ls_select(obj = input, upper = "bla"))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  cat <- ls_categorise(input, n = 5)

  output <- ls_select(obj = input, lower = 40, upper = 80)
  history <- output@history
  expect_list(history, types = "character", len = 2)
  expect_equal(history[[2]], "values greater than 40 and less than 80 have been selected")

  output <- ls_select(obj = cat, lower = 2, upper = 4)
  history <- output@history
  expect_list(history, types = "character", len = 3)
  expect_equal(history[[3]], "values greater than 2 and less than 4 have been selected")
})
