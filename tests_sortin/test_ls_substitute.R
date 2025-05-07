library(checkmate)
context("ls_substitute")

test_that("ls_substitute RasterLayer", {
  input <- rtRasters$continuous

  # test for thresh
  output <- ls_substitute(input, old = c(41:47), new = 40)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "grid")
  expect_names(x = names(output@feature), must.include = c("substituted"))
  expect_equal(object = length(output@feature$substituted), expected = 3360)
  expect_subset(x = unique(output@feature$substituted), c(21, 25, 55, 40, 77, 79, 75, 50, 91, 83, 95, 64, 72, 24, 23, 20, 22,  7,  6,  4,  5,  1, 60, 54, 73, 94, 93, 58, 97, 48, 29, 30, 37, 31, 33, 98, 66, 69, 67, 34, 11, 52, 53, 57, 28, 87, 78, 56, 39, 49, 59, 38, 81, 88, 74, 76, 71, 19, 70, 63, 100, 51, 35,  9, 99, 32, 92, 84, 18,  8, 14, 90, 17, 16, 15, 12, 13, 85,  2,  3,  0, 96, 86, 68, 82, 61))
})


test_that("Error if arguments have wrong value", {
  input <- rtRasters$continuous

  expect_error(ls_substitute(input))
  expect_error(ls_substitute(input, old = c(41:47)))
  expect_error(ls_substitute(input, old = 41.1, new = "bla"))
  expect_error(ls_substitute(input, old = "bla", new = 40))
})

test_that("history is correct", {
  input <- rtRasters$continuous
  binarised <- ls_binarise(input, thresh = 30)

  output <- ls_substitute(input, old = c(41:47), new = 40)
  history <- output@history
  expect_list(history, len = 2, types = "character")

  output <- ls_substitute(binarised, old = 1, new = 2)
  history <- output@history
  expect_list(history, len = 3, types = "character")
})
