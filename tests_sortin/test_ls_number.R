library(checkmate)
context("ls_number")


test_that("output is data.frame", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_number(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 2, nrows = 1, any.missing = FALSE)

  output <- ls_number(obj = input, scale = "patch")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- ls_number(obj = input, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 1, any.missing = FALSE)
})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_number(obj = bin, scale = "patch")
  expect_equal(output$patches, 26)

  output <- ls_number(obj = bin, scale = "class")
  expect_equal(output$classes, 2)
})

test_that("output has the correct columm names", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_number(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("landscape", "patches"))

  output <- ls_number(obj = input, layer = "categorical", scale = "patch")
  expect_names(names(output), identical.to = c("class", "patches"))

  output <- ls_number(obj = input, scale = "class")
  expect_names(names(output), identical.to = c("landscape", "classes"))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$categorical
  mat <- as.matrix(input)

  expect_error(ls_number(obj = mat))
  expect_error(ls_number(obj = input, scale = "landscape"))
  expect_error(ls_number(obj = input, layer = 1))
})

test_that("bibliography item has been created", {
  options(bibliography = NULL)
  input <- rtRasters$categorical

  output <- ls_number(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")

  options(bibliography = NULL)
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)
  disEuc <- ls_distance(bin)

  output <- ls_number(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
