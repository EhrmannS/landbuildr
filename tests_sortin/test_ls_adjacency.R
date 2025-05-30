library(checkmate)
context("ls_adjacency")


test_that("output is data.frame", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_adjacency(obj = input)
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- ls_adjacency(obj = bin)
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)

  output <- ls_adjacency(obj = input, type = "paired")
  expect_data_frame(output, ncols = 10, nrows = 9, any.missing = FALSE)

  output <- ls_adjacency(obj = input, type = "pairedSum")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- ls_adjacency(obj = input, count = "single")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)
})

test_that("output with the correct column names", {
  input <- rtRasters$categorical
  bin <- ls_binarise(rtRasters$continuous, thresh = 40)

  output <- ls_adjacency(obj = input)
  expect_names(names(output), identical.to = c("class", "likeAdj"))

  output <- ls_adjacency(obj = input, count = "single")
  expect_names(names(output), identical.to = c("class", "likeAdj"))

  output <- ls_adjacency(obj = input, layer = "categorical", type = "paired")
  expect_names(names(output), identical.to = c("class", "1", "11", "21", "24", "27", "31", "41", "44", "47"))

  output <- ls_adjacency(obj = input, type = "pairedSum")
  expect_names(names(output), identical.to = c("class", "pairedSum"))
})

test_that("Error if arguments have wrong value", {
  input <- rtRasters$categorical
  mat <- as.matrix(input)

  expect_error(ls_adjacency(obj = mat))
  expect_error(ls_adjacency(obj = input, count = "bla"))
  expect_error(ls_adjacency(obj = input, count = 3))
  expect_error(ls_adjacency(obj = input, type = "bla"))
  expect_error(ls_adjacency(obj = input, type = 1))
})
