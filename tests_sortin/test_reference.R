# library(checkmate)
# library(testthat)
# context("getBibliography")
#
#
# test_that("reference produces an output with at least two entries: R and lomm", {
#   options(bibliography = NULL)
#
#   output <- getBibliography(print = FALSE)
#   expect_list(output, len = 2, any.missing = FALSE)
#   expect_class(output, "bibentry")
# })
#
# test_that("an algorithm that shall produce bib output changes the bibentry", {
#   options(bibliography = NULL)
#   con <- rtRasters$continuous
#   binarised <- rBinarise(con, thresh = 30)
#   patches <- rPatches(binarised)
#
#   output <- getBibliography(print = FALSE)
#   expect_list(output, len = 3, any.missing = FALSE)
# })
#
# test_that("output is sent to file", {
#   options(bibliography = NULL)
#
#   getBibliography(outFile = paste0(getwd(), "/myBib.bib"))
#   expect_file_exists(paste0(getwd(), "/myBib.bib"), access = "rw")
#
#   getBibliography(outFile = "myBib.bib")
#   expect_file_exists("myBib.bib", access = "rw")
# })
#
# test_that("output is printed in the console", {
#   options(bibliography = NULL)
#
#   expect_vector(capture_output_lines(getBibliography()), size = 16)
# })
