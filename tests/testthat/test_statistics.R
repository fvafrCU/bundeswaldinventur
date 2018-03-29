#' Tests the outputs of the functions of the file statistics.R.
#'
#' @author Franziska Berg
#' @section Version: 23.10.2015
#' @name A Header for
NULL

context("statistics.R")

test_that("modal_value", {
  expect_equal(modal_value(c(1, 8, 5, 5, 7)), 5)
  expect_equal(modal_value(c(1, 8, 5, 5, 7, 1), FALSE), 1)
  expect_equal(modal_value(c(1, 8, 5, 5, 7, 1), TRUE), c(1, 5))
  expect_equal(modal_value(c(1, 8, 5, 5, 7), TRUE), 5)
  expect_equal(modal_value(c(1, 8, 5, 5, 7), FALSE), 5)

  expect_that(modal_value(list(1, 8, 5, 5, 7)), throws_error())
})
