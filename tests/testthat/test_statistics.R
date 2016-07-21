#' Tests the outputs of the functions of the file statistics.R.
#' 
#' @author Franziska Berg
#' @section Version: 23.10.2015
#' @name A Header for
NULL

context("statistics.R")

test_that("modal_value", {
  expect_equal(modal_value(c(1, 8, 5, 5, 7)), 5)
  expect_equal(modal_value(c(1, 8, 5, 5, 7), FALSE), 5)
  expect_equal(modal_value(c(1, 8, 5, 5, 7), FALSE, TRUE), 5)
  expect_equal(modal_value(c(1, 8, 5, 5, 7), FALSE, FALSE), 5)
  expect_equal(modal_value(c(1, 8, 5, 5, 7), TRUE), "5")
  expect_equal(modal_value(c(1, 8, 5, 5, 7), TRUE, TRUE), "5")
  expect_equal(modal_value(c(1, 8, 5, 5, 7), TRUE, FALSE), "5")
  
  expect_equal(modal_value(list(1, 8, 5, 5, 7)), list(5))
  expect_equal(modal_value(list(1, 8, 5, 5, 7), FALSE), list(5))
  expect_that(modal_value(list(1, 8, 5, 5, 7), FALSE, TRUE), throws_error())
  expect_equal(modal_value(list(1, 8, 5, 5, 7), FALSE, FALSE), list(5))
  expect_equal(modal_value(list(1, 8, 5, 5, 7), TRUE), NULL)
  expect_that(modal_value(list(1, 8, 5, 5, 7), TRUE, TRUE), throws_error())
  expect_equal(modal_value(list(1, 8, 5, 5, 7), TRUE, FALSE), NULL)
  
  expect_equal(modal_value(head(baeume.3[10])), structure(list(Entf = 
      c(4.73000002, 6.98000002, 7.53000021, 3.22000003, 8.92000008, 9.56000042)), 
      .Names = "Entf", row.names = c(NA, 6L), class = "data.frame"))
  expect_equal(modal_value(head(baeume.3[10]), FALSE), structure(list(Entf = 
     c(4.73000002, 6.98000002, 7.53000021, 3.22000003, 8.92000008, 9.56000042)), 
     .Names = "Entf", row.names = c(NA, 6L), class = "data.frame"))
  expect_equal(modal_value(head(baeume.3[10]), TRUE), c("3.22000003", 
      "4.73000002", "6.98000002", "7.53000021", "8.92000008", "9.56000042"))
  expect_equal(modal_value(head(baeume.3[10]), FALSE, FALSE), 
      structure(list(Entf = c(4.73000002, 6.98000002, 7.53000021, 3.22000003, 
      8.92000008, 9.56000042)), .Names = "Entf", row.names = c(NA, 6L), 
      class = "data.frame"))
  expect_that(modal_value(head(baeume.3[10]), FALSE, TRUE), throws_error())
  expect_equal(modal_value(head(baeume.3[10]), TRUE, FALSE), c("3.22000003", 
      "4.73000002", "6.98000002", "7.53000021", "8.92000008", "9.56000042"))
  expect_that(modal_value(head(baeume.3[10]), TRUE, TRUE), throws_error())
})