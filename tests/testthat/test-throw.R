testthat::context("Testing bundeswaldinventur:::throw()")
testthat::test_that("throw the bundeswaldinventur exception", {
  error_message <- "hello, testthat"
  string <- "hello, testthat"
  testthat::expect_error(
    bundeswaldinventur:::throw(string),
    error_message
  )
})
