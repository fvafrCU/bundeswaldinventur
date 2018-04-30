if (interactive()) devtools::load_all()
testthat::context("get options")

testthat::test_that("bare options", {
  set_options(reset = TRUE,  name = "bundeswaldinventur")
  result <- get_options(name = "bundeswaldinventur")
  reference <- structure(c(10, 6.18033988749895), 
                         .Names = c("graphics_width", "graphics_height"))
  testthat::expect_equal(result, reference)

  result <- get_options(name = "bundeswaldinventur", flatten_list = FALSE)
  reference <- structure(list(data_source = NULL, graphics_width = 10, 
                              graphics_height = 6.18033988749895), 
                         .Names = c("data_source", "graphics_width", 
                                    "graphics_height"))
  testthat::expect_equal(result, reference)

  result <- get_options("graphics_width", name = "bundeswaldinventur")
  reference <- structure(10, .Names = "graphics_width")
  testthat::expect_equal(result, reference)

  result <- get_options("graphics_width", name = "bundeswaldinventur", 
                        remove_names = TRUE)
  reference <- 10
  testthat::expect_equal(result, reference)
})

testthat::context("set options")

testthat::test_that("bare options", {
  result <- set_options(reset = TRUE,  name = "bundeswaldinventur")
  testthat::expect_true(result)
  result <- get_options(name = "bundeswaldinventur")
  reference <- structure(c(10, 6.18033988749895), 
                         .Names = c("graphics_width", "graphics_height"))
  testthat::expect_equal(result, reference)

  set_options("foo" = 3, name = "bundeswaldinventur")
  result <- get_options("foo", name = "bundeswaldinventur")
  reference <- structure(3, .Names = "foo")
  testthat::expect_equal(result, reference)



  set_options("foo" = 5, name = "bundeswaldinventur", overwrite = FALSE)
  result <- get_options("foo", name = "bundeswaldinventur")
  reference <- reference
  testthat::expect_equal(result, reference)

  set_options("bar" = 5, name = "bundeswaldinventur", 
              overwrite = FALSE)
  result <- get_options("bar", name = "bundeswaldinventur", remove_names = TRUE)
  reference <- 5
  testthat::expect_equal(result, reference)

  set_options(name = "bundeswaldinventur", overwrite = FALSE)
  result <- get_options(name = "bundeswaldinventur", remove_names = FALSE)
  reference <- structure(c(10, 6.18033988749895, 3, 5), 
                         .Names = c("graphics_width", "graphics_height", "foo", 
                                    "bar"))
  testthat::expect_equal(result, reference)
})

