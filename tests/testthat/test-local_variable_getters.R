#' Testing of variable_getters.R
#'
#' Tests the outputs of the functions of the file variable_getters.R.
#'
#' @author Franziska Berg
#' @section Version: 25.09.2015
#' @name get_design("a", 3) Header for
NULL

testthat::context("variable_getters.R")

testthat::test_that("get_species_groups", {
  output <- get_species_groups()

  reference <-
    structure(list(
      bagr.lab = c(
        "Fichte", "Weißtanne", "Douglasie",
        "Kiefer", "Lärchen", "sonst. Nadelholz", "Buche", "Eichen", "Roteiche",
        "Esche", "Bergahorn", "Hainbuche", "sonst. Hartholz", "Birken",
        "Erlen", "Pappeln", "sonst. Weichholz"
      ), ba.grupp = list(
        10,
        30, 40, 20, c(50, 51), c(
          11L, 12L, 13L, 14L, 15L, 16L, 17L,
          18L, 19L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 31L,
          32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 90L, 91L, 92L, 93L,
          94L, 95L, 96L, 97L, 98L, 99L
        ), 100, c(110, 111), 112, 120,
        140, 130, 141:199, c(200, 201), 210:213, 220:224, 230:299
      ),
      colors = c(
        "grey30", "red", "violet", "orange", "yellow",
        "#DC143C", "green", "darkblue", "#00BFFF", "#483D8B", "#008B8B",
        "#6B8E23", "#64BE96", "#F5DEB3", "#D2691E", "#BC8F8F", "brown"
      )
    ), .Names = c("bagr.lab", "ba.grupp", "colors"))

  testthat::expect_equal(output, reference)
})

testthat::test_that("get_design", {

  output <- get_design(TRUE, 3)
  reference <- structure(c(3575163, 3575163, 3575148, 35743, 35743, 35731, 8970,
                           8970, 8970, 100.024144587751, 100.024144587751, 
                           100.057317175562), 
                         .Dim = 3:4, 
                         .Dimnames = list(c("bwi1", "bwi2", "bwi3"), 
                                          c("a", "nte", "nt", "rf")))
  testthat::expect_equal(output, reference)
  output <- get_design(TRUE, 3, hard_coded = FALSE)
  reference <- structure(c(3575163, 3575263, 3575148, 400, 400, 400, 100, 100,
                           100, 8937.9075, 8938.1575, 8937.87), .Dim = 3:4, .Dimnames = list(
                           c("bwi1", "bwi2", "bwi3"), c("a", "nte", "nt", "rf")))
  testthat::expect_equal(output, reference)
  output <- get_design("nte", 3, hard_coded = FALSE)
  reference <- 400
  testthat::expect_equal(output, reference)
})

testthat::test_that("get_bwi_colors", {

  output <- get_bwi_colors()
  reference <- c("#9ACD32", "#7CFC00", "#006400")
  testthat::expect_equal(output, reference)
})
