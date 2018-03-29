#' Testing of tree_species_shares.r
#'
#' Tests the outputs of the functions of the file tree_species_shares.r.
#'
#' @author Franziska Berg
#' @section Version: 05.11.2015
#' @name A Header for
NULL

context("tree_species_shares.R")

test_that("tree_species_percent", {
  output <- tree_species_percent(FVBN.bagr.gw.3)

  reference <-
    c(
      34.0480299654245, 8.12350364963504, 3.37185094122167, 5.85201844026124,
      1.76833653476758, 21.8136258163657, 7.59089819439109, 12.1772800614675,
      5.25445639646562
    )

  expect_equal(output, reference)
})

test_that("melt_tree_species_percent", {
  output <- melt_tree_species_percent("FVBN.bagr.gw")

  reference <-
    structure(list(
      bwi = structure(c(
        1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L
      ), .Label = c("1987", "2002", "2012"), class = "factor"),
      group = structure(c(
        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L,
        2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
        8L, 9L
      ), .Label = c(
        "FI", "TA", "DGL", "KI", "LAE", "BU",
        "EI", "ALH", "ALN"
      ), class = "factor"), value = c(
        43.5327031406744,
        7.87648969827919, 2.26216297553824, 8.23866347712015, 1.98345551354271,
        18.6633590554827, 6.79280808704375, 7.37391233891504, 3.27644571340381,
        37.7287744906721, 7.89858168761221, 2.84416204823979, 6.77795097962688,
        1.89646865974553, 21.2065514011396, 7.34725080009367, 10.1598829131215,
        4.14037701974865, 34.0480299654245, 8.12350364963504, 3.37185094122167,
        5.85201844026124, 1.76833653476758, 21.8136258163657, 7.59089819439109,
        12.1772800614675, 5.25445639646562
      )
    ), .Names = c(
      "bwi", "group",
      "value"
    ), row.names = c(NA, -27L), class = "data.frame")

  expect_equal(output, reference)
})

test_that("cast_tree_species_percent", {
  output <- cast_tree_species_percent("FVBN.bagr.gw")

  reference <-
    structure(list(
      Baumartengruppe = structure(1:9, .Label = c(
        "FI",
        "TA", "DGL", "KI", "LAE", "BU", "EI", "ALH", "ALN"
      ), class = "factor"),
      `1987` = c(
        43.5327031406744, 7.87648969827919, 2.26216297553824,
        8.23866347712015, 1.98345551354271, 18.6633590554827, 6.79280808704375,
        7.37391233891504, 3.27644571340381
      ), `2002` = c(
        37.7287744906721,
        7.89858168761221, 2.84416204823979, 6.77795097962688, 1.89646865974553,
        21.2065514011396, 7.34725080009367, 10.1598829131215, 4.14037701974865
      ), `2012` = c(
        34.0480299654245, 8.12350364963504, 3.37185094122167,
        5.85201844026124, 1.76833653476758, 21.8136258163657, 7.59089819439109,
        12.1772800614675, 5.25445639646562
      )
    ), .Names = c(
      "Baumartengruppe",
      "1987", "2002", "2012"
    ), row.names = c(NA, -9L), class = "data.frame")

  expect_equal(output, reference)
})
