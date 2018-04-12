#' Testing of bits_and_pieces.R
#'
#' Tests the outputs of the functions of the file bits_and_pieces.R.
#'
#' @author Franziska Berg
#' @section Version: 20.10.2015
#' @name A Header for
NULL

context("bits_and_pieces.R")

test_that("add_colSums_to_data_frame", {
  output <- add_colSums_to_data_frame(head(get_data("baeume.3")[, 0:20]))

  reference <- structure(list(
    TNr = c(154, 154, 154, 154, 154, 154, 924), ENr = c(1, 1, 1, 1, 1, 1, 6),
    STP = c(0, 0, 0, 0, 0, 0, 0), BNr = c(2, 3, 4, 5, 6, 7, 27),
    Pk = c(1, 1, 1, 1, 1, 1, 6), PL = c(10, 10, 10, 10, 10, 10, 60),
    WZ1 = c(-1, -1, -1, -1, -1, -1, -6),
    BA = c(100, 100, 100, 100, 100, 100, 600), Azi = c(
      240, 240, 284, 331, 342,
      367, 1804
    ), Entf = c(
      4.73000002, 6.98000002, 7.53000021, 3.22000003,
      8.92000008, 9.56000042, 40.94000078
    ), Du = c(
      55.5, 58, 48.7999992,
      46.2000008, 44.2000008, 52.7999992, 305.5
    ), aMh = c(
      1.29999995,
      1.29999995, 1.29999995, 1.29999995, 1.29999995, 1.5, 7.99999975
    ), Bs1 = c(0, 0, 0, 0, 0, 0, 0), Bs2 = c(1, 1, 1, 1, 1, 1, 6),
    Alt1 = c(114, 114, 114, 114, 114, 114, 684), Alt2 = c(
      124,
      124, 124, 124, 124, 124, 744
    ), BHD1 = c(
      52.2000008, 54.7999992,
      47.2999992, 43.7000008, 42.2999992, 50.7999992, 291.0999984
    ), BHD2 = c(
      55.5, 58, 48.7999992, 46.2000008, 44.2000008,
      53.7999992, 306.5
    ), D031 = c(
      42.0999985, 44.2999992, 38.0999985,
      35.2000008, 34, 38.2999992, 231.9999962
    ), D032 = c(
      44.7999992,
      46.9000015, 39.2999992, 37.2000008, 35.5999985, 41, 244.7999992
    )
  ), .Names = c(
    "TNr", "ENr", "STP", "BNr", "Pk", "PL", "WZ1",
    "BA", "Azi", "Entf", "Du", "aMh", "Bs1", "Bs2", "Alt1", "Alt2",
    "BHD1", "BHD2", "D031", "D032"
  ), row.names = c(
    "1", "2", "3",
    "4", "5", "6", "Summe"
  ), class = "data.frame")

  expect_equal(output, reference)
})

context("as_percent_and_round")
test_that("correct input", {
  expect_equal(as_percent_and_round(0.2589), 25.9)
  expect_equal(as_percent_and_round(c(0.2589, 0.4896)), c(25.9, 49.0))
})

test_that("set_to_limit", {
  output <- set_to_limit(head(get_data("baeume.3")[0:20]), BHD1 > 40)

  reference <- structure(list(
    TNr = c(154L, 154L, 154L, 154L, 154L, 154L), ENr = c(
      1L,
      1L, 1L, 1L, 1L, 1L
    ), STP = c(0L, 0L, 0L, 0L, 0L, 0L), BNr = 2:7,
    Pk = c(1L, 1L, 1L, 1L, 1L, 1L), PL = c(
      10, 10, 10, 10, 10,
      10
    ), WZ1 = c(-1L, -1L, -1L, -1L, -1L, -1L), BA = c(
      100L,
      100L, 100L, 100L, 100L, 100L
    ), Azi = c(
      240L, 240L, 284L,
      331L, 342L, 367L
    ), Entf = c(
      4.73000002, 6.98000002, 7.53000021,
      3.22000003, 8.92000008, 9.56000042
    ), Du = c(
      55.5, 58, 48.7999992,
      46.2000008, 44.2000008, 52.7999992
    ), aMh = c(
      1.29999995,
      1.29999995, 1.29999995, 1.29999995, 1.29999995, 1.5
    ), Bs1 = c(
      0L,
      0L, 0L, 0L, 0L, 0L
    ), Bs2 = c(1L, 1L, 1L, 1L, 1L, 1L), Alt1 = c(
      114L,
      114L, 114L, 114L, 114L, 114L
    ), Alt2 = c(
      124L, 124L, 124L,
      124L, 124L, 124L
    ), BHD1 = c(40, 40, 40, 40, 40, 40), BHD2 = c(
      55.5,
      58, 48.7999992, 46.2000008, 44.2000008, 53.7999992
    ), D031 = c(
      42.0999985,
      44.2999992, 38.0999985, 35.2000008, 34, 38.2999992
    ), D032 = c(
      44.7999992,
      46.9000015, 39.2999992, 37.2000008, 35.5999985, 41
    )
  ), .Names = c(
    "TNr",
    "ENr", "STP", "BNr", "Pk", "PL", "WZ1", "BA", "Azi", "Entf",
    "Du", "aMh", "Bs1", "Bs2", "Alt1", "Alt2", "BHD1", "BHD2", "D031",
    "D032"
  ), row.names = c(NA, 6L), class = "data.frame")

  expect_equal(output, reference)
})

test_that("get_period", {
  expect_equal(get_period(1), "1987 - 2002")
  expect_equal(get_period(2), "2002 - 2012")
})
