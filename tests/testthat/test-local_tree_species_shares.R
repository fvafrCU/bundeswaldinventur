if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}
testthat::context("tree_species_shares.R")

testthat::test_that("tree_species_percent", {
  FVBN <- FVBN.bagrupp.akl.dkl.stratum.fun.2c
  fvbn.3 <- FVBN(
    get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups(),
    list(A.ob = 200, A.b = 100),
    list(D.unt = 0, D.ob = 50, D.b = 10, Ndh = FALSE),
    list(Wa = c(3, 5), Begehbar = 1)
  )
  result <- tree_species_percent(statistics_list = fvbn.3)
reference <-
structure(c(0, 0, 0, 0, 0, 0, 0, 20.19, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 53.03, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 26.78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0), .Dim = c(9L, 3L, 3L), .Dimnames = list(BAGR = c("FI", 
"TA", "DGL", "KI", "LAE", "BU", "EI", "ALH", "ALN"), AKL = c("1-100", 
"101-200", ">200"), DKL = c("0-9.9", "10-19.9", ">=50")))
  testthat::expect_equal(result, reference)

  fvbn.3 <- FVBN(
    get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups("bc"),
    list(A.ob = 200, A.b = 200),
    list(D.unt = 0, D.ob = 200, D.b = 200, Ndh = FALSE),
    list(Wa = c(3, 5), Begehbar = 1)
  )
  result <- tree_species_percent(statistics_list = fvbn.3)
reference <-
c(Nadel = 0, Laub = 100)
  testthat::expect_equal(result, reference)
 })

testthat::test_that("tree_species_percent", {
if (interactive()) {
  FVBN <- FVBN.bagrupp.akl.dkl.stratum.fun.2c
  fvbn.3 <- FVBN(
    get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups(),
    list(A.ob = 200, A.b = 200),
    list(D.unt = 0, D.ob = 200, D.b = 200, Ndh = FALSE),
    list(Wa = c(3, 5), Begehbar = 1)
  )
  fvbn.2 <- FVBN(
    get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups(),
    list(A.ob = 200, A.b = 200),
    list(D.unt = 0, D.ob = 200, D.b = 200, Ndh = FALSE),
    list(Wa = c(3, 5), Begehbar = 1)
  )
  fvbn.1 <- FVBN(
    get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups(),
    list(A.ob = 200, A.b = 200),
    list(D.unt = 0, D.ob = 200, D.b = 200, Ndh = FALSE),
    list(Wa = c(3, 5), Begehbar = 1)
  )
  result <- melt_tree_species_percent(statistics_list = "fvbn", bagr =  get_bwi_species_groups())
reference <-
structure(list(bwi = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
    group = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 
    2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L), .Label = c("FI", "TA", "DGL", "KI", "LAE", "BU", 
    "EI", "ALH", "ALN"), class = "factor"), value = c(0, 0, 0, 
    0, 0, 26.78, 0, 73.22, 0, 0, 0, 0, 0, 0, 26.78, 0, 73.22, 
    0, 0, 0, 0, 0, 0, 26.78, 0, 73.22, 0)), row.names = c(NA, 
-27L), class = "data.frame")
  testthat::expect_equal(result, reference)
}
 })
