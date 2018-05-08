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

