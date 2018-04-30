if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}
testthat::context("fl.stratum.fun.R")
test_that("fl.stratum.fun", {
  result <- fl.stratum.fun(
    list(Wa = c(3, 4, 5), Kreis = 317), get_data("ecken.3"),
    get_data("trakte.3"), get_design("a", 3)
  )
  reference <- 
structure(list(Flaeche = 80440.83, SE_Flaeche = 25707.4137386968), .Names = c("Flaeche", 
"SE_Flaeche"))
      testthat::expect_equal(result, reference)
})

test_that("no match for auswahl", {
  result <- fl.stratum.fun(
    list(Kreis = -9999), get_data("ecken.3"),
    get_data("trakte.3"), get_design("a", 3)
  )
  reference <- structure(list(Flaeche = NA, SE_Flaeche = NA), 
                         .Names = c("Flaeche", "SE_Flaeche"))
      testthat::expect_equal(result, reference)
})


