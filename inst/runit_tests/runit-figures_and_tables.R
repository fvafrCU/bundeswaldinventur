if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}

test_melt <- function() {
  FVBN <- FVBN.bagrupp.akl.dkl.stratum.fun.2a
assign('fvbn.3', 
       FVBN(
            get_data("baeume.3"), get_data("ecken.3"), 
                      get_data("trakte.3"), get_design("a", 3), 2, get_bwi_species_groups(),
            list(A.ob = 160, A.b = 40), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(3, 5), Begehbar = 1)
                      ), envir = .GlobalEnv)

assign("fvbn.2", fvbn.3, envir = .GlobalEnv)
assign("fvbn.1", fvbn.3, envir = .GlobalEnv)

    RUnit::checkException(melt_species_attribute_group("fvbn", "MEEEBOOB", "BAF", "AKL"))

  result <- melt_species_attribute_group("fvbn", "FI", "BAF", "AKL")
  
  reference <-
  structure(list(prediction = c(133125.998502, 101198.139288, 66412.843035, 
  27828.952032, 1337.105352, 133125.998502, 101198.139288, 66412.843035, 
  27828.952032, 1337.105352, 133125.998502, 101198.139288, 66412.843035, 
  27828.952032, 1337.105352), standard_error = c(34240.4144726496, 
  25152.0745008729, 22137.9649948745, 13102.7582575256, 1337.105352, 
  34240.4144726496, 25152.0745008729, 22137.9649948745, 13102.7582575256, 
  1337.105352, 34240.4144726496, 25152.0745008729, 22137.9649948745, 
  13102.7582575256, 1337.105352), abbreviation = structure(c(1L, 
  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "BAF", class = "factor"), 
      bwi = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
      3L, 3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
      group = structure(c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 
      1L, 2L, 3L, 4L, 5L), class = "factor", .Label = c("1-40", 
      "41-80", "81-120", "121-160", ">160")), species = c("Fichte", 
      "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", 
      "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", 
      "Fichte", "Fichte"), grouping_variable = c("AKL", "AKL", 
      "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", 
      "AKL", "AKL", "AKL", "AKL"), ownership = c("fvbn", "fvbn", 
      "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", 
      "fvbn", "fvbn", "fvbn", "fvbn", "fvbn")), .Names = c("prediction", 
  "standard_error", "abbreviation", "bwi", "group", "species", 
  "grouping_variable", "ownership"), row.names = c(NA, 15L), class = "data.frame")
  
  
  RUnit::checkEquals(reference, result)
  
  result <- melt_species_attribute_group("fvbn", "FI", "N/ha", "AKL")
  
  reference <-
structure(list(prediction = c(15498.5390923623, 574.801225994489, 
282.646128591616, 248.693916122816, 95.6629686497326, 15498.5390923623, 
574.801225994489, 282.646128591616, 248.693916122816, 95.6629686497326, 
15498.5390923623, 574.801225994489, 282.646128591616, 248.693916122816, 
95.6629686497326), standard_error = c(5177.12796759455, 89.0636806836254, 
45.1744743217733, 46.6621558642141, NaN, 5177.12796759455, 89.0636806836254, 
45.1744743217733, 46.6621558642141, NaN, 5177.12796759455, 89.0636806836254, 
45.1744743217733, 46.6621558642141, NaN), abbreviation = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "N/ha", class = "factor"), 
    bwi = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
    3L, 3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
    group = structure(c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 
    1L, 2L, 3L, 4L, 5L), class = "factor", .Label = c("1-40", 
    "41-80", "81-120", "121-160", ">160")), species = c("Fichte", 
    "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", 
    "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", 
    "Fichte", "Fichte"), grouping_variable = c("AKL", "AKL", 
    "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", 
    "AKL", "AKL", "AKL", "AKL"), ownership = c("fvbn", "fvbn", 
    "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", "fvbn", 
    "fvbn", "fvbn", "fvbn", "fvbn", "fvbn")), .Names = c("prediction", 
"standard_error", "abbreviation", "bwi", "group", "species", 
"grouping_variable", "ownership"), row.names = c(NA, 15L), class = "data.frame")
  
  
      RUnit::checkEquals(reference, result)

}

