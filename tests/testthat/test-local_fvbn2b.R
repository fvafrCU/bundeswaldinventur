testthat::context("fvbn2b")
testthat::test_that("FVBN.bagrupp.akl.dkl.stratum.fun.2b", {
  FVBN <- FVBN.bagrupp.akl.dkl.stratum.fun.2b
  result <- FVBN(
    get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups("bc"),
    list(A.ob = 200, A.b = 60),
    list(
      D.unt = 0, D.ob = 50, D.b = 25,
      Ndh = T
    ),
    list(Wa = c(3, 5), Begehbar = 1)
  )
  reference <- 
structure(list(Log = structure(list(Datum = structure(1524828672.60354, class = c("POSIXct", 
"POSIXt")), Version.baeume.b = NA_character_), .Names = c("Datum", 
"Version.baeume.b")), Stratum = structure(list(Wa = c(3, 5), 
    Begehbar = 1), .Names = c("Wa", "Begehbar")), nTE = 97L, 
    HBF = 902724.87, se.HBF = 23749.5302613181, BL = 0, se.BL = 0, 
    iBL = 0, se.iBL = 0, LK = 1, se.LK = 0, Attribute1 = c("BAF", 
    "V_DhmR", "V_DhmR_HB", "oiB", "N", "N_Dh"), Attribute2 = c("BA_Proz", 
    "V_DhmR/ha", "V_DhmR_HB/ha", "oiB/ha", "N/ha", "N_Dh/ha"), 
    Größen = c("Wert", "Standardfehler"), BAGR = c("Nadel", 
    "Laub"), AKL = c("1-60", "61-120", "121-180", ">200"), DKL = c("0-6.9", 
    "0-24.9", ">=50"), T.FVBN.Bagr.Akl.Dkl = structure(c(0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94340.6067294845, 
    28450121.8057453, 0, 0, 0, 0, 94340.6067294845, 28450121.8057453, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 6544.308414, 890454.936067471, 
    890454.936067471, 821302.940164437, 17315119.6925016, 17315119.6925016, 
    6544.308414, 890454.936067471, 890454.936067471, 821302.940164437, 
    17315119.6925016, 17315119.6925016, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 2393.561586, 1703037.63076889, 1703037.63076889, 
    1173242.97147497, 688454.265570117, 688454.265570117, 2393.561586, 
    1703037.63076889, 1703037.63076889, 1173242.97147497, 688454.265570117, 
    688454.265570117, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(6L, 
    2L, 2L, 4L, 3L)), FVBN.ha.Bagr.Akl.Dkl = structure(c(0, 0, 
    0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, NaN, NaN, Inf, Inf, 
    NaN, 0, NaN, NaN, NaN, NaN, NaN, 0, 0, 0, 0, 0, NA, NA, NA, 
    NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 
    0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 
    NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, 
    NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 
    0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0.724950495049505, 
    136.065551886667, 136.065551886667, 125.498813351683, 2645.82880223983, 
    2645.82880223983, 0.725273811922906, 2.03132467589353e-06, 
    2.03132467589353e-06, NaN, NaN, NaN, 0, 0, 0, 0, 0, NA, NA, 
    NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, 
    NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 
    0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, 
    NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 
    0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 
    NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, 
    NA, NA, NA, 0.265148514851485, 711.507755108539, 711.507755108539, 
    490.166193482254, 287.627554518297, 287.627554518297, 0.265266767048558, 
    1.42440400939131e-05, 1.42440400939131e-05, 0, 1.14507157791607e-06, 
    1.14507157791607e-06, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, 
    NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 
    0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, 
    NA, NA, NA, NA, NA), .Dim = c(6L, 2L, 2L, 4L, 3L)), nT.Bagr.Akl.Dkl = structure(c(0, 
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
    0, 0, 0, 0), .Dim = c(2L, 4L, 3L))), .Names = c("Log", "Stratum", 
"nTE", "HBF", "se.HBF", "BL", "se.BL", "iBL", "se.iBL", "LK", 
"se.LK", "Attribute1", "Attribute2", "Größen", "BAGR", "AKL", 
"DKL", "T.FVBN.Bagr.Akl.Dkl", "FVBN.ha.Bagr.Akl.Dkl", "nT.Bagr.Akl.Dkl"
))
     

  result[["Log"]] <- NULL
  reference[["Log"]] <- NULL
  testthat::expect_equal(result, reference)

  # NOTE: Es geht um unbesetzte
  # Klassen.
  A.klass <- list(A.ob = 100, A.b = 50)
  result <- FVBN(
    get_data("baeume.3")[1:10, ], get_data("ecken.3"), get_data("trakte.3"), get_design("a", 3), 2,
    get_bwi_species_groups("bc"),
    A.klass,
    list(
      D.unt = 10, D.ob = 50, D.b = 25,
      Ndh = T
    ),
    list(
      Wa = c(3, 5), Begehbar = 1,
      BTyp = c(
        110, 117, 170, 171, 410, 417, 470,
        471
      )
    )
  )
  reference <- 
structure(list(Log = structure(list(Datum = structure(1524828718.13046, class = c("POSIXct", 
"POSIXt")), Version.baeume.b = NA_character_), .Names = c("Datum", 
"Version.baeume.b")), Stratum = structure(list(Wa = c(3, 5), 
    Begehbar = 1, BTyp = c(110, 117, 170, 171, 410, 417, 470, 
    471)), .Names = c("Wa", "Begehbar", "BTyp")), nTE = 22L, 
    HBF = 196633.14, se.HBF = 37211.3201732967, BL = 0, se.BL = 0, 
    iBL = 0, se.iBL = 0, LK = 1, se.LK = 0, Attribute1 = c("BAF", 
    "V_DhmR", "V_DhmR_HB", "oiB", "N", "N_Dh"), Attribute2 = c("BA_Proz", 
    "V_DhmR/ha", "V_DhmR_HB/ha", "oiB/ha", "N/ha", "N_Dh/ha"), 
    Größen = c("Wert", "Standardfehler"), BAGR = c("Nadel", 
    "Laub"), AKL = c("1-50", "51-100", ">100"), DKL = c("0-6.9", 
    ">=50", "25-49.9"), T.FVBN.Bagr.Akl.Dkl = structure(c(0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(6L, 2L, 2L, 3L, 2L
    )), FVBN.ha.Bagr.Akl.Dkl = structure(c(0, 0, 0, 0, 0, NA, 
    NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, 
    NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 
    0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, 
    NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, 
    NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 
    0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, 
    NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 
    0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 
    NA, NA, NA, NA, NA, NA, NA), .Dim = c(6L, 2L, 2L, 3L, 2L)), 
    nT.Bagr.Akl.Dkl = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0), .Dim = c(2L, 3L, 2L))), .Names = c("Log", "Stratum", 
"nTE", "HBF", "se.HBF", "BL", "se.BL", "iBL", "se.iBL", "LK", 
"se.LK", "Attribute1", "Attribute2", "Größen", "BAGR", "AKL", 
"DKL", "T.FVBN.Bagr.Akl.Dkl", "FVBN.ha.Bagr.Akl.Dkl", "nT.Bagr.Akl.Dkl"
))
  result[["Log"]] <- NULL
  reference[["Log"]] <- NULL
  testthat::expect_equal(result, reference)



})
