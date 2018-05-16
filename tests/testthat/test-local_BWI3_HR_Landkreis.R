if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}
testthat::context("BWI3_HR_Landkreis_Funktionen_v1.R")

testthat::test_that("flaechen.kreis.fun.1", {
  result <- flaechen.kreis.fun.1(127)
  reference <- 
structure(list(Kreis = "Schwäbisch Hall", KreisCode = 127, Kreisfläche_gesamt_ha = 148406.006, 
    Waldanteil = structure(list(BWI = c(1987, 2002, 2012), Waldanteil = c(18.1, 
    18.1, 18.1)), .Names = c("BWI", "Waldanteil"), row.names = c(NA, 
    -3L), class = "data.frame"), Waldflaeche = structure(list(
        BWI = c(1987, 2002, 2012), Waldfl_GW = c(26814, 26814, 
        26814), SE_Waldfl_GW = c(15324, 15324, 15324), Waldfl_StW = c(17876, 
        17876, 17876), SE_Waldfl_StW = c(12576, 12576, 12576), 
        Waldfl_KW = c(NA_real_, NA_real_, NA_real_), SE_Waldfl_KW = c(NA_real_, 
        NA_real_, NA_real_), Waldfl_OEW = c(17876, 17876, 17876
        ), SE_Waldfl_OEW = c(12576, 12576, 12576), Waldfl_PW = c(8938, 
        8938, 8938), SE_Waldfl_PW = c(8938, 8938, 8938), Waldfl_GPW = c(NA_real_, 
        NA_real_, NA_real_), SE_Waldfl_GPW = c(NA_real_, NA_real_, 
        NA_real_), Waldfl_MPW = c(8938, 8938, 8938), SE_Waldfl_MPW = c(8938, 
        8938, 8938), Waldfl_KPW = c(NA_real_, NA_real_, NA_real_
        ), SE_Waldfl_KPW = c(NA_real_, NA_real_, NA_real_)), .Names = c("BWI", 
    "Waldfl_GW", "SE_Waldfl_GW", "Waldfl_StW", "SE_Waldfl_StW", 
    "Waldfl_KW", "SE_Waldfl_KW", "Waldfl_OEW", "SE_Waldfl_OEW", 
    "Waldfl_PW", "SE_Waldfl_PW", "Waldfl_GPW", "SE_Waldfl_GPW", 
    "Waldfl_MPW", "SE_Waldfl_MPW", "Waldfl_KPW", "SE_Waldfl_KPW"
    ), row.names = c(NA, -3L), class = "data.frame"), Waldfl_Proz = structure(list(
        Eigentum = structure(c(7L, 3L, 5L, 6L, 1L, 4L, 2L), .Label = c("GPW", 
        "KPW", "KW", "MPW", "OEW", "PW", "StW(BW)"), class = "factor"), 
        Waldfl_Proz = c(66.7, NA, 66.7, 33.3, NA, 33.3, NA), 
        SE_Waldfl_Proz = c(27.4, NA, 27.4, 27.4, NA, 27.4, NA
        )), .Names = c("Eigentum", "Waldfl_Proz", "SE_Waldfl_Proz"
    ), row.names = c(NA, -7L), class = "data.frame"), begehb_Holzbfl = structure(list(
        Eigentum = structure(c(2L, 8L, 4L, 6L, 7L, 1L, 5L, 3L
        ), .Label = c("GPW", "GW", "KPW", "KW", "MPW", "OEW", 
        "PW", "StW(BW)"), class = "factor"), b_HBF = c(26814, 
        17876, NA, 17876, 8938, NA, 8938, NA), SE_begehb_Holzbfl = c(15324, 
        12576, NA, 12576, 8938, NA, 8938, NA)), .Names = c("Eigentum", 
    "b_HBF", "SE_begehb_Holzbfl"), row.names = c(NA, -8L), class = "data.frame")), .Names = c("Kreis", 
"KreisCode", "Kreisfläche_gesamt_ha", "Waldanteil", "Waldflaeche", 
"Waldfl_Proz", "begehb_Holzbfl"))
  testthat::expect_equal(result, reference)

 })

test_that("flaechen.stratum.fun.1", {
  result <- flaechen.stratum.fun.1(list(Wa = c(3, 5), Begehbar = 1))
  reference <- 
structure(list(Stratum = structure(list(Wa = c(3, 5), Begehbar = 1), .Names = c("Wa", 
"Begehbar")), Waldflaeche = structure(list(BWI = c(1987, 2002, 
2012), Waldfl_GW = c(866977, 911667, 929538), SE_Waldfl_GW = c(29671, 
21930, 17603), Waldfl_StW = c(214510, 214510, 223447), SE_Waldfl_StW = c(42363, 
42363, 42846), Waldfl_KW = c(321765, 339640, 357515), SE_Waldfl_KW = c(43118, 
43602, 44007), Waldfl_OEW = c(536274, 554150, 580962), SE_Waldfl_OEW = c(47533, 
47158, 46460), Waldfl_PW = c(330703, 357516, 348577), SE_Waldfl_PW = c(43370, 
44007, 43814), Waldfl_GPW = c(53627, 62565, 53627), SE_Waldfl_GPW = c(21333, 
22920, 21333), Waldfl_MPW = c(143007, 143007, 169820), SE_Waldfl_MPW = c(32932, 
32932, 35240), Waldfl_KPW = c(134069, 151944, 125130), SE_Waldfl_KPW = c(32075, 
33743, 31169)), .Names = c("BWI", "Waldfl_GW", "SE_Waldfl_GW", 
"Waldfl_StW", "SE_Waldfl_StW", "Waldfl_KW", "SE_Waldfl_KW", "Waldfl_OEW", 
"SE_Waldfl_OEW", "Waldfl_PW", "SE_Waldfl_PW", "Waldfl_GPW", "SE_Waldfl_GPW", 
"Waldfl_MPW", "SE_Waldfl_MPW", "Waldfl_KPW", "SE_Waldfl_KPW"), row.names = c(NA, 
-3L), class = "data.frame"), Waldfl_Proz = structure(list(Eigentum = structure(c(7L, 
3L, 5L, 6L, 1L, 4L, 2L), .Label = c("GPW", "KPW", "KW", "MPW", 
"OEW", "PW", "StW(BW)"), class = "factor"), Waldfl_Proz = c(22, 
40, 62, 38, 6, 18, 14), SE_Waldfl_Proz = c(4.3, 5, 4.9, 4.9, 
2.4, 3.8, 3.5)), .Names = c("Eigentum", "Waldfl_Proz", "SE_Waldfl_Proz"
), row.names = c(NA, -7L), class = "data.frame"), begehb_Holzbfl = structure(list(
    Eigentum = structure(c(2L, 8L, 4L, 6L, 7L, 1L, 5L, 3L), .Label = c("GPW", 
    "GW", "KPW", "KW", "MPW", "OEW", "PW", "StW(BW)"), class = "factor"), 
    b_HBF = c(902725, 223447, 348577, 572024, 330701, 53627, 
    160882, 116192), SE_begehb_Holzbfl = c(23750, 42846, 43814, 
    46711, 43370, 21333, 34511, 30210)), .Names = c("Eigentum", 
"b_HBF", "SE_begehb_Holzbfl"), row.names = c(NA, -8L), class = "data.frame")), .Names = c("Stratum", 
"Waldflaeche", "Waldfl_Proz", "begehb_Holzbfl"))
  testthat::expect_equal(result, reference)
 })
