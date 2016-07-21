#' Testing of BWI3_HR_Landkreis_Funktionen_v1.r
#' 
#' Tests the outputs of the functions of the file 
#' BWI3_HR_Landkreis_Funktionen_v1.r.
#' 
#' @author Franziska Berg
#' @section Version: 25.09.2015
#' @name A Header for
NULL

context("BWI3_HR_Landkreis_Funktionen_v1.R")

test_that("flaechen.kreis.fun.1", {
  
  result <- flaechen.kreis.fun.1(317)
  
  reference <- structure(list(
    Kreis = "Ortenaukreis", 
    KreisCode = 317, 
    Kreisfläche_gesamt_ha = 185073.999, 
    Waldanteil = structure(list(BWI = c(1987, 2002, 2012), 
      Waldanteil = c(45.3, 45.6, 45.5)), 
      .Names = c("BWI", "Waldanteil"), 
      row.names = c(NA, -3L),
      class = "data.frame"), 
    Waldflaeche = structure(list(
       BWI = c(1987, 2002, 2012), 
       Waldfl_GW = c(84220, 84420, 83748), SE_Waldfl_GW = c(5320, 5318, 5303), 
       Waldfl_StW = c(9802, 9802, 9906), SE_Waldfl_StW = c(1820, 1820, 1817), 
        Waldfl_KW = c(32708, 33508, 33619), SE_Waldfl_KW = c(3231, 3273, 3248), 
       Waldfl_OEW = c(42510, 43310, 43525), SE_Waldfl_OEW = c(3736, 3772, 3749), 
       Waldfl_PW = c(41710, 41110, 40223), SE_Waldfl_PW = c(3614, 3584, 3523), 
       Waldfl_GPW = c(1800, 1700, 1801), SE_Waldfl_GPW = c(748, 742, 762), 
       Waldfl_MPW = c(34308, 34408, 34920), SE_Waldfl_MPW = c(3296, 3291, 3251), 
       Waldfl_KPW = c(5601, 5001, 3502), SE_Waldfl_KPW = c(1085, 1009, 768)), 
       .Names = c("BWI", "Waldfl_GW", "SE_Waldfl_GW", "Waldfl_StW", 
                  "SE_Waldfl_StW", "Waldfl_KW", "SE_Waldfl_KW", "Waldfl_OEW", 
                  "SE_Waldfl_OEW",  "Waldfl_PW", "SE_Waldfl_PW", "Waldfl_GPW", 
                  "SE_Waldfl_GPW", "Waldfl_MPW", "SE_Waldfl_MPW", "Waldfl_KPW", 
                  "SE_Waldfl_KPW"), 
       row.names = c(NA, -3L), class = "data.frame"), 
    Waldfl_Proz = structure(list(
      Eigentum = structure(c(7L, 3L, 5L, 6L, 1L, 4L, 2L), 
      .Label = c("GPW", "KPW", "KW", "MPW", "OEW", "PW", "StW(BW)"), 
      class = "factor"), 
      Waldfl_Proz = c(11.8, 40.1, 52, 48, 2.2, 41.7, 4.2), 
      SE_Waldfl_Proz = c(2, 2.9, 3, 3, 0.9, 2.9, 0.9)), 
      .Names = c("Eigentum", "Waldfl_Proz", "SE_Waldfl_Proz"), 
      row.names = c(NA, -7L), class = "data.frame"), 
    begehb_Holzbfl = structure(list(Eigentum = structure(c(2L, 8L, 4L, 6L, 7L, 
                                                           1L, 5L, 3L), 
      .Label = c("GPW", "GW", "KPW", "KW", "MPW", "OEW", "PW", "StW(BW)"), 
      class = "factor"), 
      b_HBF = c(81247, 9505, 32619, 42324, 38922, 1701, 33819, 3402), 
      SE_begehb_Holzbfl = c(5174, 1773, 3164, 3659, 3434, 714, 3179, 721)), 
      .Names = c("Eigentum", "b_HBF", "SE_begehb_Holzbfl"), 
      row.names = c(NA, -8L), class = "data.frame")), 
    .Names = c("Kreis", "KreisCode", "Kreisfläche_gesamt_ha", "Waldanteil", 
               "Waldflaeche", "Waldfl_Proz", "begehb_Holzbfl"))
  
  expect_equal(result, reference)
})

test_that("flaechen.stratum.fun.1", {
  
  result <- flaechen.stratum.fun.1(list(Wa=c(3,5), Begehbar=1))
  
  reference <- structure(list(
    Stratum = structure(list(Wa = c(3, 5), Begehbar = 1), 
                        .Names = c("Wa", "Begehbar")), 
    Waldflaeche = structure(list(BWI = c(1987, 2002, 2012), 
    Waldfl_GW = c(1316118, 1354827, 1363181), 
    SE_Waldfl_GW = c(15923,16085, 16110), 
    Waldfl_StW = c(316376, 326979, 327388), SE_Waldfl_StW = c(9781, 9994, 9967), 
    Waldfl_KW = c(519525, 537930, 545613), SE_Waldfl_KW = c(11558, 11801, 11806), 
    Waldfl_OEW = c(835902, 864909, 873000), 
    SE_Waldfl_OEW = c(14090, 14359, 14336), 
    Waldfl_PW = c(480216, 489918, 490181), 
    SE_Waldfl_PW = c(10659,10746, 10702), 
    Waldfl_GPW = c(146235, 149136, 151287), 
    SE_Waldfl_GPW = c(6718, 6792, 6822), 
    Waldfl_MPW = c(150836, 153237, 185406), 
    SE_Waldfl_MPW = c(6344, 6364, 6665), 
    Waldfl_KPW = c(183144, 187545, 153488), 
    SE_Waldfl_KPW = c(6034, 6095, 5139)), 
    .Names = c("BWI", "Waldfl_GW", "SE_Waldfl_GW", "Waldfl_StW", 
    "SE_Waldfl_StW", "Waldfl_KW", "SE_Waldfl_KW", "Waldfl_OEW", "SE_Waldfl_OEW", 
    "Waldfl_PW", "SE_Waldfl_PW", "Waldfl_GPW", "SE_Waldfl_GPW", "Waldfl_MPW", 
    "SE_Waldfl_MPW", "Waldfl_KPW", "SE_Waldfl_KPW"), 
    row.names = c(NA, -3L), class = "data.frame"), 
    Waldfl_Proz = structure(list(Eigentum = structure(c(7L, 
    3L, 5L, 6L, 1L, 4L, 2L), .Label = c("GPW", "KPW", "KW", "MPW", 
    "OEW", "PW", "StW(BW)"), class = "factor"), Waldfl_Proz = c(24, 
    40, 64, 36, 11.1, 13.6, 11.3), SE_Waldfl_Proz = c(0.7, 0.7, 0.7, 
    0.7, 0.5, 0.5, 0.4)), .Names = c("Eigentum", "Waldfl_Proz", "SE_Waldfl_Proz"
    ), row.names = c(NA, -7L), class = "data.frame"), 
    begehb_Holzbfl = structure(list(
    Eigentum = structure(c(2L, 8L, 4L, 6L, 7L, 1L, 5L, 3L), .Label = c("GPW", 
    "GW", "KPW", "KW", "MPW", "OEW", "PW", "StW(BW)"), class = "factor"), 
    b_HBF = c(1323958, 310978, 527802, 844684, 479275, 148185, 
    180904, 150186), SE_begehb_Holzbfl = c(15761, 9650, 11504, 
    13981, 10515, 6695, 6543, 5062)), .Names = c("Eigentum", "b_HBF", 
    "SE_begehb_Holzbfl"), row.names = c(NA, -8L), class = "data.frame")), 
    .Names = c("Stratum", "Waldflaeche", "Waldfl_Proz", "begehb_Holzbfl"))
  
  expect_equal(result, reference)
})