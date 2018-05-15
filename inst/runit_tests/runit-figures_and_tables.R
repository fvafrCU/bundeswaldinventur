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
  molten <- result
  result <- plot_species_attribute_group(molten)[["data"]]
  reference <- structure(list(prediction = c(15498.5390923623, 574.801225994489,
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

test_ownership_groups <- function() {


      assign('fvbn.bagr.gw.3', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.3"), get_data("ecken.3"), 
                      get_data("trakte.3"), get_design("a", 3), 2, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1))
	   )
    assign('fvbn.bagr.gw.2', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.2"), get_data("ecken.2"), 
                      get_data("trakte.2"), get_design("a", 2), 2, get_bwi_species_groups(),

					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1))
	   )
    assign('fvbn.bagr.gw.1', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.1"), get_data("ecken.1"), 
                      get_data("trakte.1"), get_design("a", 1), 1, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1))
	   )
  result <- reassemble_by_group("gw", "BAF", data_prefix = "fvbn.bagr")
reference <-
structure(list(bwi = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
    Baumartengruppe = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 
    5L, 6L, 7L, 8L, 9L), .Label = c("FI", "TA", "DGL", "KI", 
    "LAE", "BU", "EI", "ALH", "ALN"), class = "factor"), prediction = c(401259.103910681, 
    60966.931352984, 9223.15986702128, 44679.7533438112, 29988.181991633, 
    166402.405688824, 78250.2106277952, 59152.735807141, 17054.544910109, 
    376222.326820361, 88341.5405829897, 9214.33762886598, 25091.5627971649, 
    47369.9883162371, 187791.88661134, 80933.2131293814, 57783.1112706185, 
    21042.7828430412, 329903.038209, 73741.896435, 26295.21354, 
    26857.405563, 24734.661438, 190524.999642, 77120.411295, 
    105738.577248, 47808.66663), standard_error = c(44767.1164429632, 
    18786.423689262, 9223.15986702128, 15781.2996788884, 12155.3505360441, 
    31424.1393637099, 20762.3913432623, 19171.2636114755, 9254.65285554584, 
    44974.0383091226, 23482.7084861681, 9214.33762886598, 11343.6670859968, 
    17482.8499642516, 31388.1799403295, 21434.4890768358, 16675.8007261038, 
    9497.12951457268, 41954.8149210379, 20128.4249748835, 14103.3263030828, 
    11641.4403122435, 9550.961803339, 29479.9064238981, 19222.3241459563, 
    22734.1155245227, 15814.1714066418), Eigentumsart = c("Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald"), abbreviation = c("Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche")), .Names = c("bwi", 
"Baumartengruppe", "prediction", "standard_error", "Eigentumsart", 
"abbreviation"), row.names = c(NA, -27L), class = "data.frame")

  RUnit::checkEquals(reference, result)
  result <- reassemble_by_group("gw", "N/ha", data_prefix = "fvbn.bagr")
reference <-
structure(list(bwi = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
    Baumartengruppe = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 
    5L, 6L, 7L, 8L, 9L), .Label = c("FI", "TA", "DGL", "KI", 
    "LAE", "BU", "EI", "ALH", "ALN"), class = "factor"), prediction = c(7931.47076871609, 
    3115.21733556322, 92.9304927694845, 888.095234709471, 368.549251433641, 
    7187.5661724001, 1836.49652972868, 47800.3278471131, 17684.3510876721, 
    8279.9711963411, 3341.82825037951, 3056.144409606, 745.378542692887, 
    152.38797883036, 8352.93039510643, 1184.21337967457, 75187.2940755012, 
    19406.2708243732, 6508.72284682084, 4996.42983018363, 2186.31753290619, 
    1101.42360308163, 1030.35325182482, 12922.0023398628, 6243.25900055282, 
    35386.5662397738, 15297.1341704431), standard_error = c(3564.22852575939, 
    1328.55813337385, NaN, 600.889447484873, 71.1716577863641, 
    1958.05606844104, 928.099760239073, 12146.4377552185, 4562.3051316613, 
    3503.28574613098, 1825.67413576913, 4298.96975929466, 421.688181555584, 
    32.273848961113, 1874.63088036363, 537.882412993721, 24285.6109046948, 
    10869.48993408, 1720.50543798547, 1784.08662855439, 1218.00420532482, 
    563.055639349537, 740.271795525581, 4087.66079643678, 3680.62085674142, 
    12125.6105418612, 6815.46857857688), Eigentumsart = c("Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald"), abbreviation = c("Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar", "Stammzahl je Hektar", "Stammzahl je Hektar", 
    "Stammzahl je Hektar")), .Names = c("bwi", "Baumartengruppe", 
"prediction", "standard_error", "Eigentumsart", "abbreviation"
), row.names = c(NA, -27L), class = "data.frame")
  RUnit::checkEquals(reference, result)



    assign('fvbn.bagr.stw.3', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.3"), get_data("ecken.3"), 
                      get_data("trakte.3"), get_design("a", 3), 2, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1, EigArt = "StW"))
	   )
    assign('fvbn.bagr.stw.2', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.2"), get_data("ecken.2"), 
                      get_data("trakte.2"), get_design("a", 2), 2, get_bwi_species_groups(),

					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
	   )
    assign('fvbn.bagr.stw.1', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.1"), get_data("ecken.1"), 
                      get_data("trakte.1"), get_design("a", 1), 1, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
	   )
    assign('fvbn.bagr.kw.3', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.3"), get_data("ecken.3"), 
                      get_data("trakte.3"), get_design("a", 3), 2, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1, EigArt = "KW"))
	   )
    assign('fvbn.bagr.kw.2', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.2"), get_data("ecken.2"), 
                      get_data("trakte.2"), get_design("a", 2), 2, get_bwi_species_groups(),

					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
	   )
    assign('fvbn.bagr.kw.1', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.1"), get_data("ecken.1"), 
                      get_data("trakte.1"), get_design("a", 1), 1, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
	   )

    assign('fvbn.bagr.pw.3', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.3"), get_data("ecken.3"), 
                      get_data("trakte.3"), get_design("a", 3), 2, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1, EigArt = "PW"))
	   )
    assign('fvbn.bagr.pw.2', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.2"), get_data("ecken.2"), 
                      get_data("trakte.2"), get_design("a", 2), 2, get_bwi_species_groups(),

					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
	   )
    assign('fvbn.bagr.pw.1', envir = .GlobalEnv, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
            get_data("baeume.1"), get_data("ecken.1"), 
                      get_data("trakte.1"), get_design("a", 1), 1, get_bwi_species_groups(),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
	   )


   result <- all_data_by_group(abbreviation = "BAF", data_prefix = "fvbn.bagr")
   reference <- 
structure(list(bwi = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
    Baumartengruppe = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 
    5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 
    2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 
    5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 
    2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L), .Label = c("FI", 
    "TA", "DGL", "KI", "LAE", "BU", "EI", "ALH", "ALN"), class = "factor"), 
    prediction = c(401259.103910681, 60966.931352984, 9223.15986702128, 
    44679.7533438112, 29988.181991633, 166402.405688824, 78250.2106277952, 
    59152.735807141, 17054.544910109, 376222.326820361, 88341.5405829897, 
    9214.33762886598, 25091.5627971649, 47369.9883162371, 187791.88661134, 
    80933.2131293814, 57783.1112706185, 21042.7828430412, 329903.038209, 
    73741.896435, 26295.21354, 26857.405563, 24734.661438, 190524.999642, 
    77120.411295, 105738.577248, 47808.66663, 119142.631989818, 
    2049.54344345455, 9750.44454545454, 12743.8310209091, 9997.13079245455, 
    41014.269936, 13475.1143618182, 6336.81391009091, 0, 103865.635476, 
    2331.62804347826, 9326.51217391304, 5379.53222191304, 10032.5291454783, 
    45574.0017378261, 14784.387098087, 14197.7494823478, 9017.80462095652, 
    93526.765467, 1123.490259, 6285.110184, 3951.432327, 5820.340944, 
    44424.789048, 20321.141232, 39970.15464, 8023.525899, 97933.54626825, 
    12868.7992185, 0, 13413.11778525, 19896.67588575, 98695.05598725, 
    52461.941862, 19979.7984255, 6515.7345675, 91560.5202905, 
    22525.4386191042, 0, 10051.5211194583, 28157.4872375833, 
    110686.15268925, 44616.3707961042, 18664.4860267917, 4440.60072120833, 
    85693.616199, 17461.022832, 8937.87, 13968.103236, 13607.907075, 
    118183.667436, 41999.05113, 29632.614198, 19093.077894, 186748.664132521, 
    45565.303469875, 0, 18708.5797037917, 0, 25657.9269226458, 
    11321.9701880208, 32410.6898315417, 10289.4432516042, 180951.630104546, 
    63159.6527863224, 0, 9651.03491445395, 9173.11559210526, 
    31598.631280125, 21477.0155357961, 24922.4377521908, 7644.87453446053, 
    150682.656543, 55157.383344, 11072.233356, 8937.87, 5306.413419, 
    27916.543158, 14800.218933, 36135.80841, 20692.062837), standard_error = c(44767.1164429632, 
    18786.423689262, 9223.15986702128, 15781.2996788884, 12155.3505360441, 
    31424.1393637099, 20762.3913432623, 19171.2636114755, 9254.65285554584, 
    44974.0383091226, 23482.7084861681, 9214.33762886598, 11343.6670859968, 
    17482.8499642516, 31388.1799403295, 21434.4890768358, 16675.8007261038, 
    9497.12951457268, 41954.8149210379, 20128.4249748835, 14103.3263030828, 
    11641.4403122435, 9550.961803339, 29479.9064238981, 19222.3241459563, 
    22734.1155245227, 15814.1714066418, 30741.6061200135, 2049.54344345455, 
    9750.44454545454, 7714.33216420427, 7186.0173541382, 20594.6964349207, 
    9480.09964899884, 5222.25299081639, 0, 28274.573771926, 2331.62804347826, 
    9326.51217391304, 3803.71764116812, 7134.93152763837, 19919.8607645116, 
    9270.88523498641, 7966.73500683345, 7288.46955398108, 25909.3109510919, 
    1123.490259, 6285.110184, 2357.08189434446, 4097.50690212164, 
    16053.8794627622, 10090.1674213453, 15254.1753378617, 6860.66746250023, 
    24618.9545323967, 6958.42117259285, 0, 6174.39910524619, 
    9952.24004520023, 24046.760630201, 17127.8704994839, 11169.6394985728, 
    3934.13595429295, 24367.7260559393, 11706.9503136354, 0, 
    5779.64390720266, 13516.3799429772, 25012.8396984796, 16134.0505761624, 
    9828.30016531563, 3017.63018707227, 22847.3905876092, 8947.21255112159, 
    8937.87, 7377.8325809364, 7067.95956046367, 26137.0012877963, 
    14911.0960587445, 12503.1471380179, 10418.0625575776, 35619.9799912466, 
    17598.2475944483, 0, 12924.9909053091, 0, 12003.2008162162, 
    8187.35853035716, 15113.9739002398, 8366.96030066912, 34576.3499556291, 
    20910.7927213909, 0, 9180.73475832001, 9173.11559210526, 
    12960.041052075, 12302.4626390285, 11813.2638205792, 5636.53510321049, 
    30848.2871952263, 18572.6547853626, 9168.18629593708, 8937.87, 
    5306.413419, 11223.5349944232, 9012.25032189145, 14232.3678036943, 
    10436.1572791593), Eigentumsart = c("Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Staatswald", "Staatswald", "Staatswald", "Staatswald", "Staatswald", 
    "Staatswald", "Staatswald", "Staatswald", "Staatswald", "Staatswald", 
    "Staatswald", "Staatswald", "Staatswald", "Staatswald", "Staatswald", 
    "Staatswald", "Staatswald", "Staatswald", "Staatswald", "Staatswald", 
    "Staatswald", "Staatswald", "Staatswald", "Staatswald", "Staatswald", 
    "Staatswald", "Staatswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Körperschaftswald", "Körperschaftswald", 
    "Körperschaftswald", "Privatwald", "Privatwald", "Privatwald", 
    "Privatwald", "Privatwald", "Privatwald", "Privatwald", "Privatwald", 
    "Privatwald", "Privatwald", "Privatwald", "Privatwald", "Privatwald", 
    "Privatwald", "Privatwald", "Privatwald", "Privatwald", "Privatwald", 
    "Privatwald", "Privatwald", "Privatwald", "Privatwald", "Privatwald", 
    "Privatwald", "Privatwald", "Privatwald", "Privatwald"), 
    abbreviation = c("Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche")), .Names = c("bwi", "Baumartengruppe", 
"prediction", "standard_error", "Eigentumsart", "abbreviation"
), row.names = c(NA, 108L), class = "data.frame")
  RUnit::checkEquals(reference, result)
  
   by_group <- result
  result <- plot_by_group(by_group, subs = "Eigentumsart == 'Gesamtwald'")[["data"]]
reference <-
structure(list(bwi = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L), .Label = c("1987", "2002", "2012"), class = "factor"), 
    Baumartengruppe = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
    8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 
    5L, 6L, 7L, 8L, 9L), .Label = c("FI", "TA", "DGL", "KI", 
    "LAE", "BU", "EI", "ALH", "ALN"), class = "factor"), prediction = c(401259.103910681, 
    60966.931352984, 9223.15986702128, 44679.7533438112, 29988.181991633, 
    166402.405688824, 78250.2106277952, 59152.735807141, 17054.544910109, 
    376222.326820361, 88341.5405829897, 9214.33762886598, 25091.5627971649, 
    47369.9883162371, 187791.88661134, 80933.2131293814, 57783.1112706185, 
    21042.7828430412, 329903.038209, 73741.896435, 26295.21354, 
    26857.405563, 24734.661438, 190524.999642, 77120.411295, 
    105738.577248, 47808.66663), standard_error = c(44767.1164429632, 
    18786.423689262, 9223.15986702128, 15781.2996788884, 12155.3505360441, 
    31424.1393637099, 20762.3913432623, 19171.2636114755, 9254.65285554584, 
    44974.0383091226, 23482.7084861681, 9214.33762886598, 11343.6670859968, 
    17482.8499642516, 31388.1799403295, 21434.4890768358, 16675.8007261038, 
    9497.12951457268, 41954.8149210379, 20128.4249748835, 14103.3263030828, 
    11641.4403122435, 9550.961803339, 29479.9064238981, 19222.3241459563, 
    22734.1155245227, 15814.1714066418), Eigentumsart = c("Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", 
    "Gesamtwald"), abbreviation = c("Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche", "Baumartengruppenfläche", 
    "Baumartengruppenfläche", "Baumartengruppenfläche")), .Names = c("bwi", 
"Baumartengruppe", "prediction", "standard_error", "Eigentumsart", 
"abbreviation"), row.names = c(NA, 27L), class = "data.frame")
  RUnit::checkEquals(reference, result)

  result <- xtable_by_group(by_group, subs = "Eigentumsart == 'Gesamtwald'", 
                group = "Baumartengruppe",
                label_prefix = "total") 
  reference <- 
structure(list(V1 = structure(c(6L, 9L, 4L, 7L, 8L, 3L, 5L, 1L, 
2L, NA), .Label = c("ALH", "ALN", "BU", "DGL", "EI", "FI", "KI", 
"LAE", "TA"), class = "factor"), `1987` = structure(c(4L, 7L, 
10L, 5L, 3L, 1L, 8L, 6L, 2L, 9L), .Label = c("166,402.4", "17,054.54", 
"29,988.18", "401,259.1", "44,679.75", "59,152.74", "60,966.93", 
"78,250.21", "866,977", "9,223.16"), class = "factor"), `2002` = structure(c(4L, 
8L, 10L, 3L, 5L, 1L, 7L, 6L, 2L, 9L), .Label = c("187,791.9", 
"21,042.78", "25,091.56", "376,222.3", "47,369.99", "57,783.11", 
"80,933.21", "88,341.54", "893,790.7", "9,214.338"), class = "factor"), 
    `2012` = structure(c(6L, 8L, 4L, 5L, 3L, 2L, 9L, 1L, 7L, 
    10L), .Label = c("105,738.6", "190,525", "24,734.66", "26,295.21", 
    "26,857.41", "329,903", "47,808.67", "73,741.9", "77,120.41", 
    "902,724.9"), class = "factor"), `1987 - 2002` = structure(c(10L, 
    8L, 1L, 7L, 9L, 2L, 6L, 3L, 4L, 5L), .Label = c("-0.0956531", 
    "12.85407", "-2.315404", "23.38519", "3.092784", "3.428748", 
    "-43.84131", "44.90075", "57.96219", "-6.239554"), class = "factor"), 
    `2002 - 2012` = structure(c(2L, 5L, 6L, 9L, 8L, 4L, 7L, 10L, 
    3L, 1L), .Label = c("0.9995762", "-12.31168", "127.1975", 
    "1.455395", "-16.52636", "185.3728", "-4.711047", "-47.78411", 
    "7.037596", "82.99218"), class = "factor"), `1987 - 2012` = structure(c(4L, 
    7L, 6L, 8L, 3L, 2L, 1L, 10L, 5L, 9L), .Label = c("-1.443829", 
    "14.49654", "-17.51864", "-17.78304", "180.328", "185.0998", 
    "20.95393", "-39.88909", "4.123274", "78.75518"), class = "factor")), row.names = c(NA, 
-10L), class = c("xtable", "data.frame"), caption = "Baumartengruppenfläche", label = "tab:total:Baumartengruppenfläche", align = c("r", 
"l", "l", "l", "l", "l", "l", "l"), digits = c(0, 2, 2, 2, 2, 
2, 2, 2), display = c("s", "s", "s", "s", "s", "s", "s", "s"))

  RUnit::checkEquals(reference, result)
}

if (FALSE) {
    test_ownership_groups()
}
