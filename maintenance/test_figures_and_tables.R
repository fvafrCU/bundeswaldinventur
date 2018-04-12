
if (interactive()) devtools::load_all()
fvbn <- FVBN.bagrupp.akl.dkl.stratum.fun.2a
 assign('FVBN.bagr.akl.gw.3',
     fvbn(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(3, 5), Begehbar = 1))
 )
 assign('FVBN.bagr.akl.gw.2',
     fvbn(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1))
 )
 assign('FVBN.bagr.akl.gw.1',
     fvbn(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1))
 )
assign('FVBN.bagr.gw.3', 
       fvbn(
            baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(3, 5), Begehbar = 1))
       )
assign('FVBN.bagr.gw.2', 
       fvbn(
            baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1))
       )
assign('FVBN.bagr.gw.1', 
       fvbn(
            baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1))
       )
assign('FVBN.bagr.stw.3',  
       fvbn(
            baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(3, 5), Begehbar = 1, EigArt = "StW"))
       )
assign('FVBN.bagr.stw.2',
       fvbn(
            baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
       )
assign('FVBN.bagr.stw.1',
       fvbn(
            baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
       )
assign('FVBN.bagr.kw.3',
       fvbn(
            baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(3, 5), Begehbar = 1, EigArt = "KW"))
       )
assign('FVBN.bagr.kw.2',
       fvbn(
            baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
       )
assign('FVBN.bagr.kw.1',
       fvbn(
            baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
       )
assign('FVBN.bagr.pw.3',
       fvbn(
            baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(3, 5), Begehbar = 1, EigArt = "PW"))
       )
assign('FVBN.bagr.pw.2',
       fvbn(
            baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
       )
assign('FVBN.bagr.pw.1',
       fvbn(
            baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
            list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
            list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))

       )
test_melt <- function() {
    result <- melt_species_attribute_group("FVBN.bagr.akl.gw", "FI", "BAF", "AKL")

    reference <- structure(list(
                                prediction = c(
                                               101455.691800711, 123702.645472045,
                                               96149.3087141302, 109706.758770222, 82485.9048276029, 37055.9687535027,
                                               13524.6129992997, 5657.65024172331, 3072.43770128514, 55314.3531603782,
                                               108006.955226673, 111067.584122095, 75207.6715450263, 75926.7271831114,
                                               44106.3824669629, 20204.3046292027, 5624.08878814184, 3738.66173906768,
                                               62658.9084678404, 67033.9527207928, 121385.99103384, 66540.6548667543,
                                               56748.6014196971, 42709.1286320823, 21087.7330339931, 8286.85056069205,
                                               4329.93913087693
                                               ), standard_error = c(
                                               3325.6817132501, 3780.18021889631,
                                               3530.15111745596, 3883.68482898267, 3388.10598882395, 2137.52656393468,
                                               1169.25996899341, 770.392860193016, 559.728847202122, 2531.46240617407,
                                               3617.53057791328, 3654.57241289469, 3010.69550678946, 3184.42867336539,
                                               2309.55961884203, 1439.07762599681, 656.130735374848, 555.80082597679,
                                               2733.29029208858, 2750.56950603433, 3774.32693152555, 2702.71874000177,
                                               2541.73758639033, 2173.93921595373, 1453.40283817208, 800.031111798687,
                                               575.50602663425
                                               ), abbreviation = structure(c(
                                                                             1L, 1L, 1L, 1L,
                                                                             1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                                             1L, 1L, 1L, 1L, 1L, 1L, 1L
                                                                             ), .Label = "BAF", class = "factor"),
                                bwi = structure(c(
                                                  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
                                                  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                                  3L, 3L
                                                  ), .Label = c("1987", "2002", "2012"), class = "factor"),
                                group = structure(c(
                                                    1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L,
                                                    2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                                                    8L, 9L
                                                    ), class = "factor", .Label = c(
                                                    "1-20", "21-40", "41-60",
                                                    "61-80", "81-100", "101-120", "121-140", "141-160", ">160"
                                                    )), species = c(
                                "Fichte", "Fichte", "Fichte", "Fichte", "Fichte",
                                "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte",
                                "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte",
                                "Fichte", "Fichte", "Fichte", "Fichte", "Fichte", "Fichte",
                                "Fichte", "Fichte", "Fichte", "Fichte"
                                ), grouping_variable = c(
                                "AKL",
                                "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL",
                                "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL",
                                "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL", "AKL"
                                ),
                                ownership = c(
                                              "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald",
                                              "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald",
                                              "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald",
                                              "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald",
                                              "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald", "Gesamtwald",
                                              "Gesamtwald", "Gesamtwald", "Gesamtwald"
                                              )
                                ), .Names = c(
                                "prediction",
                                "standard_error", "abbreviation", "bwi", "group", "species",
                                "grouping_variable", "ownership"
                                ), row.names = c(NA, 27L), class = "data.frame")

    RUnit::checkEquals(result, reference)
}
