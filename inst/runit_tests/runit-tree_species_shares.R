if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}
FVBN <- FVBN.bagrupp.akl.dkl.stratum.fun.2c
assign("fvbn.3", FVBN(get_data("baeume.3")[1:10, TRUE], get_data("ecken.3"), 
                      get_data("trakte.3"), get_design("a", 3), 2,
                      get_bwi_species_groups(),
                      list(A.ob = 200, A.b = 200),
                      list(D.unt = 0, D.ob = 200, D.b = 200, Ndh = FALSE),
                      list(Wa = c(3, 5), Begehbar = 1)
                      ), envir = .GlobalEnv)
assign("fvbn.2", fvbn.3, envir = .GlobalEnv)
assign("fvbn.1", fvbn.3, envir = .GlobalEnv)


test_tree_species_precent <- function() {
    result <- melt_tree_species_percent(statistics_list = "fvbn",
                                        bagr = get_bwi_species_groups())
    reference <- 
        structure(list(bwi = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                          1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                          2L, 2L, 3L, 3L, 3L, 3L, 3L, 
                                          3L, 3L, 3L, 3L), 
                                        .Label = c("1987", "2002", "2012"),
                                        class = "factor"), 
                        group = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
                                            1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,
                                            1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                            8L, 9L),
                                          .Label = c("FI", "TA", "DGL", "KI",
                                                     "LAE", "BU", "EI", "ALH",
                                                     "ALN"), 
                                          class = "factor"), 
                        value = c(0, 0, 0, 0, 0, 26.78, 0, 73.22, 0, 0, 0, 0, 0,
                                  0, 26.78, 0, 73.22, 0, 0, 0, 0, 0, 0, 26.78,
                                  0, 73.22, 0)), 
                   row.names = c(NA, -27L), class = "data.frame")
    RUnit::checkEquals(result, reference)
}
if (FALSE)
    test_tree_species_precent()

test_cast_tree_species_percent <- function() {
    result <- cast_tree_species_percent(statistics_list = "fvbn")
    reference <-
        structure(list(Baumartengruppe = structure(1:9, 
                                                   .Label = c("FI", "TA", "DGL",
                                                              "KI", "LAE", "BU",
                                                              "EI", "ALH", 
                                                              "ALN"), 
                                                   class = "factor"), 
                       `1987` = c(0, 0, 0, 0, 0, 26.78, 0, 73.22, 0), 
                       `2002` = c(0, 0, 0, 0, 0, 26.78, 0, 73.22, 0), 
                       `2012` = c(0, 0, 0, 0, 0, 26.78, 0, 73.22, 0)), 
                  class = "data.frame", row.names = c(NA, -9L))
    RUnit::checkEquals(result, reference)
}
if (FALSE)
    test_cast_tree_species_percent()
