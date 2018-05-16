    print(paste('===================', district_name))
    # Natürliche Höhenstufe soll in erhalten bleiben, im Dateinamen
    # allerdings NatürlicheHöhenstufe werden.
    # TODO: werde diesen tmp_regional_name elegant los!

    if (regional_name == paste('Natürliche Höhenstufe', district_name)) {
        tmp_regional_name <- paste('NatürlicheHöhenstufe', district_name)
    } else {
        tmp_regional_name <- regional_name
    }
    regional_file_name <<- gsub("/", "_",gsub(" ", "_", tmp_regional_name))
    district_file_name <- gsub(" ", "", gsub("/", "_",
                                              ascii_umlauts(regional_file_name)))
    district_tex_name <- tex_umlauts(district_name)
    district_tex <- paste0("Auswertung_", district_file_name, ".tex")
    dot_district_tex <<- paste0(".", district_file_name, ".tex")
    abstracts_directory <- "district_abstracts"
    district_abstract <- file.path(abstracts_directory,
                                   paste0(district_file_name, ".latex"))
    ##% initialize district article
    if (file.exists(district_tex)) unlink(district_tex)
    to_tex(tex_front_matter, file_name = district_tex)
    to_tex("\\subtitle{", tex_umlauts(regional_name), "}\n",
           tex_post_title, file_name = district_tex)
    ##% insert chapters into article and report 
    to_tex("\\chapter{Regionale Auswertung ", tex_umlauts(regional_name), "}\n", 
           file_name = district_tex)
    to_tex("\\chapter{Regionale Auswertung ", tex_umlauts(regional_name), "}\n", 
           file_name = all_districts_tex)
    ##% \input{} district abstract into report if file provided:
    to_tex("\\IfFileExists{",  district_abstract, "}",
           "{\\section{Allgemeines}\n\\input{", district_abstract, "}}{}",
           file_name = all_districts_tex) 
    ##% insert a newpage into the article
    to_tex( "\\newpage", file_name = district_tex)
    ##% \input{} the districts' analysis into article and report 
    to_tex("\\input{", dot_district_tex, "}", file_name = district_tex)
    to_tex("\\input{", dot_district_tex, "}", file_name = all_districts_tex)
    if(! generate_tex_wrappers_only) {
        ##% initialize districts' analysis
        if (file.exists(dot_district_tex)) unlink(dot_district_tex)
        to_tex(generator_notice)

        ##% analyses over all species

        
        ###% calculate ownership
        ownership_types_in_district <- unique( stratum.fun(append(list(Wa = c(3, 5), Begehbar = 1, EigArt2 = levels(ecken.3$EigArt2)), regional), ecken.3)$EigArt2)
        owners = NULL
        forest_area <- fl.stratum.fun(append(list(Wa = c(3, 4, 5)), regional),
                                      ecken.3, trakte.3, A)
        for (ownership in c("BW", "StW", "KW", "GPW", "MPW", "KPW")) {
            numeric_ownership  <- which(ownership == as.character(levels(ecken.3$EigArt2)))
            if(numeric_ownership %in% ownership_types_in_district) {
                owner <- fl.proz.stratum.fun(append(list(Wa = c(3, 4, 5)), 
                                                  regional),
                                             append(list(Wa = c(3, 4, 5), 
                                                  EigArt2 = ownership),
                                                  regional), 
                                             ecken.3)

                owners <- rbind(owners,
                                    data.frame(ownership = ownership, 
                                               area = forest_area$Flaeche *
                                               owner$Fl_Proz / 100,
                                               relative = owner$Fl_Proz
                                               ))
            }
        }
        ###% plot ownership
        to_tex("\\section{Eigentumsarten}") 
        to_tex('Diese Auswertung bezieht sich auf die gesamte Waldfl\\"a{}che,
               einschlie\\ss{}lich Nichtholzboden. Alle folgenden Auswertungen
               betrachten nur den begehbaren, bestockten Holzboden.') 
        plot_ownership(owners)
        # FIXME: this is crap, we check on the internal names of global
        # lists! If we change those lists' names, we'll for sure forget this
        # peace of bullshit (the is.null(...) catches the whole country):
        is_big_stratum <- names(regional) %in% c("WGNr_BW", "RBez", "NatHoe") || is.null(names(regional))
        ##% analyses for species' groups
        if (is_big_stratum) {
            # TODO: in FVBN(): bagr.list <- BA.grupp[[1]]: we use the first list
            # element. in bagr.bwi, this is bagr.lab, where in
            # get_species_groups() # returns ba.text as first list element.
            # Instead of refactoring get_species_groups() and the calls to
            # district_groups, I swap ba.text and bagr.lab from bagr.bwi it here. 
            district_groups <<- append(
                                       append(bagr.bwi["ba.text"], 
                                              bagr.bwi[
                                                setdiff(names(bagr.bwi), 
                                                        c("ba.text", "bagr.lab")
                                                        )
                                                ])
                                                , bagr.bwi["ba.lab"])
        } else {
            ###% calculate species' area shares without grouping
            fvbn.krs <<- FVBN(baeume.3, ecken.3, trakte.3, A, 2, bagr.xx, 
                              list(A.ob = 500, A.b = 500), 
                              list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                              append(list(Wa = c(3, 5), Begehbar = 1), regional))
            baf_prozente <<- round(fvbn.krs$FVBN.ha.Bagr.Akl.Dkl[1, 1, 1:n.bagr.xx, , ], 
                                   2)
            names(baf_prozente) <- bagr.xx$ba.text
            ###% calculate species' groups specific for district
            district_groups <<- group_district_species(shares = baf_prozente)
        }
        # now that we've got the new groups per district, we use them to 
        # calculate statistics for the districts
        ###% define different stratii
        if (is_big_stratum) {
            stratii <<- list(
                             "Gesamtwald" = NULL,
                             # TODO: shouldn't this be StW plus BW, to make the
                             # following three sum up to the previous?
                             "Staatswald" = list(EigArt = c("StW")),
                             "Körperschaftswald" = list(EigArt = c("KW")),
                             "Privatwald" = list(EigArt = c("PW"))
                             )
        } else {
            public_codes <- setdiff(levels(ecken.3$EigArt), "PW")
            stratii <<- list(
                             "Gesamtwald" = NULL,
                             "Privatwald" = list(EigArt = c("PW")),
                             "Öffentlicher Wald" = list(EigArt = public_codes)
                             )
        }
        if (district_code == 437)
            stratii <<- append(stratii, 
                              c(
                              list("Gesamtwald, Wuchsgebiet 6" = 
                                   list(WGNr_BW = 6)),
                              list("Gesamtwald, Wuchsgebiet 7" = 
                                   list(WGNr_BW = 7))
                              )
                              )

        for (stratum_index in 1:length(stratii)) {
            stratum_index <<- stratum_index
            to_tex('\\newpage') 
                   to_tex('\\section{',names(stratii[stratum_index]),'}')

        ###% calculate statistics for species' groups
        b_3 <- FVBN(baeume.3, ecken.3, trakte.3, A, 2, district_groups, 
                    list(A.ob = 500, A.b = 500), 
                    list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                    append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]]))
        b_2 <- FVBN(baeume.2, ecken.2, trakte.2, A.12, 2, district_groups, 
                    list(A.ob = 500, A.b = 500), 
                    list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                    append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]))

        b_1 <- FVBN(baeume.1, ecken.1, trakte.1, A.12, 1, district_groups, 
                    list(A.ob = 500, A.b = 500), 
                    list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                    append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]))
        ###% calculate statistics for species' groups - by age
        if (is_big_stratum){
            age_classes <- list(A.ob = 160, A.b = 20) 
        } else {
            age_classes <- list(A.ob = 120, A.b = 60) 
        }
        b_3a <- FVBN(baeume.3, ecken.3, trakte.3, A, 2, district_groups, 
                     age_classes, 
                     list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                     append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]]))
        b_2a <- FVBN(baeume.2, ecken.2, trakte.2, A.12, 2, district_groups, 
                     age_classes, 
                     list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                     append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]))
        b_1a <- FVBN(baeume.1, ecken.1, trakte.1, A.12, 1, district_groups, 
                     age_classes, 
                     list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F), 
                     append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]))
        ###% calculate statistics for species' groups - by girth
        if (is_big_stratum){
            dbh_classes <- list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T)
        } else {
            dbh_classes <- list(D.unt = 0, D.ob = 50, D.b = 25, Ndh = T)
        }
        b_3g <- FVBN(baeume.3, ecken.3, trakte.3, A, 2, district_groups, 
                     list(A.ob = 500, A.b = 500), 
                     dbh_classes,
                     append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]])
                     )
        b_2g <- FVBN(baeume.2, ecken.2, trakte.2, A.12, 2, district_groups, 
                     list(A.ob = 500, A.b = 500), 
                     dbh_classes,
                     append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]))
        b_1g <- FVBN(baeume.1, ecken.1, trakte.1, A.12, 1, district_groups, 
                     list(A.ob = 500, A.b = 500), 
                     dbh_classes,
                     append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]))
        ###% plot area for species' groups
        to_tex('\\subsection{Baumartenfl\\"a{}chen}') 
        to_tex('Alle Baumartenfl\\"a{}chen enthalten anteilig Bl\\"o{}\\ss{}en
               und L\\"u{}cken.') 
        to_tex('\\subsubsection{Entwicklung von 1987 bis 2012}') 
        aa <- plot_groups_areas(b_1, b_2, b_3)
        ###% plot area by age for species' group
        to_tex('\\newpage') 
        to_tex('\\subsubsection{Entwicklung der Altersstruktur von 1987 bis 2012}') 
        plot_group_area_by_age(b_1a, b_2a, b_3a, aa)
        ###% plot stock for species' groups
        to_tex('\\newpage') 
        to_tex('\\subsection{Vorr\\"ate}') 
        to_tex('Alle Vorratsangaben umfassen Haupt- und
               Nebenbestandsvorr\"a{}te.')
        to_tex('\\subsubsection{Entwicklung von 1987 bis 2012}') 
        va <- plot_groups_stocks(b_1, b_2, b_3)
        ###% plot stock by girth for species' group
        to_tex('\\newpage') 
        to_tex('\\subsubsection{Entwicklung der Dimensionsstruktur von 1987 bis 2012}') 
        plot_group_stocks_by_girth(b_1g, b_2g, b_3g, va)
        ##% analyses over all species
        ###% calculate closeness_to_nature
        n3 <- ntns.stratum.fun(ntns.te, ecken.3, trakte.3, A, 
                               append(append(list(Wa=c(3, 5), Begehbar=1), regional), stratii[[stratum_index]]), 
                               3, 0)
        n2 <- ntns.stratum.fun(ntns.te, ecken.2, trakte.2, A, 
                               append(append(list(Wa=c(1, 2, 3), Begehbar=1), regional), stratii[[stratum_index]]), 
                               3, 0)
        ###% plot closeness_to_nature
        to_tex('\\newpage') 
        to_tex("\\subsection{Naturn\"a{}he}") 
        to_tex("Bei der Bestimmung der Naturn\"a{}he wurde der Vergleichbarkeit
               wegen f\"u{}r BWI 2 und BWI 3 die Zuordnung der
               nat\"u{}rlichen Waldgesellschaft der BWI 3 zugrundegelegt.") 
        plot_ntns(n2, n3)
        ##% silviculturally relevant species
        to_tex("\\subsection{Forstlich bedeutsame Arten}") 
        silviculturally_relevant_3 <- fba.stratum.fun(c(11:18), fba.3, 
                                              append(append(list(Wa=c(3, 5),
                                                          Begehbar=1), regional),stratii[[stratum_index]]),
                                              ecken.3, trakte.3, A)



        silviculturally_relevant_2 <- fba.stratum.fun(c(11:18), fba.2, 
                                              append(append(list(Wa=c(1:3), Begehbar=1), 
                                                   regional), stratii[[stratum_index]]),
                                              ecken.2, trakte.2, A.12)
        plot_silviculturally_relevant(silviculturally_relevant_2,
                                      silviculturally_relevant_3)
        ###% calculate deadwood
        ####% over all deadwood classes
        d_3a <- dead(totholz.3, ecken.3, trakte.3, 3, 3, NA, A,
                    append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]])
                    )
        d_3_2a <- dead(totholz.3, ecken.3, trakte.3, 3, 2, NA, A,
                      append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]])
                      )
        d_2a <- dead(totholz.2, ecken.2, trakte.2, 2, 2, NA, A,
                    append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]])
                    )
        ####% by deadwood classes
        d_3 <- dead(totholz.3, ecken.3, trakte.3, 3, 3, 
                    list(attr = "tart", kat = list(c(11, 12, 13), c(2), c(3),
                                                   c(4), c(5))), A,
                    append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]])
                    )
        d_3_2 <- dead(totholz.3, ecken.3, trakte.3, 3, 2, 
                      list(attr = "tart", kat = list(c(11, 12, 13), c(2), 
                                                     c(3), c(4), c(5))), A,
                      append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]])
                      )
        d_2 <- dead(totholz.2, ecken.2, trakte.2, 2, 2, 
                    list(attr = "tart", kat = list(c(1), c(2), c(3),
                                                   c(4), c(5))), A,
                    append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]])
                    )
        ###% plot deadwood
        to_tex('\\clearpage') 
        to_tex("\\subsection{Totholz}") 
        plot_deadwood(d_2, d_3_2, d_3, d_2a, d_3_2a, d_3a)

        ###% growth and loss
        za_23 <- iVB.ew.bagrupp.akl.dkl.stratum.fun.2(baeume.23, baeume.3,
                                                          district_groups,
                                                          list(A.ob=500,A.b=500), list(D.unt=0,D.ob=400,D.b=400,Ndh=F),
                                                          append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]]),A)
        za_12 <- iVB.ew.bagrupp.akl.dkl.stratum.fun.bwi12(baeume.12,baeume.2,
                                                          district_groups,
                                                          list(A.ob=500,A.b=500), list(D.unt=0,D.ob=400,D.b=400,Ndh=F),
                                                          append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]),A.12)

        za_12_all <- iVB.ew.bagrupp.akl.dkl.stratum.fun.bwi12(baeume.12,baeume.2,
                                                              list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
                                                              list(A.ob=500,A.b=500), list(D.unt=0,D.ob=400,D.b=400,Ndh=F),
                                                              append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]),A.12)
        to_tex('\\newpage') 
        to_tex("\\subsection{Zuwachs und ausgeschiedener Vorrat}") 
        to_tex("Ausgeschiedener Vorrat umfasst genutzte und ungenutzt im Wald
               verbliebene Mengen.")
        plot_growth_loss(za_12, za_23, za_12_all)
        ###% Nutzungen
        za_23_d <- iVB.ew.bagrupp.akl.dkl.stratum.fun.2(baeume.23, baeume.3,
                                                        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
                                                          list(A.ob=500,A.b=500), 
                                                          list(D.unt = 0, D.ob = 50, D.b = 25, Ndh = F), 
                                                          append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]]),A)
        za_12_d <- iVB.ew.bagrupp.akl.dkl.stratum.fun.2(baeume.12, baeume.2,
                                                        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
                                                          list(A.ob=500,A.b=500), 
                                                          list(D.unt = 0, D.ob = 50, D.b = 25, Ndh = F), 
                                                          append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]),A.12)

        za_23_all_dbh <- iVB.ew.bagrupp.akl.dkl.stratum.fun.2(baeume.23, baeume.3,
                                                        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
                                                          list(A.ob=500,A.b=500), 
                                                          list(D.unt=0,D.ob=400,D.b=400,Ndh=F),
                                                          append(append(list(Wa = c(3, 5), Begehbar = 1), regional), stratii[[stratum_index]]),A)
        za_12_all_dbh <- iVB.ew.bagrupp.akl.dkl.stratum.fun.2(baeume.12, baeume.2,
                                                        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
                                                          list(A.ob=500,A.b=500), 
                                                          list(D.unt=0,D.ob=400,D.b=400,Ndh=F),
                                                          append(append(list(Wa = c(1, 2, 3), Begehbar = 1), regional), stratii[[stratum_index]]),A.12)

        plot_loss(za_12_d, za_23_d, za_12_all_dbh, za_23_all_dbh)
        ###% regeneration
        to_tex('\\subsection{Verj\"ungung}') 
        to_tex('Als Verj\"u{}ngung wird die Baumschicht bis zu einer H\"o{}he
               von vier Metern betrachtet.')
         reg2_a <- verjg.kl4.bagrupp.fun(verj.kl4m.2,ecken.2,trakte.2,
                                          district_groups, 
                                          append(append(list(Wa=c(1,2,3),Begehbar=1), regional), stratii[[stratum_index]]),
                                          A.12)
        reg2_bs1 <- verjg.kl4.bagrupp.fun(subset(verj.kl4m.2,Bs==1),ecken.2,trakte.2,
                                          district_groups, 
                                          append(append(list(Wa=c(1,2,3),Begehbar=1), regional), stratii[[stratum_index]]),
                                          A.12)
        reg2_bs2 <- verjg.kl4.bagrupp.fun(subset(verj.kl4m.2,Bs==2),ecken.2,trakte.2,
                                          district_groups, 
                                          append(append(list(Wa=c(1,2,3),Begehbar=1), regional), stratii[[stratum_index]]),
                                          A.12)
        reg3_a <- verjg.kl4.bagrupp.fun(verj.kl4m.3,ecken.3,trakte.3,
                                          district_groups, 
                                          append(append(list(Wa=c(3,5),Begehbar=1), regional), stratii[[stratum_index]]),
                                          A)
        reg3_bs1 <- verjg.kl4.bagrupp.fun(subset(verj.kl4m.3,Bs==1),ecken.3,trakte.3,
                                          district_groups, 
                                          append(append(list(Wa=c(3,5),Begehbar=1), regional), stratii[[stratum_index]]),
                                          A)
        reg3_bs2 <- verjg.kl4.bagrupp.fun(subset(verj.kl4m.3,Bs==2),ecken.3,trakte.3,
                                          district_groups, 
                                          append(append(list(Wa=c(3,5),Begehbar=1), regional), stratii[[stratum_index]]),
                                          A)

        regeneration(reg2_a, reg2_bs1, reg2_bs2, reg3_a, reg3_bs1, reg3_bs2)
        }
    }
    to_tex(tex_back_matter, file_name = district_tex)
    tools::texi2dvi(district_tex, pdf = TRUE)
    file.link(
              to = file.path(reports_directory, paste0(regional_file_name, ".pdf")),
              from = file.path( paste0(sub(".tex$", "", district_tex), ".pdf"))
              )


