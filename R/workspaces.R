#!/usr/bin/Rscript --vanilla
#' provide, load or delete workspaces.
#'
#' Querying the data source is time consuming. I add two local workspaces: one
#' containing the data loaded from the database, the other containing the
#' statistics. Both are provided, loaded or deleted using the functions in this
#' file.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @docType data  
#' @name Header
NULL 

#' provide workspaces
#'
#' This function checks whether a workspace is loaded and loads it if not. In
#' case it doesn't exists (implying it's not loaded), it calls
#' create_workspace(), then loads the workspace.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @param type String giving the type of workspace to load. Should be in
#' c('DATA', 'STATISTICS').
#' @return  TRUE on success, FALSE otherwise. 
provide_workspace <- function(type) {
    status <- FALSE
    if (! type %in% c('DATA', 'STATISTICS')) {
	message("type is not in c('DATA', 'STATISTICS')")
	return(invisible(status))
    }
    is_loaded <- paste('IS_LOADED', type, sep = '_')
    if (exists(is_loaded, envir = globalenv())) {
	message(paste("script was already run, do nothing to avoid multiple ", 
		      "loading of ", tolower(type), ".", sep = ''
		      )
	)
    } else {
	## the path/to/workspace is the value of a global variable whose name is
	## the value of workspace
	workspace <- paste(type, 'WORKSPACE', sep = '_')
	if (exists(workspace)) {
	    if (file.exists(eval(parse(text = workspace)))) {
		message(paste('# loading', tolower(type), 'workspace...'))
		# in fact, we do nothing but passing down to load()
	    } else {
		create_workspace(type)
	    }
	    load(eval(parse(text = workspace)), envir = globalenv())
	    message(paste('# workspace', tolower(type), 'loaded.'))
	} else {
	    stop(paste('global variable', workspace, 'does not exist.', 
		       sep = ''
		       )
	    )
	}
	assign(is_loaded, TRUE, envir = globalenv())
    }
    status <- TRUE
    return(invisible(status))
}

#' purge workspaces
#'
#' This function checks whether a workspace exists and deletes it. It also
#' deletes the IS_LOADED_type variable provide_workspace() checks for before
#' loading.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @param type String giving the type of workspace to delete. Should be in
#' c('DATA', 'STATISTICS').
#' @return  TRUE on success, FALSE otherwise. 
purge_workspace <- function(type) { 
    status <- FALSE
    is_loaded <- paste('IS_LOADED', type, sep = '_')
    if (exists(is_loaded, envir = globalenv())) {
	rm(eval(parse(text = is_loaded)), envir = globalenv())
    } else {
	# object doesn't exists
    }
    ## the path/to/workspace is the value of a global variable whose name is
    ## the value of workspace
    workspace <- paste(type, 'WORKSPACE', sep = '_')
    if (exists(workspace)) {
	if (file.exists(eval(parse(text = workspace)))) {
	    unlink(eval(parse(text = workspace)))
	} else {
	    # workspace doesn't exists
	}
    } else {
	stop(paste('global variable ', workspace, ' does not exist.', 
		   sep = ''
		   )
	)
    }
    status <- TRUE
    return(invisible(status))
}

#' provide the data workspace
#'
#' Just an alias to \code{\link{provide_workspace}}('DATA'), for compability.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @return  See \code{\link{provide_workspace}}.
provide_data <- function() {
    status <- provide_workspace('DATA')
    return(invisible(status))
}

#' provide the statistics workspace
#'
#' Just an alias for \code{\link{provide_workspace}}('STATISTICS'), for compability.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @return  See \code{\link{provide_workspace}}.
provide_statistics <- function() {
    status <- provide_workspace('STATISTICS')
    return(invisible(status))
}
#' purge the data workspace
#'
#' Just an alias for \code{\link{purge_workspace}('DATA')}, for compability.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @return  See \code{\link{purge_workspace}}.
purge_data_workspace <- function() {
    status <- purge_workspace('DATA')
    return(invisible(status))
}

#' purge the statistics workspace
#'
#' Just an alias to \code{\link{purge_workspace}('STATISTICS')}, for compability.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @return  See \code{\link{purge_workspace}}.
purge_statistics_workspace <- function() {
    status <- purge_workspace('STATISTICS')
    return(invisible(status))
}

#' create the data workspace
#'
#' load the data from the database (well, from MS Access) to a dedicated
#' environment and save it.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @note Needs DATA_WORKSPACE to hold the path to save workspace to.
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @return  TRUE on success, FALSE otherwise. 
create_data_workspace <- function() {
    status <- FALSE
    data_directory <- file.path(MOUNT_POINT, 
				"BuI", "Projekte", "BWI3_P932", 
				"BWI_3_Inventur_P932", "07_Auswertungen", 
				"LandBW", "Daten"
				)
    data_environment <- new.env()

    switch(.Platform$OS.type
	   , 'windows' = {
	       library(RODBC)
	       channel <- odbcConnectAccess(paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"))
	       assign('baeume.3', sqlFetch(channel, "BWI_3_Baeume_B"), envir = data_environment)
	       assign('baeume.23', sqlFetch(channel, "BWI_23_Baeume_B"), envir = data_environment)
	       assign('baeume.2', sqlFetch(channel, "BWI_2_Baeume_B"), envir = data_environment)
	       assign('baeume.12', sqlFetch(channel, "BWI_12_Baeume_B"), envir = data_environment)
	       assign('baeume.1', sqlFetch(channel, "BWI_1_Baeume_B"), envir = data_environment)
	       assign('ecken.3',  sqlFetch(channel, "Eckenmerkmale_BWI3"), envir = data_environment)
	       assign('ecken.2',  sqlFetch(channel, "Eckenmerkmale_BWI2"), envir = data_environment)
	       assign('ecken.23',  sqlFetch(channel, "Eckenmerkmale_BWI23"), envir = data_environment)
	       assign('ecken.1',  sqlFetch(channel, "Eckenmerkmale_BWI1"), envir = data_environment)
	       assign('trakte.3',  sqlFetch(channel, "Trakte_BWI3"), envir = data_environment)
	       assign('trakte.2',  sqlFetch(channel, "Trakte_BWI2"), envir = data_environment)
	       assign('trakte.1',  sqlFetch(channel, "Trakte_BWI1"), envir = data_environment)
	       assign('totholz.3', sqlFetch(channel, "Totholz_B3"), envir = data_environment)
	       assign('totholz.2', sqlFetch(channel, "Totholz_B2"), envir = data_environment)
	       assign('bacode',  sqlFetch(channel, "BACode_BWI_3"), envir = data_environment)
           assign('verj.3', sqlFetch(channel, "Verjuengung_B3"), envir = data_environment)
           assign('verj.2', sqlFetch(channel, "Verjuengung_B2"), envir = data_environment)
           assign('fba.3', sqlFetch(channel,"B3v_fba"), envir = data_environment)
           assign('x_fba.3', sqlFetch(channel,"x3_fba"), envir = data_environment)
           assign('x_dichte', sqlFetch(channel,"x3_dichte"), envir = data_environment)
           assign('fba.2', sqlFetch(channel,"B2_fba"), envir = data_environment)
           {
               assign('gk_coord', sqlFetch(channel, "GK_Koord_Ist_TE"), envir = data_environment) 
               # names in GK_Koord_Ist_TE are inconsistent with with the rest.
               evalq(names(gk_coord)[which(names(gk_coord) %in% c("Tnr", "Enr"))] <-  c("TNr", "ENr"), envir = data_environment)
           }

	       odbcClose(channel)
	   }
	   , 'unix' = {
	       # message(you need to install mdbtools and the R-package 'Hmisc' on your unix box)
	       library('Hmisc')
	       assign('baeume.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_3_Baeume_B"), envir = data_environment)
	       assign('baeume.23', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_23_Baeume_B"), envir = data_environment)
	       assign('baeume.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_2_Baeume_B"), envir = data_environment)
	       assign('baeume.12', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_12_Baeume_B"), envir = data_environment)
	       assign('baeume.1', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_1_Baeume_B"), envir = data_environment)
	       assign('ecken.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Eckenmerkmale_BWI3"), envir = data_environment)
	       assign('ecken.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Eckenmerkmale_BWI2"), envir = data_environment)
	       assign('ecken.23', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Eckenmerkmale_BWI23"), envir = data_environment)
	       assign('ecken.1', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Eckenmerkmale_BWI1"), envir = data_environment)
	       assign('totholz.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Totholz_B3"), envir = data_environment)
	       assign('totholz.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Totholz_B2"), envir = data_environment)
	       assign('bacode', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BACode_BWI_3"), envir = data_environment)
           assign('verj.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Verjuengung_B3"), envir = data_environment)
           assign('verj.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Verjuengung_B2"), envir = data_environment)
           assign('fba.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "B3v_fba"), envir = data_environment)
           assign('x_fba.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "X3_fba"), envir = data_environment)
           assign('x_dichte', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "X3_dichte"), envir = data_environment)
           assign('fba.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "B2_fba"), envir = data_environment)
           {
               assign('gk_coord', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "GK_Koord_Ist_TE"), envir = data_environment) 
               # names in GK_Koord_Ist_TE are inconsistent with with the rest.
               evalq(names(gk_coord)[which(names(gk_coord) %in% c("Tnr", "Enr"))] <-  c("TNr", "ENr"), envir = data_environment)
           }
           assign('wzp4.merkmale.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_3_WZP_sonstige_Merkmale"), envir = data_environment)
           assign('wzp4.merkmale.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "BWI_2_WZP_sonstige_Merkmale"), envir = data_environment)
           assign('verj.kl4m.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Verjuengung_kl_4m_B3"), envir = data_environment)
           assign('verj.kl4m.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "Verjuengung_kl_4m_B2"), envir = data_environment)
           assign('ntns.te', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "dbo_BWI_ECKE_Naturnaehe"), envir = data_environment)
           assign('kreise', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "xbw_Landkreise_Flaechen"), envir = data_environment)
            
           {
               warning("you need to build T_<ABFRAGE> in BWI3_HR_BW.mdb by\n",
                       " opening the Abfrage <ABFRAGE>, selecting\n",
                       " Entwurfsansicht and hitting the buttons \n",
                       " 'Tabelle erstellen' and 'Ausfuehren'.")
               assign('trakte.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "T_Trakte_BWI3"), envir = data_environment)
               assign('trakte.2', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "T_Trakte_BWI2"), envir = data_environment)
               assign('trakte.1', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "T_Trakte_BWI1"), envir = data_environment)
               assign('ecke.feld.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "T_ecke_feld_3"), envir = data_environment)
               assign('schutzgebiete.3', mdb.get(allow = c("_"), paste(data_directory, "BWI3_HR_BW.mdb", sep = "/"), "T_Schutzgebiete_BWI_3"), envir = data_environment)
           }
	   }
	   , stop(paste('unkown operation system', .Platform$OS.type))
	   )
    save(list = ls(data_environment, all = TRUE), file = DATA_WORKSPACE, 
	 envir = data_environment
	 )
    status <- TRUE
    return(invisible(status))
}

#' create the statistics workspace
#'
#' calculate statistics in a dedicated
#' environment and save it.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @note Needs STATISTICS_WORKSPACE to hold the path to save workspace to.
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @return  TRUE on success, FALSE otherwise. 
create_statistics_workspace <- function() {
    status <- FALSE
    statistics_environment <- new.env()




    # (1) Alle Baumarten zusammen im Gesamtwald; ohne weitere Klassifikation
    ## BWI 3: 2012
    assign('FVBN.alleba.gw.3', envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.3, ecken.3, trakte.3, A, 2,
					       list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1))
	   )
    ## BWI 2: 2002
    assign('FVBN.alleba.gw.2', envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.2, ecken.2, trakte.2, A.12, 2,
					       list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1))
	   )
    ## BWI 1: 1987
    assign('FVBN.alleba.gw.1', envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.1, ecken.1, trakte.1, A.12, 1,
					       list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1))
	   )

    # (2) Gesamtzustand nach BWI-Baumartengruppen
    # Hinweis: für den bundesweiten Vergleich werden die definierten 9 Baumarten-
    # gruppen verwendet; für landesspezifische Auswertungen erfolgt eine zusätzliche
    # Differenzierung der ALH-Gruppe
    ## (2.1) Gesamtwald
    ### BWI 3: 2012
    assign('FVBN.bagr.gw.3', envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1))
	   )
    ### BWI 2: 2002
    assign('FVBN.bagr.gw.2', envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1))
	   )
    ### BWI 1: 1987
    assign('FVBN.bagr.gw.1', envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1))
	   )
    ## (2.2) Staatswald
    ### BWI 3: 2012
    assign('FVBN.bagr.stw.3',  envir = statistics_environment, 
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1, EigArt = "StW"))
	   )
    ### BWI 2: 2002
    assign('FVBN.bagr.stw.2', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
	   )
    ### BWI 1: 1987
    assign('FVBN.bagr.stw.1', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
	   )
    ## (2.3) Körperschaftswald
    ### BWI 3: 2012
    assign('FVBN.bagr.kw.3', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1, EigArt = "KW"))
	   )
    ### BWI 2: 2002
    assign('FVBN.bagr.kw.2', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
	   )
    ### BWI 1: 1987
    assign('FVBN.bagr.kw.1', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
	   )
    ## (2.4) Privatwald
    ### BWI 3: 2012
    assign('FVBN.bagr.pw.3', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(3, 5), Begehbar = 1, EigArt = "PW"))
	   )
    ### BWI 2: 2002
    assign('FVBN.bagr.pw.2', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
	   )
    ### BWI 1: 1987
    assign('FVBN.bagr.pw.1', envir = statistics_environment,
	   FVBN.bagrupp.akl.dkl.stratum.fun.2a(
					       baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
					       list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
					       list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))

	   )
    ## (3) Alle Baumarten nach Altersklassen
    ## (3.1) Gesamtwald
    ### BWI 3
    assign('FVBN.alleba.akl.gw.3', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.3, ecken.3, trakte.3, A, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(3, 5), Begehbar = 1))
    )
    ### BWI 2
    assign('FVBN.alleba.akl.gw.2', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.2, ecken.2, trakte.2, A.12, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1))
    )
    ### BWI 1
    assign('FVBN.alleba.akl.gw.1', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.1, ecken.1, trakte.1, A.12, 1,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1))
    )
    ## (3.2) Staatswald
    ### BWI 3
    assign('FVBN.alleba.akl.stw.3', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.3, ecken.3, trakte.3, A, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(3, 5), Begehbar = 1, EigArt = "StW"))
    )
    ### BWI 2
    assign('FVBN.alleba.akl.stw.2', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.2, ecken.2, trakte.2, A.12, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
    )
    ### BWI 1
    assign('FVBN.alleba.akl.stw.1', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.1, ecken.1, trakte.1, A.12, 1,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
    )
    ## (3.3) Körperschaftswald 
    ### BWI 3
    assign('FVBN.alleba.akl.kw.3', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.3, ecken.3, trakte.3, A, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(3, 5), Begehbar = 1, EigArt = "KW"))
    )
    ### BWI 2
    assign('FVBN.alleba.akl.kw.2', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.2, ecken.2, trakte.2, A.12, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
    )
    ### BWI 1
    assign('FVBN.alleba.akl.kw.1', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.1, ecken.1, trakte.1, A.12, 1,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
    )
    ## (3.4) Privatwald 
    ### BWI 3
    assign('FVBN.alleba.akl.pw.3', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.3, ecken.3, trakte.3, A, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(3, 5), Begehbar = 1, EigArt = "PW"))
    )
    ### BWI 2
    assign('FVBN.alleba.akl.pw.2', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.2, ecken.2, trakte.2, A.12, 2,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
    )
    ### BWI 1
    assign('FVBN.alleba.akl.pw.1', envir = statistics_environment,
        FVBN.bagrupp.akl.dkl.stratum.fun.2a(
        baeume.1, ecken.1, trakte.1, A.12, 1,
        list(bagr.lab = c("Alle Baumarten"), ba.grupp = list(c(10:299))),
        list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
        list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
    )
 
 ## (4) Nach Altersklassen und Baumartengruppen
 ## (4.1) Gesamtwald
 ### BWI 3
 assign('FVBN.bagr.akl.gw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(3, 5), Begehbar = 1))
 )
 ### BWI 2
 assign('FVBN.bagr.akl.gw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1))
 )
 ### BWI 1
 assign('FVBN.bagr.akl.gw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1))
 )
 ## (4.2) Staatswald
 ### BWI 3
 assign('FVBN.bagr.akl.stw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(3, 5), Begehbar = 1, EigArt = "StW"))
 )
 ### BWI 2
 assign('FVBN.bagr.akl.stw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
 )
 ### BWI 1
 assign('FVBN.bagr.akl.stw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
 )
 ## (4.3) Koerperschaftswald
 ### BWI 3
 assign('FVBN.bagr.akl.kw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(3, 5), Begehbar = 1, EigArt = "KW"))
 )
 ### BWI 2
 assign('FVBN.bagr.akl.kw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
 )
 ### BWI 1
 assign('FVBN.bagr.akl.kw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
 )
 ## (4.4) Privatwald
 ### BWI 3
 assign('FVBN.bagr.akl.pw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(3, 5), Begehbar = 1, EigArt = "PW"))
 )
 ### BWI 2
 assign('FVBN.bagr.akl.pw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
 )
 ### BWI 1
 assign('FVBN.bagr.akl.pw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 160, A.b = 20), list(D.unt = 0, D.ob = 500, D.b = 500, Ndh = F),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
 )
 
 # (5) Nach Durchmesserklassen und Baumartengruppen
 ## (5.1) Gesamtwald
 ### BWI 3
 assign('FVBN.bagr.dkl.gw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(3, 5), Begehbar = 1))
 )
 ### BWI 2
 assign('FVBN.bagr.dkl.gw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1))
 )
 ### BWI 1
 assign('FVBN.bagr.dkl.gw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1))
 )
 ## (5.2) Staatswald
 ### BWI 3
 assign('FVBN.bagr.dkl.stw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(3, 5), Begehbar = 1, EigArt = "StW"))
 )
 ### BWI 2
 assign('FVBN.bagr.dkl.stw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
 )
 ### BWI 1
 assign('FVBN.bagr.dkl.stw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "StW"))
     )

 ## (5.3) Koerperschaftswald
 ### BWI 3
 assign('FVBN.bagr.dkl.kw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(3, 5), Begehbar = 1, EigArt = "KW"))
 )
 ### BWI 2
 assign('FVBN.bagr.dkl.kw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
 )
 ### BWI 1
 assign('FVBN.bagr.dkl.kw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "KW"))
     )

 ## (5.4) Privatwald
 ### BWI 3
 assign('FVBN.bagr.dkl.pw.3', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.3, ecken.3, trakte.3, A, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(3, 5), Begehbar = 1, EigArt = "PW"))
 )
 ### BWI 2
 assign('FVBN.bagr.dkl.pw.2', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.2, ecken.2, trakte.2, A.12, 2, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
 )
 ### BWI 1
 assign('FVBN.bagr.dkl.pw.1', envir = statistics_environment,
     FVBN.bagrupp.akl.dkl.stratum.fun.2a(
     baeume.1, ecken.1, trakte.1, A.12, 1, bagr.bwi,
     list(A.ob = 500, A.b = 500), list(D.unt = 0, D.ob = 70, D.b = 10, Ndh = T),
     list(Wa = c(1, 2, 3), Begehbar = 1, EigArt = "PW"))
     )
# Spezielle Baumartengruppen; Aufschlüsselung nach Arten
 assign('FVBN.bagr.spec.gw.3', envir = statistics_environment,
    FVBN.bagrupp.akl.dkl.stratum.fun.2a(
    baeume.3,ecken.3,trakte.3,A,2,get_species_groups('spec'),
    list(A.ob=500,A.b=500),list(D.unt=0,D.ob=500,D.b=500,Ndh=F),
    list(Wa=c(3,5),Begehbar=1))
    )
 assign('FVBN.bagr.spec.gw.2', envir = statistics_environment,
    FVBN.bagrupp.akl.dkl.stratum.fun.2a(
    baeume.2,ecken.2,trakte.2,A.12,2,get_species_groups('spec'),
    list(A.ob=500,A.b=500),list(D.unt=0,D.ob=500,D.b=500,Ndh=F),
    list(Wa=c(1,2,3),Begehbar=1))
    )
 assign('FVBN.bagr.spec.gw.1', envir = statistics_environment,
    FVBN.bagrupp.akl.dkl.stratum.fun.2a(
    baeume.1,ecken.1,trakte.1,A.12,1,get_species_groups('spec'),
    list(A.ob=500,A.b=500),list(D.unt=0,D.ob=500,D.b=500,Ndh=F),
    list(Wa=c(1,2,3),Begehbar=1))
    )




    save(list = ls(statistics_environment, all = TRUE), 
	 file = STATISTICS_WORKSPACE, envir = statistics_environment)
    status <- TRUE
    return(invisible(status))
}
#' create a  workspace
#'
#' A wrapper to \code{\link{create_data_workspace}} and
#' \code{\link{create_statistics_workspace}}
#' nomalizing the calls to create_workspace(TYPE) for use in loops over types.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 1f51f17d5fbfb0fd3fcd9f739eb148a6d279b585 $
#' @param type String giving the type of workspace to load. Should be in
#' c('DATA', 'STATISTICS').
#' @return  TRUE on success, FALSE otherwise. 
create_workspace <- function(type) {
    status <- FALSE
    if (! type %in% c('DATA', 'STATISTICS')) {
	message("type is not in c('DATA', 'STATISTICS')")
	return(invisible(status))
    } else {
	eval(parse(text = paste('create', tolower(type), 'workspace()', sep = '_')))
	status <- TRUE
    }
    return(invisible(status))
}

