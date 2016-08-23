#' tree_species_shares.R
#'
#' reproduce tables 6 through 10 of
#' Landesspezifische_Auswertung_Bundeswaldinventur_3_Ba_Wue_2014_v1.docx
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @section Version: $Id: fec000cba1ad6b39cf9bca1e63e0b1babb7e8728 $
#' @docType data 
#' @name Header
NULL 

#' Creates vektor with percentage values for tree species
#' 
#' Function selects area for each tree from the statistical list and calculates 
#' the percentages.
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @param statistics_list Name of the list holding the statistical data. 
#' @return vektor with tree species areas in percent.
tree_species_percent <- function(statistics_list){
  checkmate::assertList(statistics_list)
    tree_species_areas <- get('T.FVBN.Bagr.Akl.Dkl', 
			      statistics_list
			      )[1, 1, , , ]
    total_area <- sum(tree_species_areas)
    return(tree_species_areas / total_area * 100)
}

#' Calculates relative tree species area
#' 
#' Function calculates the relative tree species area for each tree species 
#' group for all three BWI's.
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @param statistics_list String which defines the list holding the statistical 
#'  data.
#' @return Data Frame with information about relative tree species area for each 
#'  species. Information for all three BWI's.
melt_tree_species_percent <- function(statistics_list) {
  checkmate::assertString(statistics_list)
  tree_species_labels <- factor(bagr.bwi$bagr.lab,
                                levels = bagr.bwi$bagr.lab)
  
    return(
	Reduce(function(...) merge(..., all = T), 
	       list(
		    data.frame(bwi = '1987',
			       group = tree_species_labels,
			       value =
			       tree_species_percent(eval(
							 parse(
							       text = 
							       paste(statistics_list,
								     1, 
								     sep = '.')))
			       )
			       ),
		    data.frame(bwi = '2002',
			       group = tree_species_labels,
			       value =
			       tree_species_percent(eval(
							 parse(
							       text = 
							       paste(statistics_list,
								     2, 
								     sep = '.')))
			       )
			       ),
		    data.frame(bwi = '2012',
			       group = tree_species_labels,
			       value =
			       tree_species_percent(eval(
							 parse(
							       text = 
							       paste(statistics_list,
								     3, 
								     sep = '.')))
			       )
			       )
		    )
	       )
    )
}

#' Calculates relative tree species area
#' 
#' Function calculates the relative tree species area for each tree species 
#' group for all three BWI's. BWI's just given through the different years: 
#' 1987, 2002 and 2012.
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @param statistics_list String which defines the list holding the statistical 
#'  data.
#' @return Data Frame with information about relative tree species area for each 
#'  species. Information for all three BWI's.
cast_tree_species_percent <- function(statistics_list) {
  checkmate::assertString(statistics_list)
    molten_data <- melt_tree_species_percent(statistics_list) 
	    casted_data <- reshape2::dcast(molten_data, value.var = 'value',  group ~ bwi )
    names(casted_data)[1] <- 'Baumartengruppe'
    return(casted_data)
}
