# @NULL 

tree_species_labels <- factor(bagr.bwi$bagr.lab,
			      levels = bagr.bwi$bagr.lab) 

# data for table 7
tree_species_percent <- function(statistics_list){
    tree_species_areas <- get('T.FVBN.Bagr.Akl.Dkl', 
			      statistics_list
			      )[1, 1, , , ]
    total_area <- sum(tree_species_areas)
    return(tree_species_areas / total_area * 100)
}

melt_tree_species_percent <- function(statistics_list) {
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

cast_tree_species_percent <- function(statistics_list) {
    molten_data <- melt_tree_species_percent(statistics_list) 
	    casted_data <- reshape2::reshape2::dcast(molten_data, value.var = 'value',  group ~ bwi )
    names(casted_data)[1] <- 'Baumartengruppe'
    return(casted_data)
}
