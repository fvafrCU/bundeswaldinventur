#' Sets plot axes bold
#' 
#' Function produces a theme which sets the axes of a plot bold (black colour 
#' and letter size 20).
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @export
#' @return theme which sets axes of a plot bold.
theme_bold_axes <- function() {
    return(
	   ggplot2::theme(axis.title =  ggplot2::element_text(colour = "black", size = 20),
		 axis.text =  ggplot2::element_text(colour = "black", size = 20))
	   )
}

#' Sets plot legend bold
#' 
#' Function produces a theme which sets the legend of a plot bold (black 
#' colour and letter size 16).
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @export
#' @return theme which sets legend of a plot bold.
theme_bold_legend <- function() {
    return(
	   ggplot2::theme(legend.title = ggplot2::element_text(colour = "black", size = 16),
		 legend.text = ggplot2::element_text(colour = "black", size = 16)
		 )
	   )
}

#' Sets plot title bold
#' 
#' Function produces a theme which sets the title of a plot bold (black colour 
#' and letter size 20).
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @export
#' @return theme which sets axes of a plot bold.
theme_bold_title <- function() {
    return(
	   ggplot2::theme(plot.title = ggplot2::element_text(colour = "black", size = 20))
	   )
}

#' Change axes, title and legend style of a plot.
#' 
#' Function changes axes, title and legend styles of a plot. Styles are definded 
#' in \code{\link{theme_bold_axes}}, \code{\link{theme_bold_title}} and 
#' \code{\link{theme_bold_legend}}. 
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param obj Grafic defined by ggplot().
#' @export
#' @return Grafic with predefined axes, title and legend style.
tplot <- function(obj) {
    graphics::plot(obj + theme_bold_axes() + theme_bold_title() + theme_bold_legend() +
	 ggplot2::theme(strip.text.y = ggplot2::element_text(size=12))
	 )
}

#' Predicts values for species grouped by a variable and BWI 
#' 
#' Function predicts a value which is speciefied by the user for a variable 
#' species. The prediction is done grouped by the different BWI's and a grouping 
#' variable, given by the user. 
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param list Output (List) of BWI3_HR_Funktionen (e.g. 
#'  \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2a}}).
#' @param species String which specifies the tree species for which calculation 
#'  shall be done. Possible inputs: FI TA DGL KI LAE BU EI ALH ALN.
#' @param abbreviation String which speciefies the value for which the 
#'  calculation shall be done. Possible input depends on the existing values in 
#'  \code{list}.
#' @param group String which specifies the value by which the calculation shall 
#'  be grouped.
#' @export
#' @return Data frame with prediction for the given \code{abbreviation} and 
#'  \code{species}, grouped by \code{group} and BWI.
melt_species_attribute_group <- function(list, species, abbreviation, group) {
    bwi1 <- eval(parse(text = paste(list, 1, sep = ".")))
    bwi2 <- eval(parse(text = paste(list, 2, sep = ".")))
    bwi3 <- eval(parse(text = paste(list, 3, sep = ".")))
    owner_postfix <- gsub("^.*\\.", "", list )
    if (species %in% bwi1$BAGR) {
	species_index <- which(species == bwi1$BAGR)  
    } else {
	throw(cat("got unkown species", species, ". Should be one of",
		 bwi1$BAGR, "."
		 )
	)
    }
    if (abbreviation %in% bwi1$Attribute1) {## Totals
	attribute_index <- which(abbreviation == bwi1$Attribute1)  
	array_containing_statistics  <-  "T.FVBN.Bagr.Akl.Dkl"
    } else {## per hectar
	attribute_index <- which(abbreviation == bwi1$Attribute2)  
	array_containing_statistics  <-  "FVBN.ha.Bagr.Akl.Dkl"
    }

    bwi1_statistics <- data.frame(prediction = get(array_containing_statistics, 
						   bwi1)[attribute_index,
				  1, species_index,
				  , ],
    standard_error = get(array_containing_statistics, bwi1)[attribute_index,
				  2, species_index,
				  , ],
    abbreviation = abbreviation,
    bwi = "1987" 
    )
    bwi2_statistics <- data.frame(prediction = get(array_containing_statistics, bwi2)[attribute_index,
				  1, species_index,
				  , ],
    standard_error = get(array_containing_statistics, bwi2)[attribute_index,
				  2, species_index,
				  , ],
    abbreviation = abbreviation,
    bwi = "2002" 
    )
    bwi3_statistics <- data.frame(prediction = get(array_containing_statistics, bwi3)[attribute_index,
				  1, species_index,
				  , ],
    standard_error = get(array_containing_statistics, bwi3)[attribute_index,
				  2, species_index,
				  , ],
    abbreviation = abbreviation,
    bwi = "2012" 
    )
    data_frame <- rbind(bwi1_statistics, bwi2_statistics, bwi3_statistics)
    data_frame$group <- factor(get(group, bwi3), levels = get(group, bwi3))
    data_frame$species <- get_data("bagr.bwi")$ba.text[which(get_data("bagr.bwi")$bagr.lab == species)]
    data_frame$grouping_variable <- group
    data_frame$ownership <- get_label_for_abbreviation(owner_postfix,
						      "text_label")
    return(data_frame)



}

#' Plots the output of \code{\link{melt_species_attribute_group}}
#' 
#' Function plots the output of \code{\link{melt_species_attribute_group}}. 
#' Different BWI's are shown in three different green shades. The grouping value 
#' is shown on the x axis and the prediction is the y value.
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param data Output Dataframe of \code{\link{melt_species_attribute_group}}.
#' @export
#' @return Plot which shows calculation of 
#'  \code{\link{melt_species_attribute_group}}.
plot_species_attribute_group <- function(data) {
    return(
	   ggplot2::ggplot(data = data,
		  ggplot2::aes_string(y = "prediction",
		      x = "group",
		      fill = factor("bwi")
		      ), 
		  group = "group") +
	   ggplot2::geom_bar(stat = "identity", position =  ggplot2::position_dodge()) +
	   ggplot2::geom_errorbar(ggplot2::aes(ymin = "prediction" - "standard_error",
			     ymax = "prediction" + "standard_error"
			     ),
			 width = .3,
			 position = ggplot2::position_dodge(width=0.9)
			 ) +
	   ggplot2::scale_fill_manual(values = get_data("colors_bwi"), name = "BWI") +
	   ggplot2::theme(axis.text.x  = ggplot2::element_text(size = 8)) +
	   ggplot2::xlab(get_label_for_abbreviation(as.character(unique(data$grouping_variable)),
					  "text_label")) + 
	   ggplot2::scale_y_continuous(labels = scales::comma) +
	   ggplot2::ylab(get_label_for_abbreviation(as.character(unique(data$abbreviation)),
					  "axis_label")) + 
	   ggplot2::ggtitle(paste("Baumartengruppe", unique(data$species), "im" ,
			 unique(data$ownership)))
	   )
}

#' Reassembles by ownership and a variable abbreviation
#' 
#' Fix me! What do I do? 
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param owner String which defines the owner.
#' @param abbreviation String which defines the variable which shall be 
#'  predicted.
#' @export
#' @return Dataframe.
reassemble_by_group <- function(owner, abbreviation) {
    ownership <- get_abbreviation_for_label(owner)
    data_list <- paste("FVBN.bagr", ownership, sep = ".")
    bwi1 <- eval(parse(text = paste(data_list, 1, sep = ".")))
    bwi2 <- eval(parse(text = paste(data_list, 2, sep = ".")))
    bwi3 <- eval(parse(text = paste(data_list, 3, sep = ".")))
    if (abbreviation %in% bwi1$Attribute1) {## Totals
	index <- which(abbreviation == bwi1$Attribute1)  
	statistics_array  <-  "T.FVBN.Bagr.Akl.Dkl"
    } else {## per hectar
	index <- which(abbreviation == bwi1$Attribute2)  
	statistics_array  <-  "FVBN.ha.Bagr.Akl.Dkl"
    }
    data_frame <- 
	Reduce(function(...) merge(..., all = T), 
	       list(
		    data.frame(bwi = "1987",
			       Baumartengruppe = factor(bwi1$BAGR, 
							levels = bwi1$BAGR),
			       prediction = get(statistics_array,
						bwi1)[index, 1, , , ],
			       standard_error = get(statistics_array,
						    bwi1)[index, 2, , , ]
			       ),
		    data.frame(bwi = "2002",
			       Baumartengruppe = factor(bwi2$BAGR, 
							levels = bwi2$BAGR),
			       prediction = get(statistics_array,
						bwi2)[index, 1, , , ],
			       standard_error = get(statistics_array,
						    bwi2)[index, 2, , , ]
			       ),
		    data.frame(bwi = "2012",
			       Baumartengruppe = factor(bwi3$BAGR, 
							levels = bwi3$BAGR),
			       prediction = get(statistics_array,
						bwi3)[index, 1, , , ],
			       standard_error = get(statistics_array,
						    bwi3)[index, 2, , , ]
			       )
		    )
	       )
    data_frame$Eigentumsart <- get_label_for_abbreviation(owner, "text_label")
    data_frame$abbreviation <- get_label_for_abbreviation(abbreviation, "text_label")
    return(data_frame)
}

#' Predicts value of a variable grouped by a tree species, BWI and owner 
#' 
#' Function predicts a variable and it's standard error grouped by tree species
#' and BWI.
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param abbreviation String which defines the variable which shall be 
#'  predicted.
#' @export
#' @return Data frame with prediction for the given \code{abbreviation}, grouped 
#'  by tree species group, BWI and owner.
all_data_by_group <- function(abbreviation) {
    attribute_name <- get_abbreviation_for_label(abbreviation)
    owners <- c("gw", "stw", "kw", "pw")
    data_list <- lapply(owners, reassemble_by_group, attribute_name)
    data_frame <- do.call("rbind", data_list)
    return(data_frame)
}

#' Plots data by group
#' 
#' Function creates a bar diagramm plot of the output of 
#' \code{\link{all_data_by_group}}. The predicted value for the abbreviation is 
#' used to define the height of the bars. The bars are grouped by 
#' "Baumartengruppe" (by default) or another parameter.
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param data Output table of \code{\link{all_data_by_group}}
#' @param subs String which defines subset parameters, e.g. "Eigentumsart == 
#'  'Privatwald'".
#' @param x String which defines the column in data which shall be used for the 
#'  grouping on the x-axis. by default "Baumartengruppe"
#' @export
#' @return Bar plot
plot_by_group <- function(data, subs, x = "Baumartengruppe") {
    return(
	   ggplot2::ggplot(subset(data, eval(parse(text = subs))),
		  ggplot2::aes_string(y = "prediction",
			     x = x, #deparse(substitute(g)),
			     fill = "factor(bwi)"
			     ), 
		  group = deparse(substitute(x))) +
	   ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
	   ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "prediction" - "standard_error",
			     ymax = "prediction" + "standard_error"
			     ),
			 width = .3,
			 position = ggplot2::position_dodge(width=0.9)
			 ) +
	   ggplot2::scale_fill_manual(values = get_data("colors_bwi"), name = "BWI") +
	   ggplot2::scale_y_continuous(labels = scales::comma) +
	   ggplot2::xlab(x) +
	   ggplot2::ylab(get_label_for_abbreviation(unique(data$abbreviation), "axis_label")) +
	   ggplot2::ggtitle(gsub("[^A-z ]","", subs))
	   )
}

#' Xtable by group
#' 
#' Fix me
#' 
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param data Output table of \code{\link{all_data_by_group}}
#' @param subs String which defines subset parameters, e.g. "Eigentumsart == 
#'  'Privatwald'".
#' @param group String which defines the column in data which shall be used for the 
#'  grouping on the x-axis. by default "Baumartengruppe"
#' @param label_prefix String which defines the prefix for teh label.
#' @export
#' @return Fix me.
xtable_by_group <- function(data, subs, group = "Baumartengruppe",
			    label_prefix = "total") {
    data_received <- subset(data, eval(parse(text = subs)))
    names(data_received)[which(names(data_received) == group)] <- "grouping"

    data_molten <- reshape2::melt(data_received, 
				  measure.vars = c("prediction", 
						   "standard_error"
						   )
				  )

    data_casted <- reshape2::dcast(data_molten[data_molten[["variable"]] == "prediction", TRUE], 
                                   value.var = "value",
			 grouping ~ bwi )

    data_casted <- rbind(data_casted, c(NA, colSums(data_casted[ , -1])))
    data_casted$"1987 - 2002" <- (data_casted[, 3]-data_casted[, 2]) / data_casted[, 2] * 100
    data_casted$"2002 - 2012" <-(data_casted[, 4]-data_casted[, 3]) / data_casted[, 3] * 100
    data_casted$"1987 - 2012" <-(data_casted[, 4]-data_casted[, 2]) / data_casted[, 2] * 100
    latex_table <- xtable::xtable(prettyNum(data_casted, big.mark = ","),
			  label = paste("tab", label_prefix,
					unique(data_received$abbreviation), sep = ":"), 
			  caption = get_text_label_for_abbreviation(unique(data_received$abbreviation)),
			  ) 

    return(latex_table)

}

