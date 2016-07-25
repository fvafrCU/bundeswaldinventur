
# @NULL 
theme_bold_axes <- function() {
    return(
	   theme(axis.title =  element_text(colour = "black", size = 20),
		 axis.text =  element_text(colour = "black", size = 20))
	   )
}

theme_bold_legend <- function() {
    return(
	   theme(legend.title = element_text(colour = "black", size = 16),
		 legend.text = element_text(colour = "black", size = 16)
		 )
	   )
}

theme_bold_title <- function() {
    return(
	   theme(plot.title = element_text(colour = "black", size = 20))
	   )
}

tplot <- function(obj) {
    graphics::plot(obj + theme_bold_axes() + theme_bold_title() + theme_bold_legend() +
	 theme(strip.text.y = element_text(size=12))
	 )
}

# by abbreviation, group and bwi
melt_species_attribute_group <- function(list, species, abbreviation, group) {
    bwi1 <- eval(parse(text = paste(list, 1, sep = '.')))
    bwi2 <- eval(parse(text = paste(list, 2, sep = '.')))
    bwi3 <- eval(parse(text = paste(list, 3, sep = '.')))
    owner_postfix <- gsub("^.*\\.", '', list )
    if (species %in% bwi1$BAGR) {
	species_index <- which(species == bwi1$BAGR)  
    } else {
	stop(cat('got unkown species', species, '. Should be one of',
		 bwi1$BAGR, '.'
		 )
	)
    }
    if (abbreviation %in% bwi1$Attribute1) {## Totals
	attribute_index <- which(abbreviation == bwi1$Attribute1)  
	array_containing_statistics  <-  'T.FVBN.Bagr.Akl.Dkl'
    } else {## per hectar
	attribute_index <- which(abbreviation == bwi1$Attribute2)  
	array_containing_statistics  <-  'FVBN.ha.Bagr.Akl.Dkl'
    }

    bwi1_statistics <- data.frame(prediction = get(array_containing_statistics, 
						   bwi1)[attribute_index,
				  1, species_index,
				  , ],
    standard_error = get(array_containing_statistics, bwi1)[attribute_index,
				  2, species_index,
				  , ],
    abbreviation = abbreviation,
    bwi = '1987' 
    )
    bwi2_statistics <- data.frame(prediction = get(array_containing_statistics, bwi2)[attribute_index,
				  1, species_index,
				  , ],
    standard_error = get(array_containing_statistics, bwi2)[attribute_index,
				  2, species_index,
				  , ],
    abbreviation = abbreviation,
    bwi = '2002' 
    )
    bwi3_statistics <- data.frame(prediction = get(array_containing_statistics, bwi3)[attribute_index,
				  1, species_index,
				  , ],
    standard_error = get(array_containing_statistics, bwi3)[attribute_index,
				  2, species_index,
				  , ],
    abbreviation = abbreviation,
    bwi = '2012' 
    )
    data_frame <- rbind(bwi1_statistics, bwi2_statistics, bwi3_statistics)
    data_frame$group <- factor(get(group, bwi3), levels = get(group, bwi3))
    data_frame$species <- BAGR.BWI$ba.text[which(BAGR.BWI$bagr.lab == species)]
    data_frame$grouping_variable <- group
    data_frame$ownership <- get_label_for_abbreviation(owner_postfix,
						      'text_label')
    return(data_frame)



}

plot_species_attribute_group <- function(data) {
    return(
	   ggplot(data = data,
		  aes(y = prediction,
		      x = group,
		      fill = factor(bwi)
		      ), 
		  group = group) +
	   geom_bar(stat = 'identity', position =  position_dodge()) +
	   geom_errorbar(aes(ymin = prediction - standard_error,
			     ymax = prediction + standard_error
			     ),
			 width = .3,
			 position = position_dodge(width=0.9)
			 ) +
	   scale_fill_manual(values = COLORS_BWI, name = 'BWI') +
	   theme(axis.text.x  = element_text(size = 8)) +
	   xlab(get_label_for_abbreviation(as.character(unique(data$grouping_variable)),
					  'text_label')) + 
	   scale_y_continuous(labels = comma) +
	   ylab(get_label_for_abbreviation(as.character(unique(data$abbreviation)),
					  'axis_label')) + 
	   ggtitle(paste('Baumartengruppe', unique(data$species), 'im' ,
			 unique(data$ownership)))
	   )
}

# by  group and bwi
reassemble_by_group <- function(owner, abbreviation) {
    ownership <- get_abbreviation_for_label(owner)
    data_list <- paste('FVBN.bagr', ownership, sep = '.')
    bwi1 <- eval(parse(text = paste(data_list, 1, sep = '.')))
    bwi2 <- eval(parse(text = paste(data_list, 2, sep = '.')))
    bwi3 <- eval(parse(text = paste(data_list, 3, sep = '.')))
    if (abbreviation %in% bwi1$Attribute1) {## Totals
	index <- which(abbreviation == bwi1$Attribute1)  
	statistics_array  <-  'T.FVBN.Bagr.Akl.Dkl'
    } else {## per hectar
	index <- which(abbreviation == bwi1$Attribute2)  
	statistics_array  <-  'FVBN.ha.Bagr.Akl.Dkl'
    }
    data_frame <- 
	Reduce(function(...) merge(..., all = T), 
	       list(
		    data.frame(bwi = '1987',
			       Baumartengruppe = factor(bwi1$BAGR, 
							levels = bwi1$BAGR),
			       prediction = get(statistics_array,
						bwi1)[index, 1, , , ],
			       standard_error = get(statistics_array,
						    bwi1)[index, 2, , , ]
			       ),
		    data.frame(bwi = '2002',
			       Baumartengruppe = factor(bwi2$BAGR, 
							levels = bwi2$BAGR),
			       prediction = get(statistics_array,
						bwi2)[index, 1, , , ],
			       standard_error = get(statistics_array,
						    bwi2)[index, 2, , , ]
			       ),
		    data.frame(bwi = '2012',
			       Baumartengruppe = factor(bwi3$BAGR, 
							levels = bwi3$BAGR),
			       prediction = get(statistics_array,
						bwi3)[index, 1, , , ],
			       standard_error = get(statistics_array,
						    bwi3)[index, 2, , , ]
			       )
		    )
	       )
    data_frame$Eigentumsart <- get_label_for_abbreviation(owner, 'text_label')
    data_frame$abbreviation <- get_label_for_abbreviation(abbreviation, 'text_label')
    return(data_frame)
}

all_data_by_group <- function(abbreviation) {
    attribute_name <- get_abbreviation_for_label(abbreviation)
    owners <- c('gw', 'stw', 'kw', 'pw')
    data_list <- lapply(owners, reassemble_by_group, attribute_name)
    data_frame <- do.call('rbind', data_list)
    return(data_frame)
}

plot_by_group <- function(data, subs, x = "Baumartengruppe") {
    return(
	   ggplot(subset(data, eval(parse(text = subs))),
		  aes_string(y = "prediction",
			     x = x, #deparse(substitute(g)),
			     fill = "factor(bwi)"
			     ), 
		  group = deparse(substitute(x))) +
	   geom_bar(stat = 'identity', position = position_dodge()) +
	   geom_errorbar(aes(ymin = prediction - standard_error,
			     ymax = prediction + standard_error
			     ),
			 width = .3,
			 position = position_dodge(width=0.9)
			 ) +
	   scale_fill_manual(values = COLORS_BWI, name = 'BWI') +
	   scale_y_continuous(labels = comma) +
	   xlab(x) +
	   ylab(get_label_for_abbreviation(unique(data$abbreviation), 'axis_label')) +
	   ggtitle(gsub('[^A-z ]','', subs))
	   )
}

xtable_by_group <- function(data, subs, group = "Baumartengruppe",
			    label_prefix = 'total') {
    data_received <- subset(data, eval(parse(text = subs)))
    names(data_received)[which(names(data_received) == group)] <- 'grouping'

    data_molten <- reshape2::melt(data_received, 
				  measure.vars = c('prediction', 
						   'standard_error'
						   )
				  )

    data_casted <- reshape2::dcast(subset(data_molten, variable == 'prediction'), value.var = 'value',
			 grouping ~ bwi )

    data_casted <- rbind(data_casted, c(NA, colSums(data_casted[ , -1])))
    data_casted$'1987 - 2002' <- (data_casted[, 3]-data_casted[, 2]) / data_casted[, 2] * 100
    data_casted$'2002 - 2012' <-(data_casted[, 4]-data_casted[, 3]) / data_casted[, 3] * 100
    data_casted$'1987 - 2012' <-(data_casted[, 4]-data_casted[, 2]) / data_casted[, 2] * 100
    latex_table <- xtable::xtable(prettyNum(data_casted, big.mark = ","),
			  label = paste('tab', label_prefix,
					unique(data_received$abbreviation), sep = ':'), 
			  caption = get_text_label_for_abbreviation(unique(data_received$abbreviation)),
			  ) 

    return(latex_table)

}

