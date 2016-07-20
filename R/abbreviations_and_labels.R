#!/usr/bin/Rscript --vanilla
#' abbreviations_and_labels.R
#'
#' helper functions converting abbreviations into labels and vice versa.
#' Operating on strings, character vectors, factors and character or factor
#' columns of data.frames.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>, Franziska Berg
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @docType data  
#' @name A Header for
NULL 

#' get abbreviation for a given label or name.
#'
#' most of our or variables names and some values are abbreviated and rather 
#' hard to understand, like for example 'oiB'. 
#' There often exist different labels or abbreviations for one value.
#' This function converts text labels (or aliases that are easier to read, like 
#' 'oberirdische Biomasse',) or ambiguous abbreviations into unambiguous 
#' abbreviations. 
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This is a helper functions, it will typically be called from within
#' other functions that received aliases as user input.
#' @param label The label to be converted. 
#' @return  a string giving the abbreviation.
get_abbreviation_for_label <- function(label) {
    return(switch(as.character(label),
                  # Attribute1 
                  'Baumartengruppenfläche' = ,
                  'Baumartengruppenflaeche' = ,
                  'Baumartenfläche' = ,
                  'Baumartenflaeche' = ,
                  'BAF' = 'BAF',
                  'Derbholzvorrat' = ,
                  'V_DhmR' = 'V_DhmR',
                  'Derbholzvorrat im Hauptbestand' = ,
                  'V_DhmR_HB' = 'V_DhmR_HB',
                  'oberirdische Biomasse' = ,
                  'oiB' = 'oiB',
                  'Stammzahl' = ,
                  'N' = 'N',
                  'Derbholzstammzahl' = ,
                  'N_Dh' = 'N_Dh',
                  # Attribute2 
                  "Derbholzvorrat je Hektar"= ,
                  'V_DhmR/ha' = 'V_DhmR/ha' ,
                  'Derbholzvorrat im Hauptbestand je Hektar' = ,
                  'V_DhmR_HB/ha' = 'V_DhmR_HB/ha',
                  'oberirdische Biomasse je Hektar' = ,
                  'oiB/ha' = 'oiB/ha',
                  'Stammzahl je Hektar' = ,
                  'N/ha' = 'N/ha',
                  'Derbholzstammzahl je Hektar' = ,
                  'N_Dh/ha' = 'N_Dh/ha',
                  # Grouping
                  'Altersklasse' = ,
                  'AKL' = 'AKL',
                  'Durchmesserklasse' = ,
                  'DKL' = 'DKL',
                  # ownership
                  'bw' = ,
                  'Bw' = ,
                  'BW' = ,
                  'Bundeswald' = 'bw',
                  'StW'         = ,
                  'STW'         = ,
                  'stw'         = ,
                  'staatswald' = ,
                  'Staatswald' = 'stw',
                  'GW'         = ,
                  'gw'         = ,
                  'gesamtwald' = ,
                  'Gesamtwald' = 'gw',
                  'KW'         = ,
                  'kw'         = ,
                  'körperschaftswald' = ,
                  'Körperschaftswald' = ,
                  'koerperschaftswald' = ,
                  'Koerperschaftswald' = ,
                  'kommunalwald' = ,
                  'Kommunalwald' = 'kw',
                  'pw'         = ,
                  'PW'         = ,
                  'privatwald' = ,
                  'Privatwald' = 'pw',
                  'Kleinprivatwald' = ,
                  'KPW' =, 
                  'kpw' = 'kpw',
                  'Mittlerer Privatwald' = ,
                  'Mittl. Privatwald' = ,
                  'MPW' =, 
                  'mpw' = 'mpw',
                  'Großprivatwald' = ,
                  'GPW' =, 
                  'gpw' = 'gpw',
                  # Default
                  stop(paste('unkown label', label))
                  )
    )
}

#' get an axis label for a given abbreviation or label
#'
#' This function converts abbreviations or labels into axis labels  
#' with measurement units.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This is a helper functions, it will typically be called from within
#' other functions that plot axes with labels.
#' @param abbreviation A string giving the abbreviation to be converted.
#' @return  a string giving the axis label. This will be the abbreviation or
#' label given if no match was found.
get_axis_label_for_abbreviation <- function(abbreviation) {
    return(
           switch(get_abbreviation_for_label(abbreviation),
                  # Attribute1 
                  'Baumartengruppenfläche' = ,
                  'BAF' = 'Fläche [ha]',
                  'Derbholzvorrat' = ,
                  'V_DhmR' = expression(paste("Derbholzvorrat [", m^3, "]",  
                                              sep = "")
                  ),
                  'Derbholzvorrat im Hauptbestand' = ,
                  'V_DhmR_HB' = expression(paste("Derbholzvorrat [", m^3, "]",
                                                 "im Hauptbestand",  
                                                 sep = "")
                  ),
                  # Attribute2 
                  'Derbholzvorrat je Hektar' = ,
                  'V_DhmR/ha' = expression(paste("Derbholzvorrat [", m^3,
                                                 ha^-1, "]",  
                                                 sep = "")
                  ),
                  'Derbholzvorrat im Hauptbestand je Hektar' = ,
                  'V_DhmR_HB/ha' = expression(paste("Derbholzvorrat [", m^3, 
                                                    ha^-1, "]",
                                                    "im Hauptbestand",  
                                                    sep = "")
                  ),
                  get_text_label_for_abbreviation(abbreviation)
                  )
           )
}

#' get a label for a given abbreviation 
#'
#' Most of our attribute or variables names and some attribute values  are
#' abbreviations and rather hard to understand, like for example 'oiB'. 
#' This function converts abbreviations into labels that are easier to read,
#' This function converts abbreviations into  labels.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This is a helper functions, it will typically be called from within
#' other functions that received abbreviations as user input.
#' @param abbreviation A string giving the abbreviation to be converted.
#' @return  a string giving the label. 
get_text_label_for_abbreviation <- function(abbreviation) {
    return(
           switch(abbreviation,
                  # Attribute1 
                  'BAF' = 'Baumartengruppenfläche',
                  'V_DhmR' = "Derbholzvorrat",
                  'V_DhmR_HB' = 'Derbholzvorrat im Hauptbestand',
                  'oiB' = 'oberirdische Biomasse',
                  'N' = 'Stammzahl',
                  'N_Dh' = 'Derbholzstammzahl',
                  # Attribute2 
                  'V_DhmR/ha' = "Derbholzvorrat je Hektar",
                  'V_DhmR_HB/ha' = 'Derbholzvorrat im Hauptbestand je Hektar',
                  'oiB/ha' = 'oberirdische Biomasse je Hektar',
                  'N/ha' = 'Stammzahl je Hektar',
                  'N_Dh/ha' = 'Derbholzstammzahl je Hektar',
                  # Grouping
                  'AKL' = 'Altersklasse',
                  'DKL' = 'Durchmesserklasse',
                  # ownership
                  'bw' = ,
                  'BW' = 'Bundeswald',
                  'GW' = ,
                  'gw' = 'Gesamtwald',
                  'KW' = ,
                  'kw' = 'Körperschaftswald',
                  'PW' = ,
                  'pw' = 'Privatwald',
                  'StW' = ,
                  'STW' = ,
                  'stw' = 'Staatswald',
                  'KPW' = ,
                  'kpw' = 'Kleinprivatwald',
                  'MPW' = ,
                  'mpw' = 'Mittl. Privatwald',
                  'GPW' = ,
                  'gpw' = 'Großprivatwald',
                  # tree species group
                  'FI' = 'Fichte',
                  'TA' = 'Tanne',
                  'LAE' = 'Lärche',
                  'DGL' = 'Douglasie',
                  'EI' = 'Eiche',
                  'BU' = 'Buche',
                  'KI' = 'Kiefer',
                  'HBu' = ,
                  'Hbu' = 'Hainbuche',
                  'sNB' = ,
                  'Lärchen/sNB' = "Lärchen/Sonstiges Nadelholz",
                  'SNB' = 'Sonstiges Nadelholz',
                  'sBLB' = ,
                  'sBlb' = ,
                  'SBLB' = 'Sonstiges Buntlaubholz',
                  'Alh' = ,
                  'ALH' = 'Anderes Laubholz höherer Lebensdauer',
                  'Aln' = ,
                  'ALN' = 'Anderes Laubholz niederer Lebensdauer',
                  'Alle BA' = 'Alle Baumarten',
                  # Default
                  abbreviation
                  )
           )
}

#' get a text or axis label for a given abbreviation 
#'
#' This function is just a wrapper for get_axis_label_for_abbreviation and
#' get_text_label_for_abbreviation.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This is a helper functions, it will typically be called from within
#' other functions that received abbreviations as user input.
#' @param abbreviation A string giving the abbreviation to be converted.
#' @param label_type A string giving the label type. Should be one of
#' c('text_label', 'axis_label').
#' @return  a string giving the text or axis label. 
get_label_for_abbreviation <- function(abbreviation, 
                                       label_type = 'text_label') {
    return(
           switch(label_type,
                  'axis_label' = get_axis_label_for_abbreviation(abbreviation),
                  'text_label' =  get_text_label_for_abbreviation(abbreviation),
                  stop(paste('unkown name', name))
                  )
           )
}

#' map text labels to abbreviations
#'
#' This function is a wrapper for get_abbreviation_for_label working on
#' character vectors or factors.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This is a helper functions, it will typically be called from within
#' other functions that received labels as user input.
#' @param x a factor or character vector containing labels to be mapped.
#' @return  a factor or character vector containing the abbreviations. 
map_labels_to_abbreviations <- function(x) {
    if (is.factor(x)) {
        levels(x) <- vapply(levels(x), get_abbreviation_for_label,
                            character(length = 1)
                            )
    } else {
        x <- vapply(as.character(x), get_abbreviation_for_label,
                    character(length = 1)
                    )
    }
    return(x)
}

#' map abbreviations to text labels
#'
#' This function is a wrapper for get_text_label_for_abbreviation working on
#' character vectors or factors.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This is a helper functions, it will typically be called from within
#' other functions that received abbreviations as user input.
#' @param x a factor or character vector containing abbreviations to be
#' mapped.
#' @return  a factor or character vector containing the text labels. 
map_abbreviations_to_labels <- function(x) {
    if (is.factor(x)) {
        levels(x) <- vapply(levels(x), get_text_label_for_abbreviation,
                            character(length = 1)
                            )
    } else {
        x <- vapply(as.character(x), get_text_label_for_abbreviation,
                    character(length = 1)
                    )
    }
    return(x)
}

#' convert values in a data frame's column from abbreviations to text labels
#'
#' This function is a wrapper for map_abbreviations_to_labels.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7ecc2f88b34bff6fb25034fcd80f30997dc7cf44 $
#' @note This function will change the values in the data.frame!
#' @param datacolumn the data frame column to convert.  _Not_ a string giving
#' the data frame's column name. 
#' @return  TRUE on success, FALSE otherwise. 
revalue_data <- function(datacolumn) {
    status <- FALSE
    eval.parent(substitute(datacolumn <- map_abbreviations_to_labels(datacolumn)))
    status <- TRUE
    return(invisible(status))
}

#' Get color for ownership.
#' 
#' Get the color for a certain ownership to use in graphics.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param ownership The label describing the ownership.
#' @return ownership_colors[index] Which holds the information about the color
#'   for the wanted ownership (\code{index}).
#' @examples
#' ownership <- "Mittlerer Privatwald"
#' color <- get_color_for_ownership(ownership)
get_color_for_ownership <- function (ownership) {
    ownership_colors <- c("gw" = 'lightgreen',
                          "bw" = "darkgreen",
                          "stw" = "green", 
                          "kw" = "orange", 
                          "pw" = "blue", 
                          "gpw" = "royalblue",
                          "mpw" = "darkblue", 
                          "kpw" = "lightblue")
    index <- which(names(ownership_colors) == get_abbreviation_for_label(ownership))
    return(ownership_colors[index])
}

#' Get colors for ownership.
#' 
#' Get the color for certain ownerships to use in graphics.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param ownership A vector containing one or more ownerships.
#' @return A vector containing the colors for the ownerships, indexed by the
#'   abbreviations for the ownerships.
#' @examples
#' ownership <- c("Kleinprivatwald", "Mittlerer Privatwald")
#' colors <- get_colors_for_ownership(ownership)
get_colors_for_ownership <- function (x) {
    y <- vapply(as.character(x), get_color_for_ownership,
                character(length = 1)
                )
    return(y)
}
