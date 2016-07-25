#' Get the abbreviation of a label.
#' 
#' Replace a BWI label with the appropriate abbreviation.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param label The label which shall be replaced.
#' @return The abbreviation as character. If \code{label} is not known, the
#'   function will return 'unknown label'.
#' @export
#' @examples
#' label <- "Baumartenflaeche"
#' print(bundeswaldinventur::get_abbreviation_for_label(label))
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

#' Get axis label for abbreviation.
#'
#' Get the axis label for a abbreviation containing a label with the appropriate 
#' unit.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param abbreviation [character] A abbreviation which shall be replaced by an
#'  axis label.
#' @return An expression holding a label and an approriate unit or a label as
#'  charater. If \code{abbreviation} is not known, it will be returned.
#' @export
#' @examples
#' abbreviation <- "V_DhmR"
#' label <- get_axis_label_for_abbreviation(abbreviation)
get_axis_label_for_abbreviation <- function(abbreviation) {
    return(
           switch(bundeswaldinventur::get_abbreviation_for_label(abbreviation),
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

#' Get the text label for a abbreviation.
#' 
#' Replace an abbreviation with the appropriate text label.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param abbreviation The abbreviation which shall be replaced.
#' @return The label as character. If \code{label} is not known, the
#'   abbreviation will be returned.
#' @export
#' @examples
#' abbreviation <- "V_DhmR"
#' print(get_text_label_for_abbreviation(abbreviation))
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

#' Get the label for a abbreviation.
#' 
#' Replace an abbreviation with the appropriate label.
#' 
#' Depending on the label type, the function calls 
#' \code{\link{get_axis_label_for_abbreviation}} or 
#' \code{\link{get_text_label_for_abbreviation}}. Possible label types are 
#' 'axis_label' and 'text_label' which is the default.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param abbreviation The abbreviation which shall be replaced.
#' @param label_type Type of the label.
#' @return An expression holding a label and an approriate unit or a label as 
#'   charater depending on the \code{label_type}. If \code{label_type} is not 
#'   known, 'unknown name' and the unknown label type are returned as character
#'   vector.
#' @export
#' @examples
#' abbreviation <- "V_DhmR"
#' label <- get_label_for_abbreviation(abbreviation)
#' print(get_label_for_abbreviation(abbreviation, label_type = 'text_label'))
#' label <- get_label_for_abbreviation(abbreviation, label_type = 'axis_label')
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

#' Get abbreviations of different labels.
#' 
#' Replace labels with the appropriate abbreviations.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param x A factor or vector containing labels.
#' @return A factor or vector containing abbreviations.
#' @export
#' @examples
#' x <- c("Derbholzvorrat", "Durchmesserklasse", "Mittlerer Privatwald")
#' abbreviations <- map_labels_to_abbreviations(x)
map_labels_to_abbreviations <- function(x) {
    if (is.factor(x)) {
        levels(x) <- vapply(levels(x), bundeswaldinventur::get_abbreviation_for_label,
                            character(length = 1)
                            )
    } else {
        x <- vapply(as.character(x), bundeswaldinventur::get_abbreviation_for_label,
                    character(length = 1)
                    )
    }
    return(x)
}

#' Get labels for different abbreviations.
#' 
#' Replace abbreviations with the appropriate text labels.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param x A factor or vector containing abbreviations.
#' @return A factor or vector containing labels.
#' @export
#' @examples
#' x <- c("V_DhmR", "DKL", "MPW")
#' labels <- map_abbreviations_to_labels(x)
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

#' Revalue abbreviations to labels.
#' 
#' Replace abbreviations with the appropriate text labels.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param datacolumn Datacolumn of a table containing abbreviations.
#' @return invisible(status) Which becomes TRUE when the abbrevations were
#'   successfully replaced.
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
#' @export
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
    index <- which(names(ownership_colors) == bundeswaldinventur::get_abbreviation_for_label(ownership))
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
#' @export
#' @examples
#' ownership <- c("Kleinprivatwald", "Mittlerer Privatwald")
#' colors <- get_colors_for_ownership(ownership)
get_colors_for_ownership <- function (x) {
    y <- vapply(as.character(x), get_color_for_ownership,
                character(length = 1)
                )
    return(y)
}
