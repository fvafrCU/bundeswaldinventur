#!/usr/bin/Rscript --vanilla
#' getter functions for settings.
#'
#' most settings were stored in (global) variables, lists an the like which are 
#' hard to document.
#' They should be converted to functions.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 277e66439c05cd12cd6e0d050c667e47a9d7fa91 $
#' @docType data  
#' @name Header
NULL 

#' get the species group list
#'
#' the list of species groups is used by many functions. 
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 277e66439c05cd12cd6e0d050c667e47a9d7fa91 $
#' @param type a type be given, currently disabled. 
# TODO: there were use cases where we changed the default list. Where were they?
#' @return  a list with 
#' \itemize{ 
#'     \item[\var{bagr.lab}] a vector of tree species group labels.
#'     \item[\var{ba.grupp}] a corresponding list of vectors giving the species
#'     codes for each tree species group.
#'     \item[\var{colors}] a corresponding vector of color definitions.
#' }
get_species_groups <- function(type) { 
    return(switch(type,
                  "*" = ,
                  "spec" =  list(bagr.lab = c("Fichte", "Weißtanne", 
                                              "Douglasie", "Kiefer", "Lärchen", 
                                              "sonst. Nadelholz", 
                                              "Buche", "Eichen", 
                                              "Roteiche", "Esche", "Bergahorn",
                                              "Hainbuche", "sonst. Hartholz", 
                                              "Birken", "Erlen", "Pappeln",  
                                              "sonst. Weichholz"
                                              ), 
                            ba.grupp =list(c(10), c(30), 
                                           c(40), c(20), c(50, 51), 
                                           c(11:19, 21:29, 31:39, 90:99), 
                                           c(100), c(110, 111), 
                                           c(112), c(120), c(140), 
                                           c(130), c(141:199), 
                                           c(200, 201), c(210:213), c(220:224), 
                                           c(230:299)
                                           ), 
                            colors = c(BAGR.BWI$ba.colors[1:2],
                                       BAGR.BWI$ba.colors[3:5],
                                       '#DC143C',
                                       BAGR.BWI$ba.colors[6:7],
                                       '#00BFFF', '#483D8B', '#008B8B',
                                       '#6B8E23', BAGR.BWI$ba.colors[8], 
                                       '#F5DEB3', '#D2691E', '#BC8F8F', 
                                       BAGR.BWI$ba.colors[9]
                                       )
                            )
                  )
    )
}
