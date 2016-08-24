#' @include utils.R
NULL

#' get bwi species group list
#'
#' the list of species groups is used by many functions. 
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section warning: there is a second function \code{\link{get_species_groups}}
#' which does nearly the same but the return value is slightly different.
#' @export
#' @return  a list with 
#' \itemize{ 
#'     \item[\var{bagr.lab}] a vector of tree species group labels.
#'     \item[\var{ba.grupp}] a corresponding list of vectors giving the species
#'     codes for each tree species group.
#'     \item[\var{ba.colors}] a corresponding vector of color definitions.
#'     \item[\var{ba.text}] another vector of tree species group labels.
#' }
get_bwi_species_groups <- function() { 
    l <- list(bagr.lab = c("FI", "TA", 
                           "DGL", "KI", "LAE", 
                           "BU", "EI",
                           "ALH", "ALN"),
              ba.grupp = list(c(10:19, 90:99), c(30:39), 
                              c(40), c(20:29), c(50, 51),
                              c(100), c(110:114),
                              c(120:199), c(200:299)),
              ba.colors = c("grey30", "red", 
                            "violet", "orange", "yellow", 
                            "green", "darkblue",
                            grDevices::rgb(100,190, 150, maxColorValue = 255), "brown"),
              ba.text = c("Fichte", "Tanne", 
                          "Douglasie", "Kiefer", "L\u00e4rche",
                          "Buche", "Eiche", 
                          "ALH", "ALN")
              )
    return(l)
}

#' get the species group list
#'
#' the list of species groups is used by many functions. 
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section warning: there is a second function 
#' \code{\link{get_bwi_species_groups}}
#' which does nearly the same but the return value is slightly different and on
#' which this functions relies. What a mess.
#' @export
#' @return  a list with 
#' \itemize{ 
#'     \item[\var{bagr.lab}] a vector of tree species group labels.
#'     \item[\var{ba.grupp}] a corresponding list of vectors giving the species
#'     codes for each tree species group.
#'     \item[\var{colors}] a corresponding vector of color definitions.
#' }
get_species_groups <- function() { 
    l <- list(bagr.lab = c("Fichte", "Wei\u00dftanne", 
                           "Douglasie", "Kiefer", "L\u00e4rchen", 
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
              colors = c(get_bwi_species_groups()$ba.colors[1:2],
                         get_bwi_species_groups()$ba.colors[3:5],
                         '#DC143C',
                         get_bwi_species_groups()$ba.colors[6:7],
                         '#00BFFF', '#483D8B', '#008B8B',
                         '#6B8E23', get_bwi_species_groups()$ba.colors[8], 
                         '#F5DEB3', '#D2691E', '#BC8F8F', 
                         get_bwi_species_groups()$ba.colors[9]
                         )
              )
    return(l)
}




#' get standard bwi plotting colors
#'
#' we use standard colors for BWI 1, 2, and 3,
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @export
#' @return a vector with rgb values
get_bwi_colors <- function() {
    colors <- c(grDevices::rgb(154, 205, 050, maxColorValue = 255),
                grDevices::rgb(124, 252, 000, maxColorValue = 255), 
                grDevices::rgb(000, 100, 000, maxColorValue = 255)
		)
    return(colors)
}

#' get attributes from the underlying sampling design
#'
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param attribute The attribute of interest.
#' @param bwi The BWI to use.
#' @param hard_coded use hardcoded values or try to retrieve them from the
#' respective trakte data.frame?
#' @export
#' @return an integer
#' @examples
#' get_design("nte", "3")
#' get_design("nTE", "bwi3")
get_design <- function(attribute, bwi, hard_coded = TRUE) {
    if (isTRUE(hard_coded)) {
        nt <- 8970 # this  _is_ nrow(get_data("trakte.3"))
        bwi1 <- c(a = 3575163, nte = 35743, nt = nt) # nte _is_ sum(get_data("trakte.1")[["m"]])
        bwi2 <- bwi1
        bwi3 <- c(a = 3575148, nte = 35731, nt = nt)# nte _is_ sum(get_data("trakte.3")[["m"]]))

        # bwi3$a was taken from  
        # http://www.statistik-portal.de/Statistik-Portal/de_jb01_jahrtab1.asp
        # on 17/04/2014
    } else {
        bwi1 <- c(a = 3575163,
                  nte = sum(get_data(paste0("trakte.", 1))[["m"]]),
                  nt = nrow(get_data(paste0("trakte.", 1))))
        bwi2 <- c(a = 3575263,
                  nte = sum(get_data(paste0("trakte.", 2))[["m"]]),
                  nt = nrow(get_data(paste0("trakte.", 2))))
        bwi3 <- c(a = 3575148,
                  nte = sum(get_data(paste0("trakte.", 3))[["m"]]),
                  nt = nrow(get_data(paste0("trakte.", 3))))
    }
    values <- rbind(bwi1, bwi2, bwi3)
    values <- cbind(values, rf = values[TRUE, "a"] / values[TRUE, "nte"])
    row <- paste0("bwi", sub("bwi", "", bwi, ignore.case = TRUE))
    column <- tolower(attribute)
    value <- values[row, column]
    return(value)
}
