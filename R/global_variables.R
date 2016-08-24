# We have tons of old scripts relying on global variables.
# To keep backwards compability, we assign the variable getters functions values
# to them.
# It's ugly but I can't see any other solution.

#' @include variable_getters.R
NULL


#' BWI standard green shades
#' 
#' Standard green shades for BWI plots.
#' 
#' @format A vector with 3 rgb values (green shades).
"COLORS_BWI"
COLORS_BWI <- get_bwi_colors()

#' Total Inventory area
#' 
#' The total area of the inventory area of BWI3 (3575148 ha).
#' 
#' @format An integer number
"A" 
A <- get_design("a", 3)

#' Total Inventory area in BWI 1 and 2
#' 
#' The inventory area which covers the area of BWI1 and BWI2 (3575163 ha).
#' 
#' @format An integer number 
"A.12" 
A.12 <- get_design("a", 1)

#' Traktanzahl
#' 
#' Anzahl der in der BWI aufgenommenen Trakte (8970).
#' 
#' @format An integer number.
"nT" 
nT <- get_design("nt", 3)

#' Eckenanzahl aus BWI 1 und 2
#' 
#' Anzahl der Traktecken, die in der BWI 1 und 2 aufgenommen wurden (?) (35743).
#' 
#' @format An integer number.
"nTE.12" 
nTE.12 <- get_design("nte", 1)

#' Eckenanzahl der BWI 3
#' 
#' Anzahl der Traktecken, welche in der BWI 3 aufgenommen wurden (35731).
#' 
#' @format An integer number.
"nTE" 
nTE <- get_design("nte", 3)

#' Representationsfaktor BWI 1 und 2
#' 
#' Area representation factor of each plot (Ecke).
#' 
#' @format A double number.
"RF.12" 
RF.12 <- get_design("rf", 1)


#' Representationsfaktor BWI 3
#' 
#' Area representation factor of each plot (Ecke).
#' 
#' @format A double number.
"RF" 
RF <- get_design("rf", 3)

#' Tree species groups and further information
#' 
#' List with tree species groups used in BWI3 and further information.
#' 
#' @format A list with 4 elements
#' \describe{
#'  \item{bagr.lab}{abbreviation of tree species group}
#'  \item{ba.group}{lists with tree species numbers for tree species group}
#'  \item{ba.colors}{standard colors for tree species group}
#'  \item{ba.text}{names of tree species groups}
#'  }
"bagr.bwi"
BAGR.BWI <- bagr.bwi <- get_bwi_species_groups()

#' Tree species groups
#' 
#' List with tree species groups used in BWI3.
#' 
#' @format A vector with 9 tree species groups (abbreviations)
"bagr.list"
BAGR.LIST <- bagr.list <- get_bwi_species_groups()[["bagr.lab"]]

