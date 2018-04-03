#' @include variable_getters.R
NULL


# See inst/global_variables.R for details.
#' BWI standard green shades
#'
#' Standard green shades for BWI plots.
#'
#' @format A vector with 3 rgb values (green shades).
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{COLORS_BWI <- get_bwi_colors()}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"COLORS_BWI" <- get_bwi_colors()

#' Total Inventory area
#'
#' The total area of the inventory area of BWI3 (3575148 ha).
#'
#' @format An integer number
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{A <- get_design("a", 3)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"A" <- get_design("a", 3)

#' Total Inventory area in BWI 1 and 2
#'
#' The inventory area which covers the area of BWI1 and BWI2 (3575163 ha).
#'
#' @format An integer number
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{A.12 <- get_design("a", 1)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"A.12" <- get_design("a", 1)

#' Traktanzahl
#'
#' Anzahl der in der BWI aufgenommenen Trakte (8970).
#'
#' @format An integer number.
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{nT <- get_design("nt", 3)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"nT" <- get_design("nt", 3)

#' Eckenanzahl aus BWI 1 und 2
#'
#' Anzahl der Traktecken, die in der BWI 1 und 2 aufgenommen wurden (?) (35743).
#'
#' @format An integer number.
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{nTE.12 <- get_design("nte", 1)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"nTE.12" <- get_design("nte", 1)

#' Eckenanzahl der BWI 3
#'
#' Anzahl der Traktecken, welche in der BWI 3 aufgenommen wurden (35731).
#'
#' @format An integer number.
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{nTE <- get_design("nte", 3)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"nTE" <- get_design("nte", 3)

#' Representationsfaktor BWI 1 und 2
#'
#' Area representation factor of each plot (Ecke).
#'
#' @format A double number.
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{RF.12 <- get_design("rf", 1)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"RF.12" <- get_design("rf", 1)

#' Representationsfaktor BWI 3
#'
#' Area representation factor of each plot (Ecke).
#'
#' @format A double number.
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{RF <- get_design("rf", 3)}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"RF" <- get_design("rf", 3)

#' Tree species groups and further information
#'
#' List with tree species groups used in BWI3 and further information.
#'
#' @format A list with 4 elements
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{BAGR.BWI <- bagr.bwi <- get_bwi_species_groups()}
#' and you should use the getter function instead of the object.
#' \describe{
#'  \item{bagr.lab}{abbreviation of tree species group}
#'  \item{ba.group}{lists with tree species numbers for tree species group}
#'  \item{ba.colors}{standard colors for tree species group}
#'  \item{ba.text}{names of tree species groups}
#'  }
#' @export
#' @keywords internal
"bagr.bwi" <- get_bwi_species_groups()

#' @rdname bagr.bwi
#' @export
#' @keywords internal
"BAGR.BWI" <- bagr.bwi

#' Tree species groups
#'
#' List with tree species groups used in BWI3.
#'
#' @format A vector with 9 tree species groups (abbreviations)
#' @section Warning: this object should not exist, but we have loads of old
#' scripts relying on a global variable of that name, so it's there for
#' backward compability. It was created by
#' \code{BAGR.LIST <- bagr.list <- get_bwi_species_groups()[["bagr.lab"]]}
#' and you should use the getter function instead of the object.
#' @export
#' @keywords internal
"bagr.list" <- get_bwi_species_groups()[["bagr.lab"]]

#' @rdname bagr.list
#' @export
#' @keywords internal
"BAGR.LIST" <- bagr.list
