#!/usr/bin/Rscript --vanilla
# We have tons of old scripts relying on global variables.
# To keep backwards compability, we need to assign values to them somewhere.
# There are three ways:
# 1) We could put the code in R/, but then the variables are not available with
#  the package (although with devtools::load_all()).
# 2) We could do it in data/, but since we seem unable to use the getter 
#  functions meant to replace those global variables, and we don't wan't to 
# d ouble-code them additionally to the getter functions: this is not the way.
# 3) We create an RData from this script and put it in data/; 
#  and we extract docuemtation from this script and put it into R/. 
# Which is what we do now. 
.package_root <- file.path("..", "..") # runnnig this we're in inst/maintenance, 
                                      # not in maintenance/.
source(file.path(.package_root, "R","internals.R"))
source(file.path(.package_root, "R","utils.R"))
source(file.path(.package_root, "R","variable_getters.R"))
source(file.path(.package_root, "R","batch.R"))


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
"COLORS_BWI"
COLORS_BWI <- get_bwi_colors()

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
"A" 
A <- get_design("a", 3)

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
"A.12" 
A.12 <- get_design("a", 1)

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
"nT" 
nT <- get_design("nt", 3)

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
"nTE.12" 
nTE.12 <- get_design("nte", 1)

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
"nTE" 
nTE <- get_design("nte", 3)

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
"RF.12" 
RF.12 <- get_design("rf", 1)


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
"RF" 
RF <- get_design("rf", 3)

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
"bagr.bwi"
bagr.bwi <- get_bwi_species_groups()

#' @rdname bagr.bwi 
"BAGR.BWI"
BAGR.BWI <- get_bwi_species_groups()

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
"bagr.list"
bagr.list <- get_bwi_species_groups()[["bagr.lab"]]

#' @rdname bagr.list 
"BAGR.LIST"
BAGR.LIST <- get_bwi_species_groups()[["bagr.lab"]]

#% save the data
save(list = c("COLORS_BWI", "A", "A.12", "nT", "nTE.12", "nTE", "RF.12", "RF", 
              "bagr.bwi", "BAGR.BWI", "bagr.list", "BAGR.LIST"), 
     file = file.path(.package_root, "data", "global_variables.RData")
     )

#% put the documentation into R/
my_name <- get_script_name()
s <- readLines(my_name)
header <- paste0("# See inst/", my_name, " for details.")
documentation <- grep("^#'|^\\\"|^ $", s, value = TRUE)
file <- file(file.path(.package_root, "R", my_name))
writeLines(text = c(header, documentation), con = file)
close(file)

