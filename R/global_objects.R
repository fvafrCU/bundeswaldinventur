#' Load a Package's Data Into the Global Environment
#'
#' @param package Name of the package from which to load the data.
#' @return Invisibly a character vector of names of the objects loaded.
#' @export
get_package_data <- function(package) {
    data_frames <- utils::data(package = package)[["results"]][TRUE, "Item"]
    value <- utils::data(list = data_frames, package = package, 
                         envir = .GlobalEnv)
    return(invisible(value))
}

#' Define Global Variables Needed by Historical Analysis Scripts
#' 
#' See \code{\link{get_global_objects}}.
#' @return See \code{\link{assign}}.
#' @keywords internal
#' @export
get_global_variables <- function() {
    assign("COLORS_BWI", get_bwi_colors(), envir = .GlobalEnv)
    assign("A", get_design("a", 3), envir = .GlobalEnv)
    assign("A.12", get_design("a", 1), envir = .GlobalEnv)
    assign("nT", get_design("nt", 3), envir = .GlobalEnv)
    assign("nTE.12", get_design("nte", 1), envir = .GlobalEnv)
    assign("nTE", get_design("nte", 3), envir = .GlobalEnv)
    assign("RF.12", get_design("rf", 1), envir = .GlobalEnv)
    assign("RF", get_design("rf", 3), envir = .GlobalEnv)
    assign("bagr.bwi", get_bwi_species_groups(), envir = .GlobalEnv)
    assign("BAGR.BWI", get_bwi_species_groups(), envir = .GlobalEnv)
    assign("bagr.list", get_bwi_species_groups()[["bagr.lab"]], envir = .GlobalEnv)
    assign("BAGR.LIST", get_bwi_species_groups()[["bagr.lab"]], envir = .GlobalEnv)
}

#' Define Global Variables and Data Needed by Historical Analysis Scripts
#' 
#' A lot of historical analysis scripts rely on global objects such as variables
#' describing details of the sampling design or listing tree species groups or
#' data frames containing sampling data.
#' We replaced the global variables by getter functions (cf
#' \code{\link{get_design}}, \code{\link{get_bwi_species_groups}} and the
#' data frames by \pkg{bwibw} and \code{\link{get_data}}.
#' To stay compatible with the historical analysis scripts, this is is a wrapper
#' to provide those global variables and data frames to the global environment.
#' @param package The package to be passed to \code{\link{get_package_data}}.
#' Stick with the default execpt for testing.
#' @return Invisibly NULL.
#' @export
get_global_objects <- function(package = "bwibw") {
    get_package_data(package = package)
    get_global_variables()
    return(invisible(NULL))
}
