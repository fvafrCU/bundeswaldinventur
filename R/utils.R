#' @include internals.R
NULL


#' set options in a list
#'
#' I want to bundle options in a named list to not clutter the options().
#'
#' @note use something like 
#' .onLoad <- function(libname, pkgname) {
#'   op <- options()
#'   op.package <- list(
#'     package_name = "foo"
#'   )
#'   toset <- !(names(op.package) %in% names(op))
#'   if(any(toset)) options(op.package[toset])
#' 
#'   set_options(overwrite = FALSE)
#' 
#'   return(invisible(NULL))
#' }
#' in your zzz.R to bundle a package's options into the list named "foo".
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param name [character(1)]\cr Name of the list.
#' @param overwrite [boolean(1)]\cr Overwrite options already set? Is set to
#' FALSE on package loading to ensure your previously set list options
#' won't get overridden. Just ignore that argument.
#' @param reset [boolean(1)]\cr Reset all list options to the list's
#' defaults?
#' @param ... see \code{\link{options}}.
#' @return invisible(TRUE)
#' @export
#' @examples
#' set_options(my_value = 3, name = "my_list")
#' get_options(name = "my_list")
set_options <- function(..., 
                        name = ifelse(is.null(getOption("package_name")), 
                                      "my_list", getOption("package_name")),
                        reset = FALSE, overwrite = TRUE) {
    checkmate::qassert(name, "S1")
    checkmate::qassert(reset, "B1")
    checkmate::qassert(overwrite, "B1")
    defaults <- list(fake_data = TRUE)
    option_list <- list(...)
    if (is.null(getOption(name)) || reset)
        eval(parse(text = paste0("options(", name, " = defaults)")))
    else {
        set_options <- getOption(name)
        if (overwrite) {
            eval(parse(text = paste0("options(", name,
                                     " = utils::modifyList(set_options, option_list))")
            ))
        } else {
            if (length(option_list) == 0)
                option_list <- defaults
            is_option_unset <- !(names(option_list) %in% names(set_options))
            if(any(is_option_unset))
                eval(parse(text = paste0("options(", name, 
                                         " = append(set_options, option_list[is_option_unset]))")
            ))
        }
    }
    return(invisible(TRUE))
}

#' get options from a list
#'
#' a convenience function for \code{\link{getOption}}.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param ... see \code{\link{getOption}}/
#' @param name [character(1)]\cr Name of the list.
#' @param remove_names [boolean(1)]\cr Remove the names?
#' @param flatten_list [boolean(1)]\cr Return a vetcor?
#' @return a (possibly named) list or a vector.
#' @export
#' @examples
#' message('See help("set_options")')
get_options <- function(..., 
                        name = ifelse(is.null(getOption("package_name")), 
                                      "my_list", getOption("package_name")),
                        remove_names = FALSE, flatten_list = TRUE) {
    checkmate::qassert(name, "S1")
    checkmate::qassert(remove_names, "B1")
    checkmate::qassert(flatten_list, "B1")
    if (missing(...)) {
        option_list <- getOption(name)
    } else {
        option_names <- as.vector(...)
        options_set <- getOption(name)
        option_list  <- options_set[names(options_set) %in% option_names]
    }
    if (flatten_list) option_list <-  unlist(option_list)
    if (remove_names) names(option_list)  <- NULL
    return(option_list)
}

#' get data if we want to use example data provided with the package
#' 
#' this is a wrapper reading get_options("fake_data") and returning an error if
#' this is not set to TRUE, hence globally en/dis-abling the reading of example
#' data into function arguments. Which I need because Gerald used global 
#' data.frames in his functions.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param name a string of lenght one giving the name of the data.frame.
#' @return the data requested.
#' @export
#' @examples
#' get_data("bacode")
get_data <- function(name) {
    checkmate::qassert(name, "S1")
    if (isTRUE(as.logical(get_options("fake_data")))) {
       return(get(name))
    } else {
        throw('get_options("fake_data") did not return TRUE')
    }
}

#' find files on disk
#' 
#' look for files on disk, either scanning a vector of names or searching for
#' files with \code{\link{list.files}} and throw an error if no files are found.
#' 
#' @param file_names character vector of file names (to be checked if the files
#'        exist).
#' @param path see \code{\link{list.files}}.
#' @param pattern see \code{\link{list.files}}.
#' @param all.files see \code{\link{list.files}}.
#' @param recursive see \code{\link{list.files}}.
#' @param ignore.case see \code{\link{list.files}}.
#' 
#' @note This is merely a wrapper around \code{\link{file.exists}} or 
#'       \code{\link{list.files}}, depending on wheter file_names is given.
#' 
#' @export
#' @return a character vector of file names.
find_files <- function(file_names = NA, path = ".", 
                       pattern = ".*\\.[RrSs]$|.*\\.[RrSs]nw$",
                       all.files = TRUE, recursive = TRUE, 
                       ignore.case = FALSE) {
    if(isTRUE(is.na(file_names))) {
        file_names <- list.files(path = path, pattern = pattern, 
                                  all.files = all.files, 
                                  recursive = recursive, 
                                  ignore.case = ignore.case, 
                                  full.names = TRUE,
                                  include.dirs = FALSE, no.. = TRUE)
    } else {
        file_exists <- file.exists(file_names)
        if(! all(file_exists)) warning("file(s) ", 
                                       paste(file_names[! file_exists], 
                                             collapse = ", "), 
                                      " not found.")
        file_names <- file_names[file_exists]
    }
    if (length(file_names) == 0) stop("No matching files found.")
    return(file_names)
}

