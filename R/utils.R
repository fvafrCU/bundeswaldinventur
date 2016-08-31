#' @include internals.R
NULL

#' calculate the golden ratio
#'
#' @param x the sum of the two quantities to be in the golden ratio
#' @return a numeric vector of lenght 2, containing the two quantites, a being
#' the larger.
#' @export
#' @examples
#' golden_ratio(10)
golden_ratio <- function(x) {
    golden_ratio  <- (1 + sqrt(5))/2
    a  <- x / golden_ratio
    b  <- x - a
    return(list(a = a, b = b))
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
    if (length(file_names) == 0) throw("No matching files found.")
    return(file_names)
}

