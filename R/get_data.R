#' @include utils.R
NULL

#' Load Data from \pkg{bwibw}
#'
#' Get data if we want to use example data provided with the this of the
#' \pkg{bwibw} package.
#'
#' Allows to globally en/dis-abling the reading of example
#' data into function arguments, Which we need because Gerald used global
#' data.frames in his functions.
#'
#' @param name a string of length one giving the name of the data.frame.
#' @param package The package to load data from.
#' @return the data requested.
#' @export
#' @examples
#' if ("bwibw" %in% rownames(installed.packages())) {
#'    get_data("bacode", package = "bwibw") 
#' } else {
#'    get_data("bacode") 
#' }
get_data <- function(name, package = get_options("data_source")) {
  checkmate::qassert(name, "S1")
  checkmate::qassert(package, "S1")
  if (is.null(package)) package <- "bundeswaldinventur"
  utils::data(list = name, package = package, envir = environment())
  return(get(name))
}
