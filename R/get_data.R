#' @include utils.R
NULL

#' Load Data from \pkg{bwibw}
#'
#' Get data if we want to use example data provided with the package.
#'
#' this is a wrapper reading get_options("fake_data") and returning an error if
#' this is not set to TRUE, hence globally en/dis-abling the reading of example
#' data into function arguments. Which I need because Gerald used global
#' data.frames in his functions.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @param name a string of length one giving the name of the data.frame.
#' @return the data requested.
#' @export
#' @examples
#' if ("bwibw" %in% rownames(installed.packages())) get_data("bacode")
get_data <- function(name) {
  checkmate::qassert(name, "S1")
  if (isTRUE(as.logical(get_options("fake_data")))) {
      utils::data(list = name, package = "bwibw", envir = environment())
      return(get(name))
  } else {
    throw('get_options("fake_data") did not return TRUE')
  }
}
