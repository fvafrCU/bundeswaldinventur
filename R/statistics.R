
#' compute the mode/modal value from a set of discrete values
#'
#' base::mean() and stats::median() are part of R, mode() returns the (S-)
#' storage mode or type of its argument. A modal function is simply missing.
#' The fact is well known, there different solutions, this one is based on
#' https://stat.ethz.ch/pipermail/r-help/2001-August/014677.html and
#' http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @param multiple return multiple modal values (or just the first one)?
#' @param x a vector of values.
#' @export
#' @return  the modal value(s) of x.
modal_value <- function(x, multiple = FALSE) {
  if (!is.logical(multiple)) throw("multiple must be TRUE or FALSE")
  if (!is.vector(x) || !is.atomic(x)) throw("x must be an atomic vector")
  if (multiple) {
    table_of_frequencies <- table(x)
    modals <- names(table_of_frequencies)[table_of_frequencies
    == max(table_of_frequencies)]
    modal_value <- as.numeric(modals)
  } else {
    unique_values <- unique(x)
    values_indices <- match(x, unique_values)
    # I'm not sure if this is necessary, but why would I bother...
    frequencies <- tabulate(values_indices)
    modal_value <- unique_values[which.max(frequencies)]
  }
  return(modal_value)
}
