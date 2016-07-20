
#' compute the mode/modal value from a set of discrete values
#'
#' base::mean() and stats::median() are part of R, mode() returns the (S-)
#' storage mode or type of its argument. A modal function is simply missing. 
#' The fact is well known, there different solutions, this one is based on 
#' https://stat.ethz.ch/pipermail/r-help/2001-August/014677.html and
#' http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode.
#'
#' @author Dominik Cullmann <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 7c6d9e45f1674fa810ca20d87a9016e584540653 $
#' @param multiple return multiple modal values (or just the first one)?
#' @param x a vector of values.
#' @return  the modal value(s) of x.
modal_value <- function(x, multiple = FALSE) {
    if (! is.logical(multiple)) stop("multiple must be TRUE or FALSE")
    if (multiple) {
    table_of_frequencies <- table(as.vector(x))
    modal_value <- names(table_of_frequencies)[table_of_frequencies ==
                                         max(table_of_frequencies)]
    } else {
        unique_values <- unique(x)
        values_indices <- match(x, unique_values) 
        # I'm not sure if this is necessary, but why would I bother...
        frequencies <- tabulate(values_indices)
        modal_value <- unique_values[which.max(frequencies)]
    }
    return(modal_value)
}

