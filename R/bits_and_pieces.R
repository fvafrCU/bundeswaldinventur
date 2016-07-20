
#' add colSums to a data.frame, preserve its row.names
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 8bb9bd5c3d0ac9d48580e4497df4a9433d3ab412 $
#' @param data_frame The data from to add colSums to.
#' @param german If TRUE, the name of the colSums row will be 'Summe' and 'Sum'
#' otherwise.
#' @return  the modified data.frame. 
#' @examples
#' \dontrun{
#' add_colSums_to_data_frame(head(airquality), german = F)
#' }
add_colSums_to_data_frame <- function(data_frame, german = TRUE) {
    if (german) sum_label <- 'Summe' else sum_label  <- 'Sum'
    tmp_data <-	rbind(data_frame, sum_label = colSums(data_frame))
    names <- dimnames(data_frame)
    names[[1]][(length(names[[1]]) + 1)] <- sum_label
    dimnames(tmp_data)  <- names
    return(tmp_data)
}

#' round() numbers and multiply  them with 100
#'
#' The majority of readers can't cope with 0.1212. So we
#' change it to 12.1.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 8bb9bd5c3d0ac9d48580e4497df4a9433d3ab412 $
#' @param x the numbers to be modified.
#' @param digits The digits passed to round().
#' @return  the modified numbers. 
#' @examples
#' \dontrun{
#' round_and_prettify(head(airquality * 10^4))
#' }
as_percent_and_round <- function(x, digits = 1){
    return(round(x * 100, digits))
}

#' limit the values in a data.frame's column to a limit
#'
#' without changing the original data.frame
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 8bb9bd5c3d0ac9d48580e4497df4a9433d3ab412 $
#' @param data_frame Give the data.frame.
#' @param condition See Details
#' @return  the modified data.frame
#' @section Details:
#' \code{condition} of form \emph{variable operator limit} and will set all 
#' values of \emph{variable} to \emph{limit} where the condition is TRUE.
#' lower than Zero to Zero.
#' @examples
#' set_to_limit(mtcars, mpg < 20)
#' set_to_limit(set_to_limit(mtcars, mpg < 20), mpg > 23)
set_to_limit <- function(data_frame, condition) {
    s <- deparse(substitute(condition))
    variable <- strsplit(s, " ")[[1]][1]
    operator <- strsplit(s, " ")[[1]][2]
    limit <- strsplit(s, " ")[[1]][3]
    print(limit)
    to_eval <- paste0("data_frame[data_frame[,'", variable, "']", " ", operator, 
                       " ",limit, ", '", variable, "'] <- ", limit)
    print(to_eval)
    eval(parse(text = to_eval))
    return(data_frame)
}
# @NULL 
#FIXME:
get_period <- function(label) {
    period <- switch(as.character(label),
                     "1" = "1987 - 2002", 
                     "2"= "2002 - 2012", 
                     stop("unkown period, give 1 or 2")
                     )
    return(period)
}
