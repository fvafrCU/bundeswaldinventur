

#' apply prettyNum to a data.frame, preserve its row.names
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9be1ec3c316c4c9ca5af009959e89607c154f14c $
#' @param data_frame The data which to prettyNum().
#' @param digits The number of digits to be rounded to.
#' @return  the modified data.frame. 
#' @export
#' @examples
#' prettify_data_frame(utils::head(airquality * 10^4))
prettify_data_frame <- function(data_frame, digits = 0) {
    tmp_data <- sapply(round(data_frame, digits), prettyNum, big.mark = ",")
    row.names(tmp_data) <- row.names(data_frame)
    return(tmp_data)
}

#' print an enhanced xtable from a data.frame.
#'
#' 'Enhanced' means: add colSums(), use prettyNum()s.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9be1ec3c316c4c9ca5af009959e89607c154f14c $
#' @param data_frame The data which to xtable::xtable().
#' @param caption A caption for the LaTeX table.
#' @note The LaTeX table will have a latex label tab:data_frame. So you really
#' should avoid data_frame specifications that are not valid LaTeX names, like
#' in the examples.
#' @return TRUE on success, FALSE otherwise.
#' @export
#' @examples
#' add_colSums_prettify_and_print_xtable(utils::head(airquality * 10^4))
add_colSums_prettify_and_print_xtable <- function(data_frame, 
						  caption = 'XXX') {
    status <- FALSE
    tmp_data <- prettify_data_frame(add_colSums_to_data_frame(data_frame))
    line_portion <- 1 / (ncol(data_frame) + 1)
    names(tmp_data)
    dimnames(tmp_data)[[2]] <-
	paste('\\multicolumn{1}{>{\\centering}p{', line_portion, 
	      '\\textwidth}}{', dimnames(tmp_data)[[2]], '}')
    print(
	  xtable::xtable(tmp_data,
		 label = paste('tab', deparse(substitute(data_frame)), sep = ':'), 
		 caption = caption,
		 align = c('l', rep('r', length(colnames(data_frame))))
		 ),
	  hline.after = c(-1,0,nrow(data_frame), nrow(data_frame) + 1),
	  type='latex',
	  sanitize.colnames.function = identity
	  )
    status <- TRUE
    return(invisible(status))
}

#' round() and prettyNum() numbers
#'
#' The majority of readers can't cope with numbers like 12000.01. So we
#' change it to 12,000.
#'
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9be1ec3c316c4c9ca5af009959e89607c154f14c $
#' @param x the numbers to be modified.
#' @param digits The digits passed to round().
#' @param big_mark the big.mark passed to \code{prettyNum}.
#' @param decimal_mark the decimal.mark passed to \code{prettyNum}.
#' @return the modified numbers. 
#' @export
#' @examples
#' round_and_prettify(airquality[1:10, "Ozone"]*10^4+0.1)
round_and_prettify <- function(x, digits = 0, big_mark = ',', decimal_mark = '.') {
    return(prettyNum(round(x, digits), big.mark = big_mark, decimal.mark =
                     decimal_mark))
}

#' round() and prettyNum() numbers
#'
#' The majority of \emph{german} readers can't cope with numbers like 12000.01. 
#' So we change it to 12.000.
#'
#' @note This is just an alias for 
#' \code{\link{round_and_prettify}}(\ldots, big_mark = '.') 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9be1ec3c316c4c9ca5af009959e89607c154f14c $
#' @param x the numbers to be modified.
#' @param digits The digits passed to round().
#' @return  the modified numbers. 
#' @export
#' @examples
#' round_and_prettify_german(airquality[1:10, "Ozone"]*10^4+0.1)
round_and_prettify_german <- function(x, digits = 0) {
    return(round_and_prettify(x, digits, big_mark = '.', decimal_mark = ","))
}


# @NULL 
#FIXME:

german_number <- function(x, ...) {
    y  <- format(x, big.mark = ".", decimal.mark = ',',
                 justify = "right", scientific = FALSE, ...)
    return(y)
}
tex_umlauts <- function(string) {
    s <- gsub("ä", '\\\\"a{}', string)
    s <- gsub("Ä", '\\\\"A{}', s)
    s <- gsub("ö", '\\\\"o{}', s)
    s <- gsub("Ö", '\\\\"O{}', s)
    s <- gsub("ü", '\\\\"u{}', s)
    s <- gsub("Ü", '\\\\"U{}', s)
    s <- gsub("ß", '\\\\"s{}', s)
    return(s)
}
ascii_umlauts <- function(string) {
    s <- gsub("ä", 'ae', string)
    s <- gsub("Ä", 'Ae', s)
    s <- gsub("ö", 'oe', s)
    s <- gsub("Ö", 'Oe', s)
    s <- gsub("ü", 'ue', s)
    s <- gsub("Ü", 'Ue', s)
    s <- gsub("ß", 'ss', s)
    return(s)
}


