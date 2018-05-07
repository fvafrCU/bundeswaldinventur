#' @include utils.R
NULL

#' adapt old scripts to package bundeswaldinventur
#'
#' Old scripts were based on a set of R codes in a customization directory that
#' were controlled by customization/customization.R. These are adapted to use
#' the package bundeswaldinventur.
#'
#' @param file_names character vector naming the scripts to be adapted.
#' @param path see \code{\link{find_files}}.
#' @param pattern see \code{\link{find_files}}.
#' @param all.files see \code{\link{find_files}}.
#' @param recursive see \code{\link{find_files}}.
#' @param ignore.case see \code{\link{find_files}}.
#' @param overwrite overwrite files on disk?
#' @param backup backup files to be overwriten?
#' @param verbose be verbose?
#' @param clean_warning delete lines containing only a "options(warn=2)"
#' instruction?
#' @export
#' @return invisibly the content of the change files.
adapt_script <- function(file_names = NA, path = ".",
                         pattern = ".*\\.[RrSs]$|.*\\.[RrSs]nw$",
                         all.files = TRUE, recursive = TRUE,
                         ignore.case = FALSE,
                         overwrite = FALSE, backup = overwrite, verbose = TRUE,
                         clean_warning = FALSE) {
  file_names <- find_files(
    file_names = file_names, path = path,
    pattern = pattern, all.files = all.files,
    recursive = recursive, ignore.case = ignore.case
  )
  all_text <- NULL
  for (file_name in file_names) {
    s <- readLines(file_name)
    i <- grep("source\\(.*customization.R[\"\'])", s)
    if (length(i) == 0) {
      if (isTRUE(verbose)) warning("script ", file_name, 
                                   " doesn't seem to need adaption.")
    } else {
      inserts <- c('bundeswaldinventur::get_global_objects()', 
                   'library("bundeswaldinventur")',
                   'bundeswaldinventur::set_options(data_source = "bwibw")')
      header <- s[seq_len(i - 1)]
      bottom <- s[seq.int(from = (i + 1), to = length(s), by = 1)]

      s <- c(header, inserts, bottom)
      s <- s[ -c(
        grep("^\\ *provide_data().*", s),
        grep("^\\ *provide_statistics().*", s)
      )]
      if (isTRUE(clean_warning)) {
        s <- s[ -grep(
          "^options\\(warn=2\\)$",
          gsub("[[:space:]]", "", s)
        )]
      }
      if (isTRUE(overwrite)) {
        if (backup) {
          new_name <- paste0(file_name, "_adapted")
          file.copy(file_name, new_name, overwrite = TRUE)
          if (isTRUE(verbose)) {
            message(
              "backed up ", file_name,
              " to ", new_name, "."
            )
          }
        }
        output <- file(file_name)
        writeLines(text = s, con = output)
        close(output)
        if (isTRUE(verbose)) message("adapted ", file_name, ".")
      }
      all_text <- c(all_text, s)
    }
  }
  return(invisible(all_text))
}
