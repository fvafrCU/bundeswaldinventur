adapt_script <- function(file_names = NA, path = ".", 
                         pattern = ".*\\.[RrSs]$|.*\\.[RrSs]nw$",
                         all.files = TRUE, recursive = TRUE, 
                         ignore.case = FALSE,
                         overwrite = FALSE, backup = FALSE, verbose = TRUE
                         ) {
    file_names <- find_files(file_names = file_names, path = path, 
                             pattern = pattern, all.files = all.files, 
                             recursive = recursive, ignore.case = ignore.case)
    all_text <- NULL
    for (file_name in file_names) {
        s <- readLines(file_name)
        i <- grep("source\\(.*customization.R[\"\'])", s)
        if (length(i) == 0) {
            warning("script ", file_name, " doesn't seem to need adaption.")
        } else {
            dependencies <- c('library("bundeswaldinventur")', 'library("xtable")', 
                              'library("plyr")', 'library("reshape")', 'library("reshape2")',
                              'library("ggplot2")', 'library("scales")')
            header <- s[seq_len(i - 1)]  
            bottom <- s[seq.int(from = (i + 1), to = length(s), by = 1)]  

            s <-  c(header, dependencies, bottom)
            s <- s[ - c(grep("^\\ *provide_data().*", s), 
                        grep("^\\ *provide_statistics().*", s))] 
            if (isTRUE(overwrite)) {
                if (backup) {
                    new_name <- paste0(file_name, "_adapted")
                    file.copy(file_name, new_name, overwrite = TRUE)
                    if (isTRUE(verbose)) message("backed up ", file_name, " to ", new_name, ".")
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

adapt_script(path = "~/git/os/fva/hochrechnungen/scripts/", overwrite = TRUE, backup = TRUE)

