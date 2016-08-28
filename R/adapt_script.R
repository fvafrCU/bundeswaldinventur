adapt_script <- function(x, overwrite = FALSE, backup = FALSE) {
    s <- readLines(x)
    i <- grep("source\\(.*customization.R[\"\'])", s)
    dependencies <- c('library("bundeswaldinventur")', 'library("xtable")', 
                      'library("plyr")', 'library("reshape")', 'library("reshape2")',
                      'library("ggplot2")', 'library("scales")')
    header <- s[seq_len(i - 1)]  
    bottom <- s[seq.int(from = (i + 1), to = length(s), by = 1)]  

    s <-  c(header, dependencies, bottom)
    s <- s[ - c(grep("^\\ *provide_data().*", s), 
                grep("^\\ *provide_statistics().*", s))] 
    if (backup) {
        x <- paste0(x, "_adapted")
        output <- file(x)
        writeLines(text = s, con = output)
        close(output)
    }
    if (overwrite) {
        output <- file(x)
        writeLines(text = s, con = output)
        close(output)
    }
    return(s)

}
adapt_script("~/git/oudeis/fva/hochrechnungen/scripts/Deines.R", backup = TRUE)
