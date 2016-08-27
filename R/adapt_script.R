adapt_script <- function(x, overwrite = FALSE, backup = TRUE) {
    s <- readLines(x)
    i <- grep("source\\(.*customization.R[\"\'])", s)
    dependencies <- c('library("bundeswaldinventur")', 'library("xtable")', 
                      'library("plyr")', 'library("reshape")', 'library("reshape2")',
                      'library("ggplot2")', 'library("scales")')
    header <- s[1:(i - 1)]  
    bottom <- s[(i + 1):length(s)]  

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
adapt_script("Deines.R")
