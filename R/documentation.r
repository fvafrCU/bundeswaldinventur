#!/usr/bin/Rscript --vanilla
args <- commandArgs(trailingOnly = TRUE)
NAME <- sub(".Rnw", '', args[1], fixed = TRUE)

library(documentation)
library(methods)
if (is.na(NAME)) {
    if (! exists('FUNCTIONS_DIRECTORY')) FUNCTIONS_DIRECTORY <- './'
    suffix <- '.R$'
    files <- list.files(FUNCTIONS_DIRECTORY, pattern = suffix)
} else {
    files <- NAME
    suffix <- paste('.', strsplit(NAME, '[.]')[[1]][2], sep ='')
}
for (file in files) {
    message(file)
    pdf_file <- paste(sub(suffix, '', file), 'pdf', sep = '.')
    unlink(pdf_file)
    create_roxygen_documentation(file, copy_tmp_files_to  ='/tmp/',
                                 end_pattern = '@NULL')
}

