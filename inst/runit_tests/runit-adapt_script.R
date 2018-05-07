if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}

test_adapt_script <- function() {
    script_file <- paste0(tempfile(), ".R")
    stale_code <- c(" options(warn = 2) ",
                    "#% load data and functions", 
                    paste0("source(file.path('..', 'customization',", 
                           " 'customization.R'))"), 
                    "provide_data() # load the data from the database",
                    "library(MASS)",
                    "provide_statistics() ",
                    "x <- 3")
    writeLines(stale_code, con = script_file)
    adapt_script(path = tempdir(), overwrite = TRUE, clean_warning = TRUE)
    adapted <- readLines(script_file)
    original <- readLines(paste0(script_file, "_adapted"))
    reference <- c("#% load data and functions", 
                   "bundeswaldinventur::get_global_objects()", 
                   "library(\"bundeswaldinventur\")", 
                   "bundeswaldinventur::set_options(data_source = \"bwibw\")", 
                   "library(MASS)", "x <- 3")

    RUnit::checkIdentical(reference, adapted)
    RUnit::checkIdentical(stale_code, original)

    script_file <- paste0(tempfile(), ".R")
    code <- c("#% load data and functions", 
                    "provide_data() # load the data from the database",
                    "library(MASS)",
                    "provide_statistics() ",
                    "x <- 3")
    writeLines(code, con = script_file)
    adapt_script(file_names = script_file, overwrite = TRUE, verbose = FALSE)
    result <- readLines(script_file)
    RUnit::checkIdentical(code, result)


}
if (FALSE) test_adapt_script()

