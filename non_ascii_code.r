if (FALSE) {
    tools::showNonASCIIfile("R/german_things.R")
    tools::showNonASCIIfile("R/labeling.R")
    tools::showNonASCIIfile("R/BWI3_HR_Funktionen_v3.R")
    tools::showNonASCIIfile("R/BWI3_HR_Grafikfunktionen_v2.R")
    grep("^ *#", tools::showNonASCIIfile("R/global_variables.R"), invert = TRUE)

    grep("^ *#", readLines("R/global_variables.R"), invert  = TRUE, value = TRUE)

    stringi::stri_escape_unicode("Ä")
    stringi::stri_escape_unicode("ä")
    stringi::stri_escape_unicode("Ö")
    stringi::stri_escape_unicode("ö")
    stringi::stri_escape_unicode("Ü")
    stringi::stri_escape_unicode("ü")
    stringi::stri_escape_unicode("ß")

    stringi::stri_trans_general("ß", "latin-ascii")
}
file <- "R/global_variables.R"
l <- readLines(file)
gsub("\u00e4", stringi::stri_escape_unicode("ä"), l)

