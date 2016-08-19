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
    stringi::stri_enc_toascii("ä")
    stringi::stri_enc_toutf8("ä")
    stringi::stri_trans_general("ß", "latin-ascii")
}
file <- "R/BWI3_HR_Funktionen_v3.R"
l <- readLines(file)
is_roxygen <- grepl("#'", l)
l[! is_roxygen] <- stringi::stri_escape_unicode(l[! is_roxygen])
full <- stringi::stri_trans_general(l, "latin-ascii")
writeLines(full, con = "foo.txt")
