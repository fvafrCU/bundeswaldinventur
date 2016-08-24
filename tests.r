for (file in list.files("R/", full.names = TRUE, pattern = "^.*\\.R")) {
    try(source(file))
}
load("data/bwi.RData")
set_options(overwrite = FALSE)

