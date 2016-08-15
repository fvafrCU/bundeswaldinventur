for (file in list.files("R/", full.names = TRUE)) {
    source(file)
}
load("data/bwi.RData")

