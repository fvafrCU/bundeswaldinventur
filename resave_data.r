#!/usr/bin/Rscript --vanilla
.RDatas <- list.files("data", full.names = TRUE)
for (.RData in .RDatas) {
    load(.RData)
    save(list = ls(all.names = FALSE), file = .RData, compress = "xz")
    rm(list = ls(all.names = FALSE))
}

