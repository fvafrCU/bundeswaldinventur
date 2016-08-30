#!/usr/bin/Rscript --vanilla
.RDatas <- list.files("data", full.names = TRUE)
for (.RData in .RDatas) {
    load(.RData)
    .objects <- ls()
    for (.object in .objects) {
        .obj <- get(.object)
        if (is.list(.obj)) {
            Encoding(names(.obj)) <- "UTF-8"
            .factor_columns <- names(.obj[which(sapply(.obj, is.factor))])
            for (.factor_column in .factor_columns) {
                levels(.obj[[.factor_column]]) <- sub("^ ", "", levels(.obj[[.factor_column]]))
                Encoding(levels(.obj[[.factor_column]])) <- "UTF-8"
            }
            .character_columns <- names(.obj[which(sapply(.obj, is.character))])
            for (.character_column in .character_columns) {
                Encoding(.obj[[.character_column]]) <- "UTF-8"
            }
            assign(.object, .obj)
        }
    }
    save(list = ls(all.names = FALSE), file = .RData)
    rm(list = ls(all.names = FALSE))
}
