load("data/bwi.RData")

if (TRUE) {
    # manual try
    Encoding(levels(baeume.1$Bemerk)) <- "UTF-8"
    Encoding(levels(baeume.2$Bemerk)) <- "UTF-8"
    Encoding(levels(baeume.3$Bemerk)) <- "UTF-8"
    Encoding(levels(totholz.2$Bemerkung)) <- "UTF-8"
    Encoding(levels(totholz.3$Bemerkung)) <- "UTF-8"
} else {
    for (object in ls()) {
        obj <- get(object)
        if (is.data.frame(obj)) {
            factor_columns <- names(obj[which(sapply(obj, is.factor))])
            for (factor_column in factor_columns) {
                levels(obj[[factor_column]]) <- sub("^ ", "", levels(obj[[factor_column]]))
                Encoding(levels(obj[[factor_column]])) <- "UTF-8"
                Encoding(levels(obj[[factor_column]])) 
            }
            character_columns <- names(obj[which(sapply(obj, is.character))])
            for (character_column in character_columns) {
                levels(obj[[character_column]]) <- sub("^ ", "", levels(obj[[character_column]]))
                Encoding(levels(obj[[character_column]])) <- "UTF-8"
                Encoding(levels(obj[[character_column]])) 
            }
            assign(object, obj)
        }
    }
}
save.image(file = "data/bwi.RData")
