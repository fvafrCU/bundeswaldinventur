load("data/bwi.RData")
Encoding(levels(baeume.1$Bemerk)) <- "UTF-8"
Encoding(levels(baeume.2$Bemerk)) <- "UTF-8"
Encoding(levels(baeume.3$Bemerk)) <- "UTF-8"
Encoding(levels(totholz.2$Bemerkung)) <- "UTF-8"
Encoding(levels(totholz.3$Bemerkung)) <- "UTF-8"
save.image(file = "data/bwi.RData")
