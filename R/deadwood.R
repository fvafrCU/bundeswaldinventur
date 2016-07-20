harmonize_deadwood <- function(deadwood, to_bwi_2 = TRUE) {
    names(deadwood) <- tolower(names(deadwood))
    if (to_bwi_2) {
        # Bezeichnung <tl> durch <lge> (wie bei BWI2) ersetzen
        names(deadwood)[which("tl" == names(deadwood))] <- "lge"
        # Mittendurchmesser wird bei Bruchstücken (mit 2 Durchmessern als mittlerer
        # Walzedurchmesser ermittelt
        deadwood$dm <- ifelse(deadwood$tsd > 0 & ! is.na(deadwood$tsd),
                             round(200 * 
                                   sqrt(deadwood$tvol / deadwood$lge / pi), 1),
                             deadwood$tbd)
        # BWI3 has former tart 1 split into 11, 12 and 13. Undo that:
        deadwood[["tart"]][deadwood[["tart"]] %in% 11:13]  <- 1
    } else {
        #Attribut <anz> einfügen
        deadwood$anz <- rep(1,length(deadwood[,1]))
    }
    return(deadwood)
}
purge_deadwood_to_2 <- function(totholz) {
    t <- subset(totholz, 
                (tart != 4 & tbd >= 20) | 
                (tart == 4 & (tbd >= 60 | lge >= 0.5))
                )
    return(t)

}
drop_variables <- function(totholz) {
    needed <- c("tnr", "tvol", "thf", "anz", "tart", "tbagr", "tzg")
    totholz <- totholz[, needed] 
    return(totholz)
}
deadwood <- function(totholz = t3, v = c("tnr", "enr"), 
                     bwi = 3) {
    totholz <- harmonize_deadwood(totholz, to_bwi_2 = bwi == 3)
    # Auswahl nach Aufnahme-Kriterium:
    # nur sinnvoll, wenn Totholz-Aufnahme der BWI 3 vorliegt, dann kann entweder
    # nachden Totholfaufnahmekriterien der BWI 3 oder BWI 2 ausgewertet werden
    if (bwi == 3) {
        totholz <- purge_deadwood_to_2(totholz)
    }
    l <- eval(parse(text=paste0("list(", 
                                paste0(v, " = totholz$", v, collapse = ", "),
                                ")")))
    t <- aggregate(totholz$tvol * totholz$thf * totholz$anz, 
                   by=l,
                   FUN=sum)
    n <- paste0(v, collapse = ", ")
    index <- eval(parse(text = paste0("with(t, order(", n, "))")))
    return(t[with(t, index), ])
}
