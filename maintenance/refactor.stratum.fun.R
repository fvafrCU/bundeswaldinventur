refactored.stratum.fun <- function(auswahl, ecken) {
    # Quote characters in list via paste() to preserve them while pasting.
    # Character vectors of length 1 go to shQuote(), others to paste() for
    # unkown reasons. Found out by try and error.
    idx <- which(unlist(lapply(auswahl, is.character)) & 
                 unlist(lapply(auswahl, length)) == 1
             ) 
    auswahl[idx] <- shQuote(auswahl[idx]) 
    idx <- which(unlist(lapply(auswahl, is.character)))
    auswahl[idx] <- paste0(auswahl[idx]) # now this is some magic I don't understand


    columns <- paste0('ecken[["', names(auswahl), '"]]')
    condition <- paste(columns, auswahl, sep = " %in% ", collapse = " & ")
    return_columns <- deparse(c("TNr", "ENr", names(auswahl)))
    text <- paste0("ecken[", condition, ", ", return_columns, "]")
    stratum <- eval(parse(text = text))
    # Convert factors like the original.
    idx <- which(sapply(stratum, class) == "factor")
    values <- as.numeric(unlist(stratum))
    dim(values) <- dim(stratum)
    colnames(values) <- names(stratum)
    stratum <- as.data.frame(values)
    stratum[is.na(stratum)] <- 0 #TODO: Why that?
    return(stratum)
}

stratum.fun <- function(auswahl,ecken){
    ecken <- as.data.frame(ecken)
    #Attribute f\u00fcr Auswahl
    attribute <- names(auswahl)
    k <- length(attribute)
    n <- length(ecken[,1])
    pos <- rep(0,k)
    #Position in der Tabelle <ecken> (Spalten-Nr.) und jeweilige Teilmenge
    #bestimmen
    stratum <- ecken
    for (i in 1:k)
    {
        #pos[i] <- grep(attribute[i],names(ecken),fixed=T)[1]
        #exaktes "matching"  k\u00e4/23.01.2015
        pos[i] <- which(names(ecken)==attribute[i])
        stratum <- stratum[stratum[,pos[i]]%in%auswahl[[i]], TRUE]
        if (is.factor(stratum[,pos[i]])) stratum[,pos[i]] <-
            as.numeric(stratum[,pos[i]])
    }
    stratum <- stratum[TRUE, c(1,2,pos)]
    n.stratum <- length(stratum[,1])
    stratum[is.na(stratum)] <- 0

    return(stratum)
}
devtools::load_all()
e3 <- get_data("ecken.3")
tm <- microbenchmark::microbenchmark(times = 400,
               stratum.fun(list(Wa=c(1:5), EigArt="BW"), ecken.3),
               refactored.stratum.fun(list(Wa=c(1:5), EigArt="BW"), ecken.3)
               )
microbenchmark::autoplot.microbenchmark(tm)

