#' Selektiert die zu einem Stratum gehoerenden Stichproben
#'
#' "Stratifikator"-Funktion: selektiert die zu einem Stratum (Befundeinheit,
#' Subdomaene, Subpopulation) gehoerenden Stichproben (Traktecken) anhand
#' verschiedener Merkmale, die in der Tabelle \code{ecken} zusammengefasst sind
#' die Tabelle \code{ecken} enthaelt nur die Stichproben im Wald (Holzboden,
#' Nichtholzboden, begehbar, nicht begehbar). Aufnahmen liegen natuerlich nur
#' fuer die Traktecken auf begehbarem Holzboden vor.
#'
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 20.09.2013
#' @section Aktualisierungen: 01.12.2014 leere Menge korrigiert
#' @section Note: in \code{stratum} wird zunaechst die Gesamt-Menge, also die
#'  vollstaendige Tabelle \code{ecken} uebergeben, die dann entsprechend der in
#'  der Liste \code{auswahl} uebergebenen Attribute und den zulaessigen
#'  Werte-Mengen als Untermenge heraus gefiltert wird.
#' @section Refactored: 
#' The whole thing Fri Aug 12 15:33:04 CEST 2016 by
#' \email{dominik.cullmann@@forst.bwl.de}.
#'  See commit 57504d4e9570bb560c38195d3f308617c72c1d06.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt,
#'  anhand derer die Auswahl erfolgt.
#' @param ecken Tabelle mit allen zur Selektion dienenden Eckenmerkmalen.
#' @export
#' @return Untermenge der Ecken, die der Auswahl entsprechen.
stratum.fun <- function(auswahl, ecken) {
  # Quote characters in list via paste() to preserve them while pasting.
  # Character vectors of length 1 go to shQuote(), others to paste() for
  # unkown reasons. Found out by try and error.
  idx <- which(unlist(lapply(auswahl, is.character)) &
    unlist(lapply(auswahl, length)) == 1)
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
  stratum[is.na(stratum)] <- 0 # TODO: Why that?
  return(stratum)
}

