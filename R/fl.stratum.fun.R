#' Berechnet die Flaeche entsprechend einer Auswahl
#'
#' Funktion berechnet die Flaeche entsprechend der Definition in \code{auswahl}.
#'
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Version: 30.07.2014
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt,
#'  anhand derer die Auswahl fuer das Stratum erfolgt.
#' @param ecken Traktecken-Merkmale.
#' @param trakte Trakt-Kennwerte (m, m_HB, m_bHB, m_Wa).
#' @param A Flaeche des Inventurgebietes in ha.
#' @inheritParams fvbn.kreis.fun.1 
#' @export
#' @return Liste mit der Flaeche und dem Standardfehler.
fl.stratum.fun <- function(auswahl, ecken, trakte, A,
                           trakte.3 = get_data("trakte.3") # TODO: see below.
) {
  te <- stratum.fun(auswahl, ecken)
  nte.t <- stats::aggregate(rep(1, length(te[, 1])), by = list(te$TNr), sum)
  names(nte.t) <- c("TNr", "nte")
  utils::head(nte.t)
  nte.t <- merge(trakte[TRUE, c("TNr", "m")], nte.t, by = "TNr", all.x = T)
  nte.t[is.na(nte.t)] <- 0
  r.list <- r.variance.fun(nte.t[, 2:3], length(trakte.3[, 1])) # TODO: is trakte.3 really the one? Is this a bug?
  return(list(Flaeche = r.list$R.xy * A, SE_Flaeche = sqrt(r.list$V.R.xy) * A))
}

