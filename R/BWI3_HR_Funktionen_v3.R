# @section Version: Version 3 vom 19.08.2014 basierend auf Version 2 vom 
#  15.04.2014
# @section Aktualisierungen: 
#  22.07.2015 (Berg) Aenderungen der Kommentare zu Roxygen Format \cr
#  09.04.2015 (Cullmann) Ergaenzung \code{tryCatch} in 
#    \code{\link{verjg.kl4.bagrupp.fun}} \cr
#  09.04.2015 Einfuegung von \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2e}}
#   \cr
#  09.02.2015 Funtion \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}} bzw. 
#    \code{\link{iVB.bilanz.bagr.akl.dkl.fun.2g}} korrigiert entsprechend
#    Fassung \cr
#  09.02.2015 \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2g}} bzw. 
#		\code{\link{iVB.bilanz.bagr.akl.dkl.fun.2g}} aus 
#		"BWI3/Programme/HR/BWI3_HR_Funktionen_v3_xxx.r"
#		betreffend (1) Aggregation auf alle Baumarten durch eigene sepearte
#		Aggregation auf BAGR "AlleBA" sowie (2) bei Biomasse-Zuwachs Beschraenkung
#		auf Derbholz-Kollektiv \cr
#	09.02.2015 Variante \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2g}} bzw.
#		\code{\link{iVB.bilanz.bagr.akl.dkl.fun.2g}} eingefuegt, berechnet 
#		zusaetzlich Grundzuwachs \cr
#	07.02.2015 Korrektur in \code{\link{dkl.lab.fun}} sowie in diversen Funktionen:
#		\code{D.k <- length(dkl.lab[!is.na(dkl.lab)])} statt 
#		\code{D.k <- length(dkl.lab)} \cr
#	23.01.2015 Korrektur in \code{\link{stratum.fun}} exaktes \code{matching} mit
#		\code{which} \cr
#	15.12.2014 Erweiterung um "alle BA" in 
#    \code{\link{VB.A.bagrupp.akl.dkl.stratum.fun.3}}
#		und \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}} \cr
#	14.12.2014 neu Funktion \code{\link{stamm.merkmale.bagr.fun}} \cr
#	11.12.2014 Korrektur in \code{\link{fvbn.stratum.fun.1}} \cr
#	03.12.2014 neue Funktionen: \code{\link{fvbn.kreis.fun.1}}, 
#		\code{\link{fvbn.stratum.fun.1}}, \code{\link{fvbn.stratum.fun.2}} \cr
#	02.12.2014 neue Version \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2d}}
#		enthaelt auch die Summenwerte fuer alle Baumarten, wenn nach 
#		Baumartengruppen differenziert ausgewertet wird. \cr
#	01.12.2014 Verbesserung \code{\link{stratum.fun}}: "leere Menge" \cr
#	19.11.2014 neue Funktion \code{\link{fl.proz.stratum.fun}} \cr
#	01.11.2014 Erweiterung um Funktion \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2c}}
#		mit Derbholzstammzahlen im Hauptbestand \cr
#	27.10.2014 Funktion \code{\link{ntns.stratum.fun.2}} berechnet NTNS fuer die 
#		Schicht "kl 4m" oder "gr 4m" \cr
#	21.10.2014 \code{\link{verjg.tab.fun}} eingefuegt \cr
#	14.10.2014 \code{\link{fl.stratum.fun}} vom 30.07. am eingefuegt \cr
#	11.10.2014 \code{\link{biotop.baeume.fun}} \cr
#	11.10.2014 \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2b}} (Erweiterung 
#		um BA-Proz. mit Fehler) \cr
#	10.10.2014 Funktion zur Auswertung der Biotop-Baeume 
#		\code{\link{biotop.baeume.fun}} \cr
#	05.10.2014 Fehler bei Bestimmung der Anzahl Altersklassen <A.k>  korriegiert.
#		Abfangen des Falls, dass \code{<A.k> = 0: if (A.k == 0) A.k <- 1} \cr
#	24.08.2014 Korrektur bei den Funktionen fuer Periodenauswertungen:
#		\code{\link{VB.A.bagrupp.akl.dkl.stratum.fun.2}}, 
#		\code{\link{VB.A.bagrupp.akl.dkl.stratum.fun.3}} und
#		\code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}} Wurde festgelegt, 
#		dass die Eckenmerkmale der aktuellen Inventur (=BWI 3) fuer die 
#		Stratifikation (Argument \code{auswahl}) gilt, z.B. die Eigentumsklasse
#		\cr
#	19.08.2014 Zuwachs-Abgang fuer die Periode 1987 bis 2002 (BWI 1 zu 2)
#		\code{\link{ntns.stratum.fun}} zur Naturnaeheauswertung \cr
#	18.08.2014 Ergaenzt um \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.bwi12}}
#		\cr
#	13.08.2014 ergaenzungen von Funktionen zur Verjuengungsauswertung:
#		\code{\link{verjg.bagr.fun}} mit Standard BWI-Baumartengruppen,
#		\code{\link{verjg.bagrupp.fun}} mit frei definierbaren Baumartengruppen,
#		\code{\link{verjg.kl4.bagr.fun}} Verjuengung aus Bestockungsansprache 
#		<= 4m Hoehe nach BWI-Baumartengruppen und Verjuengungsart,
#		\code{\link{verjg.kl4.bagrupp.fun}} mit frei definierbaren Baumartengruppen
#		\cr
#	12.08.2014 Ergaenzung um Funktionen zur Verbissauswertung: 
#		\code{\link{verbiss.bagr.fun}} mit Szandard BWI-Baumartengruppen, 
#		\code{\link{verbiss.bagrupp.fun}} mit frei definierbaren Baumartengruppen
#		 \cr
#	30.07.2014 Ergaenzung fehlender Funktion 
#		\code{\link{iVB.bilanz.bagr.akl.dkl.fun}} \cr
#	30.07.2014 Kleinere Korrekturen in 
#		\code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}}
# @section TODO:
#	27.10.2014 Attributname "BW" fuer Bannwald in "Bannw" aendern wegen 
#		Verwechslung in \code{\link{stratum.fun}} mit "WGNr_BW" Pruefen, ob 
#		Korrektur moeglich

#-------------------------------------------------------------------------------
#' Zustandshochrechnung fuer einen Landkreis
#' 
#' Funktion berechnet Standard-FVBN-Auswertung fuer den Landkreis mit 
#' \code{kreiscode} fuer die in der \code{eig.list} aufgefuehrten 
#' Eigentumskategorien {gw,stw,kw,oew,pw,gpw,mpw,kpw} fuer die in 
#' \code{bwi.list} aufgefuehrten BWI-Aufnahmen {1,2,3}.Es wird die Funktion 
#' \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2d}} verwendet, welche neben 
#' Baumartengruppen auch die Summenwerte fuer alle Baumarten liefert.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 02.12.2014
#' @param kreiscode Code fuer den Landkreis.
#' @param eig.list Liste mit Eigentumskategorien (moegliche Kategorien: gw, stw, 
#'  kw, oew, pw, gpw, mpw, kpw).
#' @param bwi.list Liste mit BWI-Aufnahmen (1, 2, 3).
#' @param bagr Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @return Dataframe-Tabelle mit FVBN-Auswertung nach Eigentumsklasse und 
#'  aufgelisteter BWI.
fvbn.kreis.fun.1 <- function(kreiscode,eig.list,bwi.list,bagr){
  auswahl <- list(Wa=c(3,5),Begehbar=1,Kreis=kreiscode)
  k <- length(eig.list)
  eig.list <- toupper(eig.list)
  eig.list <- sub("STW","StW",eig.list)
  auswahl.i <- auswahl
  l <-length(bwi.list)
  fvbn.bagr.krs   <- list()
  
  for (i in 1:k)
  {
    if (eig.list[i]=="OEW")
      {auswahl.i$EigArt=c("BW","StW","KW")} else
      if (eig.list[i]%in%c("GPW","MPW","KPW"))
        {auswahl.i$EigArt2=eig.list[i]}else
        if (eig.list[i]%in%c("BW","StW","KW","PW"))
        {auswahl.i$EigArt=eig.list[i]}
    #BWI
    for (j in 1:l)
    {
      if (bwi.list[j]==1)
      {auswahl.i$Wa=c(1:3);
        baeume <- baeume.1; ecken <- ecken.1; trakte <- trakte.1;
        A.i <- A.12;inv <- 1} else
        if (bwi.list[j]==2)
        {auswahl.i$Wa=c(1:3);
          baeume <- baeume.2; ecken <- ecken.2; trakte <- trakte.2;
          A.i <- A.12; inv <- 2} else
          {auswahl.i$Wa=c(3,5);
            baeume <- baeume.3; ecken <- ecken.3; trakte <- trakte.3;
            A.i <- A; inv <- 2}
      index <- (j-1)*k + i

      fvbn.bagr.krs[[index]] <- FVBN.bagrupp.akl.dkl.stratum.fun.2d(
        baeume,ecken,trakte,A.i,inv,bagr,
        list(A.ob=500,A.b=500),list(D.unt=0,D.ob=500,D.b=500,Ndh=F),
        auswahl.i)
      fvbn.bagr.krs[[index]]$Kreis <-
          as.character(kreise[kreise$codeKreis==kreiscode,3])
      fvbn.bagr.krs[[index]]$Eigentumsart <- eig.list[i]
      fvbn.bagr.krs[[index]]$BWI <- bwi.list[j]
    }
  }

  return(fvbn.bagr.krs)
}#Ende <fvbn.kreis.fun.1>

#-------------------------------------------------------------------------------
#' Zustandshochrechnung fuer ein Stratum
#'
#' Funktion berechnet Standard-FVBN-Auswertung fuer die Befundeinheit 
#' \code{auswahl} fuer die in der \code{eig.list} aufgefuehrten 
#' Eigentumskategorien {gw, stw, kw, oew, pw, gpw, mpw, kpw} sowie die in 
#' \code{bwi.list} aufgefuehrten BWI-Aufnahmen {1,2,3}. Es wird die Funktion 
#' \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2d}} verwendet, welche neben 
#' Baumartengruppen auch die Summenwerte fuer alle Baumarten liefert.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 02.12.2014
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param eig.list Liste mit Eigentumskategorien (moegliche Kategorien: gw, stw, 
#'  kw, oew, pw, gpw, mpw, kpw).
#' @param bwi.list Liste mit BWI-Aufnahmen (1, 2, 3).
#' @param bagr Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @return Dataframe-Tabelle mit FVBN-Auswertung nach Eigentumsklasse und 
#'  aufgelisteter BWI.
fvbn.stratum.fun.1 <- function(auswahl,eig.list,bwi.list,bagr){
  
  k <- length(eig.list)
  eig.list <- toupper(eig.list)
  eig.list <- sub("STW","StW",eig.list)
  #k\u00e4/11.12.14 
  l <-length(bwi.list)
  fvbn.bagr.stratum   <- list()
  
  for (i in 1:k)
  {
    auswahl.i <- auswahl #k\u00e4/11.12.14
    if (eig.list[i]=="OEW")
      {auswahl.i$EigArt=c("BW","StW","KW")} else
      if (eig.list[i]%in%c("GPW","MPW","KPW"))
        {auswahl.i$EigArt2=eig.list[i]}else
        if (eig.list[i]%in%c("BW","StW","KW","PW"))
          {auswahl.i$EigArt=eig.list[i]}
    #BWI
    for (j in 1:l)
    {
      if (bwi.list[j]==1)
      {auswahl.i$Wa=c(1:3);
        baeume <- baeume.1; ecken <- ecken.1; trakte <- trakte.1;
        A.i <- A.12;inv <- 1} else
        if (bwi.list[j]==2)
        {auswahl.i$Wa=c(1:3);
          baeume <- baeume.2; ecken <- ecken.2; trakte <- trakte.2;
          A.i <- A.12; inv <- 2} else
          {auswahl.i$Wa=c(3,5);
            baeume <- baeume.3; ecken <- ecken.3; trakte <- trakte.3;
            A.i <- A; inv <- 2}
      index <- (j-1)*k + i

      fvbn.bagr.stratum[[index]] <- FVBN.bagrupp.akl.dkl.stratum.fun.2d(
        baeume,ecken,trakte,A.i,inv,bagr,
        list(A.ob=500,A.b=500),list(D.unt=0,D.ob=500,D.b=500,Ndh=F),
        auswahl.i)

      fvbn.bagr.stratum[[index]]$Eigentumsart <- eig.list[i]
      fvbn.bagr.stratum[[index]]$BWI <- bwi.list[j]
    }
  }

  return(fvbn.bagr.stratum)
}#Ende <fvbn.stratum.fun.1>

#-------------------------------------------------------------------------------
#' Zustandshochrechnung fuer ein Stratum
#'
#' Funktion berechnet Standard-FVBN-Auswertung fuer die Befundeinheit 
#' \code{auswahl} fuer die in der \code{eig.list} aufgefuehrten 
#' Eigentumskategorien {gw, stw, kw, oew, pw, gpw, mpw, kpw} sowie die in 
#' \code{bwi.list} aufgefuehrten BWI-Aufnahmen {1,2,3}. Es wird die Funktion 
#' \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2d}} verwendet, welche neben 
#' Baumartengruppen auch die Summenwerte fuer alle Baumarten liefert.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 02.12.2014
#' @param auswahl auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param eig.list Liste mit Eigentumskategorien (moegliche Kategorien: gw, stw, 
#'  kw, oew, pw, gpw, mpw, kpw).
#' @param bwi.list Liste mit BWI-Aufnahmen (1, 2, 3).
#' @param bagr Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @param a.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#' @param d.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0, D.ob=70, D.b=10, Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, 
#'  dass zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt 
#'  \code{D.unt} als unterste Schwelle.
#' @return Dataframe-Tabelle mit FVBN-Auswertung nach Eigentumsklasse und 
#'  aufgelisteter BWI.
fvbn.stratum.fun.2 <- function(auswahl,eig.list,bwi.list,bagr,a.klass,d.klass){
  
  k <- length(eig.list)
  eig.list <- toupper(eig.list)
  eig.list <- sub("STW","StW",eig.list)
  #k\u00e4/12.12.14
  l <-length(bwi.list)
  fvbn.bagr.stratum   <- list()
  
  for (i in 1:k)
  {
    auswahl.i <- auswahl #k\u00e4/12.12.14
    if (eig.list[i]=="OEW")
      {auswahl.i$EigArt=c("BW","StW","KW")} else
      if (eig.list[i]%in%c("GPW","MPW","KPW"))
        {auswahl.i$EigArt2=eig.list[i]}else
        if (eig.list[i]%in%c("BW","StW","KW","PW"))
        {auswahl.i$EigArt=eig.list[i]}
    #BWI
    for (j in 1:l)
    {
      if (bwi.list[j]==1)
      {auswahl.i$Wa=c(1:3);
        baeume <- baeume.1; ecken <- ecken.1; trakte <- trakte.1;
        A.i <- A.12;inv <- 1} else
        if (bwi.list[j]==2)
        {auswahl.i$Wa=c(1:3);
          baeume <- baeume.2; ecken <- ecken.2; trakte <- trakte.2;
          A.i <- A.12; inv <- 2} else
          {auswahl.i$Wa=c(3,5);
            baeume <- baeume.3; ecken <- ecken.3; trakte <- trakte.3;
            A.i <- A; inv <- 2}
      index <- (j-1)*k + i

      fvbn.bagr.stratum[[index]] <- FVBN.bagrupp.akl.dkl.stratum.fun.2d(
        baeume,ecken,trakte,A.i,inv,bagr,a.klass,d.klass,auswahl.i)

      fvbn.bagr.stratum[[index]]$Eigentumsart <- eig.list[i]
      fvbn.bagr.stratum[[index]]$BWI <- bwi.list[j]
    }
  }

  return(fvbn.bagr.stratum)
}#Ende <fvbn.stratum.fun.2>

#-------------------------------------------------------------------------------

#(2) Funktionen zur Auswertung der Inventurperiode zwischen BWI 2 und 3
#Hinweis: diese Version ist speziell f\u00fcr die Inventurperiode 2003 bis 2012
#TODO: Verallgemeinerte Version, die auch auf die Periode 1987 bis 2002
#anwendbar ist

#-------------------------------------------------------------------------------
#(2.1) AUSGESCHIEDENER VORRAT
#-------------------------------------------------------------------------------
#' Aggregiert den ausgeschiedenen Vorrat
#' 
#' Funktion wertet nach freien Baumarten-Gruppen und Alters- und Durchmesser-
#' Klassen im Stratum den ausgeschiedenen Vorrat aus (Derbholz mR, Erntevolumen 
#' oR, oberird. Biomasse oiB). Im Unterschied zu 
#' \code{VB.A.bagrupp.akl.dkl.stratum.fun.3} wird hier als 4. Groesse die 
#' ausgeschiedene Stammzahl berechnet (in Version .3 das Erntevolumen im 
#' Hauptbestand).
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 14.03.2014
#' @section Achtung: Voraussetzung ist, dass die Tabellen \code{baeume.23} 
#'  (Baeume_B-Tabelle der Vorinventur (fuer BWI 3 die Tabelle 
#'  "BWI_23_Baeume_B"), folgende Attribute muessen mind. enthalten sein: TNr, 
#'  ENr, BA, Pk, Alt2, BHD2, VolV2, oiB2, NHa1, StFl1. Man kann auch die 
#'  selektierten Attribute mit Namen ohne Kennziffer uebergeben, wenn bereits 
#'  eine eindeutige Auswahl der Attribute in <baeume> uebergeben wurde.), 
#'  \code{baeume.3}, (Baeume der Folgeinventur) \code{ecken.2}, \code{ecken.3}, 
#'  \code{trakte.3} sowie \code{bacode} eingelesen sind! \cr
#'  Das Baumattribut <Pk> muss enthalten sein!
#' @section Hinweis: Version mit freier Baumartengruppierung sowie jaehrlicher 
#'  flaechenbezogenen Nutzung analog flaechenbezogenem jaehrlichen Zuwachs sowie 
#'  Option einer Differenzierung nach Nutzungsart. \cr
#'  Ratio-Schaetzer-Varianz ueber \code{r.variance.fun} (Matrizen-Notation) 
#'  berechnet! \cr
#'  Version fuer Periode BWI 2 zu 3! \cr
#'  Auswertung erfolgt auf dem gemeinsamen Netz im Unterschied zu
#'  der historischen Version VB.A.BAGR.akl.dkl.stratum.fun, welche das Nutzungsgeschehen 
#'  auf der bei der BWI 2 erfassten Flaeche abdeckte! \cr
#'  Fuer die Berechnung der flaechenbezogenen Nutzungen muss die mittlere 
#'  Baumartenflaeche der Periode berechnet werden, d.h. es werden auch die 
#'  Standflaechen der Folgeaufnahme benoetigt! \cr 
#'  Dies wird von der Funktion \code{\link{mbaf.bagr.alt.bhd.pm.fun}} 
#'  uebernommen! \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @section TODO: Verallgemeinerung bzw. Variante fuer BWI 1 zu 2 !!!!
#' @param BA.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0, D.ob=70, D.b=10, Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, 
#'  dass zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt 
#'  \code{D.unt} als unterste Schwelle.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param N.art Differenzierung nach Nutzungsart. TRUE: Trennung nach "geerntet" 
#'  = Pk 2,3,9 und "ungenutzt" = Pk 4, 5; FALSE: keine Trennung!
#' @param A Gesamtflaeche des Inventurgebiets in ha zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit Datum und 
#'  genutzter Baumversion), \strong{Stratum} (\code{auswahl}), \strong{nTE} 
#'  (Anzahl Ecken im Stratum), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler der HBF), \strong{Attribute} (Vektor mir 
#'  berechneten Attributen), \strong{Groessen} (Vektor mit berechneten Groessen 
#'  fuer Attribute (Wert, Standardfehler)), \strong{Nutzungsart} (2 Kategorien: 
#'  geernet oder ungenutzt, wenn diese nicht definiert sind, wird "insgesamt" 
#'  ausgegeben), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.VBN.A.NArt.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer das ausgeschiedene 
#'  Kollektiv), \strong{BAF.bagr.akl.dkl} (Array mit Werten und Standardfehlern 
#'  zu Baumartenflaechen nach Baumartengruppen, Alterklassen und 
#'  Durchmesserklassen), \strong{mPL.NArt.Bagr.Akl.Dkl} (mittlere kalkulierte 
#'  Periodenlaenge mit Standardfehler), \strong{mPL.Stratum} (mittlere 
#'  Periodenlaenge als gewogenes Mittel), \strong{se.mPL.Stratum} 
#'  (Standardfehler der mittleren Periodenlaenge (mPl.Stratum)), 
#'  \strong{nT.NArt.Bagr.Akl.Dkl} (Anzahl Trakte je Nutzungsart, 
#'  Baumartengruppen, Altersklassen und Durchmesserklassen).
VB.A.bagrupp.akl.dkl.stratum.fun.2 <-
          function(BA.grupp,A.klass,D.klass,auswahl,N.art,A){
  #Bei Fl\u00e4chenbezug Reduktion auf gemeinsames Netz!!
  #k\u00e4/28.02.2014
  #---
  #k\u00e4/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
  #aktuelle Inventur (BWI 3), z.B. die Eigentumsklassenzuordnung
  #alte Fassung:
  #auswahl$Begehbar=1; auswahl$Wa=c(1:3)
  #ecken.2.s <- stratum.fun(auswahl,ecken.2)
  #neu:
  ecken.2.s <- stratum.fun(list(Wa=c(1:3),Begehbar=1),ecken.2)
  #---
  auswahl$Wa=c(3,5)
  ecken.3.s <- stratum.fun(auswahl,ecken.3)
  #gemeinsames Netz Land BW BWI 2 und 3 auf begehbarem Holzboden
  ecken.23.hb <- merge(ecken.3.s[TRUE,  c("TNr", "ENr")],
      ecken.2.s[TRUE, c("TNr", "ENr")],by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,ecken.3[TRUE, c("TNr", "ENr", "PL", "PLkal")],
      by=c("TNr","ENr"))
  stratum <- ecken.23.hb
  trakte <- trakte.3
  t.pos <- length(trakte)#Anzahl Spalten in <trakte> wird ben\u00f6tigt, um
  #Attribut-Positionen zu bestimmen
  #--------------------
  #inv <- 1
  #stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  #<y> steht hier f\u00fcr die Anzahl der Traktecken auf begehbarem HB im Stratum
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Anzahl der Trakte im Stratum
  n.t.s <- length(y[,1])
  #Anf\u00fcgen der Anzahl Traktecken (Wald und Nicht-Wald)
  y <- merge(y,trakte[TRUE, c("tnr", "m")],by=c("tnr")) 
  # this was:  
  #  y <- merge(y,subset(trakte,select=c(tnr,m),by=c(tnr)))
  # where the ellipsis of subset() swallows up the misplaces by argument (meant 
  # for merge())! 

  #Alle Traktecken im Inventurgebiet
  x <- trakte$m
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #----------------
  #HBFl. [ha]
  T.hbf <- sum(y$y)/sum(x)*A
  var.T.hbf <-  nT/(nT-1)*T.hbf^2*(sum(y$y^2)/sum(y$y)^2+sum(x^2)/sum(x)^2
                      -2*sum(y$y*y$m)/sum(y$y)/sum(x))
  se.T.hbf <- var.T.hbf^0.5 #Standardfehler
  #Hier k\u00f6nnte als Alternative die Funktion <r.variance.fun> benutzt werden
  #Hierzu m\u00fcsste eine alle Trakte umfassende Matrix <xy> mit <m> und <y>
  #\u00fcbergeben werden
  #----------------
  #k\u00e4/28.02.2014:
  baeume <- baeume.23 #muss eingelesen sein!
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))

  #HINWEIS: beim ausgeschiedenen Vorrat wird der zur Periodenmitte
  #fortgeschriebene Vorrat verwendet! volv2,vole2,oib2
  baeume.s <- merge(
          baeume[TRUE, c("tnr", "enr", "stp", "bnr", "ba", "pk", "alt1", "alt2", "bhd1", "bhd2", "volv2", "vole2", "oib2", "nha1", "stfl1")],
          stratum[TRUE, c("tnr", "enr", "pl", "plkal")],by=c("tnr","enr"))

  #BA-Gruppe dazu spielen
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  n.bagr <- length(BA.grupp[[1]])
  #BAGR-Liste
  bagr.list <- BA.grupp[[1]]
  n.bagr <- length(bagr.list)

  baeume.s <- merge(baeume.s, bagr.tab[TRUE, c("ICode", "bagr")],
                    by.x="ba",by.y="ICode",all.x=T)
  names(baeume.s) <- tolower(names(baeume.s))

  #Folgeinventur: BA-Gruppen hinzuf\u00fcgen
  baeume.3.s <- merge(baeume.3[TRUE, c("TNr", "ENr", "STP", "BNr", "Pk", "BA", "Alt1", "Alt2", "BHD1", "BHD2", "StFl2")],
                      bagr.tab[TRUE, c("ICode", "bagr")],
                    by.x="BA",by.y="ICode",all.x=T)
  names(baeume.3.s) <- tolower(names(baeume.3.s))
  baeume.3.s <- merge(baeume.3.s,stratum[TRUE, c("tnr", "enr", "pl", "plkal")],
          by=c("tnr","enr"))

  #--------------------------------------------------
  #Mittlere Baumartenfl\u00e4chen nach BAGr, AKl und DKl zur Periodenmitte nach Trakt
  mbaf.bagr.akl.dkl.tnr <- mbaf.bagr.alt.bhd.pm.fun(
                                    baeume.s,baeume.3.s,A.klass,D.klass)

  #-------------
  #Klassifizierung des Ausgeschiedenen Vorrats durchf\u00fchren
  #Alter
  A.max <- 999
  #Hinweis: A-Klassifizierung nach fortgeschriebenem Alter: alt2!!!
  baeume.s$akl <- cut(baeume.s$alt2,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  #Hinweis: D-Klassifizierung nach fortgeschriebenem BHD: bhd2!!!
  baeume.s$dkl <- cut(baeume.s$bhd2, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])

  #Array f\u00fcr mittlere BAF zur PM nach BAGr, AKl, DKl
  BAF.bagr.akl.dkl  <- array(dim=c(2,n.bagr,A.k,D.k))

  if(N.art)
    {pk.list <- list(c(2,3,9),c(4,5));n.nart <- 2}else
    {pk.list <- list(c(2:5,9)); n.nart <- 1}
  #Array f\u00fcr Ergebnisse (Totals und SE jeweils nach Nutzungsart, BAGr, AKl, DKl)
  #Nutzungsart: aus <pk>: 2 = selektiv genutzt,  3 = fl\u00e4chig genutzt
  #                       4 = am Ort verblieben, 5 = abgestorben  (stehend)
  #                       9 = unauffindbar (wird der Kategorie geerntet
  #                           zugewiesen)
  #                       2,3,9 definieren den geernteten ausgeschiedenen Vorrat
  #                       4, 5 definieren den ungenutzten (tw. aus nat\u00fcrl. Mort.
  #                       stammenden) ungenutzten ausgeschiedenen Vorrat)
  #Aus diesen Kennzahlen werden die 2 Kategorien der Nutzungsart (NArt):
  #geerntet bzw. ungenutzt festgelegt.
  #Es gibt 4 Zielgr\u00f6\u00dfen <Y>:  V [m^3Dh mR], V Eor (Erntevolumen o. R.) [m^3E oR],
  #B (oberird. Biomasse) [t], N (Anzahl), f\u00fcr die jeweils der Gesamtwert der
  #Periode ("Total") und der j\u00e4hrliche Wert berechnet wird, sowie der
  #Stichprobenfehler (SE), und zwar jeweils f\u00fcr die 2 Kategorien "geerntet" /
  #"ungenutzt" sowie 9 Baumartengruppen, A.k Alters- und D.k Durchmesserklassen
  #1. Index: 1- 4: Perioden-Total (v, v.eor, b, n);
  #          4- 8: m. j\u00e4hrlicher Wert (j.v, j.v.eor, j.b, j.n);
  #          9-12: m. j\u00e4hrlicher Wert je ha     (k\u00e4/28.02.2014)
  #2. Index: 1: Wert; 2: SE;
  #3. Index: Nutzungsart (wenn <N.art> == TRUE (1: geerntet, 2: ungenutzt)
  #4. Index: BAGr; 5. Index: A-Klasse; 6. Index: D-Klasse
  Y.na.bagr.akl.dkl    <- array(dim=c(12,2,n.nart,n.bagr,A.k,D.k))
  #mittlere kal. Periodenl\u00e4nge mit Standard-Fehler
  mPL.na.bagr.akl.dkl   <- array(dim=c(2,2,n.bagr,A.k,D.k))
  #Anzahl Trakte (PSU) je NArt, BAGR, Akl, Dkl
  nT.na.bagr.akl.dkl   <- array(dim=c(n.nart,n.bagr,A.k,D.k))
  #Mittlere Straten-PL mit SE
  ne.T <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
            names(ne.T) <- c("tnr","n.te")
  #"Periodensumme" je Trakt (mit n Ecken gewogen)
  #Bei Nutzung wird kalendarische Periodenl\u00e4nge <plkal> verwendet!
  y.pl <- stats::aggregate(stratum$plkal,by=list(stratum$tnr),mean,na.rm=T)$x*ne.T$n.te
  #mittl. PL als gewogenes Mittel
  mpl.stratum <- sum(y.pl)/sum(ne.T$n.te)
  #Berechnung des Standardfehlers
  se.mpl.stratum <- mpl.stratum*
    sqrt(nT/(nT-1)*(sum(y.pl^2)/sum(y.pl)^2+sum(ne.T$n.te^2)/sum(ne.T$n.te)^2
                              -2*sum(y.pl*ne.T$n.te)/sum(y.pl)/sum(ne.T$n.te)))

  #----------------


  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        #Baumartenfl\u00e4che zur Periodenmitte aggregieren
        baf.ba <- mbaf.bagr.akl.dkl.tnr[
                      mbaf.bagr.akl.dkl.tnr[["bagr"]]==bagr.list[i]&mbaf.bagr.akl.dkl.tnr[["akl.pm"]]==akl.lab[j]&mbaf.bagr.akl.dkl.tnr[["dkl.pm"]]==dkl.lab[k],
                      c("tnr", "mbaf")]
        #Mit allen Trakten im Inventurgebiet vereinen
        xy.baf <- merge(trakte,baf.ba,by=c("tnr"),all.x=T)
        #NA eliminieren!
        xy.baf$mbaf[is.na(xy.baf$mbaf)] <- 0
        #xy.baf$mbaf <- xy.baf$mbaf
        #xy. <- cbind(xy$m,xy$mbaf/10000)
        #Total der Baumartenfl\u00e4che
        R.list <- r.variance.fun(cbind(xy.baf$m,xy.baf$mbaf),nT)
        BAF.bagr.akl.dkl[1,i,j,k] <-  R.list$R.xy*A
        BAF.bagr.akl.dkl[2,i,j,k] <-  sqrt(R.list$V.R.xy)*A

        for (i.n in 1:n.nart)  #Nutzungsart (geerntet, ungenutzt), wenn gesetzt!
        {

          baeume.ba <- baeume.s[
              baeume.s[["pk"]]%in%pk.list[[i.n]]&baeume.s[["bagr"]]==bagr.list[i]&baeume.s[["akl"]]==akl.lab[j]&baeume.s[["dkl"]]==dkl.lab[k],
              c("tnr", "enr", "pk", "volv2", "vole2", "oib2", "nha1", "plkal")]
          if (length(baeume.ba[,1])== 0)
          {
             Y.na.bagr.akl.dkl[,1,i.n,i,j,k] <- rep(0,12) #Zielgr\u00f6\u00dfe
             Y.na.bagr.akl.dkl[,2,i.n,i,j,k] <- rep(0,12) #Stichprobenfehler (SE)

             mPL.na.bagr.akl.dkl[1:2,i.n,i,j,k]   <- rep(0,2)
             nT.na.bagr.akl.dkl[i.n,i,j,k]        <- 0        #n PSU (Trakte)
          }else
          {
            #Nach Trakt aggregieren
            #fortgeschrieben: volv2, vole2, oib2!!!!
            #Ausgeschiedener Derbholz-Vorrat [m^3 mR] als "v"
            xy <- stats::aggregate(baeume.ba$volv2*baeume.ba$nha1,by=list(baeume.ba$tnr),
                  sum)
            names(xy) <- c("tnr","v")
            #Ausgeschiedener Vorrat Erntevolumen [m^3 oR] als "v.eor"
            xy <- cbind(xy,
              stats::aggregate(baeume.ba$vole2*baeume.ba$nha1,
                                              by=list(baeume.ba$tnr),sum)$x )
            names(xy)[3] <- "v.eor"
            #Ausgeschiedener Vorrat in oberird. Biomasse [t] als "b"
            xy <- cbind(xy,stats::aggregate(baeume.ba$oib2*baeume.ba$nha1,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
            names(xy)[4] <- "b"
            #Anzahl B\u00e4ume als "n"
            xy <- cbind(xy,stats::aggregate(baeume.ba$nha1,by=list(baeume.ba$tnr),
                        sum)$x)
            names(xy)[5] <- "n"
            #Mittlere kal. Periodenl\u00e4nge je Trakt
            mpl <- stats::aggregate(baeume.ba$plkal,by=list(baeume.ba$tnr),mean)$x

            #J\u00e4hrlicher ausgeschiedener Derbholzvorrat
            xy$j.v <- xy$v/mpl
            #J\u00e4hrlicher ausgeschiedener Erntevorrat
            xy$j.v.eor <- xy$v.eor/mpl
            #J\u00e4hrlicher ausgeschiedener oi. Biomassevorrat
            xy$j.b <- xy$b/mpl
            #J\u00e4hrliche ausgeschiedene Stammzahl
            xy$j.n <- xy$n/mpl

            #Anzahl Traktecken je Trakt (Wald- und Nichtwald) und <mbaf>
            #hinzuf\u00fcgen   Hinweis: <xy.baf> enth\u00e4lt jetzt auch <m_bhb>!
            #xy <- merge(xy,xy.baf,by=c("tnr"))
            xy <- merge(xy.baf,xy,by=c("tnr"),all.x=T)
            #for (ii in 7:14) {xy[is.na(xy[,ii]),ii] <- 0}
            xy[is.na(xy)] <- 0

            #Anzahl Trakte (i.S. von PSU) im Teilkollektiv i.n,i,j,k
            nT.na.bagr.akl.dkl[i.n,i,j,k] <- length(xy[,1])

            #mittlere kal. Periodenl\u00e4nge mit Standard-Fehler
            #Anzahl Ecken je Trakt bestimmen
            nb.TE <- stats::aggregate(rep(1,length(baeume.ba[,1])),
                                    by=list(baeume.ba$tnr,baeume.ba$enr),sum)
            ne.T <- stats::aggregate(rep(1,length(nb.TE[,1])),by=list(nb.TE$Group.1),
                              sum)
            names(ne.T) <- c("tnr","n.te")
            #"Periodensumme" je Trakt (mit n Ecken gewogen)
            y.pl <- mpl*ne.T$n.te
            R.list <- r.variance.fun(cbind(ne.T$n.te,y.pl),nT)
            #mittl. PL als gewogenes Mittel
            mPL.na.bagr.akl.dkl[1,i.n,i,j,k] <- R.list$R.xy
            #alte Fassung
            #mPL.na.bagr.akl.dkl[1,i.n,i,j,k] <- sum(y.pl)/sum(ne.T$n.te)
            #Berechnung des Standardfehlers
            mPL.na.bagr.akl.dkl[2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)

            for (l in 1:8)  #4 Totale, 4 J\u00e4hrliche Totale
            {
              #Zielgr\u00f6\u00dfen Y ausgeschiedenes Kollektiv{V,V.EoR,B,N)
              #Perioden-Total, j\u00e4hrl. Total, j\u00e4hrl. Ha-Wert (k\u00e4/01-03-2014)
              R.list <- r.variance.fun(cbind(xy$m,xy[,(l+t.pos+1)]),nT)
              Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- R.list$R.xy*A
              #Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- sum(xy[,(1+l)])/sum(x)*A
              #Zugeh\u00f6riger Stichprobenfehler
              Y.na.bagr.akl.dkl[l,2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)*A
            }#End for l (Zielgr\u00f6\u00dfen)
            #Offset f\u00fcr Spalten-Position der 4 j\u00e4hrliche Ha-Werte
            off <- length(xy)-4
            #Fl\u00e4chenbezogene Zielgr\u00f6\u00dfen:
            for (l in 1:4) #4 j\u00e4hrliche Ha-Werte
            {
              #Zielgr\u00f6\u00dfen Y ausgeschiedenes Kollektiv{V,V.EoR,B,N)
              #Perioden-Total, j\u00e4hrl. Total, j\u00e4hrl. Ha-Wert (k\u00e4/01-03-2014)
              R.list <- r.variance.fun(cbind(xy$mbaf,xy[,(l+off)]),nT)
              Y.na.bagr.akl.dkl[(l+8),1,i.n,i,j,k] <- R.list$R.xy
              #Zugeh\u00f6riger Stichprobenfehler
              Y.na.bagr.akl.dkl[(l+8),2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)
            }#

          }#End if ... else
        }#End for i.n (Nutzungsart: geerntet, ungenutzt)
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
  }#End for i (BAGR)

  #-----------------------
  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k)
  
  #DKL-Labels
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #k\u00e4/16.07.14
  
  #Tabelle f\u00fcr BA-Gruppen
  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
    Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              Attribute=c("V_DhmR", "V_EoR", "oiB", "N_Dh",
                      "V_DhmR/J", "V_EoR/J", "oiB/J", "N_Dh/J",
                      "V_DhmR/ha/J", "V_EoR/ha/J", "oiB/ha/J", "N_Dh/ha/J"),
              "Gr\u00f6\u00df" = c("Wert","Standardfehler"),
              Nutzungsart = n.nart,
              BAGR=bagr.list, AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.VBN.A.NArt.Bagr.Akl.Dkl=Y.na.bagr.akl.dkl,
              BAF.bagr.akl.dkl=BAF.bagr.akl.dkl,
              mPL.NArt.Bagr.Akl.Dkl=mPL.na.bagr.akl.dkl,
              mPL.Stratum = mpl.stratum, SE.mPL.Stratum = se.mpl.stratum,
              nT.NArt.Bagr.Akl.Dkl=nT.na.bagr.akl.dkl))
}#End <VB.A.bagrupp.akl.dkl.stratum.fun.2>

#-------------------------------------------------------------------------------
#' Aggregiert den ausgeschiedenen Vorrat
#' 
#' Funktion wertet nach Baumarten-Gruppen und Alters- und Durchmesser-Klassen im 
#' Stratum den ausgeschiedenen Vorrat (Derbholz mR, Erntevolumen oR, oberird. 
#' Biomasse oiB) aus. 
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 08.07.2014
#' @section Aktualisierungen:
#'  28.02.2014 Bei Flaechenbezug Reduktion auf gemeinsames Netz!!
#'  01.05.2014 Variante mit V.EoR im Hauptbestand anstelle von Stammzahl \cr
#'  15.12.2014 Erweiterung um "Alle BA".
#' @section Achtung: Erwartet wird die Baeume_B-Tabelle der Vorinventur (fuer 
#'  BWI 3 die Tabelle "BWI_23_Baeume_B") \code{baeume.23} sowie der 
#'  Folgeinventur \code{baeume.3} Wichtig: das Baumattribut <Pk> muss enthalten 
#'  sein. Die Tabelle \code{baeume.23} muss mindestens die Attribute 
#'  \strong{TNr, ENr, BA, Pk, Alt2, BHD2, VolV2, oiB2, NHa1, StFl1} enthalten, 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in <baeume> 
#'  uebergeben wird.
#' @section Hinweis: Version mit freier Baumartengruppierung sowie jaehrlicher 
#'  flaechenbezogenen Nutzung analog flaechenbezogenem jaehrlichen Zuwachs sowie 
#'  Option der einer Differenzierung nach Nutzungsart: \code{N.art}: TRUE- 
#'  Trennung nach "geerntet" = Pk 2,3,9 und "ungenutzt" = Pk 4, 5; FALSE- keine 
#'  Trennung! \cr
#'  Ratio-Schaetzer-Varianz ueber \code{r.variance.fun} (Matrizen-Notation) 
#'  berechnet! \cr 
#'  Version fuer Periode BWI 2 zu 3! \cr
#'  Auswertung erfolgt auf dem gemeinsamen Netz im Unterschied zur Version 
#'  historischen VB.A.BAGR.akl.dkl.stratum.fun, welche das Nutzungsgeschehen 
#'  auf der bei der BWI 2 erfassten Flaeche abdeckte!
#'  Voraussetzung ist, dass die Tabellen \code{baeume.23}, \code{baeume.3}, 
#'  \code{ecken.2}, \code{ecken.3}, \code{trakte.3} sowie \code{bacode} 
#'  eingelesen sind! \cr
#'  Fuer die Berechnung der flaechenbezogenen Nutzungen muss die mittlere 
#'  Baumartenflaeche der Periode berechnet werden, d.h. es werden auch die 
#'  Standflaechen der Folgeaufnahme benoetigt! Dies wird von der Funktion 
#'  \code{\link{mbaf.bagr.alt.bhd.pm.fun}} uebernommen! \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @section TODO: Verallgemeinerung bzw. Variante fuer BWI 1 zu 2 !!!!
#' @param BA.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0, D.ob=70, D.b=10, Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, 
#'  dass zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt 
#'  \code{D.unt} als unterste Schwelle.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param N.art Differenzierung nach Nutzungsart. TRUE: Trennung nach "geerntet" 
#'  = Pk 2,3,9 und "ungenutzt" = Pk 4, 5; FALSE: keine Trennung!
#' @param A Gesamtflaeche des Inventurgebiets in ha zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit Datum und 
#'  genutzter Baumversion), \strong{Stratum} (\code{auswahl}), \strong{nTE} 
#'  (Anzahl Ecken im Stratum), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler der HBF), \strong{Attribute} (Vektor mir 
#'  berechneten Attributen), \strong{Groessen} (Vektor mit berechneten Groessen 
#'  fuer Attribute (Wert, Standardfehler)), \strong{Nutzungsart} (2 Kategorien: 
#'  geernet oder ungenutzt, wenn diese nicht definiert sind, wird "insgesamt" 
#'  ausgegeben), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.VBN.A.NArt.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer das ausgeschiedene 
#'  Kollektiv), \strong{BAF.bagr.akl.dkl} (Array mit Werten und Standardfehlern 
#'  zu Baumartenflaechen nach Baumartengruppen, Alterklassen und 
#'  Durchmesserklassen), \strong{mPL.NArt.Bagr.Akl.Dkl} (mittlere kalkulierte 
#'  Periodenlaenge mit Standardfehler), \strong{mPL.Stratum} (mittlere 
#'  Periodenlaenge als gewogenes Mittel), \strong{se.mPL.Stratum} 
#'  (Standardfehler der mittleren Periodenlaenge (mPl.Stratum)), 
#'  \strong{nT.NArt.Bagr.Akl.Dkl} (Anzahl Trakte je Nutzungsart, 
#'  Baumartengruppen, Altersklassen und Durchmesserklassen).
VB.A.bagrupp.akl.dkl.stratum.fun.3 <-
          function(BA.grupp,A.klass,D.klass,auswahl,N.art,A){
  #k\u00e4/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
  #aktuelle Inventur (BWI 3), z.B. die Eigentumsklassenzuordnung
  #alte Fassung:
  #auswahl$Begehbar=1; auswahl$Wa=c(1:3)
  #ecken.2.s <- stratum.fun(auswahl,ecken.2)
  #neu:
  ecken.2.s <- stratum.fun(list(Wa=c(1:3),Begehbar=1),ecken.2)
  #---
  auswahl$Wa=c(3,5)
  ecken.3.s <- stratum.fun(auswahl,ecken.3)
  #gemeinsames Netz Land BW BWI 2 und 3 auf begehbarem Holzboden
  ecken.23.hb <- merge(ecken.3.s[TRUE,  c("TNr", "ENr")],
      ecken.2.s[TRUE, c("TNr", "ENr")],by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,ecken.3[TRUE, c("TNr", "ENr", "PL", "PLkal")],
      by=c("TNr","ENr"))
  stratum <- ecken.23.hb
  trakte <- trakte.3
  t.pos <- length(trakte)#Anzahl Spalten in <trakte> wird ben\u00f6tigt, um
  #Attribut-Positionen zu bestimmen
  #--------------------
  #inv <- 1
  #stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  #<y> steht hier f\u00fcr die Anzahl der Traktecken auf begehbarem HB im Stratum
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Anzahl der Trakte im Stratum
  n.t.s <- length(y[,1])
  #Anf\u00fcgen der Anzahl Traktecken (Wald und Nicht-Wald)
  y <- merge(y,trakte[TRUE, c("tnr", "m")],by=c("tnr"))
  # this was:  
  #  y <- merge(y,subset(trakte,select=c(tnr,m),by=c(tnr)))
  # where the ellipsis of subset() swallows up the misplaces by argument (meant 
  # for merge())! 

  #Alle Traktecken im Inventurgebiet
  x <- trakte$m
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #----------------
  #HBFl. [ha]
  T.hbf <- sum(y$y)/sum(x)*A
  var.T.hbf <-  nT/(nT-1)*T.hbf^2*(sum(y$y^2)/sum(y$y)^2+sum(x^2)/sum(x)^2
                      -2*sum(y$y*y$m)/sum(y$y)/sum(x))
  se.T.hbf <- var.T.hbf^0.5 #Standardfehler
  #Hier k\u00f6nnte als Alternative die Funktion <r.variance.fun> benutzt werden
  #Hierzu m\u00fcsste eine alle Trakte umfassende Matrix <xy> mit <m> und <y>
  #\u00fcbergeben werden
  #----------------
  #k\u00e4/28.02.2014:
  baeume <- baeume.23 #muss eingelesen sein!
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))

  #HINWEIS: beim ausgeschiedenen Vorrat wird der zur Periodenmitte
  #fortgeschriebene Vorrat verwendet! volv2,vole2,oib2
  baeume.s <- merge(
          baeume[TRUE, c("tnr", "enr", "stp", "bnr", "ba", "pk", "alt1", "alt2", "bhd1", "bhd2", "volv2", "vole2", "oib2", "nha1", "stfl1")],
          stratum[TRUE, c("tnr", "enr", "pl", "plkal")],by=c("tnr","enr"))

  #BA-Gruppe dazu spielen
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  n.bagr <- length(BA.grupp[[1]]) + 1 #k\u00e4/15.12.14
  #BAGR-Liste
  bagr.list <- c(BA.grupp[[1]],"Alle BA")
  #n.bagr <- length(bagr.list)

  baeume.s <- merge(baeume.s, bagr.tab[TRUE, c("ICode", "bagr")],
                    by.x="ba",by.y="ICode",all.x=T)
  names(baeume.s) <- tolower(names(baeume.s))

  #Folgeinventur: BA-Gruppen hinzuf\u00fcgen
  baeume.3.s <- merge(baeume.3[TRUE, c("TNr", "ENr", "STP", "BNr", "Pk", "BA", "Alt1", "Alt2", "BHD1", "BHD2", "StFl2")],
                      bagr.tab[TRUE, c("ICode", "bagr")],
                    by.x="BA",by.y="ICode",all.x=T)
  names(baeume.3.s) <- tolower(names(baeume.3.s))
  baeume.3.s <- merge(baeume.3.s,stratum[TRUE, c("tnr", "enr", "pl", "plkal")],
          by=c("tnr","enr"))

  #--------------------------------------------------
  #Mittlere Baumartenfl\u00e4chen nach BAGr, AKl und DKl zur Periodenmitte nach Trakt
  mbaf.bagr.akl.dkl.tnr <- mbaf.bagr.alt.bhd.pm.fun(
                                    baeume.s,baeume.3.s,A.klass,D.klass)

  #-------------
  #Klassifizierung des Ausgeschiedenen Vorrats durchf\u00fchren
  #Alter
  A.max <- 999
  #Hinweis: A-Klassifizierung nach fortgeschriebenem Alter: alt2!!!
  baeume.s$akl <- cut(baeume.s$alt2,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  #Hinweis: D-Klassifizierung nach fortgeschriebenem BHD: bhd2!!!
  baeume.s$dkl <- cut(baeume.s$bhd2, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])

  #Array f\u00fcr mittlere BAF zur PM nach BAGr, AKl, DKl
  BAF.bagr.akl.dkl  <- array(dim=c(2,n.bagr,A.k,D.k))

  if(N.art)
    {pk.list <- list(c(2,3,9),c(4,5));n.nart <- 2}
  else
    {pk.list <- list(c(2:5,9)); n.nart <- 1}
  #Array f\u00fcr Ergebnisse (Totals und SE jeweils nach Nutzungsart, BAGr, AKl, DKl)
  #Nutzungsart: aus <pk>: 2 = selektiv genutzt,  3 = fl\u00e4chig genutzt
  #                       4 = am Ort verblieben, 5 = abgestorben  (stehend)
  #                       9 = unauffindbar (wird der Kategorie geerntet
  #                           zugewiesen)
  #                       2,3,9 definieren den geernteten ausgeschiedenen Vorrat
  #                       4, 5 definieren den ungenutzten (tw. aus nat\u00fcrl. Mort.
  #                       stammenden) ungenutzten ausgeschiedenen Vorrat)
  #Aus diesen Kennzahlen werden die 2 Kategorien der Nutzungsart (NArt):
  #geerntet bzw. ungenutzt festgelegt.
  #Es gibt 4 Zielgr\u00f6\u00dfen <Y>:  V [m^3Dh mR], V Eor (Erntevolumen o. R.) [m^3E oR],
  #B (oberird. Biomasse) [t], V.EoR im HB, f\u00fcr die jeweils der Gesamtwert der
  #Periode ("Total") und der j\u00e4hrliche Wert berec (neu!)hnet wird, sowie der
  #Stichprobenfehler (SE), und zwar jeweils f\u00fcr die 2 Kategorien "geerntet" /
  #"ungenutzt" sowie <n.bagr> Baumartengruppen, A.k Alters- und D.k
  #Durchmesserklassen:
  #1. Index: 1- 4: Perioden-Total (v, v.eor, b, v.eor.hb);
  #          4- 8: m. j\u00e4hrlicher Wert (j.v, j.v.eor, j.b, j.v.eor.hb);
  #          9-12: m. j\u00e4hrlicher Wert je ha     (k\u00e4/28.02.2014)
  #2. Index: 1: Wert; 2: SE;
  #3. Index: Nutzungsart (wenn <N.art> == TRUE (1: geerntet, 2: ungenutzt)
  #4. Index: BAGr; 5. Index: A-Klasse; 6. Index: D-Klasse
  Y.na.bagr.akl.dkl    <- array(dim=c(12,2,n.nart,n.bagr,A.k,D.k))
  #mittlere kal. Periodenl\u00e4nge mit Standard-Fehler
  mPL.na.bagr.akl.dkl   <- array(dim=c(2,2,n.bagr,A.k,D.k))
  #Anzahl Trakte (PSU) je NArt, BAGR, Akl, Dkl
  nT.na.bagr.akl.dkl   <- array(dim=c(n.nart,n.bagr,A.k,D.k))
  #Mittlere Straten-PL mit SE
  ne.T <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
            names(ne.T) <- c("tnr","n.te")
  #"Periodensumme" je Trakt (mit n Ecken gewogen)
  #Bei Nutzung wird kalendarische Periodenl\u00e4nge <plkal> verwendet!
  y.pl <- stats::aggregate(stratum$plkal,by=list(stratum$tnr),mean,na.rm=T)$x*ne.T$n.te
  #mittl. PL als gewogenes Mittel
  mpl.stratum <- sum(y.pl)/sum(ne.T$n.te)
  #Berechnung des Standardfehlers
  se.mpl.stratum <- mpl.stratum*
    sqrt(nT/(nT-1)*(sum(y.pl^2)/sum(y.pl)^2+sum(ne.T$n.te^2)/sum(ne.T$n.te)^2
                              -2*sum(y.pl*ne.T$n.te)/sum(y.pl)/sum(ne.T$n.te)))

  #----------------


  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        #Baumartenfl\u00e4che zur Periodenmitte aggregieren
        #k\u00e4/15.12.14
        if (i < n.bagr)
        {
          baf.ba <- mbaf.bagr.akl.dkl.tnr[
                        mbaf.bagr.akl.dkl.tnr[["bagr"]]==bagr.list[i]&
                        mbaf.bagr.akl.dkl.tnr[["akl.pm"]]==akl.lab[j]&
                        mbaf.bagr.akl.dkl.tnr[["dkl.pm"]]==dkl.lab[k],
                        c("tnr", "mbaf", "mbaf.hb")]
                
        } else
        {
          baf.ba <- mbaf.bagr.akl.dkl.tnr[
                        mbaf.bagr.akl.dkl.tnr[["akl.pm"]]==akl.lab[j]&
                        mbaf.bagr.akl.dkl.tnr[["dkl.pm"]]==dkl.lab[k],
                        c("tnr", "mbaf", "mbaf.hb")]
          baf.t <- stats::aggregate(baf.ba$mbaf,by=list(baf.ba$tnr),sum)
          baf.t <- cbind(baf.t,stats::aggregate(baf.ba$mbaf.hb,by=list(baf.ba$tnr),sum)$x)
          names(baf.t) <- names(baf.ba)
          baf.ba <- baf.t
        }#*
        #Mit allen Trakten im Inventurgebiet vereinen
        xy.baf <- merge(trakte,baf.ba,by=c("tnr"),all.x=T)
        #NA eliminieren!
        xy.baf[is.na(xy.baf)] <- 0
        #xy.baf$mbaf <- xy.baf$mbaf
        #xy. <- cbind(xy$m,xy$mbaf/10000)
        #Total der Baumartenfl\u00e4che
        R.list <- r.variance.fun(cbind(xy.baf$m,xy.baf$mbaf),nT)
        BAF.bagr.akl.dkl[1,i,j,k] <-  R.list$R.xy*A
        BAF.bagr.akl.dkl[2,i,j,k] <-  sqrt(R.list$V.R.xy)*A

        for (i.n in 1:n.nart)   #Nutzungsart (geerntet, ungenutzt), wenn gesetzt!
        {

          #K\u00e4 15.12.14
          if (i < n.bagr)
          {
            baeume.ba <- baeume.s[
                baeume.s[["pk"]]%in%pk.list[[i.n]] &
                baeume.s[["bagr"]]==bagr.list[i] &
                baeume.s[["akl"]]==akl.lab[j] &
                baeume.s[["dkl"]]==dkl.lab[k],
                c("tnr", "enr", "pk", "volv2", "vole2", "oib2", "nha1", "stfl1",
                  "plkal")]
          } else
          {
            baeume.ba <- baeume.s[
                baeume.s[["pk"]]%in%pk.list[[i.n]] & 
                baeume.s[["akl"]]==akl.lab[j] & 
                baeume.s[["dkl"]]==dkl.lab[k],
                c("tnr", "enr", "pk", "volv2", "vole2", "oib2", "nha1", "stfl1",
                  "plkal")]
          } #*
          if (length(baeume.ba[,1])== 0)
          {
             Y.na.bagr.akl.dkl[,1,i.n,i,j,k] <- rep(0,12) #Zielgr\u00f6\u00dfe
             Y.na.bagr.akl.dkl[,2,i.n,i,j,k] <- rep(0,12) #Stichprobenfehler SE)

             mPL.na.bagr.akl.dkl[1:2,i.n,i,j,k]   <- rep(0,2)
             nT.na.bagr.akl.dkl[i.n,i,j,k]        <- 0        #n PSU (Trakte)
          }else
          {
            #Nach Trakt aggregieren
            #fortgeschrieben: volv2, vole2, oib2!!!!
            #Ausgeschiedener Derbholz-Vorrat [m^3 mR] als "v"
            xy <- stats::aggregate(baeume.ba$volv2*baeume.ba$nha1,
                            by=list(baeume.ba$tnr),sum)
            names(xy) <- c("tnr","v")
            #Ausgeschiedener Vorrat Erntevolumen [m^3 oR] als "v.eor"
            xy <- cbind(xy,
              stats::aggregate(baeume.ba$vole2*baeume.ba$nha1,
                                              by=list(baeume.ba$tnr),sum)$x )
            names(xy)[3] <- "v.eor"
            #Ausgeschiedener Vorrat in oberird. Biomasse [t] als "b"
            xy <- cbind(xy,stats::aggregate(baeume.ba$oib2*baeume.ba$nha1,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
            names(xy)[4] <- "b"
            #Ausgeschiedener Vorrat Erntevolumen im Hauptbestand (01.05.14)
            xy <- cbind(xy,stats::aggregate(baeume.ba$vole2*baeume.ba$nha1*
                                      ifelse(baeume.ba$stfl1>0,1,0),
                        by=list(baeume.ba$tnr),sum)$x)
            names(xy)[5] <- "v.eor.hb"
            #Mittlere kal. Periodenl\u00e4nge je Trakt
            mpl <- stats::aggregate(baeume.ba$plkal,by=list(baeume.ba$tnr),mean)$x

            #J\u00e4hrlicher ausgeschiedener Derbholzvorrat
            xy$j.v <- xy$v/mpl
            #J\u00e4hrlicher ausgeschiedener Erntevorrat
            xy$j.v.eor <- xy$v.eor/mpl
            #J\u00e4hrlicher ausgeschiedener oi. Biomassevorrat
            xy$j.b <- xy$b/mpl
            #J\u00e4hrliche ausgeschiedene Stammzahl
            xy$j.v.eor.hb <- xy$v.eor.hb/mpl

            #Anzahl Traktecken je Trakt (Wald- und Nichtwald) und <mbaf>
            #hinzuf\u00fcgen   Hinweis: <xy.baf> enth\u00e4lt jetzt auch <m_bhb>!
            #xy <- merge(xy,xy.baf,by=c("tnr"))
            xy <- merge(xy.baf,xy,by=c("tnr"),all.x=T)
            xy[is.na(xy)] <- 0

            #Anzahl Trakte (i.S. von PSU) im Teilkollektiv i.n,i,j,k
            nT.na.bagr.akl.dkl[i.n,i,j,k] <- length(xy[,1])

            #mittlere kal. Periodenl\u00e4nge mit Standard-Fehler
            #Anzahl Ecken je Trakt bestimmen
            nb.TE <- stats::aggregate(rep(1,length(baeume.ba[,1])),
                                    by=list(baeume.ba$tnr,baeume.ba$enr),sum)
            ne.T <- stats::aggregate(rep(1,length(nb.TE[,1])),by=list(nb.TE$Group.1),
                              sum)
            names(ne.T) <- c("tnr","n.te")
            #"Periodensumme" je Trakt (mit n Ecken gewogen)
            y.pl <- mpl*ne.T$n.te
            R.list <- r.variance.fun(cbind(ne.T$n.te,y.pl),nT)
            #mittl. PL als gewogenes Mittel
            mPL.na.bagr.akl.dkl[1,i.n,i,j,k] <- R.list$R.xy
            #alte Fassung
            #mPL.na.bagr.akl.dkl[1,i.n,i,j,k] <- sum(y.pl)/sum(ne.T$n.te)
            #Berechnung des Standardfehlers
            mPL.na.bagr.akl.dkl[2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)

            for (l in 1:8)  #4 Totale, 4 J\u00e4hrliche Totale
            {
              #Zielgr\u00f6\u00dfen Y ausgeschiedenes Kollektiv{V,V.EoR,B,V.Eor.HB)
              #Perioden-Total, j\u00e4hrl. Total, j\u00e4hrl. Ha-Wert (k\u00e4/01-03-2014)
              #Attribut-Indizierung (l+t.pos+2) korrigiert (k\u00e4/08.07.14)
              R.list <- r.variance.fun(cbind(xy$m,xy[,(l+t.pos+2)]),nT)
              Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- R.list$R.xy*A
              #Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- sum(xy[,(1+l)])/sum(x)*A
              #Zugeh\u00f6riger Stichprobenfehler
              Y.na.bagr.akl.dkl[l,2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)*A
            }#End for l (Zielgr\u00f6\u00dfen)
            #Offset f\u00fcr Spalten-Position der 4 j\u00e4hrliche Ha-Werte
            off <- length(xy)-4
            #Fl\u00e4chenbezogene Zielgr\u00f6\u00dfen:
            for (l in 1:4) #4 j\u00e4hrliche Ha-Werte
            {
              #Zielgr\u00f6\u00dfen Y ausgeschiedenes Kollektiv{V,V.EoR,B,V.EoR.HB)
              #Perioden-Total, j\u00e4hrl. Total, j\u00e4hrl. Ha-Wert (k\u00e4/01-03-2014)
              #Wegen V.Eor.HB muss nach BAF mit BL bzw. OHNE BL getrennt werden
              if (l<4)
              {
                R.list <- r.variance.fun(cbind(xy$mbaf,xy[,(l+off)]),nT)
              } else  #V.EoR.HB
              {
                R.list <- r.variance.fun(cbind(xy$mbaf.hb,xy[,(l+off)]),nT)
              }
              Y.na.bagr.akl.dkl[(l+8),1,i.n,i,j,k] <- R.list$R.xy
              #Zugeh\u00f6riger Stichprobenfehler
              Y.na.bagr.akl.dkl[(l+8),2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)
            }#

          }#End if ... else
        }#End for i.n (Nutzungsart: geerntet, ungenutzt)
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
  }#End for i (BAGR)

  #-----------------------

  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k)

  #DKL-Labels
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #k\u00e4/16.07.14

  #Tabelle f\u00fcr BA-Gruppen
  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))
  if(N.art){nart <- c("geerntet","ungenutzt")}else {nart <- "insgesamt"}
  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              Attribute=c("V_DhmR", "V_EoR", "oiB", "V_EoR_HB",
                      "V_DhmR/J", "V_EoR/J", "oiB/J", "V_EoR_HB/J",
                      "V_DhmR/ha/J", "V_EoR/ha/J", "oiB/ha/J", "V_EoR_HB/ha/J"),
              "Gr\u00f6\u00df" = c("Wert","Standardfehler"),
              Nutzungsart = nart,
              BAGR=bagr.list, AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.VBN.A.NArt.Bagr.Akl.Dkl=Y.na.bagr.akl.dkl,
              BAF.bagr.akl.dkl=BAF.bagr.akl.dkl,
              mPL.NArt.Bagr.Akl.Dkl=mPL.na.bagr.akl.dkl,
              mPL.Stratum = mpl.stratum, SE.mPL.Stratum = se.mpl.stratum,
              nT.NArt.Bagr.Akl.Dkl=nT.na.bagr.akl.dkl))
}#End <VB.A.bagrupp.akl.dkl.stratum.fun.3>

#-------------------------------------------------------------------------------
#(2.2) ZUWACHS
#-------------------------------------------------------------------------------
#' Aggregiert Daten fuer Zuwachs und ausgeschiedenen Vorrat
#' 
#' Funktion aggregiert Daten fuer Zuwachs und ausgeschiedenen Vorrat von BWI 2
#' zu BWI 3 nach frei definierbaren Baumarten-, Altersklassen und 
#' Durchmesserklassengruppen. Vorgaengerversion zu 
#' \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2g}}.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de} 
#' @section Aktualisierungen:
#'  09.02.2015 Biomassezuwachs nur fuer Derbholz-Kollektiv \cr
#'  09.02.2015 Korrektur der Auswertung "Alle BA" durch eigene Aggregation \cr
#'  15.12.2014 Erweiterung um "Alle BA" \cr
#'  23.04.2014 Uebergabe von \code{baeume.23} und \code{baeume.3} \cr
#'  30.07.2014 Version mit Berechnung des jaehrlichen flaechenbezogenen 
#'    Zuwachses, wobei der jaehrliche Gesamtzuwachs als Y im Ratio-Schaetzer 
#'    steht (entspricht Vorgehen des TI): \code{lz=SumjVZ/SumBAF}, wobei 
#'    \code{SumjVZ=Sum(VZ.i/PL.i)}. Alternative: erweiterter Ratio-Schaetzer: 
#'    \code{lZ = SumVZ/SumBAF/mPL}.
#' @section Grundsatz: der Periodenzuwachs wird im sog. gemeinsamen Netz 
#'  (= Schnittmenge) einer Befundeinheit (Stratum, Domaene) berechnet benoetigte 
#'  Tabellen: <baeume.3>, <baeume.23>, <ecken.3>, <ecken.2>, <trakte.3>.
#' @section Hinweis: "Endwert-Verfahren" bzgl. S- und E-Baeume, Bilanzierung 
#'  inkomaptibel mit Vorratsdifferenz aus Zustandsgroessen! \cr
#'  Echter Einwuchs wird getrennt ausgewiesen, ausschliesslich nach dem 
#'  Kriterium (modellierter) BHD1 < 7 cm (abweichend von bisherigem Kriterium 
#'  Entf < 1.75 & BHD2 < 22).
#' @param baeume.23 Tabelle mit Baumdaten aus BWI 2.
#' @param baeume.3 Tabbelle mit Baumdaten aus BWI 3.
#' @param BA.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0, D.ob=70, D.b=10, Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, 
#'  dass zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt 
#'  \code{D.unt} als unterste Schwelle.
#' @param auswahl auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit Datum und 
#'  genutzter Baumversion), \strong{Stratum} (\code{auswahl}), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler Holzbodenflaeche), 
#'  \strong{Attribute} (Vektor mit berechneten Attributen), \strong{Groessen} 
#'  (vektor mit berechneten Grroessen ("Wert", "Standardfehler")), \strong{BAGR} 
#'  (Labels fuer Baumartengruppen aus \code{ba.grupp}), \strong{AKL} (Labels der 
#'  Altersklassen), \strong{DKL} (Labels der Durchmesserklassen), 
#'  \strong{iVB.bagr.akl.dkl} (Zuwachs-Tabelle), \strong{VB.A.bagr.akl.dkl} 
#'  (Tabelle mit ausgeschiedenem Vorrat).
iVB.ew.bagrupp.akl.dkl.stratum.fun.2 <- function(baeume.23,baeume.3,
          BA.grupp,A.klass,D.klass,auswahl,A){
  #---
  #k\u00e4/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
  #aktuelle Inventur (BWI 3), z.B. die Eigentumsklassenzuordnung
  #alte Fassung:
  #auswahl$Begehbar=1; auswahl$Wa=c(1:3)
  #ecken.2.s <- stratum.fun(auswahl,ecken.2)
  #neu:
  ecken.2.s <- stratum.fun(list(Wa=c(1:3),Begehbar=1),ecken.2)
  #---
  auswahl$Wa=c(3,5)
  ecken.3.s <- stratum.fun(auswahl,ecken.3)

  #A <- 3575148 #ha
  trakte <- trakte.3
  nT <- length(trakte[,1])

  #gemeinsames Netz Land BW BWI 2 und 3 auf begehbarem Holzboden
  ecken.23.hb <- merge(
      ecken.3.s[TRUE,  c("TNr", "ENr")],
      ecken.2.s[TRUE, c("TNr", "ENr")],
      by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,ecken.3[TRUE, c("TNr", "ENr", "PL", "PLkal")],
      by=c("TNr","ENr"))

  #-----------------------------------------------------------------------------
  #Klassifizierung durchf\u00fchren
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  
  #Erweiterung um "Alle BA"  k\u00e4/15.12.2014
  n.bagr <- length(BA.grupp[[1]]) + 1 #* +1
  BA.grupp$ba.grupp[[n.bagr]] <- c(10:299)    #*** eigentlich unn\u00f6tig!!!
  BA.grupp$bagr.lab[[n.bagr]] <- "Alle BA"    #*** 
  bagr.list <- BA.grupp[[1]]
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)  #*** 
  #Aus Komaptibilit\u00e4tsgr\u00fcnden wird Attribut-Name "bagr" auf "BaGr" ge\u00e4ndert
  #(wegen Funktion <iVB.bilanz.bagr.akl.dkl.fun>
  names(bagr.tab)[3] <- "BaGr"

  #Baumartengruppen hinzuf\u00fcgen
  baeume.3.s <- merge(baeume.3[TRUE, c("TNr", "ENr", "STP", "BNr", "Entf", "Pk",
                                       "BA", "Alt1", "Alt2", "BHD1", "BHD2", 
                                       "D031", "D032", "H1", "H2", "VolV1", 
                                       "VolV2", "VolE1", "VolE2", "oiB1", 
                                       "oiB2", "StFl2", "NHa2")],
                      bagr.tab[TRUE, c("ICode", "BaGr")],
                      by.x = "BA", by.y = "ICode", all.x = TRUE)

  baeume.2.s <- merge(baeume.23[TRUE, c("TNr", "ENr", "STP", "BNr", "Entf", 
                                        "Pk", "BA", "Alt1", "Alt2", "BHD1", 
                                        "BHD2", "D031", "D032", "H1", "H2", 
                                        "VolV1", "VolV2", "VolE1", "VolE2", 
                                        "oiB1", "oiB2", "StFl1", "NHa1")],
                      bagr.tab[TRUE, c("ICode", "BaGr")],
                      by.x = "BA", by.y = "ICode", all.x = TRUE)

  #Auf gemeinsames Netz reduzieren!
  baeume.3.s <- merge(baeume.3.s,ecken.23.hb,by=c("TNr","ENr"))
  baeume.2.s <- merge(baeume.2.s,ecken.23.hb,by=c("TNr","ENr"))

  #Zur Kontrolle
  sum(baeume.3.s$StFl2)/10000; sum(baeume.2.s$StFl1)/10000

  #Holzbodenfl\u00e4che des Stratums
  #Nach TE
  hb.te <- stats::aggregate(baeume.3.s$StFl2/10000,
                        by=list(baeume.3.s$TNr,baeume.3.s$ENr),sum)
  names(hb.te) <- c("TNr","ENr","m_HB_s")
  ecken <- merge(hb.te,ecken.23.hb[TRUE, c("TNr", "ENr", "PL", "PLkal")],
                    by=c("TNr","ENr") )
  #Nach T
  hb.t <- stats::aggregate(baeume.3.s$StFl2/10000,by=list(baeume.3.s$TNr),sum)
  names(hb.t) <- c("TNr","m_HB_s")
  hb.t.s <- merge(trakte,hb.t,by=c("TNr"),all.x=T)
  hb.t.s[is.na(hb.t.s)] <- 0
  r.list <- r.variance.fun(cbind(hb.t.s$m,hb.t.s$m_HB_s),nT)
  HBF <- r.list$R.xy*A
  se.HBF <- sqrt(r.list$V.R.xy)*A

  #-----------------------------------
  #Klassifizierung der Probeb\u00e4ume nach Alter und BHD zur Periodenmitte
  #BWI 2:
  #Alter und BHD zur Periodenmitte bestimmen (bei ausgeschiedenem Kollektiv
  #ist Alt2 und BHD2 in PM)
  baeume.2.s$bhd.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$BHD2,
      (baeume.2.s$BHD1+baeume.2.s$BHD2)/2)
  baeume.2.s$alt.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$Alt2,
      ifelse(baeume.2.s$BA<998,baeume.2.s$Alt1+baeume.2.s$PL/2,0))

  #BWI 3:
  #Alter und BHD zur Periodenmitte bestimmen (Hinweis nur relevant f\u00fcr
  #die bei der Zuwachsbilanzierung ben\u00f6tigten PB-Kategorien S- und E-B\u00e4ume)
  baeume.3.s$bhd.pm <- (baeume.3.s$BHD1+baeume.3.s$BHD2)/2
  baeume.3.s$alt.pm<- ifelse(baeume.3.s$BA<998,baeume.3.s$Alt2-baeume.3.s$PL/2,0)

  #Alter
  A.max <- 999
  #BWI 2
  baeume.2.s$akl <- cut(baeume.2.s$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  #BWI 3
  baeume.3.s$akl <- cut(baeume.3.s$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  akl.lab <- unique(baeume.3.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - length(akl.lab[ is.na(akl.lab)])
  #wegen NA (Alter 0 ausgeschlossen!)
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  #BWI 2
  baeume.2.s$dkl <- cut(baeume.2.s$bhd.pm, breaks=brks, right=F)
  #BWI 3
  baeume.3.s$dkl <- cut(baeume.3.s$bhd.pm, breaks=brks, right=F)
  dkl.lab <- unique(baeume.3.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])


  #Bilanzierungs-Komponenten klassifiziert nach BaGr, akl und dkl
  iv.es.a.t.bagr.akl.dkl <- iVB.bilanz.bagr.akl.dkl.fun(baeume.2.s,baeume.3.s,
                                                  ecken)
                                                  
  #++++
  #Korrektur k\u00e4/09.02.2015
  #Aggregation alle Baumarten
  baeume.2.s$BaGr <- "AlleBA"
  baeume.3.s$BaGr <- "AlleBA"
  iv.es.a.t.alle.akl.dkl <- iVB.bilanz.bagr.akl.dkl.fun(baeume.2.s,baeume.3.s,
                                                       ecken)
  #++++

  #-----------------------------------------------------------------------------
  #Zuwachs
  #Tabelle anlegen mit den Dimensionen
  #1. 1-12: 4 Zielgr\u00f6\u00dfen: V_DhmR, V_DhmR_HB, V_EoR, oiB in je 3 Varianten
  #     (1) Periodengesamtwert, (2) j\u00e4hrlicher Gesamtwert,
  #     (3) Ha-bezogener j\u00e4hrlicher Wert
  #     13: m. Baumartenfl\u00e4che  mit L\u00fcckenkorrektur
  #     14: m. Baumartenfl\u00e4che ohne L\u00fcckenkorr. f\u00fcr HB-B\u00e4ume
  #     15: m. Periodenl\u00e4nge
  #     16: Anzahl Trakte mit Beobachtungen
  #2. 2: Wert, Standardfehler
  #3. <n.bagr> Baumartengruppen
  #4. <A.k>    Altersklassen
  #5. <D.k>    Durchmesserklassen
  #------------------------------

  iVB.bagr.akl.dkl <- array(dim=c(16,2,n.bagr,A.k,D.k))

  #------
  #Ausgeschiedener Vorrat (Volumen, Biomasse
  #Tabelle mit den Dimensionen
  #1. 1-12: 4 Zielgr\u00f6\u00dfen: V_A_DhmR, V_A_DhmR_HB, V_A_EoR, oiB_A in je
  #         3 Varianten
  #         (1) Periodengesamtwert, (2) j\u00e4hrlicher Gesamtwert,
  #         (3) Ha-bezogener j\u00e4hrlicher Wert  = 1:12
  #     13: m. kal. Periodenl\u00e4nge
  #2. 2: Wert, Standardfehler
  #3. <n.bagr> Baumartengruppen
  #4. <A.k>    Altersklassen
  #5. <D.k>    Durchmesserklassen
  #------------------------------

  VB.A.bagr.akl.dkl <- array(dim=c(13,2,n.bagr,A.k,D.k))

  #------------------
  for (i in 1:n.bagr)
  {
    for (j in 1:A.k)
    {
      for (k in 1:D.k)
      {

        #k\u00e4/15.12.2014
        if (i < n.bagr)  #*
        {
          iv.es.a.t <- iv.es.a.t.bagr.akl.dkl[
                      iv.es.a.t.bagr.akl.dkl[["BaGr"]] == bagr.list[i] &
                      iv.es.a.t.bagr.akl.dkl[["Akl"]] == akl.lab[j] &
                      iv.es.a.t.bagr.akl.dkl[["Dkl"]] == dkl.lab[k], TRUE]
        } else           #*
        { #Alle BA  09.02.2015: ...alle... f\u00fcr BaGr AlleBA
          iv.es.a.t <- iv.es.a.t.alle.akl.dkl[
                                              iv.es.a.t.alle.akl.dkl[["Akl"]] == akl.lab[j] &
                                              iv.es.a.t.alle.akl.dkl[["Dkl"]] == dkl.lab[k], TRUE]
        } #*

        #Anzahl Trakte mit Beobachtungen
        iVB.bagr.akl.dkl[16,1,i,j,k] <- length(iv.es.a.t[,1])
        #-----------------------------------------------------------------------
        iv.bil.t <- iv.es.a.t[TRUE, c("TNr", "mBAF", "mBAF.oLK", "mPL", "mPLkal",
                                     "iV.DhmR", "iV.DhmR.HB", "iV.EoR", "iB", 
                                     "V.DhmR.A", "V.DhmR.HB.A", "V.EoR.A", 
                                     "B.A")]
        #Traktecken Wald/Nichtwald hinzuf\u00fcgen
        iv.bil.t <- merge(trakte[TRUE, c("TNr", "m", "m_HB")],iv.bil.t,
                          by=c("TNr"),all.x=T)
        #NA eliminieren
        iv.bil.t[is.na(iv.bil.t)] <- 0

        #utils::head(iv.bil.t)
        #1,   2, 3,   4,   5,       6,  7,     8,      9,         10,    11,
        #TNr, m, m_HB,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
        #12,      13,         14,     15
        #V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A
        

        iii <- 0  #Index f\u00fcr Array <iVB.bagr.akl.dkl>
        iiii <- 0 #Index f\u00fcr Array <VB.A.bagr.akl.dkl>

        for (ii in 1:3)   #Aggregationsebenen: Gesamtperiodenwert,
                          #j\u00e4hrlicher Gesamtwert, Ha-bezogener j\u00e4hrl. Wert
        {
          for (jj in 8:11) #Spaltenpositionen der 4 Zielgr\u00f6\u00dfen iV.DhmR,
                          #iV.DhmR.HB,
                          #iV.EoR, iB
          {
            iii <- iii + 1
            #Bezugsgr\u00f6\u00dfe x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: "Gr\u00f6\u00df"
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==9) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielg\u00f6\u00dfe y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPL
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,jj]/iv.bil.t[,6],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            iVB.bagr.akl.dkl[iii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            iVB.bagr.akl.dkl[iii,2,i,j,k]<- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
          #Auswertung f\u00fcr ausgeschiedenen Vorrat (k\u00e4/11.04.2014)

          for (jj in 12:15) #Spaltenpositionen der 4 Zielgr\u00f6\u00dfen V.DhmR.A,
                            #V.DhmR.HB.A, V.EoR.A, B.A
          {
            iiii <- iiii + 1
            #Bezugsgr\u00f6\u00dfe x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: "Gr\u00f6\u00df"
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==13) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielg\u00f6\u00dfe y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPLkal
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,7]>0,iv.bil.t[,jj]/iv.bil.t[,7],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            VB.A.bagr.akl.dkl[iiii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            VB.A.bagr.akl.dkl[iiii,2,i,j,k] <- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
        }
        #Mittlere BAF je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,4]),nT)
        iVB.bagr.akl.dkl[13,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[13,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere BAF ohne L\u00fcckenkorrektur je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,5]),nT)
        iVB.bagr.akl.dkl[14,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[14,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere Periodenl\u00e4nge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        #Hinweis: es sind weitere Mittelbildungen denkbar; z.B. mit der
        #mBAF gewogenen PL
        x <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,6]),nT)
        iVB.bagr.akl.dkl[15,1,i,j,k] <- R.list$R.xy
        iVB.bagr.akl.dkl[15,2,i,j,k] <- sqrt(R.list$V.R.xy)
        #Mittlere kalendarische Periodenl\u00e4nge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        x <- ifelse(iv.bil.t[,7]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,7]),nT)
        VB.A.bagr.akl.dkl[13,1,i,j,k] <- R.list$R.xy
        VB.A.bagr.akl.dkl[13,2,i,j,k] <- sqrt(R.list$V.R.xy)

      }
    }
  }
  #-----------------------
  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k)

  #DKL-Labels
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #k\u00e4/16.07.14
  
  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume.3$Bemerk[baeume.3$STP==0][1],fixed=T)
  b <- nchar(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]))
  version.baeume.b  <- substr(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl,
              HBF=HBF, se_HBF=se.HBF,
              Attribute=c("V_DhmR", "V_DhmR_HB", "V_EoR", "oiB",
                        "V_DhmR/J", "V_DhmR_HB/J", "V_EoR/J", "oiB/J",
                        "V_DhmR/ha/J", "V_DhmR_HB/ha/J", "V_EoR/ha/J", "oiB/ha/J",
                        "mBAF_mLK","mBAFoLK","mPL","nT"),
              "Gr\u00f6\u00df"=c("Wert","Standardfehler"),
              BAGR = bagr.list,
              AKL = akl.lab[1:A.k], DKL = dkl.lab,
              iVB.bagr.akl.dkl = iVB.bagr.akl.dkl,
              VB.A.bagr.akl.dkl = VB.A.bagr.akl.dkl))
} #Ende Funktion <iVB.ew.bagrupp.akl.dkl.stratum.fun.2>

#-------------------------------------------------------------------------------
#' Aggregiert Daten fuer Zuwachs und ausgeschiedenen Vorrat (Testversion)
#' 
#' Funktion aggregiert Daten fuer Zuwachs und ausgeschiedenen Vorrat von BWI 1
#' zu BWI 2 (1987 bis 2002) nach frei definierbaren Baumarten-, Altersklassen 
#' und Durchmesserklassengruppen. 
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Aktualisierungen: 
#'  23.04.2014 Uebergabe von \code{baeume.23} und \code{baeume.3} \cr
#'  30.07.2014 Version mit Berechnung des jaehrlichen flaechenbezogenen 
#'    Zuwachses, wobei der jaehrliche Gesamtzuwachs als Y im Ratio-Schaetzer 
#'    steht (entspricht Vorgehen des TI): \code{lz=SumjVZ/SumBAF}, wobei 
#'    \code{SumjVZ=Sum(VZ.i/PL.i)}. Alternative: erweiterter Ratio-Schaetzer: 
#'    \code{lZ = SumVZ/SumBAF/mPL}.
#' @section Grundsatz: Der Periodenzuwachs wird im sog. gemeinsamen Netz 
#'  (= Schnittmenge) einer Befundeinheit (Stratum, Domaene) berechnet benoetigte 
#'  Tabellen: <baeume.2>, <baeume.12>, <ecken.2>, <ecken.1>, <trakte.2>. Um die 
#'  Struktur, die fuer die Periode BWI 2 zu 3 konzipiert worden ist, ohne zu 
#'  grosse Aenderungen fuer die Periode 1987 bis 2002 (BWI 1 zu 2) nutzen zu 
#'  koennen, wird die "Logik" BWI 2 zu 3 beibehalten und lediglich die Ecken-
#'  Tabellen <ecken.1> und <ecken.2> entsprechend eingesetzt.
#' @section Hinweis: Hier muessen <baeume.12> und <baeume.2> uebergeben werden! 
#'  \cr "Endwert-Verfahren" bzgl. S- und E-Baeume, Bilanzierung inkomaptibel mit 
#'  Vorratsdifferenz aus Zustandsgroessen! \cr
#'  Echter Einwuchs wird getrennt ausgewiesen, ausschliesslich nach dem 
#'  Kriterium (modellierter) BHD1 < 7 cm (abweichend von bisherigem Kriterium 
#'  Entf < 1.75 & BHD2 < 22).
#' @param baeume.23 Tabelle mit Baumdaten aus BWI 1. Achtung Tabelle <baeume.12>
#'  uebergeben.
#' @param baeume.3 Tabbelle mit Baumdaten aus BWI 2. Achtung Tabelle <baeume.2>
#'  uebergeben.
#' @param BA.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0, D.ob=70, D.b=10, Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, 
#'  dass zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt 
#'  \code{D.unt} als unterste Schwelle.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit Datum und 
#'  genutzter Baumversion), \strong{Stratum} (\code{auswahl}), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler Holzbodenflaeche), 
#'  \strong{Attribute} (Vektor mit berechneten Attributen), \strong{Groessen} 
#'  (vektor mit berechneten Grroessen ("Wert", "Standardfehler")), \strong{BAGR} 
#'  (Labels fuer Baumartengruppen aus \code{ba.grupp}), \strong{AKL} (Labels der 
#'  Altersklassen), \strong{DKL} (Labels der Durchmesserklassen), 
#'  \strong{iVB.bagr.akl.dkl} (Zuwachs-Tabelle), \strong{VB.A.bagr.akl.dkl} 
#'  (Tabelle mit ausgeschiedenem Vorrat).
iVB.ew.bagrupp.akl.dkl.stratum.fun.bwi12 <- function(baeume.23,baeume.3,
          BA.grupp,A.klass,D.klass,auswahl,A){
  auswahl$Begehbar=1; auswahl$Wa=c(1:3)
  #Anpassung BWI 1
  ecken.2.s <- stratum.fun(auswahl,ecken.1)
  #Anpassung BWI 2
  auswahl$Wa=c(1:3)
  ecken.3.s <- stratum.fun(auswahl,ecken.2)

  #A <- 3575148 #ha
  #Anpassung BWI 2
  trakte <- trakte.2
  nT <- length(trakte[,1])

  #gemeinsames Netz Land BW BWI 1 und 2 (in der Logik BWI 2 zu 3)
  #auf begehbarem Holzboden
  ecken.23.hb <- merge(
      ecken.3.s[TRUE,  c("TNr", "ENr")],
      ecken.2.s[TRUE, c("TNr", "ENr")],
      by=c("TNr","ENr"))
  #Anpassung BWI 2    <ecken.2> statt <ecken.3>
  ecken.23.hb <- merge(ecken.23.hb,ecken.2[TRUE, c("TNr", "ENr", "PL", "PLkal")],
      by=c("TNr","ENr"))

  #-----------------------------------------------------------------------------
  #Klassifizierung durchf\u00fchren
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]
  #Aus Komaptibilit\u00e4tsgr\u00fcnden wird Attribut-Name "bagr" auf "BaGr" ge\u00e4ndert
  #(wegen Funktion <iVB.bilanz.bagr.akl.dkl.fun>
  names(bagr.tab)[3] <- "BaGr"

  #Baumartengruppen hinzuf\u00fcgen
  baeume.3.s <- merge(baeume.3[TRUE, c("TNr", "ENr", "STP", "BNr", "Entf", "Pk",
                                       "BA", "Alt1", "Alt2", "BHD1", "BHD2", 
                                       "D031", "D032", "H1", "H2", "VolV1", 
                                       "VolV2", "VolE1", "VolE2", "oiB1", "oiB2", 
                                       "StFl2", "NHa2")],
                      bagr.tab[TRUE, c("ICode", "BaGr")],
                      by.x = "BA", by.y = "ICode",all.x = TRUE)

  baeume.2.s <- merge(baeume.23[TRUE, c("TNr", "ENr", "STP", "BNr", "Entf", "Pk",
                                        "BA", "Alt1", "Alt2", "BHD1", "BHD2", 
                                        "D031", "D032", "H1", "H2", "VolV1", 
                                        "VolV2", "VolE1", "VolE2", "oiB1", 
                                        "oiB2", "StFl1", "NHa1")],
                      bagr.tab[TRUE, c("ICode", "BaGr")],
                      by.x = "BA", by.y = "ICode", all.x = TRUE)

  #Auf gemeinsames Netz reduzieren!
  baeume.3.s <- merge(baeume.3.s,ecken.23.hb,by=c("TNr","ENr"))
  baeume.2.s <- merge(baeume.2.s,ecken.23.hb,by=c("TNr","ENr"))

  #Zur Kontrolle
  sum(baeume.3.s$StFl2)/10000; sum(baeume.2.s$StFl1)/10000

  #Holzbodenfl\u00e4che des Stratums
  #Nach TE
  hb.te <- stats::aggregate(baeume.3.s$StFl2/10000,
                        by=list(baeume.3.s$TNr,baeume.3.s$ENr),sum)
  names(hb.te) <- c("TNr","ENr","m_HB_s")
  ecken <- merge(hb.te,ecken.23.hb[TRUE, c("TNr", "ENr", "PL", "PLkal")],
                    by=c("TNr","ENr") )
  #Nach T
  hb.t <- stats::aggregate(baeume.3.s$StFl2/10000,by=list(baeume.3.s$TNr),sum)
  names(hb.t) <- c("TNr","m_HB_s")
  hb.t.s <- merge(trakte,hb.t,by=c("TNr"),all.x=T)
  hb.t.s[is.na(hb.t.s)] <- 0
  r.list <- r.variance.fun(cbind(hb.t.s$m,hb.t.s$m_HB_s),nT)
  HBF <- r.list$R.xy*A
  se.HBF <- sqrt(r.list$V.R.xy)*A

  #-----------------------------------
  #Klassifizierung der Probeb\u00e4ume nach Alter und BHD zur Periodenmitte
  #BWI 2:
  #Alter und BHD zur Periodenmitte bestimmen (bei ausgeschiedenem Kollektiv
  #ist Alt2 und BHD2 in PM)
  baeume.2.s$bhd.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$BHD2,
      (baeume.2.s$BHD1+baeume.2.s$BHD2)/2)
  baeume.2.s$alt.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$Alt2,
      ifelse(baeume.2.s$BA<998,baeume.2.s$Alt1+baeume.2.s$PL/2,0))

  #BWI 3:
  #Alter und BHD zur Periodenmitte bestimmen (Hinweis nur relevant f\u00fcr
  #die bei der Zuwachsbilanzierung ben\u00f6tigten PB-Kategorien S- und E-B\u00e4ume)
  baeume.3.s$bhd.pm <- (baeume.3.s$BHD1+baeume.3.s$BHD2)/2
  baeume.3.s$alt.pm<- ifelse(baeume.3.s$BA<998,baeume.3.s$Alt2-baeume.3.s$PL/2,0)

  #Alter
  A.max <- 999
  #BWI 2
  baeume.2.s$akl <- cut(baeume.2.s$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  #BWI 3
  baeume.3.s$akl <- cut(baeume.3.s$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  akl.lab <- unique(baeume.3.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - length(akl.lab[ is.na(akl.lab)])
  #wegen NA (Alter 0 ausgeschlossen!)
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  #BWI 2
  baeume.2.s$dkl <- cut(baeume.2.s$bhd.pm, breaks=brks, right=F)
  #BWI 3
  baeume.3.s$dkl <- cut(baeume.3.s$bhd.pm, breaks=brks, right=F)
  dkl.lab <- unique(baeume.3.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])


  #Bilanzierungs-Komponenten klassifiziert nach BaGr, akl und dkl
  iv.es.a.t.bagr.akl.dkl <- iVB.bilanz.bagr.akl.dkl.fun(baeume.2.s,baeume.3.s,
                                                  ecken)

  #-----------------------------------------------------------------------------
  #Zuwachs
  #Tabelle anlegen mit den Dimensionen
  #1. 1-12: 4 Zielgr\u00f6\u00dfen: V_DhmR, V_DhmR_HB, V_EoR, oiB in je 3 Varianten
  #     (1) Periodengesamtwert, (2) j\u00e4hrlicher Gesamtwert,
  #     (3) Ha-bezogener j\u00e4hrlicher Wert
  #     13: m. Baumartenfl\u00e4che  mit L\u00fcckenkorrektur
  #     14: m. Baumartenfl\u00e4che ohne L\u00fcckenkorr. f\u00fcr HB-B\u00e4ume
  #     15: m. Periodenl\u00e4nge
  #     16: Anzahl Trakte mit Beobachtungen
  #2. 2: Wert, Standardfehler
  #3. <n.bagr> Baumartengruppen
  #4. <A.k>    Altersklassen
  #5. <D.k>    Durchmesserklassen
  #------------------------------

  iVB.bagr.akl.dkl <- array(dim=c(16,2,n.bagr,A.k,D.k))

  #------
  #Ausgeschiedener Vorrat (Volumen, Biomasse
  #Tabelle mit den Dimensionen
  #1. 1-12: 4 Zielgr\u00f6\u00dfen: V_A_DhmR, V_A_DhmR_HB, V_A_EoR, oiB_A in je
  #         3 Varianten
  #         (1) Periodengesamtwert, (2) j\u00e4hrlicher Gesamtwert,
  #         (3) Ha-bezogener j\u00e4hrlicher Wert  = 1:12
  #     13: m. kal. Periodenl\u00e4nge
  #2. 2: Wert, Standardfehler
  #3. <n.bagr> Baumartengruppen
  #4. <A.k>    Altersklassen
  #5. <D.k>    Durchmesserklassen
  #------------------------------

  VB.A.bagr.akl.dkl <- array(dim=c(13,2,n.bagr,A.k,D.k))

  #------------------
  for (i in 1:n.bagr)
  {
    for (j in 1:A.k)
    {
      for (k in 1:D.k)
      {

        iv.es.a.t <- iv.es.a.t.bagr.akl.dkl[
                      iv.es.a.t.bagr.akl.dkl[["BaGr"]] == bagr.list[i] &
                      iv.es.a.t.bagr.akl.dkl[["Akl"]] == akl.lab[j] &
                      iv.es.a.t.bagr.akl.dkl[["Dkl"]] == dkl.lab[k], TRUE]

        #Anzahl Trakte mit Beobachtungen
        iVB.bagr.akl.dkl[16,1,i,j,k] <- length(iv.es.a.t[,1])
        #-----------------------------------------------------------------------
        iv.bil.t <- iv.es.a.t[TRUE, c("TNr", "mBAF", "mBAF.oLK", "mPL", "mPLkal", 
                                     "iV.DhmR", "iV.DhmR.HB", "iV.EoR", "iB", 
                                     "V.DhmR.A", "V.DhmR.HB.A", "V.EoR.A", "B.A"
                                     )]
        #Traktecken Wald/Nichtwald hinzuf\u00fcgen
        iv.bil.t <- merge(trakte[TRUE, c("TNr", "m", "m_HB")],iv.bil.t,
                          by=c("TNr"),all.x=T)
        #NA eliminieren
        iv.bil.t[is.na(iv.bil.t)] <- 0

        #utils::head(iv.bil.t)
        #1,   2, 3,   4,   5,       6,  7,     8,      9,         10,    11,
        #TNr, m, m_HB,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
        #12,      13,         14,     15
        #V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A
        

        iii <- 0  #Index f\u00fcr Array <iVB.bagr.akl.dkl>
        iiii <- 0 #Index f\u00fcr Array <VB.A.bagr.akl.dkl>

        for (ii in 1:3)   #Aggregationsebenen: Gesamtperiodenwert,
                          #j\u00e4hrlicher Gesamtwert, Ha-bezogener j\u00e4hrl. Wert
        {
          for (jj in 8:11) #Spaltenpositionen der 4 Zielgr\u00f6\u00dfen iV.DhmR,
                          #iV.DhmR.HB,
                          #iV.EoR, iB
          {
            iii <- iii + 1
            #Bezugsgr\u00f6\u00dfe x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: "Gr\u00f6\u00df"
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==9) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielg\u00f6\u00dfe y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPL
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,jj]/iv.bil.t[,6],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            iVB.bagr.akl.dkl[iii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            iVB.bagr.akl.dkl[iii,2,i,j,k]<- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
          #Auswertung f\u00fcr ausgeschiedenen Vorrat (k\u00e4/11.04.2014)

          for (jj in 12:15) #Spaltenpositionen der 4 Zielgr\u00f6\u00dfen V.DhmR.A,
                            #V.DhmR.HB.A, V.EoR.A, B.A
          {
            iiii <- iiii + 1
            #Bezugsgr\u00f6\u00dfe x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: "Gr\u00f6\u00df"
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==13) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielg\u00f6\u00dfe y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPLkal
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,7]>0,iv.bil.t[,jj]/iv.bil.t[,7],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            VB.A.bagr.akl.dkl[iiii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            VB.A.bagr.akl.dkl[iiii,2,i,j,k] <- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
        }
        #Mittlere BAF je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,4]),nT)
        iVB.bagr.akl.dkl[13,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[13,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere BAF ohne L\u00fcckenkorrektur je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,5]),nT)
        iVB.bagr.akl.dkl[14,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[14,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere Periodenl\u00e4nge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        #Hinweis: es sind weitere Mittelbildungen denkbar; z.B. mit der
        #mBAF gewogenen PL
        x <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,6]),nT)
        iVB.bagr.akl.dkl[15,1,i,j,k] <- R.list$R.xy
        iVB.bagr.akl.dkl[15,2,i,j,k] <- sqrt(R.list$V.R.xy)
        #Mittlere kalendarische Periodenl\u00e4nge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        x <- ifelse(iv.bil.t[,7]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,7]),nT)
        VB.A.bagr.akl.dkl[13,1,i,j,k] <- R.list$R.xy
        VB.A.bagr.akl.dkl[13,2,i,j,k] <- sqrt(R.list$V.R.xy)

      }
    }
  }
  #-----------------------
  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k)

  #DKL-Labels
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #k\u00e4/16.07.14
  
  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume.3$Bemerk[baeume.3$STP==0][1],fixed=T)
  b <- nchar(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]))
  version.baeume.b  <- substr(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl,
              HBF=HBF, se_HBF=se.HBF,
              Attribute=c("V_DhmR", "V_DhmR_HB", "V_EoR", "oiB",
                        "V_DhmR/J", "V_DhmR_HB/J", "V_EoR/J", "oiB/J",
                        "V_DhmR/ha/J", "V_DhmR_HB/ha/J", "V_EoR/ha/J", "oiB/ha/J"),
              "Gr\u00f6\u00df"=c("Wert","Standardfehler"),
              BAGR = bagr.list,
              AKL = akl.lab[1:A.k], DKL = dkl.lab,
              iVB.bagr.akl.dkl = iVB.bagr.akl.dkl,
              VB.A.bagr.akl.dkl = VB.A.bagr.akl.dkl))
} #Ende Funktion <iVB.ew.bagrupp.akl.dkl.stratum.fun.bwi12>

#-------------------------------------------------------------------------------
#' Aggregiert Daten fuer Zuwachs und ausgeschiedenen Vorrat
#' 
#' Funktion aggregiert Daten fuer Zuwachs und ausgeschiedenen Vorrat von BWI 2
#' zu BWI 3 nach frei definierbaren Baumarten-, Altersklassen und 
#' Durchmesserklassengruppen. 
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de} 
#' @section Aktualisierungen: 
#'  24.01.2015 Erweiterung um Grundflaechenzuwachs \cr
#'  27.01.2015 Korrektur betreffend Aggregation auf alle Baumarten durch eigene
#'  seperate Aggregation auf Baumartengrupe "AlleBA" \cr
#'  15.12.2014 Erweiterung um "AlleBA" \cr
#'  23.04.2014 Uebergabe von \code{baeume.23} und \code{baeume.3} \cr
#'  30.07.2014 kleinere Korrekturen: Version mit Berechnung des jaehrlichen 
#'  flaechenbezogenen Zuwachses, wobei der jaehrliche Gesamtzuwachs als Y im
#'  Ratio-Schaetzer steht (entspricht Vorgehen des TI): \code{lz=SumjVZ/SumBAF}, 
#'  wobei \code{SumjVZ=Sum(VZ.i/PL.i)}. Alternative: erweiterter 
#'  Ratio-Schaetzer: \code{lZ = SumVZ/SumBAF/mPL}.
#' @section Grundsatz: Der Periodenzuwachs wird im sog. gemeinsamen Netz 
#'  (= Schnittmenge) einer Befundeinheit (Stratum, Domaene) berechnet benoetigte 
#'  Tabellen: <baeume.3>, <baeume.23>, <ecken.3>, <ecken.2>, <trakte.3>.
#' @section Hinweis: "Endwert-Verfahren" bzgl. S- und E-Baeume, Bilanzierung 
#'  inkomaptibel mit Vorratsdifferenz aus Zustandsgroessen! \cr
#'  Echter Einwuchs wird getrennt ausgewiesen, ausschliesslich nach dem Kriterium 
#'  (modellierter) BHD1 < 7 cm (abweichend von bisherigem Kriterium Entf < 1.75 
#'  & BHD2 < 22).
#' @param baeume.23 Tabelle mit Baumdaten aus BWI 2.
#' @param baeume.3 Tabbelle mit Baumdaten aus BWI 3.
#' @param BA.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0, D.ob=70, D.b=10, Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, 
#'  dass zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt 
#'  \code{D.unt} als unterste Schwelle.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5), 
#'  Begehbar=1).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit Datum und 
#'  genutzter Baumversion), \strong{Stratum} (\code{auswahl}), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler Holzbodenflaeche), 
#'  \strong{Attribute} (Vektor mit berechneten Attributen), \strong{Groessen} 
#'  (vektor mit berechneten Grroessen ("Wert", "Standardfehler")), \strong{BAGR} 
#'  (Labels fuer Baumartengruppen aus \code{ba.grupp}), \strong{AKL} (Labels der 
#'  Altersklassen), \strong{DKL} (Labels der Durchmesserklassen), 
#'  \strong{iVB.bagr.akl.dkl} (Zuwachs-Tabelle), \strong{VB.A.bagr.akl.dkl} 
#'  (Tabelle mit ausgeschiedenem Vorrat).
iVB.ew.bagrupp.akl.dkl.stratum.fun.2g <- function(baeume.23,baeume.3,
          BA.grupp,A.klass,D.klass,auswahl,A){
  #---
  #k\u00e4/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
  #aktuelle Inventur (BWI 3), z.B. die Eigentumsklassenzuordnung
  #alte Fassung:
  #auswahl$Begehbar=1; auswahl$Wa=c(1:3)
  #ecken.2.s <- stratum.fun(auswahl,ecken.2)
  #neu:
  ecken.2.s <- stratum.fun(list(Wa=c(1:3),Begehbar=1),ecken.2)
  #---
  auswahl$Wa=c(3,5)
  ecken.3.s <- stratum.fun(auswahl,ecken.3)

  #A <- 3575148 #ha
  trakte <- trakte.3
  nT <- length(trakte[,1])

  #gemeinsames Netz Land BW BWI 2 und 3 auf begehbarem Holzboden
  ecken.23.hb <- merge(
      ecken.3.s[TRUE,  c("TNr", "ENr")],
      ecken.2.s[TRUE, c("TNr", "ENr")],
      by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,ecken.3[TRUE, c("TNr", "ENr", "PL", "PLkal")],
      by=c("TNr","ENr"))

  #-----------------------------------------------------------------------------
  #Klassifizierung durchf\u00fchren
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)

  #Erweiterung um "Alle BA"  k\u00e4/15.12.2014
  n.bagr <- length(BA.grupp[[1]]) + 1 #* +1
  BA.grupp$ba.grupp[[n.bagr]] <- c(10:299)    #*** eigentlich unn\u00f6tig!!!
  BA.grupp$bagr.lab[[n.bagr]] <- "Alle BA"    #***
  bagr.list <- BA.grupp[[1]]
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)  #***
  #Aus Komaptibilit\u00e4tsgr\u00fcnden wird Attribut-Name "bagr" auf "BaGr" ge\u00e4ndert
  #(wegen Funktion <iVB.bilanz.bagr.akl.dkl.fun.2g>
  names(bagr.tab)[3] <- "BaGr"

  #Baumartengruppen hinzuf\u00fcgen
  baeume.3.s <- merge(baeume.3[TRUE, c("TNr", "ENr", "STP", "BNr", "Entf", "Pk",
                                       "BA", "Alt1", "Alt2", "BHD1", "BHD2", 
                                       "D031", "D032", "H1", "H2", "VolV1", 
                                       "VolV2", "VolE1", "VolE2", "oiB1", "oiB2", 
                                       "StFl2", "NHa2")],
                      bagr.tab[TRUE, c("ICode", "BaGr")],
                      by.x = "BA", by.y = "ICode", all.x = T)

  baeume.2.s <- merge(baeume.23[TRUE, c("TNr", "ENr", "STP", "BNr", "Entf", "Pk",
                                        "BA", "Alt1", "Alt2", "BHD1", "BHD2", 
                                        "D031", "D032", "H1", "H2", "VolV1", 
                                        "VolV2", "VolE1", "VolE2", "oiB1", 
                                        "oiB2", "StFl1", "NHa1")],
                      bagr.tab[TRUE, c("ICode", "BaGr")],
                      by.x = "BA", by.y = "ICode", all.x = T)

  #Auf gemeinsames Netz reduzieren!
  baeume.3.s <- merge(baeume.3.s,ecken.23.hb,by=c("TNr","ENr"))
  baeume.2.s <- merge(baeume.2.s,ecken.23.hb,by=c("TNr","ENr"))

  #Zur Kontrolle
  sum(baeume.3.s$StFl2)/10000; sum(baeume.2.s$StFl1)/10000

  #Holzbodenfl\u00e4che des Stratums
  #Nach TE
  hb.te <- stats::aggregate(baeume.3.s$StFl2/10000,
                        by=list(baeume.3.s$TNr,baeume.3.s$ENr),sum)
  names(hb.te) <- c("TNr","ENr","m_HB_s")
  ecken <- merge(hb.te,ecken.23.hb[TRUE, c("TNr", "ENr", "PL", "PLkal")],
                    by=c("TNr","ENr") )
  #Nach T
  hb.t <- stats::aggregate(baeume.3.s$StFl2/10000,by=list(baeume.3.s$TNr),sum)
  names(hb.t) <- c("TNr","m_HB_s")
  hb.t.s <- merge(trakte,hb.t,by=c("TNr"),all.x=T)
  hb.t.s[is.na(hb.t.s)] <- 0
  r.list <- r.variance.fun(cbind(hb.t.s$m,hb.t.s$m_HB_s),nT)
  HBF <- r.list$R.xy*A
  se.HBF <- sqrt(r.list$V.R.xy)*A

  #-----------------------------------
  #Klassifizierung der Probeb\u00e4ume nach Alter und BHD zur Periodenmitte
  #BWI 2:
  #Alter und BHD zur Periodenmitte bestimmen (bei ausgeschiedenem Kollektiv
  #ist Alt2 und BHD2 in PM)
  baeume.2.s$bhd.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$BHD2,
      (baeume.2.s$BHD1+baeume.2.s$BHD2)/2)
  baeume.2.s$alt.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$Alt2,
      ifelse(baeume.2.s$BA<998,baeume.2.s$Alt1+baeume.2.s$PL/2,0))

  #BWI 3:
  #Alter und BHD zur Periodenmitte bestimmen (Hinweis nur relevant f\u00fcr
  #die bei der Zuwachsbilanzierung ben\u00f6tigten PB-Kategorien S- und E-B\u00e4ume)
  baeume.3.s$bhd.pm <- (baeume.3.s$BHD1+baeume.3.s$BHD2)/2
  baeume.3.s$alt.pm<- ifelse(baeume.3.s$BA<998,baeume.3.s$Alt2-baeume.3.s$PL/2,0)

  #Alter
  A.max <- 999
  #BWI 2
  baeume.2.s$akl <- cut(baeume.2.s$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  #BWI 3
  baeume.3.s$akl <- cut(baeume.3.s$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  akl.lab <- unique(baeume.3.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - length(akl.lab[ is.na(akl.lab)])
  #wegen NA (Alter 0 ausgeschlossen!)
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  #BWI 2
  baeume.2.s$dkl <- cut(baeume.2.s$bhd.pm, breaks=brks, right=F)
  #BWI 3
  baeume.3.s$dkl <- cut(baeume.3.s$bhd.pm, breaks=brks, right=F)
  dkl.lab <- unique(baeume.3.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])


  #Bilanzierungs-Komponenten klassifiziert nach BaGr, akl und dkl
  iv.es.a.t.bagr.akl.dkl <- iVB.bilanz.bagr.akl.dkl.fun.2g(baeume.2.s,baeume.3.s,
                                                  ecken)
  #++++
  #Korrektur k\u00e4/27.01.2015
  #Aggregation alle Baumarten
  baeume.2.s$BaGr <- "AlleBA"
  baeume.3.s$BaGr <- "AlleBA"
  iv.es.a.t.alle.akl.dkl <- iVB.bilanz.bagr.akl.dkl.fun.2g(baeume.2.s,baeume.3.s,
                                                       ecken)
  #++++
  #-----------------------------------------------------------------------------
  #Zuwachs
  #Tabelle anlegen mit den Dimensionen
  #+++
  #1. 1-15: 5 Zielgr\u00f6\u00dfen: V_DhmR, V_DhmR_HB, V_EoR, oiB, G in je 3 Varianten
  #     (1) Periodengesamtwert, (2) j\u00e4hrlicher Gesamtwert,
  #     (3) Ha-bezogener j\u00e4hrlicher Wert
  #     16: m. Baumartenfl\u00e4che  mit L\u00fcckenkorrektur
  #     17: m. Baumartenfl\u00e4che ohne L\u00fcckenkorr. f\u00fcr HB-B\u00e4ume
  #     18: m. Periodenl\u00e4nge
  #     19: Anzahl Trakte mit Beobachtungen
  #2. 2: Wert, Standardfehler
  #3. <n.bagr> Baumartengruppen
  #4. <A.k>    Altersklassen
  #5. <D.k>    Durchmesserklassen
  #------------------------------

  iVB.bagr.akl.dkl <- array(dim=c(19,2,n.bagr,A.k,D.k))

  #------
  #Ausgeschiedener Vorrat (Volumen, Biomasse
  #Tabelle mit den Dimensionen
  #1. 1-15: 4 Zielgr\u00f6\u00dfen: V_A_DhmR, V_A_DhmR_HB, V_A_EoR, oiB_A, G_A in je
  #         3 Varianten
  #         (1) Periodengesamtwert, (2) j\u00e4hrlicher Gesamtwert,
  #         (3) Ha-bezogener j\u00e4hrlicher Wert  = 1:12
  #     16: m. kal. Periodenl\u00e4nge
  #2. 2: Wert, Standardfehler
  #3. <n.bagr> Baumartengruppen
  #4. <A.k>    Altersklassen
  #5. <D.k>    Durchmesserklassen
  #------------------------------

  VB.A.bagr.akl.dkl <- array(dim=c(16,2,n.bagr,A.k,D.k))

  #------------------
  for (i in 1:n.bagr)
  {
    for (j in 1:A.k)
    {
      for (k in 1:D.k)
      {

        #k\u00e4/15.12.2014
        if (i < n.bagr)  #*
        {
          iv.es.a.t <- iv.es.a.t.bagr.akl.dkl[
                      iv.es.a.t.bagr.akl.dkl[["BaGr"]] == bagr.list[i] &
                      iv.es.a.t.bagr.akl.dkl[["Akl"]] == akl.lab[j] &
                      iv.es.a.t.bagr.akl.dkl[["Dkl"]] == dkl.lab[k], TRUE]
        } else           #*
        { #Alle BA   k\u00e4/27.01.2015
          iv.es.a.t <- iv.es.a.t.alle.akl.dkl[
                      iv.es.a.t.alle.akl.dkl[["Akl"]] == akl.lab[j] & 
                      iv.es.a.t.alle.akl.dkl[["Dkl"]] == dkl.lab[k], TRUE]
        }

        #Anzahl Trakte mit Beobachtungen
        #+++ Dim 16 -> 19
        iVB.bagr.akl.dkl[19,1,i,j,k] <- length(iv.es.a.t[,1])
        #-----------------------------------------------------------------------
        iv.bil.t <- iv.es.a.t[TRUE, c("TNr", "mBAF", "mBAF.oLK", "mPL", "mPLkal", 
                                     "iV.DhmR", "iV.DhmR.HB", "iV.EoR", "iB", 
                                     "iG", "V.DhmR.A", "V.DhmR.HB.A", "V.EoR.A", 
                                     "B.A", "G.A")]
        #Traktecken Wald/Nichtwald hinzuf\u00fcgen
        iv.bil.t <- merge(trakte[TRUE, c("TNr", "m", "m_HB")],iv.bil.t,
                          by=c("TNr"),all.x=T)
        #NA eliminieren
        iv.bil.t[is.na(iv.bil.t)] <- 0

        #utils::head(iv.bil.t)
        #1,   2, 3,   4,   5,       6,  7,     8,      9,         10,    11,
        #TNr, m, m_HB,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
        #12, 13,      14,         15      16   17
        #iG, V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A, G.A


        iii <- 0  #Index f\u00fcr Array <iVB.bagr.akl.dkl>
        iiii <- 0 #Index f\u00fcr Array <VB.A.bagr.akl.dkl>

        for (ii in 1:3)   #Aggregationsebenen: Gesamtperiodenwert,
                          #j\u00e4hrlicher Gesamtwert, Ha-bezogener j\u00e4hrl. Wert
        {
          for (jj in 8:12) #Spaltenpositionen der 5 Zielgr\u00f6\u00dfen iV.DhmR,
                          #iV.DhmR.HB,
                          #iV.EoR, iB
                          #iG
          {
            iii <- iii + 1
            #Bezugsgr\u00f6\u00dfe x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: "Gr\u00f6\u00df"
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB, 12: iG: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==9) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielg\u00f6\u00dfe y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPL
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,jj]/iv.bil.t[,6],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            iVB.bagr.akl.dkl[iii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            iVB.bagr.akl.dkl[iii,2,i,j,k]<- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
          #Auswertung f\u00fcr ausgeschiedenen Vorrat (k\u00e4/11.04.2014)

          for (jj in 13:17) #Spaltenpositionen der 5 Zielgr\u00f6\u00dfen V.DhmR.A,
                            #V.DhmR.HB.A, V.EoR.A, B.A, G.A
          {
            iiii <- iiii + 1
            #Bezugsgr\u00f6\u00dfe x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: "Gr\u00f6\u00df"
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB, 12: iG: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            #+++ Dim 13 -> 14
            {if(jj==14) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielg\u00f6\u00dfe y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPLkal
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,7]>0,iv.bil.t[,jj]/iv.bil.t[,7],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            VB.A.bagr.akl.dkl[iiii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            VB.A.bagr.akl.dkl[iiii,2,i,j,k] <- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
        }
        #Mittlere BAF je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,4]),nT)
        #+++ Dim 13 -> 16
        iVB.bagr.akl.dkl[16,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[16,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere BAF ohne L\u00fcckenkorrektur je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,5]),nT)
        #+++ Dim 14 -> 17
        iVB.bagr.akl.dkl[17,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[17,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere Periodenl\u00e4nge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        #Hinweis: es sind weitere Mittelbildungen denkbar; z.B. mit der
        #mBAF gewogenen PL
        x <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,6]),nT)
        #+++ Dim 15 -> 18
        iVB.bagr.akl.dkl[18,1,i,j,k] <- R.list$R.xy
        iVB.bagr.akl.dkl[18,2,i,j,k] <- sqrt(R.list$V.R.xy)
        #Mittlere kalendarische Periodenl\u00e4nge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        x <- ifelse(iv.bil.t[,7]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,7]),nT)
        #+++ Dim 13 -> 16
        VB.A.bagr.akl.dkl[16,1,i,j,k] <- R.list$R.xy
        VB.A.bagr.akl.dkl[16,2,i,j,k] <- sqrt(R.list$V.R.xy)

      }
    }
  }
  #-----------------------
  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k)

  #DKL-Labels
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #k\u00e4/16.07.14

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume.3$Bemerk[baeume.3$STP==0][1],fixed=T)
  b <- nchar(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]))
  version.baeume.b  <- substr(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume.3$Bemerk[baeume.3$STP==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl,
              HBF=HBF, se_HBF=se.HBF,
              Attribute=c("V_DhmR", "V_DhmR_HB", "V_EoR", "oiB","G",
                        "V_DhmR/J", "V_DhmR_HB/J", "V_EoR/J", "oiB/J","G/J",
                        "V_DhmR/ha/J", "V_DhmR_HB/ha/J", "V_EoR/ha/J",
                        "oiB/ha/J", "G/ha/J", "mBAF_mLK","mBAFoLK","mPL","nT"),
              "Gr\u00f6\u00df"=c("Wert","Standardfehler"),
              BAGR = bagr.list,
              AKL = akl.lab[1:A.k], DKL = dkl.lab,
              iVB.bagr.akl.dkl = iVB.bagr.akl.dkl,
              VB.A.bagr.akl.dkl = VB.A.bagr.akl.dkl))
} #Ende Funktion <iVB.ew.bagrupp.akl.dkl.stratum.fun.2g>

#-------------------------------------------------------------------------------
#' Berechnet mittlere Verbissprozent der Verjuengung
#' 
#' Funktion berechnet das mittlere Verbissprozent der Verjuengung 
#' (Groessenklasse bis 1,3 m) nach BWI-Baumartengruppen entsprechend der 
#' Auswertungskonvention als sogenannter Mean of Ratios (gemaess Absprache mit TI 
#' Petra Henning bzw. Friedrich Schmitz, BMEL vom 11.08.14).
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 11.08.2014
#' @section Hinweis: Tabelle \code{bacode} muss geladen sein. \cr
#'  Folgende Verbiss-Kategorien werden ausgewertet: \cr 0- ohne Verbiss; \cr 1- 
#'  nur Verbiss der Terminalknospe (der letzten 12 Monate); \cr 2- mehrfacher 
#'  Verbiss (auch bei intakter Terminalknospe) ueber einen laengeren Zeitraum 
#'  (einschliesslich der letzten 12 Monate); \cr 12- beide Verbiss-Kategorien 
#'  (1 oder 2).
#' @param verj Tabelle, welche die Kennwerte der Verjuengung mit Schadmerkmal 
#'  Verbiss <Biss> enthaelt.
#' @param ecken Tabelle mit den Stichproben (Ecken)-Merkmalen fuer die 
#'  Startifikation gemaess der Auswahlkriterien in \code{auswahl} (gemaess der 
#'  Konvention).
#' @param trakte Traktinformationen fuer die Hochrechnung.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param inv Inventur: BWI 1: 1; BWI 2, BWI 3: 2.
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Datum), 
#'  \strong{Stratum} (\code{auswahl}), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler Holzbodenflaeche), \strong{BAGR} (vektor mit 
#'  Baumarten), \strong{m.Verbissproz.BAGR} (Prozentualer Anteil von Verbiss und 
#'  Standardfehler), \strong{BAGR} (Vektor mit Baumarten), \strong{AKL}, 
#'  \strong{DKL}, \strong{iVB.bagr.akl.dkl}, \strong{VB.A.bagr.akl.dkl}.
verbiss.bagr.fun <- function(verj,ecken,trakte,auswahl,inv,A){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> ausw\u00e4hlen
  verj.s.bis130 <- merge(verj[verj[["h"]] < 1.3, 
                         c("tnr", "enr", "ba", "nha", "biss")],
                    stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"))
                    
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  n.bagr <- 9
  #Baumartengruppen hinzuf\u00fcgen
  verj.s.bis130 <- merge(verj.s.bis130, bacode[TRUE, c("ICode", "BaGr")],
                              by.x="ba",by.y="ICode",all.x=T)
                              
  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- stats::aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.0 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.1 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.2 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)

  names(verbiss.a) <- c("TNr","ENr","BaGr","n.ges")
  names(verbiss.0) <- c("TNr","ENr","BaGr","n.vb0")
  names(verbiss.1) <- c("TNr","ENr","BaGr","n.vb1")
  names(verbiss.2) <- c("TNr","ENr","BaGr","n.vb2")
  verbiss <- merge(verbiss.a,verbiss.0,by=c("TNr","ENr","BaGr"))
  verbiss <- merge(verbiss,verbiss.1,by=c("TNr","ENr","BaGr"))
  verbiss <- merge(verbiss,verbiss.2,by=c("TNr","ENr","BaGr"))
  verbiss <- verbiss[order(verbiss$TNr,verbiss$ENr),]
  verbiss$vb0.prz <- verbiss$n.vb0/verbiss$n.ges
  verbiss$vb1.prz <- verbiss$n.vb1/verbiss$n.ges
  verbiss$vb2.prz <- verbiss$n.vb2/verbiss$n.ges
  verbiss$vb12.prz <-  (verbiss$n.vb1+verbiss$n.vb2)/verbiss$n.ges
  verbiss[is.na(verbiss)] <- 0
  #-----------------------------------------------------------------------------
  #Hochrechnen
  #-----------------------------
  #Verbiss auf Trakt aggregieren

  #Nach Baumartengruppen
  #Vorkommen der BA-Gruppe je Trakt
  verbiss.t.a <-
    stats::aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.a) <- c("TNr","BaGr","t")
  verbiss.t. <- stats::aggregate(verbiss$vb0.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- stats::aggregate(verbiss$vb1.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- stats::aggregate(verbiss$vb2.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- stats::aggregate(verbiss$vb12.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb12")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))

  m.vb.bagr <- array(dim=c(4,2,(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    verbiss.t.bagr <- verbiss.t[verbiss.t[["BaGr"]] == bagr.list[i], TRUE]
    
    for (j in 1:4)
    {
     vb.bagr <- r.variance.fun(cbind(verbiss.t.bagr$t,verbiss.t.bagr[,(3+j)]),
                                length(trakte[,1]))
     m.vb.bagr[j,1,i] <- vb.bagr$R.xy*100         #in Prozent
     m.vb.bagr[j,2,i] <- sqrt(vb.bagr$V.R.xy)*100 #in Prozent

    }
  }
  
  #-------------------------------------------
  #\u00dcber alle Baumarten
  
  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- stats::aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.0 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.1 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.2 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)

  names(verbiss.a) <- c("TNr","ENr","n.ges")
  names(verbiss.0) <- c("TNr","ENr","n.vb0")
  names(verbiss.1) <- c("TNr","ENr","n.vb1")
  names(verbiss.2) <- c("TNr","ENr","n.vb2")
  verbiss <- merge(verbiss.a,verbiss.0,by=c("TNr","ENr"))
  verbiss <- merge(verbiss,verbiss.1,by=c("TNr","ENr"))
  verbiss <- merge(verbiss,verbiss.2,by=c("TNr","ENr"))

  verbiss$vb0.prz <- verbiss$n.vb0/verbiss$n.ges
  verbiss$vb1.prz <- verbiss$n.vb1/verbiss$n.ges
  verbiss$vb2.prz <- verbiss$n.vb2/verbiss$n.ges
  verbiss$vb12.prz <-  (verbiss$n.vb1+verbiss$n.vb2)/verbiss$n.ges
  verbiss[is.na(verbiss)] <- 0

  #Vorkommen Verj\u00fcngung je Trakt
  verbiss.t.a <- stats::aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr),sum)
  names(verbiss.t.a) <- c("TNr","t")
  #Verbiss-Kategorie 0
  verbiss.t. <- stats::aggregate(verbiss$vb0.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1
  verbiss.t. <- stats::aggregate(verbiss$vb1.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 2
  verbiss.t. <- stats::aggregate(verbiss$vb2.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1|2
  verbiss.t. <- stats::aggregate(verbiss$vb12.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb12")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))


  for (i in 1:4)
  {
     r.list <- r.variance.fun(cbind(verbiss.t$t,verbiss.t[,(2+i)]),
                                length(trakte[,1]))
     m.vb.bagr[i,1,(n.bagr+1)] <- r.list$R.xy*100 #in Prozent
     m.vb.bagr[i,2,(n.bagr+1)] <- sqrt(r.list$V.R.xy)*100
  }


  Log <- list(Datum=Sys.time())

  return(list(Log=Log, Stratum=auswahl, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR=bagr.list,
              m.Verbissproz.BAGR=m.vb.bagr))
} #Ende <verbiss.bagr.fun>

#-------------------------------------------------------------------------------
#' Berechnet mittlere Verbissprozent der Verjuengung
#' 
#' Funktion berechnet das mittlere Verbissprozent der Verjuengung 
#' (Groessenklasse bis 1,3 m) nach frei definierbaren Baumartengruppen 
#' entsprechend der Auswertungskonvention als sogenannter Mean of Ratios 
#' (gemaess Absprache mit TI Petra Henning bzw. Friedrich Schmitz, BMEL vom 
#' 11.08.14).
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 11.08.2014
#' @section Hinweis: Tabelle \code{bacode} muss geladen sein. \cr
#'  Folgende Verbiss-Kategorien werden ausgewertet: \cr 0- ohne Verbiss; \cr 1- 
#'  nur Verbiss der Terminalknospe (der letzten 12 Monate); \cr 2- mehrfacher 
#'  Verbiss (auch bei intakter Terminalknospe) ueber einen laengeren Zeitraum 
#'  (einschliesslich der letzten 12 Monate); \cr 12- beide Verbiss-Kategorien 
#'  (1 oder 2).
#' @param verj Tabelle, welche die Kennwerte der Verjuengung mit Schadmerkmal 
#'  Verbiss <Biss> enthaelt.
#' @param ecken Tabelle mit den Stichproben (Ecken)-Merkmalen fuer die 
#'  Startifikation gemaess der Auswahlkriterien in \code{auswahl} (gemaess der 
#'  Konvention).
#' @param trakte Traktinformationen fuer die Hochrechnung.
#' @param ba.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199), c(200:299))).
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param inv Inventur: BWI 1: 1; BWI 2, BWI 3: 2.
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Datum), 
#'  \strong{Stratum} (\code{auswahl}), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler Holzbodenflaeche), \strong{BAGR} (Labels 
#'  fuer Baumartengruppen aus \code{ba.grupp}), \strong{m.Verbissproz.BAGR} 
#'  (Prozentualer Anteil von Verbiss und Standardfehler). 
verbiss.bagrupp.fun <- function(verj,ecken,trakte,ba.grupp,auswahl,inv,A){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A

  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> ausw\u00e4hlen
  verj.s.bis130 <- merge(verj[verj[["h"]] < 1.3, 
                         c("tnr", "enr", "ba", "nha", "biss")],
                    stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"))

  #Klassifizierung durchf\u00fchren
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(ba.grupp)
  #Attribut <bagr> in <BaGr>
  names(bagr.tab)[3] <- "BaGr"
  #BA-Gruppe dazu spielen
  verj.s.bis130 <- merge(verj.s.bis130, bagr.tab[TRUE, c("ICode", "BaGr")],
                                  by.x="ba",by.y="ICode",all.x=T)
  verj.s.bis130[is.na(verj.s.bis130)] <- 0
  n.bagr <- length(ba.grupp[[1]])
  bagr.list <- ba.grupp[[1]]

  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- stats::aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.0 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.1 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.2 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)

  names(verbiss.a) <- c("TNr","ENr","BaGr","n.ges")
  names(verbiss.0) <- c("TNr","ENr","BaGr","n.vb0")
  names(verbiss.1) <- c("TNr","ENr","BaGr","n.vb1")
  names(verbiss.2) <- c("TNr","ENr","BaGr","n.vb2")
  verbiss <- merge(verbiss.a,verbiss.0,by=c("TNr","ENr","BaGr"))
  verbiss <- merge(verbiss,verbiss.1,by=c("TNr","ENr","BaGr"))
  verbiss <- merge(verbiss,verbiss.2,by=c("TNr","ENr","BaGr"))
  verbiss <- verbiss[order(verbiss$TNr,verbiss$ENr),]
  verbiss$vb0.prz <- verbiss$n.vb0/verbiss$n.ges
  verbiss$vb1.prz <- verbiss$n.vb1/verbiss$n.ges
  verbiss$vb2.prz <- verbiss$n.vb2/verbiss$n.ges
  verbiss$vb12.prz <-  (verbiss$n.vb1+verbiss$n.vb2)/verbiss$n.ges
  verbiss[is.na(verbiss)] <- 0
  #-----------------------------------------------------------------------------
  #Hochrechnen
  #-----------------------------
  #Verbiss auf Trakt aggregieren

  #Nach Baumartengruppen
  #Vorkommen der BA-Gruppe je Trakt
  verbiss.t.a <-
    stats::aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.a) <- c("TNr","BaGr","t")
  verbiss.t. <- stats::aggregate(verbiss$vb0.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- stats::aggregate(verbiss$vb1.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- stats::aggregate(verbiss$vb2.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- stats::aggregate(verbiss$vb12.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb12")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))

  m.vb.bagr <- array(dim=c(4,2,(n.bagr+1)))

  for (i in 1:n.bagr)
  {
    verbiss.t.bagr <- verbiss.t[verbiss.t[["BaGr"]] == bagr.list[i],TRUE]

    for (j in 1:4)
    {
     vb.bagr <- r.variance.fun(cbind(verbiss.t.bagr$t,verbiss.t.bagr[,(3+j)]),
                                length(trakte[,1]))
     m.vb.bagr[j,1,i] <- vb.bagr$R.xy*100         #in Prozent
     m.vb.bagr[j,2,i] <- sqrt(vb.bagr$V.R.xy)*100 #in Prozent

    }
  }

  #-------------------------------------------
  #\u00dcber alle Baumarten

  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- stats::aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.0 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.1 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.2 <-stats::aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)

  names(verbiss.a) <- c("TNr","ENr","n.ges")
  names(verbiss.0) <- c("TNr","ENr","n.vb0")
  names(verbiss.1) <- c("TNr","ENr","n.vb1")
  names(verbiss.2) <- c("TNr","ENr","n.vb2")
  verbiss <- merge(verbiss.a,verbiss.0,by=c("TNr","ENr"))
  verbiss <- merge(verbiss,verbiss.1,by=c("TNr","ENr"))
  verbiss <- merge(verbiss,verbiss.2,by=c("TNr","ENr"))

  verbiss$vb0.prz <- verbiss$n.vb0/verbiss$n.ges
  verbiss$vb1.prz <- verbiss$n.vb1/verbiss$n.ges
  verbiss$vb2.prz <- verbiss$n.vb2/verbiss$n.ges
  verbiss$vb12.prz <-  (verbiss$n.vb1+verbiss$n.vb2)/verbiss$n.ges
  verbiss[is.na(verbiss)] <- 0

  #Vorkommen Verj\u00fcngung je Trakt
  verbiss.t.a <- stats::aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr),sum)
  names(verbiss.t.a) <- c("TNr","t")
  #Verbiss-Kategorie 0
  verbiss.t. <- stats::aggregate(verbiss$vb0.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1
  verbiss.t. <- stats::aggregate(verbiss$vb1.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 2
  verbiss.t. <- stats::aggregate(verbiss$vb2.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1|2
  verbiss.t. <- stats::aggregate(verbiss$vb12.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb12")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))


  for (i in 1:4)
  {
     r.list <- r.variance.fun(cbind(verbiss.t$t,verbiss.t[,(2+i)]),
                                length(trakte[,1]))
     m.vb.bagr[i,1,(n.bagr+1)] <- r.list$R.xy*100 #in Prozent
     m.vb.bagr[i,2,(n.bagr+1)] <- sqrt(r.list$V.R.xy)*100
  }


  Log <- list(Datum=Sys.time())

  return(list(Log=Log, Stratum=auswahl, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR=bagr.list,
              m.Verbissproz.BAGR=m.vb.bagr))

} #Ende <verbiss.bagrupp.fun>

#-------------------------------------------------------------------------------
#' Aggregiert Stammzahlen der Verjuengung nach Baumartengruppen
#'
#' Funktion aggregiert die Stammzahlen der Verjuengung nach 
#' BWI-Baumartengruppen.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.08.2014
#' @section Hinweis: Tabelle \code{bacode} muss geladen sein.
#' @param verj Tabelle, welche die Kennwerte der Verjuengung mit Schadmerkmal 
#'  Verbiss <Biss> enthaelt.
#' @param ecken Tabelle mit den Stichproben (Ecken)-Merkmalen fuer die 
#'  Startifikation gemaess der Auswahlkriterien in \code{auswahl} (gemaess der 
#'  Konvention).
#' @param trakte Traktinformationen fuer die Hochrechnung.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param inv Inventur: BWI 1: 1; BWI 2, BWI 3: 2.
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Datum), 
#'  \strong{Stratum} (\code{auswahl}), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler Holzbodenflaeche), \strong{BAGR} (Vektor mit
#'  verschiedenen Baumarten), \strong{n.Verjg.BAGR} (2-dimensionalles Array mit 
#'  Informationen zu absoluter Flaeche und Standardfehler (1. Dimension) sowie 
#'  Baumartenanteile in Prozent (2. Dimension) nach Baumartengruppen und fuer 
#'  alle Baumarten).
verjg.bagr.fun <- function(verj,ecken,trakte,auswahl,inv,A){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Atttributnahmen in <verj>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> ausw\u00e4hlen
  verj.s <- merge(verj[TRUE, c("tnr", "enr", "ba", "h", "oib", "nha")],
                    stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"))
                    
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  n.bagr <- 9
  #Baumartengruppen hinzuf\u00fcgen
  verj.s <- merge(verj.s, bacode[TRUE, c("ICode", "BaGr")],
                              by.x="ba",by.y="ICode",all.x=T)
  verj.s[is.na(verj.s)] <- 0
  names(verj.s)[7]<- "bagr"                            
  #-----------------------------------------------------------------------------
  #Hochrechnen
  #-----------------------------
  #Verj\u00fcngung auf Trakt aggregieren
  #Nach Baumartengruppen  
  verj.t <-
    stats::aggregate(verj.s$nha,by=list(verj.s$tnr,verj.s$bagr),sum)
  names(verj.t) <- c("tnr","bagr","n")
  #Alle Baumarten zusammen
  verj.t.a <-
    stats::aggregate(verj.s$nha,by=list(verj.s$tnr),sum)
  names(verj.t.a) <- c("tnr","n.ges")
  
  #Array-Tabelle definieren:
  #1. Dimension: Totale je BAGR, Antielprozente
  #2. Dimension: Wert, Standardfehler 
  #3. Bauartengruppen + Gesamt
  n.verj.bagr <- array(dim=c(2,2,(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    verj.t.bagr <- verj.t[verj.t[["bagr"]] == bagr.list[i], TRUE]
    #Gesamtzahlen nach BAGR
    xy <- merge(trakte[TRUE, c("tnr", "m")],
                verj.t.bagr[TRUE,  c("tnr", "n")],by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    vj.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    n.verj.bagr[1,1,i] <- vj.bagr$R.xy * A        
    n.verj.bagr[1,2,i] <- sqrt(vj.bagr$V.R.xy)* A 
    #Baumartenanteile in Prozent (St\u00fcckzahl BAGR/Gesamtst\u00fcckzahl)
    xy <- merge(verj.t.a,xy[TRUE, c("tnr", "n")],by="tnr",all.y=T)
    xy[is.na(xy)] <- 0
    vj.proz.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1])) 
    n.verj.bagr[2,1,i] <- vj.proz.bagr$R.xy * 100        #Prozent        
    n.verj.bagr[2,2,i] <- sqrt(vj.proz.bagr$V.R.xy)* 100 #Prozent 
  }
  
  #-------------------------------------------
  #\u00dcber alle Baumarten
  
  verj.t.a <- merge(trakte[TRUE, c("tnr", "m")],verj.t.a,by="tnr",all.x=T) 
  verj.t.a[is.na(verj.t.a)] <- 0 
  r.list <- r.variance.fun(cbind(verj.t.a$m,verj.t.a$n.ges),length(trakte[,1]))
  n.verj.bagr[1,1,(n.bagr+1)] <- r.list$R.xy*A
  n.verj.bagr[1,2,(n.bagr+1)] <- sqrt(r.list$V.R.xy)*A
  #Baumartenanteile in Prozent (St\u00fcckzahl BAGR/Gesamtst\u00fcckzahl)
  n.verj.bagr[2,1,(n.bagr+1)] <- 100 #Prozent        
  n.verj.bagr[2,2,(n.bagr+1)] <- 0   #Prozent 
  
  Log <- list(Datum=Sys.time())

  return(list(Log=Log, Stratum=auswahl, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR=bagr.list,
              n.Verjg.BAGR=n.verj.bagr))

} #Ende <verjg.bagr.fun>

#-------------------------------------------------------------------------------
#' Aggregiert Stammzahlen der Verjuengung nach Baumartengruppen
#'
#' Funktion aggregiert die Stammzahlen der Verjuengung nach frei definierbaren 
#' Baumartengruppen.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.08.2014
#' @section Hinweis: Tabelle \code{bacode} muss geladen sein.
#' @param verj Tabelle, welche die Kennwerte der Verjuengung mit Schadmerkmal 
#'  Verbiss <Biss> enthaelt.
#' @param ecken Tabelle mit den Stichproben (Ecken)-Merkmalen fuer die 
#'  Startifikation gemaess der Auswahlkriterien in \code{auswahl} (gemaess der 
#'  Konvention).
#' @param trakte Traktinformationen fuer die Hochrechnung.
#' @param ba.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199), c(200:299))).
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param inv Inventur: BWI 1: 1; BWI 2, BWI 3: 2.
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Datum), 
#'  \strong{Stratum} (\code{auswahl}), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler Holzbodenflaeche), \strong{BAGR} (Labels 
#'  fuer Baumartengruppen aus \code{ba.grupp}), \strong{n.Verjg.BAGR} 
#'  (2-dimensionalles Array mit Informationen zu absoluter 
#'  Flaeche und Standardfehler (1. Dimension) sowie Baumartenanteile in Prozent
#'  (2. Dimension) nach Baumartengruppen und fuer alle Baumarten).
verjg.bagrupp.fun <- function(verj,ecken,trakte,ba.grupp,auswahl,inv,A){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> ausw\u00e4hlen
  verj.s <- merge(verj[TRUE, c("tnr", "enr", "ba", "h", "oib", "nha")],
                    stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"))
                    
  #Klassifizierung durchf\u00fchren
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(ba.grupp)
  n.bagr <- length(ba.grupp[[1]])
  bagr.list <- ba.grupp[[1]]
  #BA-Gruppe dazu spielen
  verj.s <- merge(verj.s, bagr.tab[TRUE, c("ICode", "bagr")],
                                  by.x="ba",by.y="ICode",all.x=T)
  verj.s[is.na(verj.s)] <- 0
  names(verj.s)[7]<- "bagr"                            
  #-----------------------------------------------------------------------------
  #Hochrechnen
  #-----------------------------
  #Verj\u00fcngung auf Trakt aggregieren
  #Nach Baumartengruppen  
  verj.t <-
    stats::aggregate(verj.s$nha,by=list(verj.s$tnr,verj.s$bagr),sum)
  names(verj.t) <- c("tnr","bagr","n")
  #Alle Baumarten zusammen
  verj.t.a <-
    stats::aggregate(verj.s$nha,by=list(verj.s$tnr),sum)
  names(verj.t.a) <- c("tnr","n.ges")
  
  #Array-Tabelle definieren:
  #1. Dimension: Totale je BAGR, Antielprozente
  #2. Dimension: Wert, Standardfehler 
  #3. Bauartengruppen + Gesamt
  n.verj.bagr <- array(dim=c(2,2,(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    verj.t.bagr <- verj.t[verj.t[["bagr"]] == bagr.list[i], TRUE]
    #Gesamtzahlen nach BAGR
    xy <- merge(trakte[TRUE, c("tnr", "m")],
                verj.t.bagr[TRUE,  c("tnr", "n")],by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    vj.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    n.verj.bagr[1,1,i] <- vj.bagr$R.xy * A        
    n.verj.bagr[1,2,i] <- sqrt(vj.bagr$V.R.xy)* A 
    #Baumartenanteile in Prozent (St\u00fcckzahl BAGR/Gesamtst\u00fcckzahl)
    xy <- merge(verj.t.a,xy[TRUE, c("tnr", "n")],by="tnr",all.y=T)
    xy[is.na(xy)] <- 0
    vj.proz.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1])) 
    n.verj.bagr[2,1,i] <- vj.proz.bagr$R.xy * 100        #Prozent        
    n.verj.bagr[2,2,i] <- sqrt(vj.proz.bagr$V.R.xy)* 100 #Prozent 
  }
  
  #-------------------------------------------
  #\u00dcber alle Baumarten
  
  verj.t.a <- merge(trakte[TRUE, c("tnr", "m")],verj.t.a,by="tnr",all.x=T) 
  verj.t.a[is.na(verj.t.a)] <- 0 
  r.list <- r.variance.fun(cbind(verj.t.a$m,verj.t.a$n.ges),length(trakte[,1]))
  n.verj.bagr[1,1,(n.bagr+1)] <- r.list$R.xy*A
  n.verj.bagr[1,2,(n.bagr+1)] <- sqrt(r.list$V.R.xy)*A
  #Baumartenanteile in Prozent (St\u00fcckzahl BAGR/Gesamtst\u00fcckzahl)
  n.verj.bagr[2,1,(n.bagr+1)] <- 100 #Prozent        
  n.verj.bagr[2,2,(n.bagr+1)] <- 0   #Prozent 
  
  Log <- list(Datum=Sys.time())

  return(list(Log=Log, Stratum=auswahl, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR=bagr.list,
              n.Verjg.BAGR=n.verj.bagr))

} #Ende <verjg.bagrupp.fun>

#-------------------------------------------------------------------------------
#' Berechnet Verjuengungsflaechen nach Baumartengruppen und Verjuengungsart
#'
#' Funktion wertet die Verjuengungsaufnahme im PK (Probebaumkennziffer) 
#' Bestockung <= 4m Hoehe aus. Es werden die Flaechen aus Bedeckungsgrad- und 
#' Baumartenanteilsschaetzung differenziert nach BWI-Baumartengruppen und 
#' Verjuengungsart berechnet.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.08.2014
#' @param verj.kl4 Tabelle mit den Attributen der Bestockungsansprache <= 4m.
#' @param ecken Stichprobenmerkmale.
#' @param trakte Traktparameter.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Datum), 
#'  \strong{Stratum} (\code{auswahl}), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler Holzbodenflaeche), \strong{BAGR} (Vektor mit
#'  verschiedenen Baumarten), \strong{VjgArt} (Vektor mit verschiedenen 
#'  Verjuengungsarten), \strong{Verjg.kl4m.BAF,VArt.BAGR} (absolute Flaeche vom 
#'  Inventurgebiet und Standardfehler nach Verjuengungsart und ueber alle 
#'  Baumarten, alle Verjuengungsarten zusammen und nach Baumartengruppen, 
#'  alle Verjuengungsrten und alle Baumartengruppen zusammen).
verjg.kl4.bagr.fun <- function(verj.kl4,ecken,trakte,auswahl,A){
  #Auswertungseinheit festlegen
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Attributnahmen in <verj.kl4>
  names(verj.kl4) <- tolower(names(verj.kl4))
  
  #Baumartenfl\u00e4chen nach Baumart herleiten:
  #BAF = Deckungsgrad (Zehntel) * Anteil (Zehntel)/100
  verj.kl4$baf <- verj.kl4$dg*verj.kl4$anteil/100
  
  #Attribute und Untermenge des Stratums aus <verj.kl4> ausw\u00e4hlen
  verj.kl4.s <- 
          merge(verj.kl4, stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"))
   
  #Baumartengruppe hinzuf\u00fcgen
  verj.kl4.s <- merge(verj.kl4.s,bacode[TRUE, c("ICode", "BaGr")],by.x="ba",
                      by.y="ICode",all.x=T)
  names(verj.kl4.s)[11] <- "bagr"
  #Nach Trakt, BAGR und Verj\u00fcngungsart aggregieren
  vj.kl4.t <- stats::aggregate(verj.kl4.s$baf,
                    by=list(verj.kl4.s$tnr,verj.kl4.s$bagr,verj.kl4.s$vjgart),
                    sum)
  names(vj.kl4.t) <- c("tnr","bagr","vart","baf")
  
  #Verj\u00fcngungs-Baumartenfl\u00e4che nach BA-Gruppe und Verj\u00fcngungsart
  
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  vart.list <-
    c("Naturverj\u00fcngung","Pflanzung","Saat","Stockausschlag","nicht zuzuordnen")
  n.bagr <- length(bagr.list)
  n.vart <- length(vart.list)
  
  #Array definieren
  vj.kl4.baf.bagr.vart <- array(dim=c(2,(n.vart+1),(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    #BAGR
    vj.kl4.t.bagr <- vj.kl4.t[vj.kl4.t[["bagr"]]==bagr.list[i], TRUE]
    #Nach Verj\u00fcngungsarten
    for (j in 1:n.vart)
    {
      vj.kl4.t.vart <- vj.kl4.t.bagr[vj.kl4.t.bagr[["vart"]] == vart.list[j], 
                                     c("tnr", "baf")]
      xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.vart,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,i] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,i] <- sqrt(r.list$V.R.xy) * A
      
      #\u00dcber alle Baumartengruppen einer Verj\u00fcngungsart
      vj.kl4.t.a <- stats::aggregate(vj.kl4.t$baf[vj.kl4.t$vart==vart.list[j]],
                      by=list(vj.kl4.t$tnr[vj.kl4.t$vart==vart.list[j]]),sum)
      names(vj.kl4.t.a) <- c("tnr","baf")
      xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.a,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,(n.bagr+1)] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,(n.bagr+1)] <- sqrt(r.list$V.R.xy) * A
    }
    #Alle Verj\u00fcngungsarten zusammen nach BA-Gruppe
    vj.kl4.t.a <- stats::aggregate(vj.kl4.t.bagr$baf,by=list(vj.kl4.t.bagr$tnr),sum)
    names(vj.kl4.t.a) <- c("tnr","baf")
    xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.a,by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    vj.kl4.baf.bagr.vart[1,(n.vart+1),i] <- r.list$R.xy * A
    vj.kl4.baf.bagr.vart[2,(n.vart+1),i] <- sqrt(r.list$V.R.xy) * A
  }
  
  
  #Alle Verj\u00fcngungsarten und alle Baumartengruppen
  vj.kl4.t.a <- stats::aggregate(vj.kl4.t$baf,by=list(vj.kl4.t$tnr),sum)
  names(vj.kl4.t.a) <- c("tnr","baf")
  xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.a,by="tnr",all.x=T)
  xy[is.na(xy)] <- 0
  r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
  vj.kl4.baf.bagr.vart[1,(n.vart+1),(n.bagr+1)] <- r.list$R.xy * A
  vj.kl4.baf.bagr.vart[2,(n.vart+1),(n.bagr+1)] <- sqrt(r.list$V.R.xy) * A
  
  #Ausgabe
  Log <- list(Datum=Sys.time())

  return(list(Log=Log, Stratum=auswahl, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR=bagr.list, VjgArt=vart.list,
              Verjg.kl4m.BAF.VArt.BAGR=vj.kl4.baf.bagr.vart))

} #Ende <verjg.kl4.bagr.fun>

#-------------------------------------------------------------------------------
#' Berechnet Verjuengungsflaechen nach Baumartengruppen und Verjuengungsart
#'
#' Funktion wertet die Verjuengungsaufnahme im PK (Probebaumkennziffer) 
#' Bestockung <= 4m Hoehe aus. Es werden die Flaechen aus Bedeckungsgrad- und 
#' Baumartenanteilsschaetzung differenziert nach frei definierbaren 
#' BWI-Baumartengruppen und Verjuengungsart berechnet.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 14.08.2014
#' @param verj.kl4 Tabelle mit den Attributen der Bestockungsansprache <= 4m.
#' @param ecken Stichprobenmerkmale.
#' @param trakte Traktparameter.
#' @param ba.grupp Liste mit BA-Zusammenfassungen zu Baumgruppen mit Bezeichner 
#'  der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", "DglKiLae", 
#'  "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51),c(100),c(110,111), c(112:199), c(200:299))).
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Komponenten: \strong{Log} (Datum), 
#'  \strong{Stratum} (\code{auswahl}), \strong{HBF} (Holzbodenflaeche in ha), 
#'  \strong{se.HBF} (Standardfehler Holzbodenflaeche), \strong{BAGR} (Labels 
#'  fuer Baumartengruppen aus \code{ba.grupp}), \strong{VjgArt} (Vektor mit verschiedenen 
#'  Verjuengungsarten), \strong{Verjg.kl4m.BAF,VArt.BAGR} (absolute Flaeche vom 
#'  Inventurgebiet und Standardfehler nach Verjuengungsart und ueber alle 
#'  Baumarten, alle Verjuengungsarten zusammen und nach Baumartengruppen, 
#'  alle Verjuengungsrten und alle Baumartengruppen zusammen).
verjg.kl4.bagrupp.fun <- function(verj.kl4,ecken,trakte,ba.grupp,auswahl,A){
  #Auswertungseinheit festlegen
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- tryCatch(stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum),
                error = function(e) return(data.frame(NA, 0))
                )
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Attributnahmen in <verj.kl4>
  names(verj.kl4) <- tolower(names(verj.kl4))
  
  #Baumartenfl\u00e4chen nach Baumart herleiten:
  #BAF = Deckungsgrad (Zehntel) * Anteil (Zehntel)/100
  verj.kl4$baf <- verj.kl4$dg*verj.kl4$anteil/100
  
  #Attribute und Untermenge des Stratums aus <verj.kl4> ausw\u00e4hlen
  verj.kl4.s <- 
          merge(verj.kl4, stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"))
   
  #Baumartengruppen-Zuordnungstabelle f\u00fcr BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(ba.grupp)
  #Attribut <bagr> in <BaGr>
  names(bagr.tab)[3] <- "BaGr"
  #BA-Gruppe dazu spielen
  verj.kl4.s <- merge(verj.kl4.s, bagr.tab[TRUE, c("ICode", "BaGr")],
                                  by.x="ba",by.y="ICode",all.x=T)
  verj.kl4.s[is.na(verj.kl4.s)] <- 0
  n.bagr <- length(ba.grupp[[1]])
  bagr.list <- ba.grupp[[1]]
  names(verj.kl4.s)[11] <- "bagr"
    
  #Verj\u00fcngungsart
  vart.list <-
    c("Naturverj\u00fcngung","Pflanzung","Saat","Stockausschlag","nicht zuzuordnen")
  n.vart <- length(vart.list)
  
  #Nach Trakt, BAGR und Verj\u00fcngungsart aggregieren
  vj.kl4.t <- tryCatch(stats::aggregate(verj.kl4.s$baf,
                                 by=list(verj.kl4.s$tnr, verj.kl4.s$bagr,
                                         verj.kl4.s$vjgart),
                                 sum),
                       error = function(e) return(data.frame(NA, NA, NA, 0))
                       )
  names(vj.kl4.t) <- c("tnr","bagr","vart","baf")
  
  #Array definieren
  vj.kl4.baf.bagr.vart <- array(dim=c(2,(n.vart+1),(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    #BAGR
    vj.kl4.t.bagr <- vj.kl4.t[vj.kl4.t[["bagr"]] == bagr.list[i], TRUE]
    #Nach Verj\u00fcngungsarten
    for (j in 1:n.vart)
    {
      vj.kl4.t.vart <- vj.kl4.t.bagr[vj.kl4.t.bagr[["vart"]] == vart.list[j],
                                     c("tnr", "baf")]
      xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.vart,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,i] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,i] <- sqrt(r.list$V.R.xy) * A
      
      #\u00dcber alle Baumartengruppen einer Verj\u00fcngungsart
      vj.kl4.t.a <- 
          tryCatch(stats::aggregate(stats::na.fail(vj.kl4.t$baf[vj.kl4.t$vart ==
                                     vart.list[j]]) ,
                             by = list(stats::na.fail(vj.kl4.t$tnr[vj.kl4.t$vart == 
                                       vart.list[j]])), 
                             sum), 
                   error = function(e) return(data.frame(NA, 0))
                   )
      names(vj.kl4.t.a) <- c("tnr","baf")
      xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.a,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,(n.bagr+1)] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,(n.bagr+1)] <- sqrt(r.list$V.R.xy) * A
    }
    #Alle Verj\u00fcngungsarten zusammen nach BA-Gruppe
    vj.kl4.t.a <-
        tryCatch(stats::aggregate(vj.kl4.t.bagr$baf,by=list(vj.kl4.t.bagr$tnr),sum),
                   error = function(e) return(data.frame(NA, 0))
                   )
    names(vj.kl4.t.a) <- c("tnr","baf")
    xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.a,by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    vj.kl4.baf.bagr.vart[1,(n.vart+1),i] <- r.list$R.xy * A
    vj.kl4.baf.bagr.vart[2,(n.vart+1),i] <- sqrt(r.list$V.R.xy) * A
  }
  
  #Alle Verj\u00fcngungsarten und alle Baumartengruppen
  vj.kl4.t.a <-
      tryCatch(stats::aggregate(stats::na.fail(vj.kl4.t$baf),by=list(stats::na.fail(vj.kl4.t$tnr)),sum),
                error = function(e) return(data.frame(NA, 0))
                )
  names(vj.kl4.t.a) <- c("tnr","baf")
  xy <- merge(trakte[TRUE, c("tnr", "m")],vj.kl4.t.a,by="tnr",all.x=T)
  xy[is.na(xy)] <- 0
  r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
  vj.kl4.baf.bagr.vart[1,(n.vart+1),(n.bagr+1)] <- r.list$R.xy * A
  vj.kl4.baf.bagr.vart[2,(n.vart+1),(n.bagr+1)] <- sqrt(r.list$V.R.xy) * A
  
  #Ausgabe
  Log <- list(Datum=Sys.time())

  return(list(Log=Log, Stratum=auswahl, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR=bagr.list, VjgArt=vart.list,
              Verjg.kl4m.BAF.VArt.BAGR=vj.kl4.baf.bagr.vart))

} #Ende <verjg.kl4.bagrupp.fun>

#-------------------------------------------------------------------------------
#' Berechnet Flaechen und Flaechenanteile der Naturnaehestufen
#' 
#' Funktion berechnet fuer das in \code{auswahl} definierte Stratum die Flaechen 
#' und Flaechenanteile (\%) der 5 Naturnaehestufen (ntns) fuer BWI 2 oder BWI 3
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 19.08.2014
#' @param ntns.te alle Traktecken die Naturnaehestufen in allen Varianten 
#'  (Schichten, Gesamtwert fuer Hauptbestockung) fuer BWI 2 und 3 sowie fuerr BWI 
#'  2 in den 2 Vaianten natWG BWI 2 und natWG BWI 3.
#' @param ecken Stichprobenmerkmale der jeweiligen BWI (2 oder 3).
#' @param trakte Traktmermale (m, m_HB, m_bHB, m_Wa).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param bwi Angabe welche BWI (2 oder 3).
#' @param natwg Differenzierung bei Auswertung fuer BWI 2: 2: nat. WG der 3
#'  damaligen BWI 2 oder 3: bei BWI 3 aktualisierte nat. WG, die rueckwirkend 
#'  auf BWI 2 angewandt wird.
#' @return Liste mit folgenden Komponenten: \strong{Datum}, \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler der 
#'  Holzbodenflaeche), \strong{NTNS} (Vektor mit den verschiedenen ntns), 
#'  \strong{NTNS.Flaeche.Anteil} (absoluter und relativer Flaechenanteil vom 
#'  Inventurgebiet (?) und jeweiliger Standardfehler).
ntns.stratum.fun <- function(ntns.te,ecken,trakte,A,auswahl,bwi,natwg){
  stratum <- stratum.fun(auswahl,ecken)
  if (bwi==3){
    stratum <- merge(stratum,ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_BaWue_BWI3")],
            by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
    NTNS <- "BWI 3"
  } else{ #BWI 2: Fallunterscheidung nach verwendeten nat. WG
    if (natwg==3){
        stratum <- merge(stratum,
                         ntns.te[TRUE, 
                                 c("Tnr", "Enr", "NTNS_F_BaWue_BWI2_natwgBWI3")],
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
        NTNS <- "BWI 2 mit nat. WG der BWI 3"
      } else{ #Nat. WG der BWI 2
        stratum <- merge(stratum,
                         ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_BaWue_BWI2")],
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
        NTNS <- "BWI 2 mit nat. WG der BWI 2"
      }
  }
  names(stratum)[length(stratum)] <- "ntns"
  #Anzahl TE nach Trakt und NTNS aggregieren
  xy.ntns.t <- stats::aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr,stratum$ntns),sum)
  names(xy.ntns.t) <- c("TNr","ntns","n.ntns")
  #Anzahl TE nach Trakt aggregieren
  xy.t <- stats::aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr),sum)
  names(xy.t) <- c("TNr","n")

  #Array definieren
  #1. Dim (2): 1: Fl\u00e4che ha HB; 2: Prozent-Anteil;
  #2. Dim (2): 1: Wert, 2: Standardfehler
  #3. Dim (6): 1-5 Naturn\u00e4hestufen + "keine Angabe"
  
  ntns.f.ant <- array(dim=c(2,2,6))
  
  for (i in 1:6){
    if (i < 6){
      xy.ntns.i.t <- xy.ntns.t[xy.ntns.t[["ntns"]] == i, c("TNr", "n.ntns")]
    } else{  #OHNE NTNS-Angabe
      xy.ntns.i.t <- xy.ntns.t[! xy.ntns.t[["ntns"]] %in% c(1:5), 
                               c("TNr","n.ntns")]
    }
    names(xy.ntns.i.t)[2] <- "n.ntns"
    xy.i.t <- merge(trakte[TRUE, c("TNr", "m")],
                      xy.ntns.i.t,by="TNr",all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    #Fl\u00e4che
    r.list <- r.variance.fun(xy.i.t[,2:3], length(trakte[,1]))
    ntns.f.ant[1,1,i] <- r.list$R.xy*A
    ntns.f.ant[1,2,i] <- sqrt(r.list$V.R.xy)*A
    #Fl\u00e4chenanteil
    xy.i.t <- merge(xy.t[TRUE, c("TNr", "n")],xy.ntns.i.t,by=c("TNr"),all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    r.list <- r.variance.fun(xy.i.t[,2:3],length(trakte[,1]))
    ntns.f.ant[2,1,i] <- r.list$R.xy*100
    ntns.f.ant[2,2,i] <- sqrt(r.list$V.R.xy)*100
  }
  #HBF des Stratums
  xy.i.t <- merge(trakte[TRUE, c("TNr", "m")],xy.t,by="TNr",all.x=T)
  xy.i.t[is.na(xy.i.t)] <- 0
  r.list <- r.variance.fun(xy.i.t[,2:3],length(trakte[,1]))

  return (list(Datum=Sys.time(),
               HBF = r.list$R.xy*A, se.HBF = sqrt(r.list$V.R.xy)*A,
               NTNS = c("sehr naturnah", "naturnah", "bedingt naturnah",
                      "kulturbetont", "kulturbestimmt", "keine Angabe"),
               NTNS.Flaeche.Anteil = ntns.f.ant))
}#Ende <ntns.stratum.fun>

#-------------------------------------------------------------------------------
#' Berechnet Flaechen und Flaechenanteile der Naturnaehestufen
#'
#' Funktion berechnet fuer das in \code{auswahl} definierte Stratum die Flaechen 
#' und Flaechenanteile (\%) der 5 Naturnaehestufen (ntns) fuer die Schicht 
#' "kl 4m" oder "gr 4m" fuer BWI 2 oder BWI 3.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 27.10.2014
#' @section Version: 1.0 basierend auf \code{\link{ntns.stratum.fun}}
#' @param ntns.te alle Traktecken die Naturnaehestufen in allen Varianten 
#'  (Schichten, Gesamtwert fuer Hauptbestockung) fuer BWI 2 und 3 sowie fuer BWI 2 
#'  in den 2 Vaianten natWG BWI 2 und natWG BWI 3.
#' @param ecken Stichprobenmerkmale der jeweiligen BWI (2 oder 3).
#' @param trakte Traktmermale (m, m_HB, m_bHB, m_Wa).
#' @param A Gesamtflaeche des Inventurgebiets in ha.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param bwi Angabe welche BWI (2 oder 3).
#' @param natwg Differenzierung bei Auswertung fuer BWI 2: 2: nat. WG der 3
#'  damaligen BWI 2 oder 3: bei BWI 3 aktualisierte nat. WG, die rueckwirkend 
#'  auf BWI 2 angewandt wird.
#' @param schicht Angabe zur Schicht(1: kleiner 4m, 2: groesser 4 m)
#' @return Liste mit folgenden Komponenten: \strong{Datum}, \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler der 
#'  Holzbodenflaeche), \strong{NTNS_Kollektiv} (Angabe zu BWI, Schicht und bei 
#'  BWI 2 ob die natuerliche Waldgesellschaft von der BWI 2 oder von der BWI 3 
#'  genutzt wurde), \strong{NTNS} (Vektor mit den verschiedenen ntns), 
#'  \strong{NTNS.Flaeche.Anteil} (absoluter und relativer Flaechenanteil vom 
#'  Inventurgebiet (?) und jeweiliger Standardfehler).
ntns.stratum.fun.2 <- function(ntns.te,ecken,trakte,A,auswahl,bwi,natwg,schicht)
  {
  stratum <- stratum.fun(auswahl,ecken)
  #NTNS-Attribut entsprechend Schicht
  if (bwi==3){
    if (schicht==1){ #kleiner 4m
      stratum <- merge(stratum,
                       ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_kl4m_BA_Anteile_BWI3")],
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
      NTNS <- "BWI 3 Schicht < 4m"
    } else{
      stratum <- merge(stratum,
                       ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_gr4m_alleBaeume_BWI3")],
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
      NTNS <- "BWI 3 Schicht > 4m"
    }
  } else{ #BWI 2: Fallunterscheidung nach verwendeten nat. WG
    if (natwg==3){
        if (schicht==1){
          stratum <- merge(stratum,
                           ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_kl4m_BA_Anteile_BWI2_natwgBWI3")],
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht < 4 m mit nat. WG der BWI 3"
        } else{
          stratum <- merge(stratum,
                           ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_gr4m_alleBaeume_BWI2_natwgBWI3")],
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht > 4 m mit nat. WG der BWI 3"
        }
      } else{
        #Nat. WG der BWI 2
        if (schicht==1){
          stratum <- merge(stratum,
                           ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_kl4m_BA_Anteile_BWI2")],
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht < 4 m mit nat. WG der BWI 2"
        }else{
          stratum <- merge(stratum,
                           ntns.te[TRUE, c("Tnr", "Enr", "NTNS_F_gr4m_alleBaeume_BWI2")],
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht > 4 m mit nat. WG der BWI 2"
        }
      }
  }
  names(stratum)[length(stratum)] <- "ntns"
  #Anzahl TE nach Trakt und NTNS aggregieren
  xy.ntns.t <- stats::aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr,stratum$ntns),sum)
  names(xy.ntns.t) <- c("TNr","ntns","n.ntns")
  #Anzahl TE nach Trakt aggregieren
  xy.t <- stats::aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr),sum)
  names(xy.t) <- c("TNr","n")

  #Array definieren
  #1. Dim (2): 1: Fl\u00e4che ha HB; 2: Prozent-Anteil;
  #2. Dim (2): 1: Wert, 2: Standardfehler
  #3. Dim (6): 1-5 Naturn\u00e4hestufen + "keine Angabe"
  
  ntns.f.ant <- array(dim=c(2,2,6))
  
  for (i in 1:6){
    if (i < 6){
      xy.ntns.i.t <- xy.ntns.t[xy.ntns.t[["ntns"]] == i, c("TNr", "n.ntns")]
    } else   {#OHNE NTNS-Angabe
      xy.ntns.i.t <- xy.ntns.t[! xy.ntns.t[["ntns"]] %in% c(1:5), c("TNr", "n.ntns")]
    }
    names(xy.ntns.i.t)[2] <- "n.ntns"
    xy.i.t <- merge(trakte[TRUE, c("TNr", "m")],
                      xy.ntns.i.t,by="TNr",all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    #Fl\u00e4che
    r.list <- r.variance.fun(xy.i.t[,2:3], length(trakte[,1]))
    ntns.f.ant[1,1,i] <- r.list$R.xy*A
    ntns.f.ant[1,2,i] <- sqrt(r.list$V.R.xy)*A
    #Fl\u00e4chenanteil
    xy.i.t <- merge(xy.t[TRUE, c("TNr", "n")],xy.ntns.i.t,by=c("TNr"),all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    r.list <- r.variance.fun(xy.i.t[,2:3],length(trakte[,1]))
    ntns.f.ant[2,1,i] <- r.list$R.xy*100
    ntns.f.ant[2,2,i] <- sqrt(r.list$V.R.xy)*100
  }
  #HBF des Stratums
  xy.i.t <- merge(trakte[TRUE, c("TNr", "m")],xy.t,by="TNr",all.x=T)
  xy.i.t[is.na(xy.i.t)] <- 0
  r.list <- r.variance.fun(xy.i.t[,2:3],length(trakte[,1]))

  return (list(Datum=Sys.time(),
               HBF = r.list$R.xy*A, se.HBF = sqrt(r.list$V.R.xy)*A,
               NTNS_Kollektiv = NTNS,
               NTNS = c("sehr naturnah", "naturnah", "bedingt naturnah",
                      "kulturbetont", "kulturbestimmt", "keine Angabe"),
               NTNS.Flaeche.Anteil = ntns.f.ant))
}
#Ende <ntns.stratum.fun.2>

#-------------------------------------------------------------------------------
#' Funktion zur Auswertung der Biotop-Baeume
#' 
#' Funktion wertet Biotop-Baeume aus, die bei der 3. BWI als Merkmal der 
#' Probebaeume in der WZP 4 aufgenommen worden sind. Als Biotop-Baeume werden 
#' die Probebaeume erfasst, welche mindestens eines der folgenden Merkmale 
#' erfuellen: FaulKon, Hoehle, Bizarr, Uralt, Horst, MBiotop.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 10.10.2014
#' @section Hinweis: funktioniert nicht fuer <baume.1> der BWI 1!!!
#' @param wzp4.merkmale Tabelle, welche die besonderen Biotop-Baum-Merkmale 
#'  enthaelt u.a. FaulKon, Hoehle, Bizarr, Uralt, Horst, MBiotop.
#' @param baeume Probebaum-Attribute.
#' @param ecken Trakteckenmerkmale.
#' @param trakte traktbezogene Informationen (u.a. m, m_HB, ...).
#' @param A Flaeche des gesamten Inventurgebietes in ha.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und genutzten Baeumen(?)), \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), 
#'  \strong{Biotopbaumkennwerte} (Dataframetabelle mit Spalten fuer Attribute, 
#'  Wert und Standardfehler. Folgende Attribute werden aggregiert: 
#'  Gesamtstueckzahl, Stueckzahl je ha, gesamtes Volumen, Volumen je ha, 
#'  Stueckvolumen, gesamtes Volumen der Laubbaeume, Laubbaumvolumen je ha, 
#'  gesamtes Nadelbaumvolumen, Nadelbaumvolumen je ha, gesamte oberirdische 
#'  Biomasse, oberirdische Biomasse je ha).
biotop.baeume.fun <- function(wzp4.merkmale,baeume,ecken,trakte,A,auswahl){
  #(1) Befundeinheit festlegen (Traktecken ausw\u00e4hlen)
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  names(baeume) <- tolower(names(baeume))
  names(wzp4.merkmale) <- tolower(names(wzp4.merkmale))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen in <baeume>: <2> entfernen
  names(baeume) <- sub(2,names(baeume),replacement="")

  #(2) Biotop-B\u00e4ume definieren
  wzp4.merkmale <- merge(baeume[baeume$stp==0,
                        c("tnr", "enr", "bnr", "ba", "alt", "bhd", "h", "volv", "oib", "nha", "stfl")], 
                        wzp4.merkmale, by=c("tnr","enr","bnr"),all.x=T)
  wzp4.merkmale[is.na(wzp4.merkmale)] <- 0
  #Biotop-B\u00e4ume
  #Merkmal: FaulKon, Hoehle, Bizarr, Uralt, Horst, MBiotop
  #Neues Merkmal <BiotopB> festlegen
  wzp4.merkmale$biotopb <-
      wzp4.merkmale$faulkon + wzp4.merkmale$hoehle + wzp4.merkmale$bizarr + wzp4.merkmale$uralt + wzp4.merkmale$horst + wzp4.merkmale$mbiotop

  #Probeb\u00e4ume im Stratum ausw\u00e4hlen
  wzp4.merkmale.s <- merge(wzp4.merkmale[TRUE, c("tnr", "enr", "bnr", "pk", 
                                                 "biotopb", "ba", "alt", "bhd",
                                                 "volv", "oib", "nha", "stfl")],
                           stratum[TRUE, c("tnr", "enr")],
                           by=c("tnr", "enr"), all.y=T)
  wzp4.merkmale.s[is.na(wzp4.merkmale.s)] <- 0
  
  #---------------------
  #(3) Fl\u00e4chen
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  #------
  #HBF nach Trakt im Stratum
  xy <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Bl\u00f6\u00dfen (BL): BA=999, L\u00fccken (iBL): BA=998
  xy <- cbind(xy,stats::aggregate(ifelse(
                  wzp4.merkmale.s$ba==999,wzp4.merkmale.s$stfl/10000,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
  xy <- cbind(xy,stats::aggregate(ifelse(wzp4.merkmale.s$ba==998,wzp4.merkmale.s$stfl,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(trakte[TRUE, c("tnr", "m")], xy, by = c("tnr"), all.x = T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl

  #HBFl. [ha]
  r.list= r.variance.fun(xy[TRUE, c("m", "hbf")], nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Bl\u00f6\u00dfen [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "bl")], nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Bl\u00f6\u00dfen ("L\u00fccken") [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "ibl")], nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #L\u00fcckenkorrekturfaktor
  r.list <- r.variance.fun(xy[TRUE, c("hbf.ba", "hbf")], nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)

  #(4) Biotopbaum-Attribute auf Trakt aggregieren
  biotop.t <- stats::aggregate(ifelse(wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$pk<=1,
                        wzp4.merkmale.s$nha,0),by=list(wzp4.merkmale.s$tnr),sum)
  #Vorrat
  biotop.t <- cbind(biotop.t,stats::aggregate(ifelse(wzp4.merkmale.s$biotopb>0
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)
              
  #Oberirdische Biomasse
  biotop.t <- cbind(biotop.t,stats::aggregate(ifelse(wzp4.merkmale.s$biotopb>0
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$oib,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)

  #Starkholzvorrat
  biotop.t <- cbind(biotop.t,stats::aggregate(ifelse(
              wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$bhd>=50
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)

  #LB-Vorrat
  biotop.t <- cbind(biotop.t,stats::aggregate(ifelse(
              wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$ba%in%c(100:299)
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)

  #NB-Vorrat
  biotop.t <- cbind(biotop.t,stats::aggregate(ifelse(
              wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$ba%in%c(10:99)
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)



  names(biotop.t) <- c("tnr","N.BB","V.BB","oiB.BB","SthV.BB","V.BB.LB",
                        "V.BB.NB")
  utils::head(biotop.t)

  biotop.t <- merge(xy[TRUE, c("tnr", "m", "hbf")], 
                    biotop.t, 
                    by = "tnr",
                    all.x=T)
  biotop.t[is.na(biotop.t)] <- 0

  n <- length(trakte[,1])
  #St\u00fcckzahl
  R.xy <- r.variance.fun(cbind(biotop.t$m,biotop.t$N.BB),n)
  (T.N.BB <- R.xy$R.xy*A)
  (se.T.N.BB <- R.xy$V.R.xy^0.5*A)

  #je ha HBF
  R.xy <- r.variance.fun(cbind(biotop.t$hbf,biotop.t$N.BB),n)
  (N.BB.ha <- R.xy$R.xy)
  (se.N.BB.ha <- R.xy$V.R.xy^0.5)

  #Vorrat
  R.xy <- r.variance.fun(cbind(biotop.t$m,biotop.t$V.BB),n)
  (T.V.BB <- R.xy$R.xy*A)
  (se.T.V.BB <- R.xy$V.R.xy^0.5*A)

  #je ha HBF
  R.xy <- r.variance.fun(cbind(biotop.t$hbf,biotop.t$V.BB),n)
  (V.BB.ha <- R.xy$R.xy)
  (se.V.BB.ha <- R.xy$V.R.xy^0.5)

  #St\u00fcckvolumen
  R.xy <- r.variance.fun(cbind(biotop.t$N.BB,biotop.t$V.BB),n)
  (V.BB.Stck <- R.xy$R.xy)
  (se.V.BB.Stck <- R.xy$V.R.xy^0.5)

  #Vorrat der Laubb\u00e4ume
  R.xy <- r.variance.fun(cbind(biotop.t$m,biotop.t$V.BB.LB),n)
  (T.V.BB.LB <- R.xy$R.xy*A)
  (se.T.V.BB.LB <- R.xy$V.R.xy^0.5*A)

  #je ha HBF
  R.xy <- r.variance.fun(cbind(biotop.t$hbf,biotop.t$V.BB.LB),n)
  (V.BB.LB.ha <- R.xy$R.xy)
  (se.V.BB.LB.ha <- R.xy$V.R.xy^0.5)

  #Vorrat der Nadelb\u00e4ume
  R.xy <- r.variance.fun(cbind(biotop.t$m,biotop.t$V.BB.NB),n)
  (T.V.BB.NB <- R.xy$R.xy*A)
  (se.T.V.BB.NB <- R.xy$V.R.xy^0.5*A)

  #je ha HBF
  R.xy <- r.variance.fun(cbind(biotop.t$hbf,biotop.t$V.BB.NB),n)
  (V.BB.NB.ha <- R.xy$R.xy)
  (se.V.BB.NB.ha <- R.xy$V.R.xy^0.5)
  
  #oberird. Biomasse    (in t umgerechnet)
  R.xy <- r.variance.fun(cbind(biotop.t$m,biotop.t$oiB.BB),n)
  (T.oiB.BB <- R.xy$R.xy*A/1000)
  (se.T.oiB.BB <- R.xy$V.R.xy^0.5*A/1000)

  #je ha HBF
  R.xy <- r.variance.fun(cbind(biotop.t$hbf,biotop.t$oiB.BB),n)
  (oiB.BB.ha <- R.xy$R.xy/1000)
  (se.oiB.BB.ha <- R.xy$V.R.xy^0.5/1000)

  
  #(4) Ergebnisliste
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return( list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Biotopbaumkennwerte=
              data.frame(Attribute=c("Gesamtzahl","Zahl_je_ha","Vorrat_m3_mR",
              "Vorrat_m3_je_ha","Stueckvolumen_m3","Vorrat_LB_m3",
              "Vorrat_LB_m3_je_ha","Vorrat_NB_m3","Vorrat_NB_m3_je_ha",
              "oi_Biom_t","oi_Biom_t_je_ha"),
              Wert=c(T.N.BB,N.BB.ha,T.V.BB,V.BB.ha,V.BB.Stck,T.V.BB.LB,
              V.BB.LB.ha,T.V.BB.NB,V.BB.NB.ha,T.oiB.BB,oiB.BB.ha),
              SE = c(se.T.N.BB,se.N.BB.ha,se.T.V.BB,se.V.BB.ha,se.V.BB.Stck,
              se.T.V.BB.LB,se.V.BB.LB.ha,se.T.V.BB.NB,se.V.BB.NB.ha,
              se.T.oiB.BB,se.oiB.BB.ha)
              )))
              
}  
#Ende <biotop.baeume.fun>

#-------------------------------------------------------------------------------
#' Aggregiert Flaechenwerte und Standardfehler fuer gegebene Stammmerkmale
#' 
#' Funktion agregiert Flaechenwerte und Standardfehler fuer Gesamtwerte und 
#' Werte je ha fuer ein definiertes Stratum. Dabei werden Flaechen von Waldecken
#' berechnet und Stammwerte zu Stueckzahl, Volumen und oberirdische Biomasse.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Version: 1.0 vom 04.11.2014
#' @section Hinweis: 
#'  nur fuer BWI 2 und 3! \cr
#'  Da nur das WZP-4-Kollektiv beruecksichtigt wird, koennen keine 
#'  Baumartenflaechen berechnet werden!
#' @param wzp4.merkmale Tabelle, welche die besonderen Stammmerkmale enthaelt; 
#'  moegliche Merkmale sind: \cr
#'  BWI 3: merkmal.list.3 <- c("tot", "jschael", "aeschael", "ruecke", "pilz",
#'  "harz", "kaefer", "sstamm", "faulkon", "hoehle", "bizarr", "uralt", "horst",
#'  "mbiotop"). \cr
#'  BWI 2: merkmal.list.2 <- c("tot", "jschael", "aeschael", "ruecke", "pilz",
#'  "harz", "kaefer", "sstamm", "hoehle").
#' @param baeume Probebaum-Attribute.
#' @param ecken Trakteckenmerkmale.
#' @param trakte traktbezogene Informationen (u.a. m, m_HB, ...).
#' @param A Flaeche des Inventurgebietes in ha.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param merkmale legt das/die Merkmal/e fest (es koennen mehrere angegeben 
#'  werden, diese werden als Vereinigungsmenge je Baum interpretiert!)
#' @param ba.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab"). Z.B. list(bagr.lab = c("FiTa",
#'  "DglKiLae","Bu","Ei","BLb","WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199),c(200:299))).
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und genutzten Baeumen(?)), \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), \strong{Merkmal} (?), 
#'  \strong{BAGR} (Baumgruppenlabel der eingegebenen Liste \code{ba.grupp}), 
#'  \strong{Kennwerte} (Vektor mit Kennwerten), \strong{Kennwert_Tabelle_BAGR} 
#'  (2-D array mit Werten und Standardfehler fuer gesamte Stueckzahl, Stueckzahl je ha, gesamtes Volumen, 
#'  Volumen je ha, Stueckvolumen, gesamte oberirdische Biomasse, oberirdische 
#'  Biomasse je ha, Anteil Merkmal an Gesamtflaeche, Anteil des Merkmal-Vorrats 
#'  am Gesamtvorrat).
stamm.merkmale.bagr.fun <- function(wzp4.merkmale,baeume,ecken,trakte,A,
            auswahl,merkmale,ba.grupp){
  #(1) Befundeinheit festlegen (Traktecken ausw\u00e4hlen)
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  names(baeume) <- tolower(names(baeume))
  names(wzp4.merkmale) <- tolower(names(wzp4.merkmale))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen in <baeume>: <2> entfernen
  names(baeume) <- sub(2,names(baeume),replacement="")

  #Aus Kompatibilt\u00e4ts-Gr\u00fcnden werden die nur in der BWI 3 vorkommenden
  #Attribute <Ast> und <Ast_Hoe> im DS der BWI 3 entfernt
  ast.pos <- grep("Ast",names(wzp4.merkmale))
  if (length(ast.pos)>0)
  {
    wzp4.merkmale <- wzp4.merkmale[TRUE, -c(ast.pos)]
  }
  #(2) Merkmal-DS <wzp4.merkmale> mit Attribut-Auswahl aus <baeume> verkn\u00fcpfen
  wzp4.merkmale <- merge(baeume[baeume$stp==0,
                        c("tnr", "enr", "bnr", "ba", "alt", "bhd", "h", "volv", "oib", "nha", "stfl")],
                        wzp4.merkmale, by=c("tnr","enr","bnr"),all.x=T)
  wzp4.merkmale[is.na(wzp4.merkmale)] <- 0
  
  #Probeb\u00e4ume im Stratum mit den Merkmalen in  <merkmale> ausw\u00e4hlen
  #Anzahl zu ber\u00fccksichtigender Merkmale
  k <- length(merkmale)
  mm.pos <- rep(0,k)
  #Wenn mehrere Merkmale als Kriterium angegeben sind, wird ein Attribut
  #gebildet, welches die Vereinigungsmenge der betrachteten Merkmale darstellt
  wzp4.merkmale$merkmal.s <- rep(0,length(wzp4.merkmale[,1]))
  for (i in 1:k)
  {
    mm.pos[i] <- grep(toupper(merkmale[i]),toupper(names(wzp4.merkmale)))
    wzp4.merkmale$merkmal.s <- wzp4.merkmale$merkmal.s+ wzp4.merkmale[,mm.pos[i]]
  }
  mm.s.pos <- length(wzp4.merkmale)
  wzp4.merkmale.s <- merge(
                           wzp4.merkmale[TRUE, c(1:3,12,mm.pos,mm.s.pos,4:11)], #TODO: use names, not index numbers!
                           stratum[TRUE, c("tnr", "enr")], 
    by=c("tnr","enr"),all.y=T)
  wzp4.merkmale.s[is.na(wzp4.merkmale.s)] <- 0 
  
  #---------------------
  #(3) Fl\u00e4chen
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  #------
  #HBF nach Trakt im Stratum
  xy <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Bl\u00f6\u00dfen (BL): BA=999, L\u00fccken (iBL): BA=998
  xy <- cbind(xy,stats::aggregate(ifelse(
                  wzp4.merkmale.s$ba==999,wzp4.merkmale.s$stfl/10000,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
  xy <- cbind(xy,stats::aggregate(ifelse(wzp4.merkmale.s$ba==998,wzp4.merkmale.s$stfl,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(trakte[TRUE, c("tnr", "m")],xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl

  #HBFl. [ha]
  r.list= r.variance.fun(xy[TRUE, c("m", "hbf")],nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Bl\u00f6\u00dfen [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "bl")],nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Bl\u00f6\u00dfen ("L\u00fccken") [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "ibl")],nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #L\u00fcckenkorrekturfaktor
  r.list <- r.variance.fun(xy[TRUE, c("hbf.ba", "hbf")],nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)

  #(4) Attribute des Merkmals auf Trakt aggregieren
  
  #getrennt f\u00fcr die Baumarten(gruppen)

  Kennwerte=c("Gesamtzahl","Zahl_je_ha","Vorrat_m3_mR",
              "Vorrat_m3_mR_je_ha","Stueckvolumen_m3_mR","oi_Biom_t",
              "oi_Biom_t_je_ha","Anteil_Gesamtzahl_Proz",
              "Anteil_Gesamtvorrat_Proz")
  n.kw <- length(Kennwerte)
  #Anzahl BA-Gruppen
  n.bagr <- length(ba.grupp$ba.grupp)
  n.bagr <- n.bagr+1
  #Alle Baumarten erg\u00e4nzen
  ba.grupp$bagr.lab[[n.bagr]] <- "Alle BA"
  ba.grupp$ba.grupp[[n.bagr]] <- c(10:299)
  #Array  f\u00fcr Ergebnisse je  BAGR (Dimension 2 steht f\u00fcr Wert und Standardfehler)
  attr.bagr.tab <- array(dim=c(n.kw,2,n.bagr))
  
  for (i in 1:n.bagr)
  {
    merkmal.t <- stats::aggregate(ifelse(
      wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba 
      %in% ba.grupp$ba.grupp[[i]],wzp4.merkmale.s$nha,0),
      by=list(wzp4.merkmale.s$tnr),sum)
    #Vorrat
    merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(wzp4.merkmale.s$merkmal.s>0
      &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
                
    #Oberirdische Biomasse
    merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(wzp4.merkmale.s$merkmal.s>0
      &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$oib,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
  
    #Starkholzvorrat
    merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
      wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$bhd>=50
      &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
    
    #Gesamtzahl 
    merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
      wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha,0), by=list(wzp4.merkmale.s$tnr),sum)$x)
    #Gesamtvorrat 
    merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
      wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
    
  
  
    names(merkmal.t) <- c("tnr","N.MM","V.MM","oiB.MM","SthV.MM","N.ges","V.ges")
    utils::head(merkmal.t)
  
    merkmal.t <- merge(xy[TRUE, c("tnr", "m", "hbf")],merkmal.t,by="tnr",
                      all.x=T)
    merkmal.t[is.na(merkmal.t)] <- 0
  
    n <- length(trakte.3[,1])
    #St\u00fcckzahl
    R.xy <- r.variance.fun(cbind(merkmal.t$m,merkmal.t$N.MM),n)
    (T.N.MM <- R.xy$R.xy*A)
    (se.T.N.MM <- R.xy$V.R.xy^0.5*A)
  
    #je ha HBF
    R.xy <- r.variance.fun(cbind(merkmal.t$hbf,merkmal.t$N.MM),n)
    (N.MM.ha <- R.xy$R.xy)
    (se.N.MM.ha <- R.xy$V.R.xy^0.5)
  
    #Vorrat
    R.xy <- r.variance.fun(cbind(merkmal.t$m,merkmal.t$V.MM),n)
    (T.V.MM <- R.xy$R.xy*A)
    (se.T.V.MM <- R.xy$V.R.xy^0.5*A)
  
    #je ha HBF
    R.xy <- r.variance.fun(cbind(merkmal.t$hbf,merkmal.t$V.MM),n)
    (V.MM.ha <- R.xy$R.xy)
    (se.V.MM.ha <- R.xy$V.R.xy^0.5)
  
    #St\u00fcckvolumen
    R.xy <- r.variance.fun(cbind(merkmal.t$N.MM,merkmal.t$V.MM),n)
    (V.MM.Stck <- R.xy$R.xy)
    (se.V.MM.Stck <- R.xy$V.R.xy^0.5)
  
    #oberird. Biomasse    (in t umgerechnet)
    R.xy <- r.variance.fun(cbind(merkmal.t$m,merkmal.t$oiB.MM),n)
    (T.oiB.MM <- R.xy$R.xy*A/1000)
    (se.T.oiB.MM <- R.xy$V.R.xy^0.5*A/1000)
  
    #je ha HBF
    R.xy <- r.variance.fun(cbind(merkmal.t$hbf,merkmal.t$oiB.MM),n)
    (oiB.MM.ha <- R.xy$R.xy/1000)
    (se.oiB.MM.ha <- R.xy$V.R.xy^0.5/1000)
  
    #Anteil des Merkmals an der Gesamtzahl
    R.xy <- r.variance.fun(cbind(merkmal.t$N.ges,merkmal.t$N.MM),n)
    (Anteil.MM.N <- R.xy$R.xy)
    (se.Anteil.MM.N <- R.xy$V.R.xy^0.5)
    
    #Anteil des Merkmal-Vorrats am Gesamtvorrat
    R.xy <- r.variance.fun(cbind(merkmal.t$V.ges,merkmal.t$V.MM),n)
    (Anteil.MM.V <- R.xy$R.xy)
    (se.Anteil.MM.V <- R.xy$V.R.xy^0.5)
    
    
    #Ablegen in Tabelle
    attr.bagr.tab[,1,i] <- 
              c(T.N.MM,N.MM.ha,T.V.MM,V.MM.ha,V.MM.Stck,T.oiB.MM,oiB.MM.ha,
              Anteil.MM.N*100,Anteil.MM.V*100)
    attr.bagr.tab[,2,i] <- c(se.T.N.MM,se.N.MM.ha,se.T.V.MM,se.V.MM.ha,se.V.MM.Stck,
              se.T.oiB.MM,se.oiB.MM.ha,se.Anteil.MM.N*100,se.Anteil.MM.V*100)
  } 
  
  #(4) Ergebnisliste
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return( list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Merkmale=merkmale,BAGR=ba.grupp$bagr.lab,
              Kennwerte=Kennwerte,
              Kennwert_Tabelle_BAGR=attr.bagr.tab))
              
}  
#Ende <stamm.merkmale.bagr.fun>

#------------------------------------------------------------------------------------
#' Berechnet Verteilung der Dichtestufen
#'
#' Funktion berechnet Verteilung der Dichtestufen fuer Forstlich bedeutsame 
#' Arten (FBA) entsprechend der Definition der BWI 3; die Funktion kann auch 
#' fuer die entsprechende FBA-Tabelle der BWI 2 <fba.2> angewandt werden 
#' (dort sind aber nur die Arten 11 bis 18 erfasst).
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Version: 01.05.2015
#' @section Note: Folgende FBA sind bei BWI 3 definiert:Adlerfarn, Brennessel,
#'  Riedgras, Honiggras, Reitgras, Heidekraut, Heidelbeere, Brombeere, 
#'  Riesenbaerenklau, Riesenknoeterich, Druesiges Springkraut, Kleinbluetiges 
#'  Springkraut, Kermesbeere; mit Code 11:23.
#' @section Hinweis: fehlende Angaben ("keine Angabe") werden als Nicht-
#'  Vorkommen interpretiert. \cr
#'  Zur Flaechenberechnung: Die Beobachtung des Vorkommens wird in visuell 
#'  angesprochene Dichtestufen angegeben: 0: "nicht vorhanden", 1: "selten, bis 
#'  10%", 2: "haeufig, > 10 bis 50%", 3:"flaechig, > 50%". Diese Dichten werden in 
#'  Flaechenanteile umgerechnet: 0: 0; 1: 0.05; 2: 0.3; 3: 0.7; da jede 
#'  Stichprobe mit dem Repraesentationsfaktor (RF) hochgerechnet wird, ergeben 
#'  sich hieraus Flaechenschaetzungen in ha
#'  d.fl.ant <- data.frame(dichte=0:3,fl.ant=c(0,0.05,0.3,0.7))
#' @param fba Vektor mit den auszuwertenden FBA, wird NA oder "alle" uebergeben, 
#'  werden alle ausgewertet.
#' @param fba.tab Die bei der BWI 3 erfolgte FBA-Dichte-Ansprachen je Traktecke.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt.
#' @param ecken Ecken-Tabelle.
#' @param trakte Trakte-Tabelle.
#' @param A Flaeche des Inventurgebietes in ha.
#' @return Liste mit folgenden Attributen: \strong{Stratum}, \strong{HBF} 
#'  (Holzbodenflaeche), \strong{se.HBF} (Standardfehler Holzbodenflaeche), 
#'  \strong{FBA}, \strong{Dichtestufen}, \strong{FBA.Dichtevertlg}, 
#'  \strong{FBA.Flaeche}. Wobei FBA.Dichtevertlg ein Array mit den
#'  Dichteanteilen der FBA ist und FBA.Flaeche ein Array ist, welches Wert und 
#'  Standardfehler fuer Flaeche (ha) und Flaechenanteile enthaelt.
fba.stratum.fun <- function(fba,fba.tab,auswahl,ecken,trakte,A){
  #Auswertungseinheit festlegen
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfl\u00e4che des Stratums
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(trakte[TRUE, c("tnr", "m")],
             y, by = c("tnr"), all.x = T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #-------------------
  nte.t <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(nte.t) <- c("tnr","nte")   
  #--------------------
  #FBA
  names(fba.tab) <- tolower(names(fba.tab))
  fba.tab <- merge(stratum[TRUE, c("tnr", "enr")],
                   fba.tab[TRUE, c("tnr", "enr", "fba", "dichte")],
                   by = c("tnr", "enr"), all.x = T)
  #fehlende Angaben werden als Nicht-Vorkommen interpretiert
  fba.tab$dichte[is.na(fba.tab$dichte)] <- 0
  #Falls aus Originaldaten Code -1 f\u00fcr fehlende Angabe vorkommt
  fba.tab$dichte[fba.tab$dichte==-1] <- 0
  
  fba.lab <- c("Adlerfarn", "Brennessel",  "Riedgras", "Honiggras",  "Reitgras",
               "Heidekraut", "Heidelbeere","Brombeere", "Riesenb\u00e4renklau",
               "Riesenkn\u00f6terich", "Dr\u00fcsiges Springkraut","Kleinbl\u00fctiges Springkraut",
               "Kermesbeere")
  fba.code <- c(11:23)
  
  if (is.na(fba[1]) | tolower(fba[1])=="alle")
  {fba <- c(11:23) }
  
  #d.lab <- c("k.A.","fehlt","selten","h\u00e4ufig","fl\u00e4chig")
  #entsprechend den Codes -1, 0, 1, 2, 3
  d.lab <- c("nicht vorhanden","selten, bis 10%","h\u00e4ufig, > 10 bis 50%","fl\u00e4chig, > 50%")
  #entsprechend den Codes 0, 1, 2, 3
  #entsprechende Fl\u00e4chenanteile f\u00fcr 0,1,2,3
  d.fl.ant <- data.frame(dichte=0:3,fl.ant=c(0,0.05,0.3,0.7))
  fba.tab <- merge(fba.tab,d.fl.ant,by="dichte",all.x=T)
  fba.tab <- fba.tab[TRUE, c(2:4,1,5)] #TODO: Use named index.
  #Auf \u00fcberschie\u00dfende Fl\u00e4che pr\u00fcfen
  n.fba.te <- stats::aggregate(cbind(ifelse(fba.tab$dichte<=0,0,1),fba.tab$fl.ant),
                        by=list(fba.tab$tnr,fba.tab$enr),sum)
  names(n.fba.te) <- c("tnr","enr","n.fba","sum.fl.ant")
  #\u00dcberschie\u00dfende Fl\u00e4che auf eins korrigieren
  fba.tab <- merge(fba.tab,n.fba.te,by=c("tnr","enr"),all.x=T)
  #fba.tab$fl.ant.0 <- fba.tab$fl.ant
  fba.tab$fl.ant <- ifelse(fba.tab$sum.fl.ant>1,fba.tab$fl.ant/fba.tab$sum.fl.ant,fba.tab$fl.ant)
  
  #---------------------
  #Ausgabe-Tabelle als Array
  #Dichte-Anteile
  #jeweils Wert und Fehler f\u00fcr 4 Dichte-Stufen nach FBA
  fba.d <-array(dim=c(2,4,length(fba)))
  #Fl\u00e4chen und Fl\u00e4chenanteile
  #jeweils Wert und Fehler f\u00fcr FBA f\u00fcr Fl\u00e4che (ha) und Fl\u00e4chenanteil
  fba.fl <-array(dim=c(2,length(fba),2))
  
  ii <- 0
  for (i in fba)
  {
    
    ii <- ii+1
    fba.i <- merge(stratum,
                   fba.tab[fba.tab[["fba"]] == i, 
                           c("tnr", "enr", "fba", "dichte", "fl.ant")],
                   by=c("tnr", "enr"),
                   all.x = T)
    fba.i$dichte[is.na(fba.i$dichte)] <- 0
    fba.i$fl.ant[is.na(fba.i$fl.ant)] <- 0
    
    for (j in 0:3)
    {
      nte.d.j.t <- stats::aggregate(ifelse(fba.i$dichte==j,1,0),by=list(fba.i$tnr),sum)
      names(nte.d.j.t) <- c("tnr","nd")
      nte.d.j.t <- merge(nte.t,nte.d.j.t,by=c("tnr"),all.x=T)
      nte.d.j.t[is.na(nte.d.j.t)] <- 0
      r.list <-  r.variance.fun(nte.d.j.t[,2:3],length(trakte[,1]))
      fba.d[1,(j+1),ii] <- round(r.list$R.xy,4)
      fba.d[2,(j+1),ii] <- round(sqrt(r.list$V.R.xy),5)      
    }
    #Fl\u00e4che sch\u00e4tzen als Total
    fl.i.t <- stats::aggregate(fba.i$fl.ant,by=list(fba.i$tnr),sum)
    names(fl.i.t) <- c("tnr","fl")
    #daher Verkn\u00fcpfung mit <y>
    fl.i.t <- merge(y[,1:2],fl.i.t,by="tnr",all.x=T)
    fl.i.t[is.na(fl.i.t)] <- 0
    r.list <-  r.variance.fun(fl.i.t[,2:3],length(trakte[,1]))
    fba.fl[1,ii,1] <- round(r.list$R.xy*A,1)
    fba.fl[2,ii,1] <- round(sqrt(r.list$V.R.xy)*A,2)
    
    #Fl\u00e4che sch\u00e4tzen als Anteil
    fl.i.t <- stats::aggregate(fba.i$fl.ant,by=list(fba.i$tnr),sum)
    names(fl.i.t) <- c("tnr","fl")
    #Verkn\u00fcpfung mit <nte.t>
    fl.i.t <- merge(nte.t,fl.i.t,by="tnr",all.x=T)
    fl.i.t[is.na(fl.i.t)] <- 0
    r.list <-  r.variance.fun(fl.i.t[,2:3],length(trakte[,1]))
    fba.fl[1,ii,2] <- round(r.list$R.xy,4)
    fba.fl[2,ii,2] <- round(sqrt(r.list$V.R.xy),4)
    
  }
  
  return(list(Stratum = auswahl,
              HBF = T.hbf, se.HBF = se.T.hbf,
              FBA = fba.lab[match(fba,fba.code)],
              Dichtestufen = d.lab,
              FBA.Dichtevertlg=fba.d,
              "FBA.Fl\u00e4che" = fba.fl))
}
#Ende Funktion <fba.stratum.fun>

#-------------------------------------------------------------------------------
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
#' @return Liste mit der Flaeche und dem Standardfehler.
fl.stratum.fun <- function(auswahl,ecken,trakte,A){
  te <- stratum.fun(auswahl,ecken)
  nte.t <- stats::aggregate(rep(1,length(te[,1])),by=list(te$TNr),sum)
  names(nte.t) <- c("TNr","nte")
  utils::head(nte.t)
  nte.t <- merge(trakte[TRUE, c("TNr", "m")],nte.t,by="TNr",all.x=T)
  nte.t[is.na(nte.t)] <- 0
  r.list <- r.variance.fun(nte.t[,2:3],length(trakte.3[,1]))
  return(list(Flaeche=r.list$R.xy*A, SE_Flaeche=sqrt(r.list$V.R.xy)*A))
}

#-------------------------------------------------------------------------------
#' Berechnet den relativen Anteil einer Untermenge
#' 
#' Funktion berechnet den relativen Anteil des \code{substratums} im 
#' \code{stratum}, welches das uebergeordnete Stratum (Obermenge) definiert.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section version: 18.11.2014
#' @param stratum Liste, welche die Eckenmerkmale enthaelt nachdem das 
#'  uebergeordnete Stratum (Obermenge) definiert wird.
#' @param substratum Liste, welche die Eckenmermale enthaelt nachdem das 
#'  Substratum (Untermenge) definiert wird.
#' @param ecken Traktecken-Merkmale, die zur Stratenbildung dienen.
#' @return Liste mit relativen Anteil des Substratums am Stratum und dessen 
#'  Standardfehler.
fl.proz.stratum.fun <- function(stratum,substratum,ecken){
  te <- stratum.fun(stratum,ecken)
  nte.t <- stats::aggregate(rep(1,length(te[,1])),by=list(te$TNr),sum)
  names(nte.t) <- c("TNr","nte")
  substratum.1 <- stratum
  k <- length(names(substratum))
  for (i in 1:k){
    substratum.1[[names(substratum)[i]]] <- substratum[[i]]
  }
  te.s <- stratum.fun(substratum.1,ecken)
  nte.s.t <- stats::aggregate(rep(1,length(te.s[,1])),by=list(te.s$TNr),sum)
  names(nte.s.t) <- c("TNr","nte.s")
  utils::head(nte.s.t)
    nte.s.t <- merge(nte.t,nte.s.t,by="TNr",all.x=T)
  nte.s.t[is.na(nte.s.t)] <- 0
  r.list <- r.variance.fun(nte.s.t[,2:3],length(trakte.3[,1]))
  return(list(Fl_Proz=r.list$R.xy*100,
              SE_Fl_Proz = r.list$V.R.xy^0.5*100))
}

#-------------------------------------------------------------------------------
# UNTERFUNKTIONEN
#-------------------------------------------------------------------------------
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
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl erfolgt.
#' @param ecken Tabelle mit allen zur Selektion dienenden Eckenmerkmalen.
#' @return Untermenge der Ecken, die der Auswahl entsprechen.
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
#-------------------------------------------------------------------------------
#' Klassifiziert und aggegiert die mittlere Baumartenflaeche
#' 
#' Funktion fuehrt eine Klassifikation der mittleren Baumartenflaeche nach 
#' Baumarten-Gruppe, BHD und Alter zur Periodenmitte durch und aggregiert nach 
#' Trakt.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 04.03.2014.
#' @param baeume.vor Tabelle fuer Vorinventur mit Baumartengruppe, 
#'  Attribute: tnr, enr, bnr, pk, pl, ba, bagr, bhd1, bhd2, alt1, alt2, stfl1, 
#'  stfl2.
#' @param baeume.folg Tabelle fuer Folgeinventur mit Baumartengruppe,
#'  Attribute: tnr, enr, bnr, pk, pl, ba, bagr, bhd1, bhd2, alt1, alt2, 
#'  stfl1, stfl2.
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160,A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern fuer Durchmesser z.B. 
#'  list(D.unt=0,D.ob=70,D.b=10,Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, dass 
#'  zusaetzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt \code{D.unt} 
#'  als unterste Schwelle.
#' @return Dataframe-Tabelle mit folgenden Attributen: tnr, bagr, akl.pm,
#'  dkl.pm, baf1, baf2, mbaf, mbaf.hb. Wobei mbaf die mittlere Baumartenflaeche 
#'  und mbaf.hb die mittlere Baumartenflaeche im Hauptbestand enthaelt.
mbaf.bagr.alt.bhd.pm.fun <- function(baeume.vor,baeume.folg,A.klass,D.klass){
  names(baeume.vor) <- tolower(names(baeume.vor))
  names(baeume.folg) <- tolower(names(baeume.folg))
  #Vorinventur (aus Sicht Folgeinventur):
  #pk == 0; B\u00e4ume mit BHD1 < 7 cm! (STP=1,2)
  baeume.vor$bhd.pm <- ifelse(baeume.vor$pk%in%c(2:5,9),baeume.vor$bhd2,
                        (baeume.vor$bhd1+baeume.vor$bhd2)/2)
  baeume.vor$alt.pm <- ifelse(baeume.vor$pk%in%c(2:5,9),baeume.vor$alt2,
                ifelse(baeume.vor$ba < 998,baeume.vor$alt1+baeume.vor$pl/2,0))
  #Folgeinventur
  #Differenzieren nach Kollektiven: STP == 0, STP > 0
  baeume.folg$bhd.pm <- (baeume.folg$bhd1+baeume.folg$bhd2)/2
  baeume.folg$alt.pm <- ifelse(baeume.folg$ba<998,baeume.folg$alt2-baeume.folg$pl/2,
                0)
  #Klassifizierung durchf\u00fchren
  #Alter
  A.max <- 999
  #Hinweis: A-Klassifizierung nach fortgeschriebenem Alter: alt2!!!
  baeume.vor$akl.pm <- cut(baeume.vor$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)
  baeume.folg$akl.pm <- cut(baeume.folg$alt.pm,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T)

  akl.lab <- unique(baeume.vor$akl.pm)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7){
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else{
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  #Hinweis: D-Klassifizierung nach fortgeschriebenem BHD: bhd2!!!
  baeume.vor$dkl.pm <- cut(baeume.vor$bhd.pm, breaks=brks, right=F)
  baeume.folg$dkl.pm <- cut(baeume.folg$bhd.pm, breaks=brks, right=F)

  dkl.lab <- unique(baeume.vor$dkl.pm)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])

  #L\u00fcckenkorrekturfaktoren
  #Vorinventur
  lk.v <- sum(baeume.vor$stfl1)/sum(baeume.vor$stfl1[baeume.vor$ba<998])
  lk.f <- sum(baeume.folg$stfl2)/sum(baeume.folg$stfl2[baeume.folg$ba<998])
  baeume.vor  <- baeume.vor[baeume.vor[["ba"]] < 998, TRUE]
  baeume.folg  <- baeume.folg[baeume.folg[["ba"]] < 998, TRUE]
  #Umrechnen in ha
  baf1.tnr <- stats::aggregate(baeume.vor$stfl1*lk.v/10000,
    by=list(baeume.vor$tnr,baeume.vor$bagr,baeume.vor$akl.pm,baeume.vor$dkl.pm),
    sum)
  names(baf1.tnr) <- c("tnr","bagr","akl.pm","dkl.pm","baf1")
  baf2.tnr <- stats::aggregate(baeume.folg$stfl2*lk.f/10000,
    by=list(baeume.folg$tnr,baeume.folg$bagr,baeume.folg$akl.pm,baeume.folg$dkl.pm),
    sum)
  names(baf2.tnr) <- c("tnr","bagr","akl.pm","dkl.pm","baf2")
  baf12.tnr <- merge(baf1.tnr,baf2.tnr,by=c("tnr","bagr","akl.pm","dkl.pm"),
                all.x=T, all.y=T)
  #NA entfernen
  baf12.tnr$baf1[is.na(baf12.tnr$baf1)] <- 0
  baf12.tnr$baf2[is.na(baf12.tnr$baf2)] <- 0
  #Mittlere Baumartenfl\u00e4che
  baf12.tnr$mbaf <- (baf12.tnr$baf1+baf12.tnr$baf2)/2
  #Mittlere Baumartenfl\u00e4che im Hauptbestand
  baf12.tnr$mbaf.hb <- (baf12.tnr$baf1/lk.v+baf12.tnr$baf2/lk.f)/2
  return(baf12.tnr)
}
#Ende <mbaf.bagr.alt.bhd.pm.fun>

#-------------------------------------------------------------------------------
#' Berechnet Ratio-Schaetzer und seine Varianz
#' 
#' Funktion berechnet Ratio-Schaetzer und seine Varianz ueber Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.03.2014
#' @section Note: 
#'  Version mit \code{xy}, das grundsaetzlich alle Trakte des 
#'  Inventurgebiets umfasst. \cr
#'  Der zugehoerige Ratio-Schaetzer ist R = y/x = S.y/n/S.x/n \cr
#'  <trakte> ist der Datensatz der Trakte im gesamten Inventurgebiet, die Trakte 
#'  sind die primaeren Stichprobeneinheiten, <trakte> enthaelt (u.a.) die Anzahl 
#'  <m> der Traktecken (Subplots) je Trakt --> nicht im Code enthalten (?) \cr
#'  \code{y} sind die Beobachtungen im Stratum (der Untermenge der Trakte), \cr
#'  \code{x}: sind (1) bei Totalen die  Traktecken (Wald/Nichtwald), (2) bei 
#'  Ratio zwischen beob. Groessen die jeweilige Groesse im Stratum (als Untermnege 
#'  der Trakte im Inventurgebiet).
#' @param xy Matrix mit  x1, x2 und y als Spalten.
#' @param n Anzahl der Spalten der Matrix, Anzahl der Trakte im Inventurgebiet.
#' @return Liste mit Ratio-Schaetzer(\code{R.xy}) und Varianz des Ratio-
#'  Schaetzers (\code{V.R.xy}).
r.variance.fun <- function(xy,n){
  #Ratio-Sch\u00e4tzer
  S.y <- sum(xy[,2]); S.x <- sum(xy[,1])
  R.xy <- S.y/S.x
  #Jacobi-Vektor: dR/dS.x/n = -n*S.y/S.x^2, dR/dS.y/n = n/S.x
  J.xy <- c(-n*S.y/S.x^2,n/S.x)
  #Varianz-Covarianz-Matrix von xy ("reduziert")
  xy <- as.matrix(xy)
  VC.xy <- (t(xy)%*%xy)/n/(n-1)
  #Varianz des Ratio-Sch\u00e4tzers
  V.R.xy <- (t(J.xy)%*%VC.xy)%*%J.xy
  return(list(R.xy = R.xy, V.R.xy=as.numeric(V.R.xy)))
}
#Ende <r.variance.fun>

#-------------------------------------------------------------------------------
#' Berechnet einen "kombinierten" Ratio-Schaetzer und seine Varianz
#' 
#' Funktion berechnet einen "kombinierten" Ratio-Schaetzer mit 2 x-Groessen im 
#' Nenner und seine Varianz ueber Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 10.03.2014
#' @section Note: version mit \code{xy}, das grundsaetzlich alle Trakte des 
#'  Inventurgebiets umfasst. \cr
#'  Der zugehoerige Ratio-Schaetzer ist R = y/x = S.y/n/S.x1/n * S.t/S.x2. \cr
#'  \code{t} ist eine Indikatorvariable, welche das Auftreten einer Beobachtung 
#'  von x2 (x2 > 0) erfasst (t = 1, wenn x2 > 0; 0, sonst) \cr
#'  \code{y} sind die Beobachtungen im Stratum (der Untermenge der Trakte),
#'  x1: sind (1) bei Totalen die  Traktecken (Wald/Nichtwald), (2) bei Ratio 
#'  zwischen beob. Groessen die jeweilige Groesse im Stratum (als Untermnege der 
#'  Trakte im Inventurgebiet).
#' @param xy Matrix mit  x1, x2 und y als Spalten.
#' @param n Anzahl der Spalten der Matrix, Anzahl der Trakte im Inventurgebiet.
#' @return Liste mit Ratio-Schaetzer(\code{R.xxy}) und Varianz des Ratio-
#'  Schaetzers (\code{V.R.xxy}).
r.xxy.variance.fun <- function(xy,n){
  #n <- length(trakte[,1])
  #n <- length(xy[,1])
  #Indikator-Variable t f\u00fcr x2 > 0
  ti <- ifelse(xy[,2]>0,1,0)
  #in Datensatz einf\u00fcgen (an 3. Stelle)
  xy <- cbind(xy[,1:2],ti,y=xy[,3])    #TODO: use named index.
  #Summenterme = Komponenten des Ratio-Sch\u00e4tzers
  S.x1 <- sum(xy[,1]); S.x2 <- sum(xy[,2])
  S.ti <- sum(xy[,3]); S.y  <- sum(xy[,4])
  #Ratio-Sch\u00e4tzer
  R.xxy <- S.y/S.x1*S.ti/S.x2
  #Jacobi-Vektor:
  #dR/dS.x1/n = -n*S.y*S.ti/S.x2/S.x1^2, dR/dS.x2  = -S.y*S.ti/S.x1/S.x2^2
  #dR/dS.ti   = S.y/S.x1/S.x2,           dR/dS.y/n = n*S.ti/S.x1/S.x2
  #l\u00e4sst sich mit R.xxy = S.y*S.t/S.x1/S.x2 umformen in
  #dR/dS.x1/n = -R.xxy*n/S.x1, dR/dS.x2/n  = -R.xxy*n/S.x2
  #dR/dS.ti/n   =  R.xxy*n/S.ti,   dR/dS.y/n = R.xxy*n/S.y
  J.xxy <- n*R.xxy*c(-1/S.x1,-1/S.x2,1/S.ti,1/S.y)
  #Varianz-Covarianz-Matrix von xy ("reduziert")
  xy <- as.matrix(xy)
  VC.xxy <- (t(xy)%*%xy)/n/(n-1)
  #Varianz des Ratio-Sch\u00e4tzers
  V.R.xxy <- (t(J.xxy)%*%VC.xxy)%*%J.xxy
  return(list(R.xxy = R.xxy, V.R.xxy=as.numeric(V.R.xxy)))
}
#Ende <r.xxy.variance.fun>

#-------------------------------------------------------------------------------
#' Aggregiert Zuwachsbilanzkomponenten fuer eine Zuwachsperiode
#'
#' Funktion aggregiert Zuwachsbilanzkomponenten furer eine Zuwachsperiode aus
#' Aufnahmedaten der Vor- und Folgeinventur.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Note: Erwartet werden die nach Baumartengruppe-Gruppe(BaGr), 
#'  Altersklasse (akl) und Durchmesser-Klasse (dkl) jweils zur Periodenmitte 
#'  klassifizierten Probebaum-Datensaetze der Vor- und Folgeinventur.
#' @section Aktualisierungen: 
#'  11.04.2014 (1) Einfuegen Derbholzvolumen im Hauptbestand, (2) Ausgeschiedener 
#'    Vorrat \cr
#'  09.02.2015 Biomassezuwachs auf Derbholz-Kollektiv beschraenkt (echter 
#'  Einwuchs);  Grund: Einwuchs ins Derbholz-Kollektiv wird als Zuwachs explizit 
#'  erfasst, da bei Vorinventur nicht vorhanden. Wuerde auch die Biomasse 
#'  unterhalb der Derbholzschwelle einbebzogen, muesste der Biomasse-Vorrat 
#'  unterhalb der Derbholzschwelle der Vorinventur abgezogen werden! Beim echten 
#'  Einwuchs wird die Differenz gebildet oiB2 -oiB1)...
#' @param baeume.vor Messdaten der Vorinventur.
#' @param baeume.folg Messdaten der Folgeinventur.
#' @param ecken Eckeninformationen.
#' @return Dataframe-Tabelle mit Informationen ueber Zuwachsbilanzen, Biomasse 
#'  in Tonnen, Grundflaeche in m² und Baumartenflaeche.
iVB.bilanz.bagr.akl.dkl.fun <- function(baeume.vor,baeume.folg,ecken){
  #(1)Berechnung der Bilanzkomponenten iV.S und iV.E aus baeume.folg
  #nach Trakt, BAGR, akl, dkl
  baeume.ba <- baeume.folg[TRUE, c("TNr", "ENr", "STP", "BNr", "Pk", "BHD1", 
                                   "BHD2", "BaGr", "akl", "dkl", "VolV1", 
                                   "VolV2", "VolE1", "VolE2", "oiB1", "oiB2", 
                                   "NHa2", "StFl2")]

  #(1.1) S-Baeume
  #(1.111) Derbholz m.R.
  iv.es.t <- stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(1.112) Derbholz m.R. im Hauptbestand (k\u00e4/11.04.2014)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0)* ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.12) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.13) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.2) Es-Baeume (Kriterium: modellierter BHD1 >= 7 cm)
  #(1.211) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.212) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0)*ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.22) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.23) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.3) Ew-Baeume (echter Einwuchs)
  #(1.311) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.312) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7,1,0)*ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.32) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolE2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.33) oberird. Biomasse (nur Derbholzkollektiv k\u00e4/09.02.2015)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #BA-Fl\u00e4che (\u00fcber alle PB der BAGr)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$StFl2,
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)
  #Endvorrat (S2,Es)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk<=1 & baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  names(iv.es.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.S","iV.DhmR.HB.S",
          "iV.EoR.S","iB.S","iV.DhmR.Es","iV.DhmR.HB.Es","iV.EoR.Es",
          "iB.Es","V.DhmR.E","V.DhmR.HB.E","V.EoR.E","B.E","BAF2","V.DhmR.S2.Es")
  utils::head(iv.es.t)

  #(2) Berechnung der Bilanzkomponenten iV.A aus baeume.vor
  baeume.ba <- baeume.vor[TRUE, c("TNr", "ENr", "STP", "BNr", "Pk", "BHD1", 
                                  "BHD2", "BaGr", "akl", "dkl", "VolV1", 
                                  "VolV2", "VolE1", "VolE2", "oiB1", "oiB2", 
                                  "NHa1", "StFl1")]
  #(2.1) A-Baeume
  #(2.111) Derbholz m.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(2.112) Derbholz m.R. im Hauptbestand Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.113) Derbholz m.R. ausgeschiedener Vorrat (insgesamt zur PM)
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.114) Derbholz m.R. im Hauptbestand ausgeschiedener Vorrat (insgesamt zur PM) 
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(2.121) Erntevolumen o.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.122) Erntevolumen o.R. ausgeschiedene Vorrat
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate(baeume.ba$VolE2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.131) oberird. Biomasse  Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.132) oberird. Biomasse  ausgeschiedener Vorrat
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$oiB2*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.14) BA-Fl\u00e4che (\u00fcber alle PB der BAGr)
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$StFl1,
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)

  #Anfangsvorrat (S1
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$VolV1*baeume.ba$NHa1*ifelse(baeume.ba$Pk==1,1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$VolV1*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk%in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)


  names(iv.a.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.A","iV.DhmR.HB.A",
    "V.DhmR.A","V.DhmR.HB.A","iV.EoR.A","V.EoR.A", "iB.A","B.A","BAF1",
    "V.DhmR.S1","V.DhmR.A1")
  utils::head(iv.a.t)

  iv.es.a.t <- merge(iv.es.t,iv.a.t, by=c("TNr","BaGr","Akl","Dkl"),all.x=T,all.y=T)
  #NA-Werte auf 0 setzen, um traktweise zu bilanzieren
  iv.es.a.t[is.na(iv.es.a.t)] <- 0
  length(iv.es.a.t[,1])
 
  #Periodenl\u00e4nge je Trakt hinzuf\u00fcgen
  nte.pl.t <- stats::aggregate(rep(1,length(ecken[,1])),by=list(ecken$TNr),sum)
  nte.pl.t <- cbind(nte.pl.t,
                     stats::aggregate(ecken$PL,by=list(ecken$TNr),mean)$x,
                     stats::aggregate(ecken$PLkal,by=list(ecken$TNr),mean)$x)
  names(nte.pl.t) <- c("TNr","nTE","mPL","mPLkal")
  iv.es.a.t <- merge(iv.es.a.t,nte.pl.t,by=c("TNr"),all.x=T)

  #Zuwachsbilanz auf Traktebene (PSU) zu erstellen
  #Ausgangsdatensatz ist <iv.es.a.t>

  #  iv.es.a.t[is.na(iv.es.a.t)] <- 0
  
  utils::head(iv.es.a.t)

  #Zuwachsbilanzen
  iv.es.a.t$iV.DhmR <-
  iv.es.a.t$iV.DhmR.S+iv.es.a.t$iV.DhmR.Es+iv.es.a.t$V.DhmR.E+iv.es.a.t$iV.DhmR.A
  iv.es.a.t$iV.DhmR.HB <-
  iv.es.a.t$iV.DhmR.HB.S+iv.es.a.t$iV.DhmR.HB.Es+iv.es.a.t$V.DhmR.HB.E+iv.es.a.t$iV.DhmR.HB.A
  iv.es.a.t$iV.EoR <-
  iv.es.a.t$iV.EoR.S+iv.es.a.t$iV.EoR.Es+iv.es.a.t$V.EoR.E+iv.es.a.t$iV.EoR.A
  #Biomasse in Tonnen
  iv.es.a.t$iB <-
  (iv.es.a.t$iB.S + iv.es.a.t$iB.Es + iv.es.a.t$B.E  + iv.es.a.t$iB.A)/1000
  iv.es.a.t$B.A <- iv.es.a.t$B.A/1000

  #Mittlere Baumartenfl\u00e4che
  hbf <- length(ecken[,1]) #ohne REF!!!
  lk.2 <- hbf/sum(baeume.folg$StFl2[baeume.folg$Pk!=8])*10000
  lk.1 <- hbf/sum(baeume.vor$StFl1[baeume.vor$Pk!=8])*10000
  iv.es.a.t$mBAF <- (iv.es.a.t$BAF1*lk.1+iv.es.a.t$BAF2*lk.2)/2
  #Mitt. Baumartenfl\u00e4che OHNE L\u00fcckenkorrektur (Bezugsfl\u00e4che f\u00fcr Hauptbestand)
  iv.es.a.t$mBAF.oLK <- 
        (iv.es.a.t$BAF1+iv.es.a.t$BAF2)/2
  
  
  return(iv.es.a.t)
}
#Ende <iVB.bilanz.bagr.akl.dkl.fun>

#-------------------------------------------------------------------------------
#' Aggregiert Zuwachsbilanzkomponenten fuer eine Zuwachsperiode
#'
#' Funktion aggregiert Zuwachsbilanzkomponenten furer eine Zuwachsperiode aus
#' Aufnahmedaten der Vor- und Folgeinventur.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Note: Erwartet werden die nach Baumartengruppe-Gruppe(BaGr), 
#'  Altersklasse (akl) und Durchmesser-Klasse (dkl) jweils zur Periodenmitte 
#'  klassifizierten Probebaum-Datensaetze der Vor- und Folgeinventur.
#' @section Aktualisierungen: 
#'  11.04.2014 (1) Einfuegen Derbholzvolumen im Hauptbestand, (2) Ausgeschiedener 
#'    Vorrat \cr
#'  15.01.2015 Grundflaechenzuwachs bzw. -abgang \cr
#'  09.02.2015 (1) oberird. Biomasse: Beschraenkung auf Derbholzkollektiv (vom 
#'    26.01.2015) wird beibehalten, zusaetzlich wird beim echten Einwuchs die 
#'    Differenz  oiB2 - oiB1 gebildet! \cr
#'    (2) Beim Grundflaechenzuwachs wird nur das Derbholz betrachtet, daher wird 
#'    beim echten Einwuchs die Differenz zur Derbholzschwelle gebildet: 
#'    (BHD2^2 - 7^2)/.... Generell wird beim Derbholzkollektiv alles als Zuwachs 
#'    betrachtet, welches innerhalb der Periode die Kriterien erf?llt:
#'    (a) Derbholz-Kollektiv: d.h. BHD2  > 7 (bei E- und S-Baeumen), bei den 
#'    A-Baeumen ebenfalls (hier ist BHD1 immer >= 7) Dies gilt auch fuer die 
#'    Biomasse, da die Veraenderung zur Vorinventur nicht erfasst wird, der 
#'    Biomassevorrat unterhalb der Derbholzschwelle muesste abgezogen werden!
#' @section Hinweis: 
#'  09.02.2015 Bei der Klassifikation nach Durchmessern zur Periodenmitte tritt 
#'  das Phaenomen auf, dass der Teil des Kollektivs herausfaellt, dessen BHD zur
#'  Periodenmitte noch nicht 7 cm erreicht hat! bzw. dieser Zuwachs wird der 
#'  Klasse BHD < 7 cm zugewiesen.
#' @param baeume.vor Messdaten der Vorinventur.
#' @param baeume.folg Messdaten der Folgeinventur.
#' @param ecken Eckeninformationen.
#' @return Dataframe-Tabelle mit Informationen ueber Zuwachsbilanzen, Biomasse 
#'  in Tonnen, Grundflaeche in m² und Baumartenflaeche.
iVB.bilanz.bagr.akl.dkl.fun.2g <- function(baeume.vor,baeume.folg,ecken){
  #(1)Berechnung der Bilanzkomponenten iV.S und iV.E aus baeume.folg
  #nach Trakt, BAGR, akl, dkl       + k\u00e4/09.02.2015: <bhd.pm>
  baeume.ba <- baeume.folg[TRUE, c("TNr", "ENr", "STP", "BNr", "Pk", "BHD1", 
                                   "BHD2", "bhd.pm", "BaGr", "akl", "dkl", 
                                   "VolV1", "VolV2", "VolE1", "VolE2", "oiB1", 
                                   "oiB2", "NHa2", "StFl2")]

  #(1.1) S-Baeume
  #(1.111) Derbholz m.R.
  iv.es.t <- stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(1.112) Derbholz m.R. im Hauptbestand (k\u00e4/11.04.2014)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0)* ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.12) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.13) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(1.14) Grundfl\u00e4che (Derbholz)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$BHD2^2-baeume.ba$BHD1^2)*pi/40000*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(1.2) Es-Baeume (Kriterium: modellierter BHD1 >= 7 cm)
  #(1.211) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.212) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0)*ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.22) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.23) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(1.24) Grundfl\u00e4che (Derbholz)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$BHD2^2-baeume.ba$BHD1^2)*pi/40000*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(1.3) Ew-Baeume (echter Einwuchs)   
  
  #(1.311) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2 >=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.312) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2 >=7,1,0)
          *ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.32) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolE2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2 >=7 ,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.33) oberird. Biomasse: nur Derbholzkollektiv k\u00e4/26.01.2015
  #k\u00e4/09.02.2015: hier wird die Differenz gebildet, da auch der Anfangswert mit
  #BHD < 7 cm eine Biomasse hat (im Unterschied zum Derbholzvolumen!)
  #Das Erreichen der Derbholzschwelle wird ebenfalls ber\u00fccksichtigt!
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(1.34) Grundfl\u00e4che (Derbholz) +k\u00e4/09.02.2015: Derbholz-Zuwachs: 7[cm]²abziehen!!  
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate((baeume.ba$BHD2^2-7^2)*pi/40000*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
          
  #BA-Fl\u00e4che (\u00fcber alle PB der BAGr)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$StFl2,
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)
  #Endvorrat (S2,Es)
  iv.es.t <- cbind(iv.es.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk<=1 & baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  names(iv.es.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.S","iV.DhmR.HB.S",
          "iV.EoR.S","iB.S","iG.S","iV.DhmR.Es","iV.DhmR.HB.Es","iV.EoR.Es",
          "iB.Es","iG.Es","V.DhmR.E","V.DhmR.HB.E","V.EoR.E","B.E","G.E","BAF2",
          "V.DhmR.S2.Es")
  utils::head(iv.es.t)

  #(2) Berechnung der Bilanzkomponenten iV.A aus baeume.vor
  baeume.ba <- baeume.vor[TRUE, c("TNr", "ENr", "STP", "BNr", "Pk", "BHD1", 
                                  "BHD2", "BaGr", "akl", "dkl", "VolV1", 
                                  "VolV2", "VolE1", "VolE2", "oiB1", "oiB2", 
                                  "NHa1", "StFl1")]
  #(2.1) A-Baeume
  #(2.111) Derbholz m.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(2.112) Derbholz m.R. im Hauptbestand Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.113) Derbholz m.R. ausgeschiedener Vorrat (insgesamt zur PM)
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.114) Derbholz m.R. im Hauptbestand ausgeschiedener Vorrat (insgesamt zur PM)
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(2.121) Erntevolumen o.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.122) Erntevolumen o.R. ausgeschiedene Vorrat
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate(baeume.ba$VolE2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.131) oberird. Biomasse  Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.132) oberird. Biomasse  ausgeschiedener Vorrat
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$oiB2*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(2.141) Grundfl\u00e4che Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          stats::aggregate((baeume.ba$BHD2^2-baeume.ba$BHD1^2)*pi/40000*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.142) Grundfl\u00e4che  ausgeschiedener Vorrat
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$BHD2^2*pi/40000*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(2.14) BA-Fl\u00e4che (\u00fcber alle PB der BAGr)
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$StFl1,
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)

  #Anfangsvorrat (S1
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$VolV1*baeume.ba$NHa1*ifelse(baeume.ba$Pk==1,1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  iv.a.t <- cbind(iv.a.t,
      stats::aggregate(baeume.ba$VolV1*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk%in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)


  names(iv.a.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.A","iV.DhmR.HB.A",
    "V.DhmR.A","V.DhmR.HB.A","iV.EoR.A","V.EoR.A", "iB.A","B.A","iG.A","G.A",
    "BAF1","V.DhmR.S1","V.DhmR.A1")
  utils::head(iv.a.t)

  iv.es.a.t <- merge(iv.es.t,iv.a.t, by=c("TNr","BaGr","Akl","Dkl"),all.x=T,all.y=T)
  #NA-Werte auf 0 setzen, um traktweise zu bilanzieren
  iv.es.a.t[is.na(iv.es.a.t)] <- 0
  length(iv.es.a.t[,1])

  #Periodenl\u00e4nge je Trakt hinzuf\u00fcgen
  nte.pl.t <- stats::aggregate(rep(1,length(ecken[,1])),by=list(ecken$TNr),sum)
  nte.pl.t <- cbind(nte.pl.t,
                     stats::aggregate(ecken$PL,by=list(ecken$TNr),mean)$x,
                     stats::aggregate(ecken$PLkal,by=list(ecken$TNr),mean)$x)
  names(nte.pl.t) <- c("TNr","nTE","mPL","mPLkal")
  iv.es.a.t <- merge(iv.es.a.t,nte.pl.t,by=c("TNr"),all.x=T)

  #Zuwachsbilanz auf Traktebene (PSU) zu erstellen
  #Ausgangsdatensatz ist <iv.es.a.t>

  #  iv.es.a.t[is.na(iv.es.a.t)] <- 0

  utils::head(iv.es.a.t)

  #Zuwachsbilanzen
  iv.es.a.t$iV.DhmR <-
  iv.es.a.t$iV.DhmR.S+iv.es.a.t$iV.DhmR.Es+iv.es.a.t$V.DhmR.E+iv.es.a.t$iV.DhmR.A
  iv.es.a.t$iV.DhmR.HB <-
  iv.es.a.t$iV.DhmR.HB.S+iv.es.a.t$iV.DhmR.HB.Es+iv.es.a.t$V.DhmR.HB.E+iv.es.a.t$iV.DhmR.HB.A
  iv.es.a.t$iV.EoR <-
  iv.es.a.t$iV.EoR.S+iv.es.a.t$iV.EoR.Es+iv.es.a.t$V.EoR.E+iv.es.a.t$iV.EoR.A
  #Biomasse in Tonnen
  iv.es.a.t$iB <-
  (iv.es.a.t$iB.S + iv.es.a.t$iB.Es + iv.es.a.t$B.E  + iv.es.a.t$iB.A)/1000
  iv.es.a.t$B.A <- iv.es.a.t$B.A/1000
  #Grundfl\u00e4che in m²
  iv.es.a.t$iG <-
  (iv.es.a.t$iG.S + iv.es.a.t$iG.Es + iv.es.a.t$G.E  + iv.es.a.t$iG.A)

  #Mittlere Baumartenfl\u00e4che
  hbf <- length(ecken[,1]) #ohne REF!!!
  lk.2 <- hbf/sum(baeume.folg$StFl2[baeume.folg$Pk!=8])*10000
  lk.1 <- hbf/sum(baeume.vor$StFl1[baeume.vor$Pk!=8])*10000
  iv.es.a.t$mBAF <- (iv.es.a.t$BAF1*lk.1+iv.es.a.t$BAF2*lk.2)/2
  #Mitt. Baumartenfl\u00e4che OHNE L\u00fcckenkorrektur (Bezugsfl\u00e4che f\u00fcr Hauptbestand)
  iv.es.a.t$mBAF.oLK <-
        (iv.es.a.t$BAF1+iv.es.a.t$BAF2)/2


  return(iv.es.a.t)
}#Ende <iVB.bilanz.bagr.akl.dkl.fun.2g>

#-------------------------------------------------------------------------------
#' Prueft ob Baumart in Baumartengruppen vorkommt
#' 
#' Funktion prueft ob ein bestimmter BWI-Baumarten-Code in einer Liste mit
#' Baumartenzusammenfassung zu Baumgruppen enthalten ist.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @param ba BWI-Baumart-Code.
#' @param ba.klass Liste mit Baumartenzusammenfassung zu Baumgruppen \cr
#'  Beispiel ba.klass <- list(c(10:19,30:39,90:99), c(20:29,40,50,51), c(100),
#'  c(110,111), c(112,120:199),c(200:299)).
#' @return Wenn der Baumartencode in der Liste mit der Baumartenzusammenfassung 
#'  gefunden wurde, wird die Position des ersten Vectors in der Liste, welcher 
#'  die Baumart enthaelt, zurueck gegeben. Wenn der Baumartencode nicht gefunden
#'  wurde, wird NA zurueck gegeben.
ba.klass.fun <- function(ba,ba.klass){
  i <- 0
  while (i < length(ba.klass)){
    i <- i+1
    ba.in <- ba%in%ba.klass[[i]]
    if(ba.in) {break}
  }
  return(ifelse(ba.in,i,NA))
}

#-------------------------------------------------------------------------------
#' Erzeugt Zuordnungstabelle mit Baumartengruppen-Bezeichner
#'
#' Funktion erzeugt Zuordnungstabelle mit Baumartengruppen-Bezeichner, welche
#' benoetigt wird, um Klassifikation nach frei zu definierenden Baumartengruppen 
#' zu ermoeglichen.
#'
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @param bacodes Basistabelle mit Baumartencodes.
#' @param ba.lab.klass Liste mit Angaben zu Baumartenlabel und Baumartencode \cr
#'  Beispiel: ba.lab.klass <- list(ba.lab = c("FiTa","DglKiLae","Bu","Ei","BLb",
#'  "WLb"), ba.code = list(c(10:19,30:39,90:99), c(20:29,40,50,51), c(100),
#'  c(110,111), c(112:199), c(200:299))).
#' @return Dataframe-Tabelle mit Baumartengruppen-Nummer und dem entspechenden 
#'  Baumartengruppen-Label.
ba.klass.lab.tab.fun <- function(ba.lab.klass, bacodes = get_data("bacode")){
   n <-  length(bacodes[,1])
   ba.klass.tab <- data.frame(ICode=bacodes$ICode,bagr.nr=rep(0,n))
   for (i in 1:n){
      ba.klass.tab$bagr.nr[i] <- ba.klass.fun(ba.klass.tab$ICode[i],ba.lab.klass[[2]])
      ba.klass.tab$bagr[i] <- as.character(ba.lab.klass[[1]][ba.klass.tab$bagr.nr[i]])
   }

   return(ba.klass.tab)
}

#-------------------------------------------------------------------------------
#' Erzeugt Durchmesserklassen-Beschriftung
#' 
#' Erzeugt ein Label fuer die Durchmesserklassen-Beschriftung aus der 
#' Klassifizierungsregel und der Anzahl der Durchmesserklassen.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 16.07.2014
#' @section Korrekturdatum: 07.02.2015
#' @section Note: Anzahl der Durchmesserklassen wird vom rufenden Programm 
#'  errechnet.
#' @param D.klass vorgegebene Klassifizierungsregel.
#' @param D.k Anzahl der Durchmesserklassen.
#' @return Label fuer Durchmesserklassen-Beschriftung.
dkl.lab.fun <- function(D.klass,D.k){
  dkl.lab <- rep(0,D.k)
  if (D.k > 1){
    if (D.klass[[4]]){
      dkl.lab[1] <- "0-6.9"
      dkl.lab[2] <- paste("7-",D.klass[[3]]-0.1,sep="")
      for (ii in 3:(D.k-1)){
        dkl.lab[ii] <- paste((ii-2)*D.klass[[3]],"-",(ii-1)*D.klass[[3]]-0.1,
                sep="")
      }
    } else{
      for (ii in 1:(D.k-1)){
          dkl.lab[ii] <- paste(D.klass[[3]]*(ii-1),"-",D.klass[[3]]*ii-0.1,sep="")
        }
        dkl.lab[D.k] <- paste(">",D.klass[[1]],sep="")
    }
    dkl.lab[D.k] <- paste(">=",D.klass[[2]],sep="")
  }else
  {dkl.lab[D.k] <- paste(D.klass[[1]],"-",D.klass[[2]],sep="")} #k\u00e4/07.02.2015
  
  return(dkl.lab)
}

#-------------------------------------------------------------------------------
#' Erzeugt Altersklassen-Beschriftung
#' 
#' Erzeugt Label fuer die Altersklassen-Beschriftung aus der 
#' Klassififizierungsregel und der Anzahl an Altersklassen.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 16.07.2014
#' @section Note: Anzahl der Altersklassen wird vom rufenden Programm berechnet.
#' @param A.klass vorgegebene Klassifizierungsregel.
#' @param A.k Anzahl der Altersklassen.
#' @return Label fuer Altersklassen-Beschriftung.
akl.lab.fun <- function(A.klass,A.k){
  akl.lab <- rep(0,A.k)
  if (A.k > 1){
    for (ii in 1:(A.k-1)){
      akl.lab[ii] <- paste(A.klass[[2]]*(ii-1)+1,"-",A.klass[[2]]*ii,sep="")
    }
    akl.lab[A.k] <- paste(">",A.klass[[1]],sep="")
  } else{akl.lab[A.k] <- paste("0-",A.klass[[1]],sep="")}
  
  return(akl.lab)
}

#-------------------------------------------------------------------------------
# HILFSFUNKTIONEN
#-------------------------------------------------------------------------------
#'Erstelle Dataframe-Tabelle fuer Verjuengung
#'
#'Funktion erzeugt aus Ergebnistabelle der Funktion \code{\link{verjg.bagr.fun}}
#'bzw. \code{\link{verjg.bagrupp.fun}} eine Dataframe-Tabelle.
#'
#'@author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#'@section Note: Hilfsfunktion zur Tabellenausgabe.
#'@param vj.tab Ergebnistabelle aus \code{\link{verjg.bagr.fun}} oder
#'  \code{\link{verjg.bagrupp.fun}}.
#'@return Dataframe-Tabelle.
verjg.tab.fun <- function(vj.tab){
  return(data.frame(BAGR=c(vj.tab$BAGR,"Alle"),
    "St\u00fcckzahl_Verj_Mio"=round(vj.tab$n.Verjg.BAGR[1,1,]/1e6,3),
    "SE_St\u00fcckzahl_Verj_Mio"=round(vj.tab$n.Verjg.BAGR[1,2,]/1e6,3),
    Anteil_Proz = round(vj.tab$n.Verjg.BAGR[2,1,],1),
    SE_Anteil_Proz = round(vj.tab$n.Verjg.BAGR[2,2,],2) ))
}
#' 
#'
#'Funktion erzeugt aus Ergebnistabelle der Funktion \code{\link{verjg.bagr.fun}}
#'bzw. \code{\link{verjg.bagrupp.fun}} eine Dataframe-Tabelle.
#'
#'@author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#'@param wzp4.merkmale Eine Tabelle der besonderen Stammmerkmale,
#' BWI 3
#' merkmal.list.3 <- c("tot","jschael","aeschael","ruecke","pilz","harz","kaefer",
#'                "sstamm","faulkon","hoehle","bizarr","uralt","horst","mbiotop")
#' BWI 2
#' merkmal.list.2 <- c("tot","jschael","aeschael","ruecke","pilz","harz","kaefer",
#'               "sstamm","hoehle")                
#' @param baeume Datentabelle mit Bauminformationen. Die Tabelle muss mindestens 
#'  die Attribute TNr, ENr, BA, Alt., BHD., VolV., oiB., NHa., StFl. enthalten 
#'  (. steht fuer 1 oder 2, je nachdem welcher Zustand ausgewertet werden soll), 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in 
#'  \code{baeume} uebergeben wird.
#' @param ecken Eckenmerkmale.
#' @param trakte Traktmerkmale.
#' @param auswahl auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param A Gesamtflaeche in ha des Inventurgebiets zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @param merkmale legt das/die Merkmal/e fest (es koennen mehrere angegeben 
#'  werden, diese werden als Vereinigungsmenge je Baum interpretiert!)
#' @param ba.grupp Liste mit Baumarten-Zusammenfassungen zu Baumgruppen mit 
#'  Bezeichner der Baumarten-Gruppen ("lab") z.B. list(bagr.lab = c("FiTa", 
#'  "DglKiLae", "Bu", "Ei", "BLb", "WLb"), ba.grupp =list(c(10:19,30:39,90:99), 
#'  c(20:29,40,50,51), c(100), c(110,111), c(112:199), c(200:299))).
#' @param A.klass Liste mit den Klassifizierungsparametern fuers Alter: z.B. 
#'  list(A.ob=160, A.b=20).
#'  
#' @note da nur das WZP-4-Kollektiv beruecksichtigt wird, koennen keine 
#' Baumartenflaechen berechnet werden!
#'@return Eine Liste mit Zeug
stamm.merkmale.bagr.akl.fun <- function(wzp4.merkmale,baeume,ecken,trakte,A,
                                    auswahl,merkmale,ba.grupp,A.klass)
{
#Version 1,  05.11.2015
#basierend auf <stamm.merkmale.bagr.fun>
#nur f\u00fcr BWI 2 und 3!
  #(1) Befundeinheit festlegen (Traktecken ausw\u00e4hlen)
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  names(baeume) <- tolower(names(baeume))
  names(wzp4.merkmale) <- tolower(names(wzp4.merkmale))
  #"Neutralisierung" der ben\u00f6tigten Attributnamen in <baeume>: <2> entfernen
  names(baeume) <- sub(2,names(baeume),replacement="")
  
  #Aus Kompatibilt\u00e4ts-Gr\u00fcnden werden die nur in der BWI 3 vorkommenden
  #Attribute <Ast> und <Ast_Hoe> im DS der BWI 3 entfernt
  ast.pos <- grep("Ast",names(wzp4.merkmale))
  if (length(ast.pos)>0)
  {
    wzp4.merkmale <- wzp4.merkmale[TRUE, -c(ast.pos)]
  }
  #(2) Merkmal-DS <wzp4.merkmale> mit Attribut-Auswahl aus <baeume> verkn\u00fcpfen
  wzp4.merkmale <- merge(baeume[baeume[["stp"]] == 0,
                                c("tnr", "enr", "bnr", "ba", "alt", "bhd", 
                                  "h", "volv", "oib", "nha", "stfl")],
                         wzp4.merkmale, by=c("tnr","enr","bnr"),all.x=T)
  wzp4.merkmale[is.na(wzp4.merkmale)] <- 0
  
  #Probeb\u00e4ume im Stratum mit den Merkmalen in  <merkmale> ausw\u00e4hlen
  #Anzahl zu ber\u00fccksichtigender Merkmale
  k <- length(merkmale)
  mm.pos <- rep(0,k)
  #Wenn mehrere Merkmale als Kriterium angegeben sind, wird ein Attribut
  #gebildet, welches die Vereinigungsmenge der betrachteten Merkmale darstellt
  wzp4.merkmale$merkmal.s <- rep(0,length(wzp4.merkmale[,1]))
  for (i in 1:k)
  {
    mm.pos[i] <- grep(toupper(merkmale[i]),toupper(names(wzp4.merkmale)))
    wzp4.merkmale$merkmal.s <- wzp4.merkmale$merkmal.s+ wzp4.merkmale[,mm.pos[i]]
  }
  mm.s.pos <- length(wzp4.merkmale)
  wzp4.merkmale.s <- merge(
                           wzp4.merkmale[TRUE, c(1:3,12,mm.pos,mm.s.pos,4:11)], #TODO: use names, not index numbers!
                           stratum[TRUE, c("tnr", "enr")],
                           by=c("tnr","enr"),all.y=T)
  wzp4.merkmale.s[is.na(wzp4.merkmale.s)] <- 0 
  
  #---------------------
  #(3) Fl\u00e4chen
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  #------
  #HBF nach Trakt im Stratum
  xy <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Bl\u00f6\u00dfen (BL): BA=999, L\u00fccken (iBL): BA=998
  xy <- cbind(xy,stats::aggregate(ifelse(
    wzp4.merkmale.s$ba==999,wzp4.merkmale.s$stfl/10000,0),
    by=list(wzp4.merkmale.s$tnr),sum)$x)
  xy <- cbind(xy,stats::aggregate(ifelse(wzp4.merkmale.s$ba==998,wzp4.merkmale.s$stfl,0),
                           by=list(wzp4.merkmale.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(trakte[TRUE, c("tnr", "m")],xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl
  
  #HBFl. [ha]
  r.list= r.variance.fun(xy[TRUE, c("m", "hbf")],nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Bl\u00f6\u00dfen [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "bl")],nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Bl\u00f6\u00dfen ("L\u00fccken") [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "ibl")],nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #L\u00fcckenkorrekturfaktor
  r.list <- r.variance.fun(xy[TRUE, c("hbf.ba", "hbf")],nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  
  #Altersklassifikation
  
  A.max <- 999
  #----- k\u00e4/15.02.
  wzp4.merkmale.s$akl <- as.integer(cut(wzp4.merkmale.s$alt,
                                 breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T))
  akl.lab <- unique(wzp4.merkmale.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Maximale Anzahl Altersklassen aus A-Kl-Parametern <A.klass>
  if(A.klass[[1]]>A.klass[[2]])
  {
    max.A.k <- A.klass[[1]]/A.klass[[2]] + 1
    if (A.k < max.A.k) {A.k <- max.A.k }
  }
  
  
  #(4) Attribute des Merkmals auf Trakt aggregieren
  
  #getrennt f\u00fcr die Baumarten(gruppen)
  
  Kennwerte=c("Gesamtzahl","Zahl_je_ha","Vorrat_m3_mR",
              "Vorrat_m3_mR_je_ha","Stueckvolumen_m3_mR","oi_Biom_t",
              "oi_Biom_t_je_ha","Anteil_Gesamtzahl_Proz",
              "Anteil_Gesamtvorrat_Proz")
  n.kw <- length(Kennwerte)
  #Anzahl BA-Gruppen
  n.bagr <- length(ba.grupp$ba.grupp)
  n.bagr <- n.bagr+1
  #Alle Baumarten erg\u00e4nzen
  ba.grupp$bagr.lab[[n.bagr]] <- "Alle BA"
  ba.grupp$ba.grupp[[n.bagr]] <- c(10:299)
  #Array  f\u00fcr Ergebnisse je  BAGR und AKL (Dimension 2 steht f\u00fcr Wert und Standardfehler)
  attr.bagr.akl.tab <- array(dim=c(n.kw,2,n.bagr,A.k))
  
  for (j in 1:A.k)
  {
    for (i in 1:n.bagr)
    {
      merkmal.t <- stats::aggregate(ifelse(
        wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba 
        %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,wzp4.merkmale.s$nha,0),
        by=list(wzp4.merkmale.s$tnr),sum)
      #Vorrat
      merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
                  wzp4.merkmale.s$merkmal.s>0
                  &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
                  wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Oberirdische Biomasse
      merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
                  wzp4.merkmale.s$merkmal.s>0
                  &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
                  wzp4.merkmale.s$nha*wzp4.merkmale.s$oib,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Starkholzvorrat
      merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
        wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$bhd>=50
        &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
        wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
        by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Gesamtzahl 
      merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
        wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
        wzp4.merkmale.s$nha,0), by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Gesamtvorrat 
      merkmal.t <- cbind(merkmal.t,stats::aggregate(ifelse(
        wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
        wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
        by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      
      
      names(merkmal.t) <- c("tnr","N.MM","V.MM","oiB.MM","SthV.MM","N.ges","V.ges")
      utils::head(merkmal.t)
      
      merkmal.t <- merge(xy[TRUE, c("tnr", "m", "hbf")],merkmal.t,by="tnr",
                         all.x=T)
      merkmal.t[is.na(merkmal.t)] <- 0
      
      n <- length(trakte.3[,1])
      #St\u00fcckzahl
      R.xy <- r.variance.fun(cbind(merkmal.t$m,merkmal.t$N.MM),n)
      (T.N.MM <- R.xy$R.xy*A)
      (se.T.N.MM <- R.xy$V.R.xy^0.5*A)
      
      #je ha HBF
      R.xy <- r.variance.fun(cbind(merkmal.t$hbf,merkmal.t$N.MM),n)
      (N.MM.ha <- R.xy$R.xy)
      (se.N.MM.ha <- R.xy$V.R.xy^0.5)
      
      #Vorrat
      R.xy <- r.variance.fun(cbind(merkmal.t$m,merkmal.t$V.MM),n)
      (T.V.MM <- R.xy$R.xy*A)
      (se.T.V.MM <- R.xy$V.R.xy^0.5*A)
      
      #je ha HBF
      R.xy <- r.variance.fun(cbind(merkmal.t$hbf,merkmal.t$V.MM),n)
      (V.MM.ha <- R.xy$R.xy)
      (se.V.MM.ha <- R.xy$V.R.xy^0.5)
      
      #St\u00fcckvolumen
      R.xy <- r.variance.fun(cbind(merkmal.t$N.MM,merkmal.t$V.MM),n)
      (V.MM.Stck <- R.xy$R.xy)
      (se.V.MM.Stck <- R.xy$V.R.xy^0.5)
      
      #oberird. Biomasse    (in t umgerechnet)
      R.xy <- r.variance.fun(cbind(merkmal.t$m,merkmal.t$oiB.MM),n)
      (T.oiB.MM <- R.xy$R.xy*A/1000)
      (se.T.oiB.MM <- R.xy$V.R.xy^0.5*A/1000)
      
      #je ha HBF
      R.xy <- r.variance.fun(cbind(merkmal.t$hbf,merkmal.t$oiB.MM),n)
      (oiB.MM.ha <- R.xy$R.xy/1000)
      (se.oiB.MM.ha <- R.xy$V.R.xy^0.5/1000)
      
      #Anteil des Merkmals an der Gesamtzahl
      R.xy <- r.variance.fun(cbind(merkmal.t$N.ges,merkmal.t$N.MM),n)
      (Anteil.MM.N <- R.xy$R.xy)
      (se.Anteil.MM.N <- R.xy$V.R.xy^0.5)
      
      #Anteil des Merkmal-Vorrats am Gesamtvorrat
      R.xy <- r.variance.fun(cbind(merkmal.t$V.ges,merkmal.t$V.MM),n)
      (Anteil.MM.V <- R.xy$R.xy)
      (se.Anteil.MM.V <- R.xy$V.R.xy^0.5)
      
      
      #Ablegen in Tabelle
      attr.bagr.akl.tab[,1,i,j] <- 
        c(T.N.MM,N.MM.ha,T.V.MM,V.MM.ha,V.MM.Stck,T.oiB.MM,oiB.MM.ha,
          Anteil.MM.N*100,Anteil.MM.V*100)
      attr.bagr.akl.tab[,2,i,j] <- c(se.T.N.MM,se.N.MM.ha,se.T.V.MM,se.V.MM.ha,se.V.MM.Stck,
                               se.T.oiB.MM,se.oiB.MM.ha,se.Anteil.MM.N*100,se.Anteil.MM.V*100)
    } 
  }
  
  #(4) Ergebnisliste
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
              Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))
  akl.lab <- akl.lab.fun(A.klass,A.k)
  return( list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
               BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
               Merkmale=merkmale,BAGR=ba.grupp$bagr.lab, AKL = akl.lab,
               Kennwerte=Kennwerte,
               Kennwert_Tabelle_BAGR=attr.bagr.akl.tab))
  
}  #Ende <stamm.merkmale.bagr.akl.fun>
