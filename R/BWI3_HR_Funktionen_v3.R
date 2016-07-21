#' BWI 3 Hochrechnung
#'
#' Funktionen fuer die BWI-Auswertung, bestehend aus Berechnungen zur 
#' Zustandserhebung, des Zuwachses, des ausgeschiedener Vorrat, Verjuengung, 
#' Verbiss und weitere. 
#'
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}, Dominik Cullmann
#'  \email{dominik.cullmann@@forst.bwl.de}, Franziska Berg
#' @section Version: Version 3 vom 19.08.2014 basierend auf Version 2 vom 
#'  15.04.2014
#' @section Aktualisierungen: 
#'  22.07.2015 (Berg) Aenderungen der Kommentare zu Roxygen Format \cr
#'  09.04.2015 (Cullmann) Ergaenzung \code{tryCatch} in 
#'    \code{\link{verjg.kl4.bagrupp.fun}} \cr
#'  09.04.2015 Einfuegung von \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2e}}
#'   \cr
#'  09.02.2015 Funtion \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}} bzw. 
#'    \code{\link{iVB.bilanz.bagr.akl.dkl.fun.2}} korrigiert entsprechend
#'    Fassung \cr
#'  09.02.2015 \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2g}} bzw. 
#'		\code{\link{iVB.bilanz.bagr.akl.dkl.fun.2g}} aus 
#'		"BWI3/Programme/HR/BWI3_HR_Funktionen_v3_xxx.r"
#'		betreffend (1) Aggregation auf alle Baumarten durch eigene sepearte
#'		Aggregation auf BAGR "AlleBA" sowie (2) bei Biomasse-Zuwachs Beschraenkung
#'		auf Derbholz-Kollektiv \cr
#'	09.02.2015 Variante \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2g}} bzw.
#'		\code{\link{iVB.bilanz.bagr.akl.dkl.fun.2g}} eingefuegt, berechnet 
#'		zusaetzlich Grundzuwachs \cr
#'	07.02.2015 Korrektur in \code{\link{dkl.lab.fun}} sowie in diversen Funktionen:
#'		\code{D.k <- length(dkl.lab[!is.na(dkl.lab)])} statt 
#'		\code{D.k <- length(dkl.lab)} \cr
#'	23.01.2015 Korrektur in \code{\link{stratum.fun}} exaktes \code{matching} mit
#'		\code{which} \cr
#'	15.12.2014 Erweiterung um "alle BA" in 
#'    \code{\link{VB.A.bagrupp.akl.dkl.stratum.fun.3}}
#'		und \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}} \cr
#'	14.12.2014 neu Funktion \code{\link{stamm.merkmale.bagr.fun}} \cr
#'	11.12.2014 Korrektur in \code{\link{fvbn.stratum.fun.1}} \cr
#'	03.12.2014 neue Funktionen: \code{\link{fvbn.kreis.fun.1}}, 
#'		\code{\link{fvbn.stratum.fun.1}}, \code{\link{fvbn.stratum.fun.2}} \cr
#'	02.12.2014 neue Version \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2d}}
#'		enthaelt auch die Summenwerte fuer alle Baumarten, wenn nach 
#'		Baumartengruppen differenziert ausgewertet wird. \cr
#'	01.12.2014 Verbesserung \code{\link{stratum.fun}}: "leere Menge" \cr
#'	19.11.2014 neue Funktion \code{\link{fl.proz.stratum.fun}} \cr
#'	01.11.2014 Erweiterung um Funktion \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2c}}
#'		mit Derbholzstammzahlen im Hauptbestand \cr
#'	27.10.2014 Funktion \code{\link{ntns.stratum.fun.2}} berechnet NTNS fuer die 
#'		Schicht "kl 4m" oder "gr 4m" \cr
#'	21.10.2014 \code{\link{verjg.tab.fun}} eingefuegt \cr
#'	14.10.2014 \code{\link{fl.stratum.fun}} vom 30.07. am eingefuegt \cr
#'	11.10.2014 \code{\link{biotop.baeume.fun}} \cr
#'	11.10.2014 \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2b}} (Erweiterung 
#'		um BA-Proz. mit Fehler) \cr
#'	10.10.2014 Funktion zur Auswertung der Biotop-Baeume 
#'		\code{\link{biotop.baeume.fun}} \cr
#'	05.10.2014 Fehler bei Bestimmung der Anzahl Altersklassen <A.k>  korriegiert.
#'		Abfangen des Falls, dass \code{<A.k> = 0: if (A.k == 0) A.k <- 1} \cr
#'	24.08.2014 Korrektur bei den Funktionen fuer Periodenauswertungen:
#'		\code{\link{VB.A.bagrupp.akl.dkl.stratum.fun.2}}, 
#'		\code{\link{VB.A.bagrupp.akl.dkl.stratum.fun.3}} und
#'		\code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}} Wurde festgelegt, 
#'		dass die Eckenmerkmale der aktuellen Inventur (=BWI 3) fuer die 
#'		Stratifikation (Argument \code{auswahl}) gilt, z.B. die Eigentumsklasse
#'		\cr
#'	19.08.2014 Zuwachs-Abgang fuer die Periode 1987 bis 2002 (BWI 1 zu 2)
#'		\code{\link{ntns.stratum.fun}} zur Naturnaeheauswertung \cr
#'	18.08.2014 Ergaenzt um \code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.bwi12}}
#'		\cr
#'	13.08.2014 ergaenzungen von Funktionen zur Verjuengungsauswertung:
#'		\code{\link{verjg.bagr.fun}} mit Standard BWI-Baumartengruppen,
#'		\code{\link{verjg.bagrupp.fun}} mit frei definierbaren Baumartengruppen,
#'		\code{\link{verjg.kl4.bagr.fun}} Verjuengung aus Bestockungsansprache 
#'		<= 4m Hoehe nach BWI-Baumartengruppen und Verjuengungsart,
#'		\code{\link{verjg.kl4.bagrupp.fun}} mit frei definierbaren Baumartengruppen
#'		\cr
#'	12.08.2014 Ergaenzung um Funktionen zur Verbissauswertung: 
#'		\code{\link{verbiss.bagr.fun}} mit Szandard BWI-Baumartengruppen, 
#'		\code{\link{verbiss.bagrupp.fun}} mit frei definierbaren Baumartengruppen
#'		 \cr
#'	30.07.2014 Ergaenzung fehlender Funktion 
#'		\code{\link{iVB.bilanz.bagr.akl.dkl.fun}} \cr
#'	30.07.2014 Kleinere Korrekturen in 
#'		\code{\link{iVB.ew.bagrupp.akl.dkl.stratum.fun.2}}
#' @section TODO:
#'	27.10.2014 Attributname "BW" fuer Bannwald in "Bannw" aendern wegen 
#'		Verwechslung in \code{\link{stratum.fun}} mit "WGNr_BW" Pruefen, ob 
#'		Korrektur moeglich
#'@name A header for
NULL

#-------------------------------------------------------------------------------
#(1) FUNKTIONEN zur ZUSTANDSHOCHRECHNUNG
#-------------------------------------------------------------------------------
#' Zustandshochrechnung (version 2a)
#' 
#' Funktion wertet nach frei definierbaren BA-Gruppen, Alters- und Durchmesser-
#' Klassen im Stratum aus. Die berechneten Zielgroessen sind Flaeche, Vorrat, 
#' Biomasse und Stammzahl (N). Variante mit Varianzschaetzung ueber 
#' Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.03.2014
#' @section Aktualisierungen: 
#'  19.04.2014 bei Ha-Bezug fuer Hauptbestands(HB)-Vorrat wird Luecken-Korrektur 
#'    rueckgaengig gemacht, um Bezug nur zur HB-Flaeche zu erzielen 
#'    (\code{T.bl}, \code{T.ibl}). 
#' @section Hinweis: Verallgemeinerte Version fuer die Auswertung 
#'  unterschiedlicher Inventurzeit-punkte, also BWI 1, BWI 2, BWI 3. \cr
#'  Die Funktion \code{\link{stratum.fun}} benoetigt desweiteren die Tabelle 
#'  \code{bacode}. \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @param baeume Datentabelle mit Bauminformationen. Die Tabelle muss mindestens 
#'  die Attribute TNr, ENr, BA, Alt., BHD., VolV., oiB., NHa., StFl. enthalten 
#'  (. steht fuer 1 oder 2, je nachdem welcher Zustand ausgewertet werden soll), 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in 
#'  \code{baeume} uebergeben wird.
#' @param ecken Eckenmerkmale.
#' @param trakte Traktmerkmale.
#' @param A Gesamtflaeche in ha des Inventurgebiets zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @param inv Inventur 1:= Vorinventur, 2:= Folgeinventur, wenn eine 
#'  \code{baeume}-Tablle mit Attributen aus zwei Aufnahmezeitpunkten uebergeben 
#'  wird. Wenn dies der Fall ist, dann entsprechen die jeweils fuer die 
#'  auszuwertende Inventur relevanten Attribute der Konvention: Aktuelle 
#'  ("Folge") Inventur hat im Attributnamen eine 2 am Ende, also "Alt2", "BHD2", 
#'  "D032", "H2", "HSt2", "VolV2", "VolE2", "oiB2", "StFl2", "NHa2", die 
#'  Vorinventur eine 1; d.h. im Falle der BWI 3 haben die Attributbezeichner die 
#'  2 im Namen, die Daten der Vorinventur BWI 2 die 1. Um mit denselben 
#'  Algorithmen fuer beide Zustaende arbeiten zu koennen, werden fuer den 
#'  jeweiligen Auswertungsfall \code{inv} (Vorinventur = 1 bzw. Folge-(aktuelle) 
#'  Inventur =2 die Attibutnamen "neutralisiert", also die Kennung 1 oder 2 im 
#'  Attributnamen entfernt.
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
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und Version.baeume.b, \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), \strong{Attribute1} 
#'  (Vektor mit berechneten absoluten Attributnamen), \strong{Attribute2} 
#'  (vektor mit berechneten pro ha Attributname), \strong{Größen} (Vektor mit 
#'  berechneten Groessen), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.FVBN.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer Gesamtwerte jeweils 
#'  fuer 9 Baumartengruppen, Alters- und Durchmesserklassen), 
#'  \strong{FVBN.ha.Bagr.Akl.Dkl} (Array mit berechneten Groessen (Wert und 
#'  Standardfehler) fuer hektabezogene Kenngroessen), \strong{nT.bagr.Akl.Dkl} 
#'  (Anzahl Traktecken (?)).
FVBN.bagrupp.akl.dkl.stratum.fun.2a <-
          function(baeume,ecken,trakte,A,inv,BA.grupp,A.klass,D.klass,auswahl){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))
  #"Neutralisierung" der benötigten Attributnamen
  names(baeume) <- sub(inv,names(baeume),replacement="")
  #Attribute und Untermenge des Stratums aus <baeume> auswählen
  baeume.s <- merge(subset(baeume,select=c(tnr,enr,ba,alt,bhd,volv,oib,nha,stfl)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"),all.y=T)

  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  #BA-Gruppe dazu spielen
  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  baeume.s[is.na(baeume.s)] <- 0
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]

  #------------------
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])

  #---------------------
  #Flächen
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==999,baeume.s$stfl/10000,0),
                  by=list(baeume.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==998,baeume.s$stfl,0),
                  by=list(baeume.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl

  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  #---------------------------------------
  #Alter
  A.max <- 999
  #----- kä/15.02.
  baeume.s$akl <- as.integer(cut(baeume.s$alt,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T))
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Maximale Anzahl Altersklassen aus A-Kl-Parametern <A.klass>
  if(A.klass[[1]]>A.klass[[2]])
  {
    max.A.k <- A.klass[[1]]/A.klass[[2]] + 1
    if (A.k < max.A.k) {A.k <- max.A.k }
  }
  #----- kä/15.02.

  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  baeume.s$dkl <- cut(baeume.s$bhd, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])
  #----- kä/15.02.
  #Maximale Anzahl D-klassen aus D-Kl-Parametern <D.klass>
  if(D.klass[[2]]>D.klass[[3]])
  {
    max.D.k <- (D.klass[[2]]-D.klass[[1]])/D.klass[[2]] + 2
    if (D.k < max.D.k) {D.k <- max.D.k }
  }
  #----- kä/15.02.

  #Array für Ergebnisse (Totals und SE jeweils nach BAGr, AKl, DKl)
  #Es gibt 6 Zielgrößen <Y>: BAF [ha], V [m³Dh mR], VHb (nur Hauptbestand)
  #[m³Dh mR], B (oberird. Biomasse) [t], N (gesamtzahl), NDh (Gesamtzahl
  #Derbholzbäume)
  #Für diese 6 Größen werden jeweils der Gesamtwert ("Total") und der
  #Stichprobenfehler (SE) berechnet, und zwar jeweils für die 9 Baumartengruppen
  #sowie A.k Alters- und D.k Durchmesserklassen
  Y.bagr.akl.dkl    <- array(dim=c(6,2,n.bagr,A.k,D.k))
  nT.bagr.akl.dkl   <- array(dim=c(n.bagr,A.k,D.k))
  #Hektarbezogene Kennwerte. 5 Zielgrößen:  Vha, VHbha, Bha, Nha, NDhha
  Yha.bagr.akl.dkl  <- array(dim=c(5,2,n.bagr,A.k,D.k))
  #----------------

  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        baeume.ba <- subset(baeume.s,
                      bagr==bagr.list[i]&akl==akl.lab[j]&dkl==dkl.lab[k],
                      select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
        if (length(baeume.ba[,1])== 0)
        {
           Y.bagr.akl.dkl[1:6,1,i,j,k]    <- rep(0,6)  #Zielgröße Total
           Y.bagr.akl.dkl[1:6,2,i,j,k]    <- rep(0,6)  #Stichprobenfehler (SE)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           nT.bagr.akl.dkl[i,j,k]    <- 0  #n PSU (Trakte)
        }else
        {
          #Nach Trakt aggregieren
          #BAF der BA-Gruppe [ha] als "x"
          xy <- aggregate(baeume.ba$stfl,by=list(baeume.ba$tnr),sum)
          names(xy) <- c("tnr","x")
          xy$x <- xy$x/10000*lk  #Umrechnung in ha  und Lückenkorrektur
          #Derbholz-Vorrat [m³ mR] als "v"
          xy <- cbind(xy,aggregate(baeume.ba$volv*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[3] <- "v"
          #Derbholz-Vorrat [m³ mR] im Hauptbestand als "v.hb"
          xy <- cbind(xy,
            aggregate(baeume.ba$volv*baeume.ba$nha*ifelse(baeume.ba$stfl>0,1,0),
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[4] <- "v.hb"
          #oberird. Biomasse [t] als "b"
          xy <- cbind(xy,aggregate(baeume.ba$oib*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
          names(xy)[5] <- "b"
          #Anzahl Bäume als "n"
          xy <- cbind(xy,aggregate(baeume.ba$nha,by=list(baeume.ba$tnr),sum)$x)
          names(xy)[6] <- "n"
          #Anzahl nur Derbholz-Bäume (bhd>=7)  als "ndh"
          #sofern in der Klassifizierung solche vorkommen!
          x.ndh <- try(aggregate(baeume.ba$nha*ifelse(baeume.ba$bhd>=7,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh)
          names(xy)[7] <- "ndh"

          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,subset(trakte,select=c(tnr,m),by=c(tnr)))
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---
          xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
          #for(ii in 3:8) {xy[is.na(xy[,ii]),ii] <- 0}
          xy[is.na(xy)] <- 0

          for (l in 1:6)
          {
            #Zielgrößen Y {BAF,V,VHb,B,N,NDh)
            R.list <- r.variance.fun(cbind(xy$m,xy[,(2+l)]),nT)
            #Total
            Y.bagr.akl.dkl[l,1,i,j,k] <- R.list$R.xy*A
            #sum(xy[,(1+l)])/sum(x)*A *ifelse(l==1,lk,1)
                                      #Lückenkorrektur bei BAF bereits erfolgt
            #Zugehöriger Stichprobenfehler
            Y.bagr.akl.dkl[l,2,i,j,k] <- sqrt(R.list$V.R.xy)*A

            #Ratio-Schätzer (Vha, VHbha, Bha, Nha, NDhha) 5!
            if (l > 1)    #Hinweis: l+1 = Spalte der Zielgröße in xy; Spalte 2
                          #ist BA-Fläche (HB) als Bezugsfläche (= xy$x)
            {
              #Bei Ha-Vorrat Hauptbestand muss Flächenbezug die Fläche OHNE
              #Lückenkorrektur sein, daher wird durch <lk>idiert
              #TODO prüfen kä/17.04.2014
              lk.c <- ifelse(l==3,lk,1) #lk.c macht bei HB-Vorrat Lückenkorrektur
              #rückgängig
              R.list <- r.variance.fun(cbind(xy$x/lk.c,xy[,(2+l)]),nT)
              l.ha <- l-1
              Yha.bagr.akl.dkl[l.ha,1,i,j,k] <- R.list$R.xy
              #              Y.bagr.akl.dkl[l,1,i,j,k]/Y.bagr.akl.dkl[1,1,i,j,k]
              Yha.bagr.akl.dkl[l.ha,2,i,j,k] <- sqrt(R.list$V.R.xy)
            }#End if
          }#End for l (Zielgrößen)
        }#End if ... else
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
  }#End for i (BAGR)
  #-----------------------
  #Tabelle für BA-Gruppen

  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k) #kä/16.07.14
  #-------------------------
  #DKL-Labels  kä/16.07.2014
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Attribute1=c("BAF", "V_DhmR","V_DhmR_HB", "oiB", "N", "N_Dh"),
              Attribute2=c("V_DhmR/ha","V_DhmR_HB/ha","oiB/ha","N/ha","N_Dh/ha"),
              Größen = c("Wert","Standardfehler"),
              BAGR=bagr.list,AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.FVBN.Bagr.Akl.Dkl=Y.bagr.akl.dkl,
              FVBN.ha.Bagr.Akl.Dkl=Yha.bagr.akl.dkl,
              nT.Bagr.Akl.Dkl=nT.bagr.akl.dkl))
}#End <FVBN.bagrupp.akl.dkl.stratum.fun.2a>

#-------------------------------------------------------------------------------
#' Zustandshochrechnung (version 2b)
#' 
#' Funktion wertet nach frei definierbaren BA-Gruppen, Alters- und Durchmesser-
#' Klassen im Stratum aus. Die berechneten Zielgroessen sind Flaeche, Vorrat, 
#' Biomasse und Stammzahl (N). Variante mit Varianzschaetzung ueber 
#' Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.03.2014
#' @section Aktualisierungen: 
#'  11.10.2014 Erweiterung um Baumartenanteile mit Fehler \cr
#'  19.04.2014 bei Ha-Bezug fuer Hauptbestands(HB)-Vorrat wird Luecken-Korrektur 
#'    rueckgaengig gemacht, um Bezug nur zur HB-Flaeche zu erzielen 
#'    (\code{T.bl}, \code{T.ibl}). 
#' @section Hinweis: Verallgemeinerte Version fuer die Auswertung 
#'  unterschiedlicher Inventurzeit-punkte, also BWI 1, BWI 2, BWI 3. \cr
#'  Die Funktion \code{\link{stratum.fun}} benoetigt desweiteren die Tabelle 
#'  \code{bacode}. \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @param baeume Datentabelle mit Bauminformationen. Die Tabelle muss mindestens 
#'  die Attribute TNr, ENr, BA, Alt., BHD., VolV., oiB., NHa., StFl. enthalten 
#'  (. steht fuer 1 oder 2, je nachdem welcher Zustand ausgewertet werden soll), 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in 
#'  \code{baeume} uebergeben wird.
#' @param ecken Eckenmerkmale.
#' @param trakte Traktmerkmale.
#' @param A Gesamtflaeche in ha des Inventurgebiets zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @param inv Inventur 1:= Vorinventur, 2:= Folgeinventur, wenn eine 
#'  \code{baeume}-Tablle mit Attributen aus zwei Aufnahmezeitpunkten uebergeben 
#'  wird. Wenn dies der Fall ist, dann entsprechen die jeweils fuer die 
#'  auszuwertende Inventur relevanten Attribute der Konvention: Aktuelle 
#'  ("Folge") Inventur hat im Attributnamen eine 2 am Ende, also "Alt2", "BHD2", 
#'  "D032", "H2", "HSt2", "VolV2", "VolE2", "oiB2", "StFl2", "NHa2", die 
#'  Vorinventur eine 1; d.h. im Falle der BWI 3 haben die Attributbezeichner die 
#'  2 im Namen, die Daten der Vorinventur BWI 2 die 1. Um mit denselben 
#'  Algorithmen fuer beide Zustaende arbeiten zu koennen, werden fuer den 
#'  jeweiligen Auswertungsfall \code{inv} (Vorinventur = 1 bzw. Folge-(aktuelle) 
#'  Inventur =2 die Attibutnamen "neutralisiert", also die Kennung 1 oder 2 im 
#'  Attributnamen entfernt.
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
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und Version.baeume.b, \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), \strong{Attribute1} 
#'  (Vektor mit berechneten absoluten Attributnamen), \strong{Attribute2} 
#'  (vektor mit berechneten pro ha Attributname), \strong{Größen} (Vektor mit 
#'  berechneten Groessen), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.FVBN.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer Gesamtwerte jeweils 
#'  fuer 9 Baumartengruppen, Alters- und Durchmesserklassen), 
#'  \strong{FVBN.ha.Bagr.Akl.Dkl} (Array mit berechneten Groessen (Wert und 
#'  Standardfehler) fuer hektabezogene Kenngroessen), \strong{nT.bagr.Akl.Dkl} 
#'  (Anzahl Traktecken (?)).
FVBN.bagrupp.akl.dkl.stratum.fun.2b <-
          function(baeume,ecken,trakte,A,inv,BA.grupp,A.klass,D.klass,auswahl){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))
  #"Neutralisierung" der benötigten Attributnamen
  names(baeume) <- sub(inv,names(baeume),replacement="")
  #Attribute und Untermenge des Stratums aus <baeume> auswählen
  baeume.s <- merge(subset(baeume,select=c(tnr,enr,ba,alt,bhd,volv,oib,nha,stfl)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"),all.y=T)

  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  #BA-Gruppe dazu spielen
  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  baeume.s[is.na(baeume.s)] <- 0
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]

  #------------------
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])

  #---------------------
  #Flächen
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==999,baeume.s$stfl/10000,0),
                  by=list(baeume.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==998,baeume.s$stfl,0),
                  by=list(baeume.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl
  #kä/11.10.2014  wegen BA-Anteilen muss hbf je Trakt im Stratum erhalten bleiben
  xy.s <- xy
  #----
  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  #---------------------------------------
  #Alter
  A.max <- 999
  #----- kä/15.02.
  baeume.s$akl <- as.integer(cut(baeume.s$alt,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T))
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Maximale Anzahl Altersklassen aus A-Kl-Parametern <A.klass>
  if(A.klass[[1]]>A.klass[[2]])
  {
    max.A.k <- A.klass[[1]]/A.klass[[2]] + 1
    if (A.k < max.A.k) {A.k <- max.A.k }
  }
  #----- kä/15.02.

  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  baeume.s$dkl <- cut(baeume.s$bhd, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])
  #----- kä/15.02.
  #Maximale Anzahl D-klassen aus D-Kl-Parametern <D.klass>
  if(D.klass[[2]]>D.klass[[3]])
  {
    max.D.k <- (D.klass[[2]]-D.klass[[1]])/D.klass[[2]] + 2
    if (D.k < max.D.k) {D.k <- max.D.k }
  }
  #----- kä/15.02.

  #Array für Ergebnisse (Totals und SE jeweils nach BAGr, AKl, DKl)
  #Es gibt 6 Zielgrößen <Y>: BAF [ha], V [m³Dh mR], VHb (nur Hauptbestand)
  #[m³Dh mR], B (oberird. Biomasse) [t], N (gesamtzahl), NDh (Gesamtzahl
  #Derbholzbäume)
  #Für diese 6 Größen werden jeweils der Gesamtwert ("Total") und der
  #Stichprobenfehler (SE) berechnet, und zwar jeweils für die 9 Baumartengruppen
  #sowie A.k Alters- und D.k Durchmesserklassen
  Y.bagr.akl.dkl    <- array(dim=c(6,2,n.bagr,A.k,D.k))
  nT.bagr.akl.dkl   <- array(dim=c(n.bagr,A.k,D.k))
  #Hektarbezogene Kennwerte. urspr. 5 Zielgrößen: Vha, VHbha, Bha, Nha, NDhha
  #erweitert um BAGr-Anteil:  BAFAnt, Vha, VHbha, Bha, Nha, NDhha
  #Daher 1. Dimension 6 statt 5
  Yha.bagr.akl.dkl  <- array(dim=c(6,2,n.bagr,A.k,D.k))
  #kä/26.09.14: Baumartenanteile  (Prozent)
  ant.bagr.akl.dkl  <- array(dim=c(2,A.k,D.k))
  #---
  #----------------

  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        baeume.ba <- subset(baeume.s,
                      bagr==bagr.list[i]&akl==akl.lab[j]&dkl==dkl.lab[k],
                      select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
        if (length(baeume.ba[,1])== 0)
        {
           Y.bagr.akl.dkl[1:6,1,i,j,k]    <- rep(0,6)  #Zielgröße Total
           Y.bagr.akl.dkl[1:6,2,i,j,k]    <- rep(0,6)  #Stichprobenfehler (SE)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           nT.bagr.akl.dkl[i,j,k]    <- 0  #n PSU (Trakte)
        }else
        {
          #Nach Trakt aggregieren
          #BAF der BA-Gruppe [ha] als "x"
          xy <- aggregate(baeume.ba$stfl,by=list(baeume.ba$tnr),sum)
          names(xy) <- c("tnr","x")
          xy$x <- xy$x/10000*lk  #Umrechnung in ha  und Lückenkorrektur
          #Derbholz-Vorrat [m³ mR] als "v"
          xy <- cbind(xy,aggregate(baeume.ba$volv*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[3] <- "v"
          #Derbholz-Vorrat [m³ mR] im Hauptbestand als "v.hb"
          xy <- cbind(xy,
            aggregate(baeume.ba$volv*baeume.ba$nha*ifelse(baeume.ba$stfl>0,1,0),
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[4] <- "v.hb"
          #oberird. Biomasse [t] als "b"
          xy <- cbind(xy,aggregate(baeume.ba$oib*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
          names(xy)[5] <- "b"
          #Anzahl Bäume als "n"
          xy <- cbind(xy,aggregate(baeume.ba$nha,by=list(baeume.ba$tnr),sum)$x)
          names(xy)[6] <- "n"
          #Anzahl nur Derbholz-Bäume (bhd>=7)  als "ndh"
          #sofern in der Klassifizierung solche vorkommen!
          x.ndh <- try(aggregate(baeume.ba$nha*ifelse(baeume.ba$bhd>=7,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh)
          names(xy)[7] <- "ndh"

          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,subset(trakte,select=c(tnr,m),by=c(tnr)))
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---  Ergänzug 11.10. <m_bhb>    Hinweis: Offset für Indizierung ange-
          #passt!!
          #des Weiteren merge mit <xy.s> wegen <hbf> (begehbare HBF) für 
          #Berechnung der BA-Anteile
          xy <- merge(subset(xy.s,select=c(tnr,m,hbf)),xy,by=c("tnr"),all.x=T)
          #xy <- merge(subset(trakte,select=c(tnr,m,m_bhb)),xy,by=c("tnr"),all.x=T)
          xy[is.na(xy)] <- 0

          for (l in 1:6)
          {
            #Zielgrößen Y {BAF,V,VHb,B,N,NDh)   (Offset von (2+l) auf (3+l)
            #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
            R.list <- r.variance.fun(cbind(xy$m,xy[,(3+l)]),nT)
            #Total
            Y.bagr.akl.dkl[l,1,i,j,k] <- R.list$R.xy*A
            #sum(xy[,(1+l)])/sum(x)*A *ifelse(l==1,lk,1)
                                      #Lückenkorrektur bei BAF bereits erfolgt
            #Zugehöriger Stichprobenfehler
            Y.bagr.akl.dkl[l,2,i,j,k] <- sqrt(R.list$V.R.xy)*A

            #Ratio-Schätzer:
            #neu:11.10.14: BAFAnt, Vha, VHbha, Bha, Nha, NDhha) = 6!
            #if (l > 1)    #Hinweis: l+1 = Spalte der Zielgröße in xy; Spalte 2
                          #ist BA-Fläche (HB) als Bezugsfläche (= xy$x)
            if (l == 1) #BAF-Anteil (an HBF)
            {
              R.list <- r.variance.fun(cbind(xy$hbf,xy[,(3+l)]),nT)
              #in Prozent
              R.list$R.xy <- R.list$R.xy*100
              R.list$V.R.xy <- R.list$V.R.xy*10000
            } else
            {  
              #Bei Ha-Vorrat Hauptbestand (l==3) muss Flächenbezug die Fläche 
              #OHNE Lückenkorrektur sein, daher wird durch <lk>idiert
              #TODO prüfen kä/17.04.2014
              lk.c <- ifelse(l==3,lk,1) #lk.c macht bei HB-Vorrat Lückenkorrektur
              #rückgängig Hinweis: Offset von (2+l) auf (3+l)
              #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
              R.list <- r.variance.fun(cbind(xy$x/lk.c,xy[,(3+l)]),nT)
              #l.ha <- l-1   ka/11.10.14
            }#End if ... else
            l.ha <- l
            Yha.bagr.akl.dkl[l.ha,1,i,j,k] <- R.list$R.xy
              #              Y.bagr.akl.dkl[l,1,i,j,k]/Y.bagr.akl.dkl[1,1,i,j,k]
            Yha.bagr.akl.dkl[l.ha,2,i,j,k] <- sqrt(R.list$V.R.xy)
            
          }#End for l (Zielgrößen)
        }#End if ... else 
        #---
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
    #---
  }#End for i (BAGR)
  #-----------------------
  
  
  #---
  #Tabelle für BA-Gruppen

  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k) #kä/16.07.14
  #-------------------------
  #DKL-Labels  kä/16.07.2014
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Attribute1=c("BAF", "V_DhmR","V_DhmR_HB", "oiB", "N", "N_Dh"),
              #Attribute2 erweitert um "BA_Proz" (=BAFAnt)  kä/11.10.14
              Attribute2=c("BA_Proz","V_DhmR/ha","V_DhmR_HB/ha","oiB/ha","N/ha",
                            "N_Dh/ha"),
              Größen = c("Wert","Standardfehler"),
              BAGR=bagr.list,AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.FVBN.Bagr.Akl.Dkl=Y.bagr.akl.dkl,
              FVBN.ha.Bagr.Akl.Dkl=Yha.bagr.akl.dkl,
              nT.Bagr.Akl.Dkl=nT.bagr.akl.dkl))
}#End <FVBN.bagrupp.akl.dkl.stratum.fun.2b>

#-------------------------------------------------------------------------------
#' Zustandshochrechnung (version 2c)
#' 
#' Funktion wertet nach frei definierbaren BA-Gruppen, Alters- und Durchmesser-
#' Klassen im Stratum aus. Die berechneten Zielgroessen sind Flaeche, Vorrat, 
#' Biomasse und Stammzahl (N). Variante mit Varianzschaetzung ueber 
#' Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.03.2014
#' @section Aktualisierungen: 
#'  01.11.2014 Neue Version mit Stammzahlen im Hauptbestand \cr
#'  11.10.2014 Erweiterung um Baumartenanteile mit Fehler \cr
#'  19.04.2014 bei Ha-Bezug fuer Hauptbestands(HB)-Vorrat wird Luecken-Korrektur 
#'    rueckgaengig gemacht, um Bezug nur zur HB-Flaeche zu erzielen 
#'    (\code{T.bl}, \code{T.ibl}). 
#' @section Hinweis: Verallgemeinerte Version fuer die Auswertung 
#'  unterschiedlicher Inventurzeit-punkte, also BWI 1, BWI 2, BWI 3. \cr
#'  Die Funktion \code{\link{stratum.fun}} benoetigt desweiteren die Tabelle 
#'  \code{bacode}. \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @param baeume Datentabelle mit Bauminformationen. Die Tabelle muss mindestens 
#'  die Attribute TNr, ENr, BA, Alt., BHD., VolV., oiB., NHa., StFl. enthalten 
#'  (. steht fuer 1 oder 2, je nachdem welcher Zustand ausgewertet werden soll), 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in 
#'  \code{baeume} uebergeben wird.
#' @param ecken Eckenmerkmale.
#' @param trakte Traktmerkmale.
#' @param A Gesamtflaeche in ha des Inventurgebiets zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @param inv Inventur 1:= Vorinventur, 2:= Folgeinventur, wenn eine 
#'  \code{baeume}-Tablle mit Attributen aus zwei Aufnahmezeitpunkten uebergeben 
#'  wird. Wenn dies der Fall ist, dann entsprechen die jeweils fuer die 
#'  auszuwertende Inventur relevanten Attribute der Konvention: Aktuelle 
#'  ("Folge") Inventur hat im Attributnamen eine 2 am Ende, also "Alt2", "BHD2", 
#'  "D032", "H2", "HSt2", "VolV2", "VolE2", "oiB2", "StFl2", "NHa2", die 
#'  Vorinventur eine 1; d.h. im Falle der BWI 3 haben die Attributbezeichner die 
#'  2 im Namen, die Daten der Vorinventur BWI 2 die 1. Um mit denselben 
#'  Algorithmen fuer beide Zustaende arbeiten zu koennen, werden fuer den 
#'  jeweiligen Auswertungsfall \code{inv} (Vorinventur = 1 bzw. Folge-(aktuelle) 
#'  Inventur =2 die Attibutnamen "neutralisiert", also die Kennung 1 oder 2 im 
#'  Attributnamen entfernt.
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
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und Version.baeume.b, \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), \strong{Attribute1} 
#'  (Vektor mit berechneten absoluten Attributnamen), \strong{Attribute2} 
#'  (vektor mit berechneten pro ha Attributname), \strong{Größen} (Vektor mit 
#'  berechneten Groessen), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.FVBN.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer Gesamtwerte jeweils 
#'  fuer 9 Baumartengruppen, Alters- und Durchmesserklassen), 
#'  \strong{FVBN.ha.Bagr.Akl.Dkl} (Array mit berechneten Groessen (Wert und 
#'  Standardfehler) fuer hektabezogene Kenngroessen), \strong{nT.bagr.Akl.Dkl} 
#'  (Anzahl Traktecken (?)).
FVBN.bagrupp.akl.dkl.stratum.fun.2c <-
          function(baeume,ecken,trakte,A,inv,BA.grupp,A.klass,D.klass,auswahl){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))
  #"Neutralisierung" der benötigten Attributnamen
  names(baeume) <- sub(inv,names(baeume),replacement="")
  #Attribute und Untermenge des Stratums aus <baeume> auswählen
  baeume.s <- merge(subset(baeume,select=c(tnr,enr,ba,alt,bhd,volv,oib,nha,stfl)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"),all.y=T)

  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  #BA-Gruppe dazu spielen
  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  baeume.s[is.na(baeume.s)] <- 0
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]

  #------------------
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])

  #---------------------
  #Flächen
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==999,baeume.s$stfl/10000,0),
                  by=list(baeume.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==998,baeume.s$stfl,0),
                  by=list(baeume.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl
  #kä/11.10.2014  wegen BA-Anteilen muss hbf je Trakt im Stratum erhalten bleiben
  xy.s <- xy
  #----
  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  #---------------------------------------
  #Alter
  A.max <- 999
  #----- kä/15.02.
  baeume.s$akl <- as.integer(cut(baeume.s$alt,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T))
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Maximale Anzahl Altersklassen aus A-Kl-Parametern <A.klass>
  if(A.klass[[1]]>A.klass[[2]])
  {
    max.A.k <- A.klass[[1]]/A.klass[[2]] + 1
    if (A.k < max.A.k) {A.k <- max.A.k }
  }
  #----- kä/15.02.

  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  baeume.s$dkl <- cut(baeume.s$bhd, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])
  #----- kä/15.02.
  #Maximale Anzahl D-klassen aus D-Kl-Parametern <D.klass>
  if(D.klass[[2]]>D.klass[[3]])
  {
    max.D.k <- (D.klass[[2]]-D.klass[[1]])/D.klass[[2]] + 2
    if (D.k < max.D.k) {D.k <- max.D.k }
  }
  #----- kä/15.02.

  #Array für Ergebnisse (Totals und SE jeweils nach BAGr, AKl, DKl)
  #kä/01.11.14
  #Es gibt 7 Zielgrößen <Y>: BAF [ha], V [m³Dh mR], VHb (nur Hauptbestand)
  #[m³Dh mR], B (oberird. Biomasse) [t], N (gesamtzahl), NDh (Gesamtzahl
  #Derbholzbäume)
  #NDhHB (Gesamtzahl Derbholzbäume im Hauptbestand)
  #Für diese 7 Größen werden jeweils der Gesamtwert ("Total") und der
  #Stichprobenfehler (SE) berechnet, und zwar jeweils für die 9 Baumartengruppen
  #sowie A.k Alters- und D.k Durchmesserklassen
  Y.bagr.akl.dkl    <- array(dim=c(7,2,n.bagr,A.k,D.k))
  nT.bagr.akl.dkl   <- array(dim=c(n.bagr,A.k,D.k))
  #Hektarbezogene Kennwerte. urspr. 5 Zielgrößen: Vha, VHbha, Bha, Nha, NDhha
  #erweitert um BAGr-Anteil:  BAFAnt, Vha, VHbha, Bha, Nha, NDhha
  #Daher 1. Dimension 6 statt 5
  Yha.bagr.akl.dkl  <- array(dim=c(7,2,n.bagr,A.k,D.k))
  #kä/26.09.14: Baumartenanteile  (Prozent)
  ant.bagr.akl.dkl  <- array(dim=c(2,A.k,D.k))
  #---
  #----------------

  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        baeume.ba <- subset(baeume.s,
                      bagr==bagr.list[i]&akl==akl.lab[j]&dkl==dkl.lab[k],
                      select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
        if (length(baeume.ba[,1])== 0)
        {
           Y.bagr.akl.dkl[1:6,1,i,j,k]    <- rep(0,6)  #Zielgröße Total
           Y.bagr.akl.dkl[1:6,2,i,j,k]    <- rep(0,6)  #Stichprobenfehler (SE)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           nT.bagr.akl.dkl[i,j,k]    <- 0  #n PSU (Trakte)
        }else
        {
          #Nach Trakt aggregieren
          #BAF der BA-Gruppe [ha] als "x"
          xy <- aggregate(baeume.ba$stfl,by=list(baeume.ba$tnr),sum)
          names(xy) <- c("tnr","x")
          xy$x <- xy$x/10000*lk  #Umrechnung in ha  und Lückenkorrektur
          #Derbholz-Vorrat [m³ mR] als "v"
          xy <- cbind(xy,aggregate(baeume.ba$volv*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[3] <- "v"
          #Derbholz-Vorrat [m³ mR] im Hauptbestand als "v.hb"
          xy <- cbind(xy,
            aggregate(baeume.ba$volv*baeume.ba$nha*ifelse(baeume.ba$stfl>0,1,0),
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[4] <- "v.hb"
          #oberird. Biomasse [t] als "b"
          xy <- cbind(xy,aggregate(baeume.ba$oib*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
          names(xy)[5] <- "b"
          #Anzahl Bäume als "n"
          xy <- cbind(xy,aggregate(baeume.ba$nha,by=list(baeume.ba$tnr),sum)$x)
          names(xy)[6] <- "n"
          #Anzahl nur Derbholz-Bäume (bhd>=7)  als "ndh"
          #sofern in der Klassifizierung solche vorkommen!
          x.ndh <- try(aggregate(baeume.ba$nha*ifelse(baeume.ba$bhd>=7,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh)
          names(xy)[7] <- "ndh"
          x.ndh.hb <- try(aggregate(baeume.ba$nha*ifelse(
              baeume.ba$bhd>=7 & baeume.ba$stfl>0,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh.hb <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh.hb)
          names(xy)[8] <- "ndh.hb"

          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,subset(trakte,select=c(tnr,m),by=c(tnr)))
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---  Ergänzug 11.10. <m_bhb>    Hinweis: Offset für Indizierung ange-
          #passt!!
          #des Weiteren merge mit <xy.s> wegen <hbf> (begehbare HBF) für
          #Berechnung der BA-Anteile
          xy <- merge(subset(xy.s,select=c(tnr,m,hbf)),xy,by=c("tnr"),all.x=T)
          #xy <- merge(subset(trakte,select=c(tnr,m,m_bhb)),xy,by=c("tnr"),all.x=T)
          xy[is.na(xy)] <- 0

          for (l in 1:7)
          {
            #Zielgrößen Y {BAF,V,VHb,B,N,NDh,NDhHb)   (Offset von (2+l) auf (3+l)
            #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
            R.list <- r.variance.fun(cbind(xy$m,xy[,(3+l)]),nT)
            #Total
            Y.bagr.akl.dkl[l,1,i,j,k] <- R.list$R.xy*A
            #sum(xy[,(1+l)])/sum(x)*A *ifelse(l==1,lk,1)
                                      #Lückenkorrektur bei BAF bereits erfolgt
            #Zugehöriger Stichprobenfehler
            Y.bagr.akl.dkl[l,2,i,j,k] <- sqrt(R.list$V.R.xy)*A

            #Ratio-Schätzer:
            #neu:11.10.14: BAFAnt, Vha, VHbha, Bha, Nha, NDhha, NDhHbha) = 7!
            #if (l > 1)    #Hinweis: l+1 = Spalte der Zielgröße in xy; Spalte 2
                          #ist BA-Fläche (HB) als Bezugsfläche (= xy$x)
            if (l == 1) #BAF-Anteil (an HBF)
            {
              R.list <- r.variance.fun(cbind(xy$hbf,xy[,(3+l)]),nT)
              #in Prozent
              R.list$R.xy <- R.list$R.xy*100
              R.list$V.R.xy <- R.list$V.R.xy*10000
            } else
            {
              #Bei Ha-Vorrat Hauptbestand bzw. NDh/ha Hauptbestand
              #(l==3 bzw l == 7) muss Flächenbezug die Fläche
              #OHNE Lückenkorrektur sein, daher wird durch <lk>idiert
              #TODO prüfen kä/17.04.2014
              lk.c <- ifelse(l %in% c(3,7) ,lk,1) #lk.c macht bei HB-Vorrat
              #Lückenkorrektur rückgängig Hinweis: Offset von (2+l) auf (3+l)
              #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
              R.list <- r.variance.fun(cbind(xy$x/lk.c,xy[,(3+l)]),nT)
              #l.ha <- l-1   ka/11.10.14
            }#End if ... else
            l.ha <- l
            Yha.bagr.akl.dkl[l.ha,1,i,j,k] <- R.list$R.xy
              #              Y.bagr.akl.dkl[l,1,i,j,k]/Y.bagr.akl.dkl[1,1,i,j,k]
            Yha.bagr.akl.dkl[l.ha,2,i,j,k] <- sqrt(R.list$V.R.xy)

          }#End for l (Zielgrößen)
        }#End if ... else
        #---
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
    #---
  }#End for i (BAGR)
  #-----------------------


  #---
  #Tabelle für BA-Gruppen

  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k) #kä/16.07.14
  #-------------------------
  #DKL-Labels  kä/16.07.2014
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Attribute1=c("BAF", "V_DhmR","V_DhmR_HB", "oiB", "N", "N_Dh", "N_Dh_HB"),
              #Attribute2 erweitert um "BA_Proz" (=BAFAnt)  kä/11.10.14
              Attribute2=c("BA_Proz","V_DhmR/ha","V_DhmR_HB/ha","oiB/ha","N/ha",
                            "N_Dh/ha", "N_Dh_HB/ha"),
              Größen = c("Wert","Standardfehler"),
              BAGR=bagr.list,AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.FVBN.Bagr.Akl.Dkl=Y.bagr.akl.dkl,
              FVBN.ha.Bagr.Akl.Dkl=Yha.bagr.akl.dkl,
              nT.Bagr.Akl.Dkl=nT.bagr.akl.dkl))
}#End <FVBN.bagrupp.akl.dkl.stratum.fun.2c>

#-------------------------------------------------------------------------------
#' Zustandshochrechnung (version 2d)
#' 
#' Funktion wertet nach frei definierbaren BA-Gruppen, Alters- und Durchmesser-
#' Klassen im Stratum aus. Die berechneten Zielgroessen sind Flaeche, Vorrat, 
#' Biomasse und Stammzahl (N). Variante mit Varianzschaetzung ueber 
#' Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.03.2014
#' @section Aktualisierungen: 
#'  02.12.2014 Erweiterung um alle Baumarten: wenn nach BAGR differenziert wird, 
#'    wird zusatzlich die "Summen"- Auswertung fuer alle Baumarten aufgerufen 
#'    und das Ergebnis in die Tabelle eingebaut \cr
#'  01.11.2014 Neue Version mit Stammzahlen im Hauptbestand \cr
#'  11.10.2014 Erweiterung um Baumartenanteile mit Fehler \cr
#'  19.04.2014 bei Ha-Bezug fuer Hauptbestands(HB)-Vorrat wird Luecken-Korrektur 
#'    rueckgaengig gemacht, um Bezug nur zur HB-Flaeche zu erzielen 
#'    (\code{T.bl}, \code{T.ibl}). 
#' @section Hinweis: Verallgemeinerte Version fuer die Auswertung 
#'  unterschiedlicher Inventurzeit-punkte, also BWI 1, BWI 2, BWI 3. \cr
#'  Die Funktion \code{\link{stratum.fun}} benoetigt desweiteren die Tabelle 
#'  \code{bacode}. \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @param baeume Datentabelle mit Bauminformationen. Die Tabelle muss mindestens 
#'  die Attribute TNr, ENr, BA, Alt., BHD., VolV., oiB., NHa., StFl. enthalten 
#'  (. steht fuer 1 oder 2, je nachdem welcher Zustand ausgewertet werden soll), 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in 
#'  \code{baeume} uebergeben wird.
#' @param ecken Eckenmerkmale.
#' @param trakte Traktmerkmale.
#' @param A Gesamtflaeche in ha des Inventurgebiets zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @param inv Inventur 1:= Vorinventur, 2:= Folgeinventur, wenn eine 
#'  \code{baeume}-Tablle mit Attributen aus zwei Aufnahmezeitpunkten uebergeben 
#'  wird. Wenn dies der Fall ist, dann entsprechen die jeweils fuer die 
#'  auszuwertende Inventur relevanten Attribute der Konvention: Aktuelle 
#'  ("Folge") Inventur hat im Attributnamen eine 2 am Ende, also "Alt2", "BHD2", 
#'  "D032", "H2", "HSt2", "VolV2", "VolE2", "oiB2", "StFl2", "NHa2", die 
#'  Vorinventur eine 1; d.h. im Falle der BWI 3 haben die Attributbezeichner die 
#'  2 im Namen, die Daten der Vorinventur BWI 2 die 1. Um mit denselben 
#'  Algorithmen fuer beide Zustaende arbeiten zu koennen, werden fuer den 
#'  jeweiligen Auswertungsfall \code{inv} (Vorinventur = 1 bzw. Folge-(aktuelle) 
#'  Inventur =2 die Attibutnamen "neutralisiert", also die Kennung 1 oder 2 im 
#'  Attributnamen entfernt.
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
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und Version.baeume.b, \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), \strong{Attribute1} 
#'  (Vektor mit berechneten absoluten Attributnamen), \strong{Attribute2} 
#'  (vektor mit berechneten pro ha Attributname), \strong{Größen} (Vektor mit 
#'  berechneten Groessen), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.FVBN.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer Gesamtwerte jeweils 
#'  fuer 9 Baumartengruppen, Alters- und Durchmesserklassen), 
#'  \strong{FVBN.ha.Bagr.Akl.Dkl} (Array mit berechneten Groessen (Wert und 
#'  Standardfehler) fuer hektabezogene Kenngroessen), \strong{nT.bagr.Akl.Dkl} 
#'  (Anzahl Traktecken (?)).
FVBN.bagrupp.akl.dkl.stratum.fun.2d <-
          function(baeume,ecken,trakte,A,inv,BA.grupp,A.klass,D.klass,auswahl){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))
  #"Neutralisierung" der benötigten Attributnamen
  names(baeume) <- sub(inv,names(baeume),replacement="")
  #Attribute und Untermenge des Stratums aus <baeume> auswählen
  baeume.s <- merge(subset(baeume,select=c(tnr,enr,ba,alt,bhd,volv,oib,nha,stfl)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"),all.y=T)

  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  #BA-Gruppe dazu spielen
  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  baeume.s[is.na(baeume.s)] <- 0
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]

  #------------------
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])

  #---------------------
  #Flächen
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==999,baeume.s$stfl/10000,0),
                  by=list(baeume.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==998,baeume.s$stfl,0),
                  by=list(baeume.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl
  #kä/11.10.2014  wegen BA-Anteilen muss hbf je Trakt im Stratum erhalten bleiben
  xy.s <- xy
  #----
  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  #---------------------------------------
  #Alter
  A.max <- 999
  #----- kä/15.02.
  baeume.s$akl <- as.integer(cut(baeume.s$alt,
                      breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T))
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Maximale Anzahl Altersklassen aus A-Kl-Parametern <A.klass>
  if(A.klass[[1]]>A.klass[[2]])
  {
    max.A.k <- A.klass[[1]]/A.klass[[2]] + 1
    if (A.k < max.A.k) {A.k <- max.A.k }
  }
  #----- kä/15.02.

  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  baeume.s$dkl <- cut(baeume.s$bhd, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])
  #----- kä/15.02.
  #Maximale Anzahl D-klassen aus D-Kl-Parametern <D.klass>
  if(D.klass[[2]]>D.klass[[3]])
  {
    max.D.k <- (D.klass[[2]]-D.klass[[1]])/D.klass[[2]] + 2
    if (D.k < max.D.k) {D.k <- max.D.k }
  }
  #----- kä/15.02.

  #Array für Ergebnisse (Totals und SE jeweils nach BAGr, AKl, DKl)
  #kä/01.11.14
  #Es gibt 7 Zielgrößen <Y>: BAF [ha], V [m³Dh mR], VHb (nur Hauptbestand)
  #[m³Dh mR], B (oberird. Biomasse) [t], N (gesamtzahl), NDh (Gesamtzahl
  #Derbholzbäume)
  #NDhHB (Gesamtzahl Derbholzbäume im Hauptbestand)
  #Für diese 7 Größen werden jeweils der Gesamtwert ("Total") und der
  #Stichprobenfehler (SE) berechnet, und zwar jeweils für die 9 Baumartengruppen
  #sowie A.k Alters- und D.k Durchmesserklassen
  Y.bagr.akl.dkl    <- array(dim=c(7,2,n.bagr,A.k,D.k))
  nT.bagr.akl.dkl   <- array(dim=c(n.bagr,A.k,D.k))
  #Hektarbezogene Kennwerte. urspr. 5 Zielgrößen: Vha, VHbha, Bha, Nha, NDhha
  #erweitert um BAGr-Anteil:  BAFAnt, Vha, VHbha, Bha, Nha, NDhha
  #Daher 1. Dimension 6 statt 5
  Yha.bagr.akl.dkl  <- array(dim=c(7,2,n.bagr,A.k,D.k))
  #kä/26.09.14: Baumartenanteile  (Prozent)
  ant.bagr.akl.dkl  <- array(dim=c(2,A.k,D.k))
  #---
  #----------------

  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        baeume.ba <- subset(baeume.s,
                      bagr==bagr.list[i]&akl==akl.lab[j]&dkl==dkl.lab[k],
                      select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
        if (length(baeume.ba[,1])== 0)
        {
           Y.bagr.akl.dkl[1:6,1,i,j,k]    <- rep(0,6)  #Zielgröße Total
           Y.bagr.akl.dkl[1:6,2,i,j,k]    <- rep(0,6)  #Stichprobenfehler (SE)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
           nT.bagr.akl.dkl[i,j,k]    <- 0  #n PSU (Trakte)
        }else
        {
          #Nach Trakt aggregieren
          #BAF der BA-Gruppe [ha] als "x"
          xy <- aggregate(baeume.ba$stfl,by=list(baeume.ba$tnr),sum)
          names(xy) <- c("tnr","x")
          xy$x <- xy$x/10000*lk  #Umrechnung in ha  und Lückenkorrektur
          #Derbholz-Vorrat [m³ mR] als "v"
          xy <- cbind(xy,aggregate(baeume.ba$volv*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[3] <- "v"
          #Derbholz-Vorrat [m³ mR] im Hauptbestand als "v.hb"
          xy <- cbind(xy,
            aggregate(baeume.ba$volv*baeume.ba$nha*ifelse(baeume.ba$stfl>0,1,0),
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[4] <- "v.hb"
          #oberird. Biomasse [t] als "b"
          xy <- cbind(xy,aggregate(baeume.ba$oib*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
          names(xy)[5] <- "b"
          #Anzahl Bäume als "n"
          xy <- cbind(xy,aggregate(baeume.ba$nha,by=list(baeume.ba$tnr),sum)$x)
          names(xy)[6] <- "n"
          #Anzahl nur Derbholz-Bäume (bhd>=7)  als "ndh"
          #sofern in der Klassifizierung solche vorkommen!
          x.ndh <- try(aggregate(baeume.ba$nha*ifelse(baeume.ba$bhd>=7,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh)
          names(xy)[7] <- "ndh"
          x.ndh.hb <- try(aggregate(baeume.ba$nha*ifelse(
              baeume.ba$bhd>=7 & baeume.ba$stfl>0,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh.hb <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh.hb)
          names(xy)[8] <- "ndh.hb"

          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,subset(trakte,select=c(tnr,m),by=c(tnr)))
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---  Ergänzug 11.10. <m_bhb>    Hinweis: Offset für Indizierung ange-
          #passt!!
          #des Weiteren merge mit <xy.s> wegen <hbf> (begehbare HBF) für
          #Berechnung der BA-Anteile
          xy <- merge(subset(xy.s,select=c(tnr,m,hbf)),xy,by=c("tnr"),all.x=T)
          #xy <- merge(subset(trakte,select=c(tnr,m,m_bhb)),xy,by=c("tnr"),all.x=T)
          xy[is.na(xy)] <- 0

          for (l in 1:7)
          {
            #Zielgrößen Y {BAF,V,VHb,B,N,NDh,NDhHb)   (Offset von (2+l) auf (3+l)
            #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
            R.list <- r.variance.fun(cbind(xy$m,xy[,(3+l)]),nT)
            #Total
            Y.bagr.akl.dkl[l,1,i,j,k] <- R.list$R.xy*A
            #sum(xy[,(1+l)])/sum(x)*A *ifelse(l==1,lk,1)
                                      #Lückenkorrektur bei BAF bereits erfolgt
            #Zugehöriger Stichprobenfehler
            Y.bagr.akl.dkl[l,2,i,j,k] <- sqrt(R.list$V.R.xy)*A

            #Ratio-Schätzer:
            #neu:11.10.14: BAFAnt, Vha, VHbha, Bha, Nha, NDhha, NDhHbha) = 7!
            #if (l > 1)    #Hinweis: l+1 = Spalte der Zielgröße in xy; Spalte 2
                          #ist BA-Fläche (HB) als Bezugsfläche (= xy$x)
            if (l == 1) #BAF-Anteil (an HBF)
            {
              R.list <- r.variance.fun(cbind(xy$hbf,xy[,(3+l)]),nT)
              #in Prozent
              R.list$R.xy <- R.list$R.xy*100
              R.list$V.R.xy <- R.list$V.R.xy*10000
            } else
            {
              #Bei Ha-Vorrat Hauptbestand bzw. NDh/ha Hauptbestand
              #(l==3 bzw l == 7) muss Flächenbezug die Fläche
              #OHNE Lückenkorrektur sein, daher wird durch <lk>idiert
              #TODO prüfen kä/17.04.2014
              lk.c <- ifelse(l %in% c(3,7) ,lk,1) #lk.c macht bei HB-Vorrat
              #Lückenkorrektur rückgängig Hinweis: Offset von (2+l) auf (3+l)
              #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
              R.list <- r.variance.fun(cbind(xy$x/lk.c,xy[,(3+l)]),nT)
              #l.ha <- l-1   ka/11.10.14
            }#End if ... else
            l.ha <- l
            Yha.bagr.akl.dkl[l.ha,1,i,j,k] <- R.list$R.xy
              #              Y.bagr.akl.dkl[l,1,i,j,k]/Y.bagr.akl.dkl[1,1,i,j,k]
            Yha.bagr.akl.dkl[l.ha,2,i,j,k] <- sqrt(R.list$V.R.xy)

          }#End for l (Zielgrößen)
        }#End if ... else
        #---
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
    #---
  }#End for i (BAGR)
  #-----------------------
  #Erweiterung um Auswertung über alle Baumarten kä/02.12.2014
  if(n.bagr > 1)
  {
    fvbn.alleba <- FVBN.bagrupp.akl.dkl.stratum.fun.2c(
      baeume,ecken,trakte,A,inv,
      list(bagr.lab = c("Alle Baumarten"),ba.grupp =list(c(10:299))),
      A.klass,D.klass,auswahl)
    t.fvbn  <- array(dim=c(7,2,(n.bagr+1),A.k,D.k))
    fvbn.ha <- array(dim=c(7,2,(n.bagr+1),A.k,D.k))
    nt      <- array(dim=c((n.bagr+1),A.k,D.k))
    t.fvbn[,,1:n.bagr,,] <- Y.bagr.akl.dkl
    fvbn.ha[,,1:n.bagr,,]<- Yha.bagr.akl.dkl
    nt[1:n.bagr,,]       <- nT.bagr.akl.dkl
    t.fvbn[,,(n.bagr+1),,]   <- fvbn.alleba$T.FVBN.Bagr.Akl.Dkl
    fvbn.ha[,,(n.bagr+1),,]  <- fvbn.alleba$FVBN.ha.Bagr.Akl.Dkl
    nt[(n.bagr+1),,]         <- fvbn.alleba$nT.Bagr.Akl.Dkl
    Y.bagr.akl.dkl       <- t.fvbn
    Yha.bagr.akl.dkl     <- fvbn.ha
    nT.bagr.akl.dkl      <- nt 
    bagr.list            <- c(bagr.list,"Alle BA")
  }
  #---
  #Tabelle für BA-Gruppen

  #AKL-Labels
  akl.lab <- akl.lab.fun(A.klass,A.k) #kä/16.07.14
  #-------------------------
  #DKL-Labels  kä/16.07.2014
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Attribute1=c("BAF", "V_DhmR","V_DhmR_HB", "oiB", "N", "N_Dh", "N_Dh_HB"),
              #Attribute2 erweitert um "BA_Proz" (=BAFAnt)  kä/11.10.14
              Attribute2=c("BA_Proz","V_DhmR/ha","V_DhmR_HB/ha","oiB/ha","N/ha",
                            "N_Dh/ha", "N_Dh_HB/ha"),
              Größen = c("Wert","Standardfehler"),
              BAGR=bagr.list,AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.FVBN.Bagr.Akl.Dkl=Y.bagr.akl.dkl,
              FVBN.ha.Bagr.Akl.Dkl=Yha.bagr.akl.dkl,
              nT.Bagr.Akl.Dkl=nT.bagr.akl.dkl))
}#End <FVBN.bagrupp.akl.dkl.stratum.fun.2d>

#-------------------------------------------------------------------------------
#' Zustandshochrechnung (Version 2e)
#' 
#' Funktion wertet nach frei definierbaren BA-Gruppen, Alters- und Durchmesser-
#' Klassen im Stratum aus. Die berechneten Zielgroessen sind Flaeche, Vorrat, 
#' Biomasse und Stammzahl (N). Variante mit Varianzschaetzung ueber 
#' Fehlerfortpflanzung.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 13.03.2014
#' @section Aktualisierungen: 
#'  13.03.2015 Korrektur \code{akl.lab} bzw. \code{dkl.lab} \cr
#'  22.02.2015 Erweiterung um "alle DKL" und "alle AKL" \cr
#'  02.12.2014 Erweiterung um alle Baumarten: wenn nach BAGR differenziert wird, 
#'    wird zusatzlich die "Summen"- Auswertung fuer alle Baumarten aufgerufen 
#'    und das Ergebnis in die Tabelle eingebaut \cr
#'  01.11.2014 Neue Version mit Stammzahlen im Hauptbestand \cr
#'  11.10.2014 Erweiterung um Baumartenanteile mit Fehler \cr
#'  19.04.2014 bei Ha-Bezug fuer Hauptbestands(HB)-Vorrat wird Luecken-Korrektur 
#'    rueckgaengig gemacht, um Bezug nur zur HB-Flaeche zu erzielen 
#'    (\code{T.bl}, \code{T.ibl}). 
#' @section Hinweis: Verallgemeinerte Version fuer die Auswertung 
#'  unterschiedlicher Inventurzeit-punkte, also BWI 1, BWI 2, BWI 3. \cr
#'  Die Funktion \code{\link{stratum.fun}} benoetigt desweiteren die Tabelle 
#'  \code{bacode}. \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @param baeume Datentabelle mit Bauminformationen. Die Tabelle muss mindestens 
#'  die Attribute TNr, ENr, BA, Alt., BHD., VolV., oiB., NHa., StFl. enthalten 
#'  (. steht fuer 1 oder 2, je nachdem welcher Zustand ausgewertet werden soll), 
#'  man kann auch die selektierten Attribute mit Namen ohne Kennziffer 
#'  uebergeben, wenn bereits eine eindeutige Auswahl der Attribute in 
#'  \code{baeume} uebergeben wird.
#' @param ecken Eckenmerkmale.
#' @param trakte Traktmerkmale.
#' @param A Gesamtflaeche in ha des Inventurgebiets zum jeweiligen 
#'  Inventurzeitpunkt (sollte eigentlich konstant sein).
#' @param inv Inventur 1:= Vorinventur, 2:= Folgeinventur, wenn eine 
#'  \code{baeume}-Tablle mit Attributen aus zwei Aufnahmezeitpunkten uebergeben 
#'  wird. Wenn dies der Fall ist, dann entsprechen die jeweils fuer die 
#'  auszuwertende Inventur relevanten Attribute der Konvention: Aktuelle 
#'  ("Folge") Inventur hat im Attributnamen eine 2 am Ende, also "Alt2", "BHD2", 
#'  "D032", "H2", "HSt2", "VolV2", "VolE2", "oiB2", "StFl2", "NHa2", die 
#'  Vorinventur eine 1; d.h. im Falle der BWI 3 haben die Attributbezeichner die 
#'  2 im Namen, die Daten der Vorinventur BWI 2 die 1. Um mit denselben 
#'  Algorithmen fuer beide Zustaende arbeiten zu koennen, werden fuer den 
#'  jeweiligen Auswertungsfall \code{inv} (Vorinventur = 1 bzw. Folge-(aktuelle) 
#'  Inventur =2 die Attibutnamen "neutralisiert", also die Kennung 1 oder 2 im 
#'  Attributnamen entfernt.
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
#' @return Liste mit folgenden Komponenten: \strong{Log} (Liste mit 
#'  Erstellungsdatum und Version.baeume.b, \strong{Stratum} 
#'  (\code{auswahl}), \strong{nTE} (Anzahl Ecken im Stratum), \strong{HBF} 
#'  (Holzbodenflaeche in ha), \strong{se.HBF} (Standardfehler HBF), \strong{BL} 
#'  (Flaeche der Bloessen in ha), \strong{se.BL} (Standardfehler BL), 
#'  \strong{iBL} (Flaeche der idellen Bloessen ("Luecken") in ha), 
#'  \strong{se.iBL} (Standardfehler iBL), \strong{LK} (relative 
#'  Lueckenkorrektur), \strong{se.LK}  (Standardfehler LK), \strong{Attribute1} 
#'  (Vektor mit berechneten absoluten Attributnamen), \strong{Attribute2} 
#'  (vektor mit berechneten pro ha Attributname), \strong{Größen} (Vektor mit 
#'  berechneten Groessen), \strong{BAGR} (Labels fuer Baumartengruppen aus 
#'  \code{ba.grupp}), \strong{AKL} (Labels der Altersklassen), \strong{DKL} 
#'  (Labels der Durchmesserklassen), \strong{T.FVBN.Bagr.Akl.Dkl} (Array 
#'  mit berechneten Groessen (Wert und Standardfehler) fuer Gesamtwerte jeweils 
#'  fuer 9 Baumartengruppen, Alters- und Durchmesserklassen), 
#'  \strong{FVBN.ha.Bagr.Akl.Dkl} (Array mit berechneten Groessen (Wert und 
#'  Standardfehler) fuer hektabezogene Kenngroessen), \strong{nT.bagr.Akl.Dkl} 
#'  (Anzahl Traktecken (?)).
FVBN.bagrupp.akl.dkl.stratum.fun.2e <-
  function(baeume,ecken,trakte,A,inv,BA.grupp,A.klass,D.klass,auswahl){
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))
  #"Neutralisierung" der benötigten Attributnamen
  names(baeume) <- sub(inv,names(baeume),replacement="")
  #Attribute und Untermenge des Stratums aus <baeume> auswählen
  baeume.s <- merge(subset(baeume,select=c(tnr,enr,ba,alt,bhd,volv,oib,nha,stfl)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"),all.y=T)
  
  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  #BA-Gruppe dazu spielen
  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                    by.x="ba",by.y="ICode",all.x=T)
  baeume.s[is.na(baeume.s)] <- 0
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]
  
  #------------------
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  
  #---------------------
  #Flächen
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==999,baeume.s$stfl/10000,0),
                           by=list(baeume.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(baeume.s$ba==998,baeume.s$stfl,0),
                           by=list(baeume.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl
  #kä/11.10.2014  wegen BA-Anteilen muss hbf je Trakt im Stratum erhalten bleiben
  xy.s <- xy
  #----
  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  #---------------------------------------
  #Alter
  A.max <- 999
  #----- kä/15.02.
  baeume.s$akl <- as.integer(cut(baeume.s$alt,
                                 breaks=c(seq(0,A.klass[[1]],A.klass[[2]]),A.max),right=T))
  akl.lab <- unique(baeume.s$akl)
  akl.lab <- as.character(akl.lab[order(akl.lab)])
  A.k <- length(akl.lab) - 1 #wegen NA (Alter 0 ausgeschlossen!)
  if (A.k == 0) A.k <- 1
  #Maximale Anzahl Altersklassen aus A-Kl-Parametern <A.klass>
  if(A.klass[[1]]>A.klass[[2]])
  {
    max.A.k <- A.klass[[1]]/A.klass[[2]] + 1
    if (A.k < max.A.k) {A.k <- max.A.k }
  }
  #----- kä/15.02.
  #+++ 22.02.2015
  #um Klasse "Alle AKl" erweiteren
  if (A.k > 1) {A.k <- A.k + 1}
  #+++
  #Durchmesser
  D.max <- 999
  if (D.klass[["Ndh"]] & D.klass[[1]] < 7)
  {
    brks <- c(0,7,seq(D.klass[[1]]+D.klass[[3]],D.klass[[2]],D.klass[[3]]),D.max)
  } else
  {
    brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  }
  baeume.s$dkl <- cut(baeume.s$bhd, breaks=brks, right=F)
  dkl.lab <- unique(baeume.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab[!is.na(dkl.lab)])
  #----- kä/15.02.
  #Maximale Anzahl D-klassen aus D-Kl-Parametern <D.klass>
  if(D.klass[[2]]>D.klass[[3]])
  {
    max.D.k <- (D.klass[[2]]-D.klass[[1]])/D.klass[[2]] + 2
    if (D.k < max.D.k) {D.k <- max.D.k }
  }
  #----- kä/15.02.
  #+++ 22.02.2015
  #um Klasse "alle DKl" erweiteren
  if (D.k > 1) {D.k <- D.k + 1}
  #+++
  #Array für Ergebnisse (Totals und SE jeweils nach BAGr, AKl, DKl)
  #kä/01.11.14
  #Es gibt 7 Zielgrößen <Y>: BAF [ha], V [m³Dh mR], VHb (nur Hauptbestand)
  #[m³Dh mR], B (oberird. Biomasse) [t], N (gesamtzahl), NDh (Gesamtzahl
  #Derbholzbäume)
  #NDhHB (Gesamtzahl Derbholzbäume im Hauptbestand)
  #Für diese 7 Größen werden jeweils der Gesamtwert ("Total") und der
  #Stichprobenfehler (SE) berechnet, und zwar jeweils für die 9 Baumartengruppen
  #sowie A.k Alters- und D.k Durchmesserklassen
  Y.bagr.akl.dkl    <- array(dim=c(7,2,n.bagr,A.k,D.k))
  nT.bagr.akl.dkl   <- array(dim=c(n.bagr,A.k,D.k))
  #Hektarbezogene Kennwerte. urspr. 5 Zielgrößen: Vha, VHbha, Bha, Nha, NDhha
  #erweitert um BAGr-Anteil:  BAFAnt, Vha, VHbha, Bha, Nha, NDhha
  #Daher 1. Dimension 6 statt 5
  Yha.bagr.akl.dkl  <- array(dim=c(7,2,n.bagr,A.k,D.k))
  #kä/26.09.14: Baumartenanteile  (Prozent)
  ant.bagr.akl.dkl  <- array(dim=c(2,A.k,D.k))
  #---
  #----------------
  
  for (i in 1:n.bagr)   #Baumartengruppen
  {
    for (j in 1:A.k)    #Altersklassen
    {
      for (k in 1:D.k)  #Durchmesserklassen
      {
        if (j == A.k) #alle AKl
        {
          if (k == D.k)
          {
            baeume.ba <- subset(baeume.s,
                                bagr==bagr.list[i],select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
          } else
          {
            baeume.ba <- subset(baeume.s,
                                bagr==bagr.list[i]&dkl==dkl.lab[k],
                                select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
          }
        }else
        {
          if (k == D.k)
          {
            baeume.ba <- subset(baeume.s,
                                bagr==bagr.list[i]&akl==akl.lab[j],
                                select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
          }else
          {
            baeume.ba <- subset(baeume.s,
                                bagr==bagr.list[i]&akl==akl.lab[j]&dkl==dkl.lab[k],
                                select=c(tnr,enr,bhd,dkl,volv,oib,nha,stfl))
          }
        }
        if (length(baeume.ba[,1])== 0)
        {
          Y.bagr.akl.dkl[1:6,1,i,j,k]    <- rep(0,6)  #Zielgröße Total
          Y.bagr.akl.dkl[1:6,2,i,j,k]    <- rep(0,6)  #Stichprobenfehler (SE)
          Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
          Yha.bagr.akl.dkl[1:5,1,i,j,k]  <- rep(0,5)
          nT.bagr.akl.dkl[i,j,k]    <- 0  #n PSU (Trakte)
        }else
        {
          #Nach Trakt aggregieren
          #BAF der BA-Gruppe [ha] als "x"
          xy <- aggregate(baeume.ba$stfl,by=list(baeume.ba$tnr),sum)
          names(xy) <- c("tnr","x")
          xy$x <- xy$x/10000*lk  #Umrechnung in ha  und Lückenkorrektur
          #Derbholz-Vorrat [m³ mR] als "v"
          xy <- cbind(xy,aggregate(baeume.ba$volv*baeume.ba$nha,
                                   by=list(baeume.ba$tnr),sum)$x )
          names(xy)[3] <- "v"
          #Derbholz-Vorrat [m³ mR] im Hauptbestand als "v.hb"
          xy <- cbind(xy,
                      aggregate(baeume.ba$volv*baeume.ba$nha*ifelse(baeume.ba$stfl>0,1,0),
                                by=list(baeume.ba$tnr),sum)$x )
          names(xy)[4] <- "v.hb"
          #oberird. Biomasse [t] als "b"
          xy <- cbind(xy,aggregate(baeume.ba$oib*baeume.ba$nha,
                                   by=list(baeume.ba$tnr),sum)$x/1000)
          names(xy)[5] <- "b"
          #Anzahl Bäume als "n"
          xy <- cbind(xy,aggregate(baeume.ba$nha,by=list(baeume.ba$tnr),sum)$x)
          names(xy)[6] <- "n"
          #Anzahl nur Derbholz-Bäume (bhd>=7)  als "ndh"
          #sofern in der Klassifizierung solche vorkommen!
          x.ndh <- try(aggregate(baeume.ba$nha*ifelse(baeume.ba$bhd>=7,1,0),
                                 by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh)
          names(xy)[7] <- "ndh"
          x.ndh.hb <- try(aggregate(baeume.ba$nha*ifelse(
            baeume.ba$bhd>=7 & baeume.ba$stfl>0,1,0),
            by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh.hb <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh.hb)
          names(xy)[8] <- "ndh.hb"
          
          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,subset(trakte,select=c(tnr,m),by=c(tnr)))
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---  Ergänzug 11.10. <m_bhb>    Hinweis: Offset für Indizierung ange-
          #passt!!
          #des Weiteren merge mit <xy.s> wegen <hbf> (begehbare HBF) für
          #Berechnung der BA-Anteile
          xy <- merge(subset(xy.s,select=c(tnr,m,hbf)),xy,by=c("tnr"),all.x=T)
          #xy <- merge(subset(trakte,select=c(tnr,m,m_bhb)),xy,by=c("tnr"),all.x=T)
          xy[is.na(xy)] <- 0
          
          for (l in 1:7)
          {
            #Zielgrößen Y {BAF,V,VHb,B,N,NDh,NDhHb)   (Offset von (2+l) auf (3+l)
            #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
            R.list <- r.variance.fun(cbind(xy$m,xy[,(3+l)]),nT)
            #Total
            Y.bagr.akl.dkl[l,1,i,j,k] <- R.list$R.xy*A
            #sum(xy[,(1+l)])/sum(x)*A *ifelse(l==1,lk,1)
            #Lückenkorrektur bei BAF bereits erfolgt
            #Zugehöriger Stichprobenfehler
            Y.bagr.akl.dkl[l,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
            
            #Ratio-Schätzer:
            #neu:11.10.14: BAFAnt, Vha, VHbha, Bha, Nha, NDhha, NDhHbha) = 7!
            #if (l > 1)    #Hinweis: l+1 = Spalte der Zielgröße in xy; Spalte 2
            #ist BA-Fläche (HB) als Bezugsfläche (= xy$x)
            if (l == 1) #BAF-Anteil (an HBF)
            {
              R.list <- r.variance.fun(cbind(xy$hbf,xy[,(3+l)]),nT)
              #in Prozent
              R.list$R.xy <- R.list$R.xy*100
              R.list$V.R.xy <- R.list$V.R.xy*10000
            } else
            {
              #Bei Ha-Vorrat Hauptbestand bzw. NDh/ha Hauptbestand
              #(l==3 bzw l == 7) muss Flächenbezug die Fläche
              #OHNE Lückenkorrektur sein, daher wird durch <lk>idiert
              #TODO prüfen kä/17.04.2014
              lk.c <- ifelse(l %in% c(3,7) ,lk,1) #lk.c macht bei HB-Vorrat
              #Lückenkorrektur rückgängig Hinweis: Offset von (2+l) auf (3+l)
              #wegen zusätzlichem Attribut <m_bhb> geändert (kä/11.10.14)
              R.list <- r.variance.fun(cbind(xy$x/lk.c,xy[,(3+l)]),nT)
              #l.ha <- l-1   ka/11.10.14
            }#End if ... else
            l.ha <- l
            Yha.bagr.akl.dkl[l.ha,1,i,j,k] <- R.list$R.xy
            #              Y.bagr.akl.dkl[l,1,i,j,k]/Y.bagr.akl.dkl[1,1,i,j,k]
            Yha.bagr.akl.dkl[l.ha,2,i,j,k] <- sqrt(R.list$V.R.xy)
            
          }#End for l (Zielgrößen)
        }#End if ... else
        #---
      }#End for k (D-Klassen)
    }#End for j (A-Klassen)
    #---
  }#End for i (BAGR)
  #-----------------------
  #Erweiterung um Auswertung über alle Baumarten kä/02.12.2014
  if(n.bagr > 1)
  {
    #Ruft sich selber auf!!!! "...2e" (kä/22.02.2015)
    #fvbn.alleba <- FVBN.bagrupp.akl.dkl.stratum.fun.2c(
    fvbn.alleba <- FVBN.bagrupp.akl.dkl.stratum.fun.2e(
      baeume,ecken,trakte,A,inv,
      list(bagr.lab = c("Alle Baumarten"),ba.grupp =list(c(10:299))),
      A.klass,D.klass,auswahl)
    t.fvbn  <- array(dim=c(7,2,(n.bagr+1),A.k,D.k))
    fvbn.ha <- array(dim=c(7,2,(n.bagr+1),A.k,D.k))
    nt      <- array(dim=c((n.bagr+1),A.k,D.k))
    t.fvbn[,,1:n.bagr,,] <- Y.bagr.akl.dkl
    fvbn.ha[,,1:n.bagr,,]<- Yha.bagr.akl.dkl
    nt[1:n.bagr,,]       <- nT.bagr.akl.dkl
    t.fvbn[,,(n.bagr+1),,]   <- fvbn.alleba$T.FVBN.Bagr.Akl.Dkl
    fvbn.ha[,,(n.bagr+1),,]  <- fvbn.alleba$FVBN.ha.Bagr.Akl.Dkl
    nt[(n.bagr+1),,]         <- fvbn.alleba$nT.Bagr.Akl.Dkl
    Y.bagr.akl.dkl       <- t.fvbn
    Yha.bagr.akl.dkl     <- fvbn.ha
    nT.bagr.akl.dkl      <- nt
    bagr.list            <- c(bagr.list,"Alle BA")
  }
  #---
  #Tabelle für BA-Gruppen
  
  #AKL-Labels
  #Hinweis: A.k um 1 reduzieren bei Übergabe an Funktion kä/13.03.2015
  akl.lab <- akl.lab.fun(A.klass,ifelse(A.k>1,(A.k-1),A.k)) #kä/16.07.14
  #+++ 22.02.2015
  if (A.k > 1) akl.lab <- c(akl.lab,"Alle AKl")
  #-------------------------
  #DKL-Labels  kä/16.07.2014 
  #Hinweis: D.k um 1 reduzieren bei Übergabe an Funktion kä/13.03.2015
  dkl.lab <- dkl.lab.fun(D.klass,(D.k-1)) #kä/16.07.14
  #+++ 22.02.2015
  if (D.k > 1) dkl.lab <- c(dkl.lab,"Alle DKl")
  
  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",baeume$bemerk[baeume$stp==0][1],fixed=T)
  b <- nchar(as.character(baeume$bemerk[baeume$stp==0][1]))
  version.baeume.b  <- substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b)
  Log <- list(Datum=Sys.time(),
              Version.baeume.b=substr(as.character(baeume$bemerk[baeume$stp==0][1]),a,b))
  
  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BL=T.bl,se.BL=se.T.bl,iBL=T.ibl,se.iBL=se.T.ibl,LK=lk,se.LK=se.lk,
              Attribute1=c("BAF", "V_DhmR","V_DhmR_HB", "oiB", "N", "N_Dh", "N_Dh_HB"),
              #Attribute2 erweitert um "BA_Proz" (=BAFAnt)  kä/11.10.14
              Attribute2=c("BA_Proz","V_DhmR/ha","V_DhmR_HB/ha","oiB/ha","N/ha",
                           "N_Dh/ha", "N_Dh_HB/ha"),
              Größen = c("Wert","Standardfehler"),
              BAGR=bagr.list,AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.FVBN.Bagr.Akl.Dkl=Y.bagr.akl.dkl,
              FVBN.ha.Bagr.Akl.Dkl=Yha.bagr.akl.dkl,
              nT.Bagr.Akl.Dkl=nT.bagr.akl.dkl))
}#End <FVBN.bagrupp.akl.dkl.stratum.fun.2e>

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
#' Funktion berechnet Standard-FVBN-Auswertung für die Befundeinheit 
#' \code{auswahl} fuer die in der \code{eig.list} aufgefuehrten 
#' Eigentumskategorien {gw, stw, kw, oew, pw, gpw, mpw, kpw} sowie die in 
#' \code{bwi.list} aufgefuehrten BWI-Aufnahmen {1,2,3}. Es wird die Funktion 
#' \code{\link{FVBN.bagrupp.akl.dkl.stratum.fun.2d}} verwendet, welche neben 
#' Baumartengruppen auch die Summenwerte für alle Baumarten liefert.
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
  #kä/11.12.14 
  l <-length(bwi.list)
  fvbn.bagr.stratum   <- list()
  
  for (i in 1:k)
  {
    auswahl.i <- auswahl #kä/11.12.14
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
  #kä/12.12.14
  l <-length(bwi.list)
  fvbn.bagr.stratum   <- list()
  
  for (i in 1:k)
  {
    auswahl.i <- auswahl #kä/12.12.14
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
#Hinweis: diese Version ist speziell für die Inventurperiode 2003 bis 2012
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
#'  Version für Periode BWI 2 zu 3! \cr
#'  Auswertung erfolgt auf dem gemeinsamen Netz im Unterschied zur Version 
#'  \code{\link{VB.A.BAGR.akl.dkl.stratum.fun}}, welche das Nutzungsgeschehen 
#'  auf der bei der BWI 2 erfassten Flaeche abdeckt! \cr
#'  Fuer die Berechnung der flaechenbezogenen Nutzungen muss die mittlere 
#'  Baumartenflaeche der Periode berechnet werden, d.h. es werden auch die 
#'  Standflaechen der Folgeaufnahme benoetigt! \cr 
#'  Dies wird von der Funktion \code{\link{mbaf.bagr.alt.bhd.pm.fun}} 
#'  uebernommen! \cr
#'  Um Konflikte mit unterschiedlicher Gross- und Kleinschreibung bei den 
#'  Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle 
#'  Attributnamen auf Kleinschreibung umgestellt.
#' @section TODO: Verallgemeinerung bzw. Variante für BWI 1 zu 2 !!!!
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
#'  berechneten Attributen), \strong{Größen} (Vektor mit berechneten Groessen 
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
  #Bei Flächenbezug Reduktion auf gemeinsames Netz!!
  #kä/28.02.2014
  #---
  #kä/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
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
  ecken.23.hb <- merge(subset(ecken.3.s, select=c(TNr,ENr)),
      subset(ecken.2.s,select=c(TNr,ENr)),by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,subset(ecken.3,select=c(TNr,ENr,PL,PLkal)),
      by=c("TNr","ENr"))
  stratum <- ecken.23.hb
  trakte <- trakte.3
  t.pos <- length(trakte)#Anzahl Spalten in <trakte> wird benötigt, um
  #Attribut-Positionen zu bestimmen
  #--------------------
  #inv <- 1
  #stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  #<y> steht hier für die Anzahl der Traktecken auf begehbarem HB im Stratum
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Anzahl der Trakte im Stratum
  n.t.s <- length(y[,1])
  #Anfügen der Anzahl Traktecken (Wald und Nicht-Wald)
  y <- merge(y,subset(trakte,select=c(tnr,m),by=c(tnr)))
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
  #Hier könnte als Alternative die Funktion <r.variance.fun> benutzt werden
  #Hierzu müsste eine alle Trakte umfassende Matrix <xy> mit <m> und <y>
  #übergeben werden
  #----------------
  #kä/28.02.2014:
  baeume <- baeume.23 #muss eingelesen sein!
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))

  #HINWEIS: beim ausgeschiedenen Vorrat wird der zur Periodenmitte
  #fortgeschriebene Vorrat verwendet! volv2,vole2,oib2
  baeume.s <- merge(
          subset(baeume,select=c(tnr,enr,stp,bnr,ba,pk,alt1,alt2,bhd1,bhd2,volv2,
                                  vole2,oib2,nha1,stfl1)),
          subset(stratum,select=c(tnr,enr,pl,plkal)),by=c("tnr","enr"))

  #BA-Gruppe dazu spielen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  n.bagr <- length(BA.grupp[[1]])
  #BAGR-Liste
  bagr.list <- BA.grupp[[1]]
  n.bagr <- length(bagr.list)

  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                    by.x="ba",by.y="ICode",all.x=T)
  names(baeume.s) <- tolower(names(baeume.s))

  #Folgeinventur: BA-Gruppen hinzufügen
  baeume.3.s <- merge(subset(baeume.3,select=c(TNr,ENr,STP,BNr,Pk,BA,Alt1,Alt2,
                  BHD1,BHD2,StFl2)),subset(bagr.tab,select=c(ICode,bagr)),
                    by.x="BA",by.y="ICode",all.x=T)
  names(baeume.3.s) <- tolower(names(baeume.3.s))
  baeume.3.s <- merge(baeume.3.s,subset(stratum,select=c(tnr,enr,pl,plkal)),
          by=c("tnr","enr"))

  #--------------------------------------------------
  #Mittlere Baumartenflächen nach BAGr, AKl und DKl zur Periodenmitte nach Trakt
  mbaf.bagr.akl.dkl.tnr <- mbaf.bagr.alt.bhd.pm.fun(
                                    baeume.s,baeume.3.s,A.klass,D.klass)

  #-------------
  #Klassifizierung des Ausgeschiedenen Vorrats durchführen
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

  #Array für mittlere BAF zur PM nach BAGr, AKl, DKl
  BAF.bagr.akl.dkl  <- array(dim=c(2,n.bagr,A.k,D.k))

  if(N.art)
    {pk.list <- list(c(2,3,9),c(4,5));n.nart <- 2}else
    {pk.list <- list(c(2:5,9)); n.nart <- 1}
  #Array für Ergebnisse (Totals und SE jeweils nach Nutzungsart, BAGr, AKl, DKl)
  #Nutzungsart: aus <pk>: 2 = selektiv genutzt,  3 = flächig genutzt
  #                       4 = am Ort verblieben, 5 = abgestorben  (stehend)
  #                       9 = unauffindbar (wird der Kategorie geerntet
  #                           zugewiesen)
  #                       2,3,9 definieren den geernteten ausgeschiedenen Vorrat
  #                       4, 5 definieren den ungenutzten (tw. aus natürl. Mort.
  #                       stammenden) ungenutzten ausgeschiedenen Vorrat)
  #Aus diesen Kennzahlen werden die 2 Kategorien der Nutzungsart (NArt):
  #geerntet bzw. ungenutzt festgelegt.
  #Es gibt 4 Zielgrößen <Y>:  V [m³Dh mR], V Eor (Erntevolumen o. R.) [m³E oR],
  #B (oberird. Biomasse) [t], N (Anzahl), für die jeweils der Gesamtwert der
  #Periode ("Total") und der jährliche Wert berechnet wird, sowie der
  #Stichprobenfehler (SE), und zwar jeweils für die 2 Kategorien "geerntet" /
  #"ungenutzt" sowie 9 Baumartengruppen, A.k Alters- und D.k Durchmesserklassen
  #1. Index: 1- 4: Perioden-Total (v, v.eor, b, n);
  #          4- 8: m. jährlicher Wert (j.v, j.v.eor, j.b, j.n);
  #          9-12: m. jährlicher Wert je ha     (kä/28.02.2014)
  #2. Index: 1: Wert; 2: SE;
  #3. Index: Nutzungsart (wenn <N.art> == TRUE (1: geerntet, 2: ungenutzt)
  #4. Index: BAGr; 5. Index: A-Klasse; 6. Index: D-Klasse
  Y.na.bagr.akl.dkl    <- array(dim=c(12,2,n.nart,n.bagr,A.k,D.k))
  #mittlere kal. Periodenlänge mit Standard-Fehler
  mPL.na.bagr.akl.dkl   <- array(dim=c(2,2,n.bagr,A.k,D.k))
  #Anzahl Trakte (PSU) je NArt, BAGR, Akl, Dkl
  nT.na.bagr.akl.dkl   <- array(dim=c(n.nart,n.bagr,A.k,D.k))
  #Mittlere Straten-PL mit SE
  ne.T <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
            names(ne.T) <- c("tnr","n.te")
  #"Periodensumme" je Trakt (mit n Ecken gewogen)
  #Bei Nutzung wird kalendarische Periodenlänge <plkal> verwendet!
  y.pl <- aggregate(stratum$plkal,by=list(stratum$tnr),mean,na.rm=T)$x*ne.T$n.te
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
        #Baumartenfläche zur Periodenmitte aggregieren
        baf.ba <- subset(mbaf.bagr.akl.dkl.tnr,
                      bagr==bagr.list[i]&akl.pm==akl.lab[j]&dkl.pm==dkl.lab[k],
                      select=c(tnr,mbaf))
        #Mit allen Trakten im Inventurgebiet vereinen
        xy.baf <- merge(trakte,baf.ba,by=c("tnr"),all.x=T)
        #NA eliminieren!
        xy.baf$mbaf[is.na(xy.baf$mbaf)] <- 0
        #xy.baf$mbaf <- xy.baf$mbaf
        #xy. <- cbind(xy$m,xy$mbaf/10000)
        #Total der Baumartenfläche
        R.list <- r.variance.fun(cbind(xy.baf$m,xy.baf$mbaf),nT)
        BAF.bagr.akl.dkl[1,i,j,k] <-  R.list$R.xy*A
        BAF.bagr.akl.dkl[2,i,j,k] <-  sqrt(R.list$V.R.xy)*A

        for (i.n in 1:n.nart)  #Nutzungsart (geerntet, ungenutzt), wenn gesetzt!
        {

          baeume.ba <- subset(baeume.s,
              pk%in%pk.list[[i.n]]&bagr==bagr.list[i]&akl==akl.lab[j]
              &dkl==dkl.lab[k],select=c(tnr,enr,pk,volv2,vole2,oib2,nha1,plkal))
          if (length(baeume.ba[,1])== 0)
          {
             Y.na.bagr.akl.dkl[,1,i.n,i,j,k] <- rep(0,12) #Zielgröße
             Y.na.bagr.akl.dkl[,2,i.n,i,j,k] <- rep(0,12) #Stichprobenfehler (SE)

             mPL.na.bagr.akl.dkl[1:2,i.n,i,j,k]   <- rep(0,2)
             nT.na.bagr.akl.dkl[i.n,i,j,k]        <- 0        #n PSU (Trakte)
          }else
          {
            #Nach Trakt aggregieren
            #fortgeschrieben: volv2, vole2, oib2!!!!
            #Ausgeschiedener Derbholz-Vorrat [m³ mR] als "v"
            xy <- aggregate(baeume.ba$volv2*baeume.ba$nha1,by=list(baeume.ba$tnr),
                  sum)
            names(xy) <- c("tnr","v")
            #Ausgeschiedener Vorrat Erntevolumen [m³ oR] als "v.eor"
            xy <- cbind(xy,
              aggregate(baeume.ba$vole2*baeume.ba$nha1,
                                              by=list(baeume.ba$tnr),sum)$x )
            names(xy)[3] <- "v.eor"
            #Ausgeschiedener Vorrat in oberird. Biomasse [t] als "b"
            xy <- cbind(xy,aggregate(baeume.ba$oib2*baeume.ba$nha1,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
            names(xy)[4] <- "b"
            #Anzahl Bäume als "n"
            xy <- cbind(xy,aggregate(baeume.ba$nha1,by=list(baeume.ba$tnr),
                        sum)$x)
            names(xy)[5] <- "n"
            #Mittlere kal. Periodenlänge je Trakt
            mpl <- aggregate(baeume.ba$plkal,by=list(baeume.ba$tnr),mean)$x

            #Jährlicher ausgeschiedener Derbholzvorrat
            xy$j.v <- xy$v/mpl
            #Jährlicher ausgeschiedener Erntevorrat
            xy$j.v.eor <- xy$v.eor/mpl
            #Jährlicher ausgeschiedener oi. Biomassevorrat
            xy$j.b <- xy$b/mpl
            #Jährliche ausgeschiedene Stammzahl
            xy$j.n <- xy$n/mpl

            #Anzahl Traktecken je Trakt (Wald- und Nichtwald) und <mbaf>
            #hinzufügen   Hinweis: <xy.baf> enthält jetzt auch <m_bhb>!
            #xy <- merge(xy,xy.baf,by=c("tnr"))
            xy <- merge(xy.baf,xy,by=c("tnr"),all.x=T)
            #for (ii in 7:14) {xy[is.na(xy[,ii]),ii] <- 0}
            xy[is.na(xy)] <- 0

            #Anzahl Trakte (i.S. von PSU) im Teilkollektiv i.n,i,j,k
            nT.na.bagr.akl.dkl[i.n,i,j,k] <- length(xy[,1])

            #mittlere kal. Periodenlänge mit Standard-Fehler
            #Anzahl Ecken je Trakt bestimmen
            nb.TE <- aggregate(rep(1,length(baeume.ba[,1])),
                                    by=list(baeume.ba$tnr,baeume.ba$enr),sum)
            ne.T <- aggregate(rep(1,length(nb.TE[,1])),by=list(nb.TE$Group.1),
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

            for (l in 1:8)  #4 Totale, 4 Jährliche Totale
            {
              #Zielgrößen Y ausgeschiedenes Kollektiv{V,V.EoR,B,N)
              #Perioden-Total, jährl. Total, jährl. Ha-Wert (kä/01-03-2014)
              R.list <- r.variance.fun(cbind(xy$m,xy[,(l+t.pos+1)]),nT)
              Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- R.list$R.xy*A
              #Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- sum(xy[,(1+l)])/sum(x)*A
              #Zugehöriger Stichprobenfehler
              Y.na.bagr.akl.dkl[l,2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)*A
            }#End for l (Zielgrößen)
            #Offset für Spalten-Position der 4 jährliche Ha-Werte
            off <- length(xy)-4
            #Flächenbezogene Zielgrößen:
            for (l in 1:4) #4 jährliche Ha-Werte
            {
              #Zielgrößen Y ausgeschiedenes Kollektiv{V,V.EoR,B,N)
              #Perioden-Total, jährl. Total, jährl. Ha-Wert (kä/01-03-2014)
              R.list <- r.variance.fun(cbind(xy$mbaf,xy[,(l+off)]),nT)
              Y.na.bagr.akl.dkl[(l+8),1,i.n,i,j,k] <- R.list$R.xy
              #Zugehöriger Stichprobenfehler
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
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14
  
  #Tabelle für BA-Gruppen
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
              Größen = c("Wert","Standardfehler"),
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
#'  \code{\link{VB.A.BAGR.akl.dkl.stratum.fun}}, welche das Nutzungsgeschehen 
#'  auf der bei der BWI 2 erfassten Flaeche abdeckt!
#'  Voraussetzung ist, dass die Tabellen \code{baeume.23}, \code{baeume.3}, 
#'  \code{ecken.2}, \code{ecken.3}, \code{trakte.3} sowie \code{bacode} 
#'  eingelesen sind! \cr
#'  Fuer die Berechnung der flaechenbezogenen Nutzungen muss die mittlere 
#'  Baumartenflaeche der Periode berechnet werden, d.h. es werden auch die 
#'  Standflaechen der Folgeaufnahme benoetigt! Dies wird von der Funktion 
#'  \code{\link{mbaf.bagr.alt.bhd.pm.fun}} übernommen! \cr
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
#'  berechneten Attributen), \strong{Größen} (Vektor mit berechneten Groessen 
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
  #kä/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
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
  ecken.23.hb <- merge(subset(ecken.3.s, select=c(TNr,ENr)),
      subset(ecken.2.s,select=c(TNr,ENr)),by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,subset(ecken.3,select=c(TNr,ENr,PL,PLkal)),
      by=c("TNr","ENr"))
  stratum <- ecken.23.hb
  trakte <- trakte.3
  t.pos <- length(trakte)#Anzahl Spalten in <trakte> wird benötigt, um
  #Attribut-Positionen zu bestimmen
  #--------------------
  #inv <- 1
  #stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  #<y> steht hier für die Anzahl der Traktecken auf begehbarem HB im Stratum
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Anzahl der Trakte im Stratum
  n.t.s <- length(y[,1])
  #Anfügen der Anzahl Traktecken (Wald und Nicht-Wald)
  y <- merge(y,subset(trakte,select=c(tnr,m),by=c(tnr)))
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
  #Hier könnte als Alternative die Funktion <r.variance.fun> benutzt werden
  #Hierzu müsste eine alle Trakte umfassende Matrix <xy> mit <m> und <y>
  #übergeben werden
  #----------------
  #kä/28.02.2014:
  baeume <- baeume.23 #muss eingelesen sein!
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(baeume) <- tolower(names(baeume))

  #HINWEIS: beim ausgeschiedenen Vorrat wird der zur Periodenmitte
  #fortgeschriebene Vorrat verwendet! volv2,vole2,oib2
  baeume.s <- merge(
          subset(baeume,select=c(tnr,enr,stp,bnr,ba,pk,alt1,alt2,bhd1,bhd2,volv2,
                                  vole2,oib2,nha1,stfl1)),
          subset(stratum,select=c(tnr,enr,pl,plkal)),by=c("tnr","enr"))

  #BA-Gruppe dazu spielen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  n.bagr <- length(BA.grupp[[1]]) + 1 #kä/15.12.14
  #BAGR-Liste
  bagr.list <- c(BA.grupp[[1]],"Alle BA")
  #n.bagr <- length(bagr.list)

  baeume.s <- merge(baeume.s, subset(bagr.tab,select=c(ICode,bagr)),
                    by.x="ba",by.y="ICode",all.x=T)
  names(baeume.s) <- tolower(names(baeume.s))

  #Folgeinventur: BA-Gruppen hinzufügen
  baeume.3.s <- merge(subset(baeume.3,select=c(TNr,ENr,STP,BNr,Pk,BA,Alt1,Alt2,
                  BHD1,BHD2,StFl2)),subset(bagr.tab,select=c(ICode,bagr)),
                    by.x="BA",by.y="ICode",all.x=T)
  names(baeume.3.s) <- tolower(names(baeume.3.s))
  baeume.3.s <- merge(baeume.3.s,subset(stratum,select=c(tnr,enr,pl,plkal)),
          by=c("tnr","enr"))

  #--------------------------------------------------
  #Mittlere Baumartenflächen nach BAGr, AKl und DKl zur Periodenmitte nach Trakt
  mbaf.bagr.akl.dkl.tnr <- mbaf.bagr.alt.bhd.pm.fun(
                                    baeume.s,baeume.3.s,A.klass,D.klass)

  #-------------
  #Klassifizierung des Ausgeschiedenen Vorrats durchführen
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

  #Array für mittlere BAF zur PM nach BAGr, AKl, DKl
  BAF.bagr.akl.dkl  <- array(dim=c(2,n.bagr,A.k,D.k))

  if(N.art)
    {pk.list <- list(c(2,3,9),c(4,5));n.nart <- 2}
  else
    {pk.list <- list(c(2:5,9)); n.nart <- 1}
  #Array für Ergebnisse (Totals und SE jeweils nach Nutzungsart, BAGr, AKl, DKl)
  #Nutzungsart: aus <pk>: 2 = selektiv genutzt,  3 = flächig genutzt
  #                       4 = am Ort verblieben, 5 = abgestorben  (stehend)
  #                       9 = unauffindbar (wird der Kategorie geerntet
  #                           zugewiesen)
  #                       2,3,9 definieren den geernteten ausgeschiedenen Vorrat
  #                       4, 5 definieren den ungenutzten (tw. aus natürl. Mort.
  #                       stammenden) ungenutzten ausgeschiedenen Vorrat)
  #Aus diesen Kennzahlen werden die 2 Kategorien der Nutzungsart (NArt):
  #geerntet bzw. ungenutzt festgelegt.
  #Es gibt 4 Zielgrößen <Y>:  V [m³Dh mR], V Eor (Erntevolumen o. R.) [m³E oR],
  #B (oberird. Biomasse) [t], V.EoR im HB, für die jeweils der Gesamtwert der
  #Periode ("Total") und der jährliche Wert berec (neu!)hnet wird, sowie der
  #Stichprobenfehler (SE), und zwar jeweils für die 2 Kategorien "geerntet" /
  #"ungenutzt" sowie <n.bagr> Baumartengruppen, A.k Alters- und D.k
  #Durchmesserklassen:
  #1. Index: 1- 4: Perioden-Total (v, v.eor, b, v.eor.hb);
  #          4- 8: m. jährlicher Wert (j.v, j.v.eor, j.b, j.v.eor.hb);
  #          9-12: m. jährlicher Wert je ha     (kä/28.02.2014)
  #2. Index: 1: Wert; 2: SE;
  #3. Index: Nutzungsart (wenn <N.art> == TRUE (1: geerntet, 2: ungenutzt)
  #4. Index: BAGr; 5. Index: A-Klasse; 6. Index: D-Klasse
  Y.na.bagr.akl.dkl    <- array(dim=c(12,2,n.nart,n.bagr,A.k,D.k))
  #mittlere kal. Periodenlänge mit Standard-Fehler
  mPL.na.bagr.akl.dkl   <- array(dim=c(2,2,n.bagr,A.k,D.k))
  #Anzahl Trakte (PSU) je NArt, BAGR, Akl, Dkl
  nT.na.bagr.akl.dkl   <- array(dim=c(n.nart,n.bagr,A.k,D.k))
  #Mittlere Straten-PL mit SE
  ne.T <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
            names(ne.T) <- c("tnr","n.te")
  #"Periodensumme" je Trakt (mit n Ecken gewogen)
  #Bei Nutzung wird kalendarische Periodenlänge <plkal> verwendet!
  y.pl <- aggregate(stratum$plkal,by=list(stratum$tnr),mean,na.rm=T)$x*ne.T$n.te
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
        #Baumartenfläche zur Periodenmitte aggregieren
        #kä/15.12.14
        if (i < n.bagr)
        {
          baf.ba <- subset(mbaf.bagr.akl.dkl.tnr,
                        bagr==bagr.list[i]&akl.pm==akl.lab[j]&dkl.pm==dkl.lab[k],
                        select=c(tnr,mbaf,mbaf.hb))
                
        } else
        {
          baf.ba <- subset(mbaf.bagr.akl.dkl.tnr,
                        akl.pm==akl.lab[j]&dkl.pm==dkl.lab[k],
                        select=c(tnr,mbaf,mbaf.hb))
          baf.t <- aggregate(baf.ba$mbaf,by=list(baf.ba$tnr),sum)
          baf.t <- cbind(baf.t,aggregate(baf.ba$mbaf.hb,by=list(baf.ba$tnr),sum)$x)
          names(baf.t) <- names(baf.ba)
          baf.ba <- baf.t
        }#*
        #Mit allen Trakten im Inventurgebiet vereinen
        xy.baf <- merge(trakte,baf.ba,by=c("tnr"),all.x=T)
        #NA eliminieren!
        xy.baf[is.na(xy.baf)] <- 0
        #xy.baf$mbaf <- xy.baf$mbaf
        #xy. <- cbind(xy$m,xy$mbaf/10000)
        #Total der Baumartenfläche
        R.list <- r.variance.fun(cbind(xy.baf$m,xy.baf$mbaf),nT)
        BAF.bagr.akl.dkl[1,i,j,k] <-  R.list$R.xy*A
        BAF.bagr.akl.dkl[2,i,j,k] <-  sqrt(R.list$V.R.xy)*A

        for (i.n in 1:n.nart)   #Nutzungsart (geerntet, ungenutzt), wenn gesetzt!
        {

          #Kä 15.12.14
          if (i < n.bagr)
          {
            baeume.ba <- subset(baeume.s,
                pk%in%pk.list[[i.n]]&bagr==bagr.list[i]&akl==akl.lab[j]
                &dkl==dkl.lab[k],select=c(tnr,enr,pk,volv2,vole2,oib2,nha1,stfl1,
                plkal))
          } else
          {
            baeume.ba <- subset(baeume.s,
                pk%in%pk.list[[i.n]]&akl==akl.lab[j]
                &dkl==dkl.lab[k],select=c(tnr,enr,pk,volv2,vole2,oib2,nha1,stfl1,
                plkal))
          } #*
          if (length(baeume.ba[,1])== 0)
          {
             Y.na.bagr.akl.dkl[,1,i.n,i,j,k] <- rep(0,12) #Zielgröße
             Y.na.bagr.akl.dkl[,2,i.n,i,j,k] <- rep(0,12) #Stichprobenfehler SE)

             mPL.na.bagr.akl.dkl[1:2,i.n,i,j,k]   <- rep(0,2)
             nT.na.bagr.akl.dkl[i.n,i,j,k]        <- 0        #n PSU (Trakte)
          }else
          {
            #Nach Trakt aggregieren
            #fortgeschrieben: volv2, vole2, oib2!!!!
            #Ausgeschiedener Derbholz-Vorrat [m³ mR] als "v"
            xy <- aggregate(baeume.ba$volv2*baeume.ba$nha1,
                            by=list(baeume.ba$tnr),sum)
            names(xy) <- c("tnr","v")
            #Ausgeschiedener Vorrat Erntevolumen [m³ oR] als "v.eor"
            xy <- cbind(xy,
              aggregate(baeume.ba$vole2*baeume.ba$nha1,
                                              by=list(baeume.ba$tnr),sum)$x )
            names(xy)[3] <- "v.eor"
            #Ausgeschiedener Vorrat in oberird. Biomasse [t] als "b"
            xy <- cbind(xy,aggregate(baeume.ba$oib2*baeume.ba$nha1,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
            names(xy)[4] <- "b"
            #Ausgeschiedener Vorrat Erntevolumen im Hauptbestand (01.05.14)
            xy <- cbind(xy,aggregate(baeume.ba$vole2*baeume.ba$nha1*
                                      ifelse(baeume.ba$stfl1>0,1,0),
                        by=list(baeume.ba$tnr),sum)$x)
            names(xy)[5] <- "v.eor.hb"
            #Mittlere kal. Periodenlänge je Trakt
            mpl <- aggregate(baeume.ba$plkal,by=list(baeume.ba$tnr),mean)$x

            #Jährlicher ausgeschiedener Derbholzvorrat
            xy$j.v <- xy$v/mpl
            #Jährlicher ausgeschiedener Erntevorrat
            xy$j.v.eor <- xy$v.eor/mpl
            #Jährlicher ausgeschiedener oi. Biomassevorrat
            xy$j.b <- xy$b/mpl
            #Jährliche ausgeschiedene Stammzahl
            xy$j.v.eor.hb <- xy$v.eor.hb/mpl

            #Anzahl Traktecken je Trakt (Wald- und Nichtwald) und <mbaf>
            #hinzufügen   Hinweis: <xy.baf> enthält jetzt auch <m_bhb>!
            #xy <- merge(xy,xy.baf,by=c("tnr"))
            xy <- merge(xy.baf,xy,by=c("tnr"),all.x=T)
            xy[is.na(xy)] <- 0

            #Anzahl Trakte (i.S. von PSU) im Teilkollektiv i.n,i,j,k
            nT.na.bagr.akl.dkl[i.n,i,j,k] <- length(xy[,1])

            #mittlere kal. Periodenlänge mit Standard-Fehler
            #Anzahl Ecken je Trakt bestimmen
            nb.TE <- aggregate(rep(1,length(baeume.ba[,1])),
                                    by=list(baeume.ba$tnr,baeume.ba$enr),sum)
            ne.T <- aggregate(rep(1,length(nb.TE[,1])),by=list(nb.TE$Group.1),
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

            for (l in 1:8)  #4 Totale, 4 Jährliche Totale
            {
              #Zielgrößen Y ausgeschiedenes Kollektiv{V,V.EoR,B,V.Eor.HB)
              #Perioden-Total, jährl. Total, jährl. Ha-Wert (kä/01-03-2014)
              #Attribut-Indizierung (l+t.pos+2) korrigiert (kä/08.07.14)
              R.list <- r.variance.fun(cbind(xy$m,xy[,(l+t.pos+2)]),nT)
              Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- R.list$R.xy*A
              #Y.na.bagr.akl.dkl[l,1,i.n,i,j,k] <- sum(xy[,(1+l)])/sum(x)*A
              #Zugehöriger Stichprobenfehler
              Y.na.bagr.akl.dkl[l,2,i.n,i,j,k] <- sqrt(R.list$V.R.xy)*A
            }#End for l (Zielgrößen)
            #Offset für Spalten-Position der 4 jährliche Ha-Werte
            off <- length(xy)-4
            #Flächenbezogene Zielgrößen:
            for (l in 1:4) #4 jährliche Ha-Werte
            {
              #Zielgrößen Y ausgeschiedenes Kollektiv{V,V.EoR,B,V.EoR.HB)
              #Perioden-Total, jährl. Total, jährl. Ha-Wert (kä/01-03-2014)
              #Wegen V.Eor.HB muss nach BAF mit BL bzw. OHNE BL getrennt werden
              if (l<4)
              {
                R.list <- r.variance.fun(cbind(xy$mbaf,xy[,(l+off)]),nT)
              } else  #V.EoR.HB
              {
                R.list <- r.variance.fun(cbind(xy$mbaf.hb,xy[,(l+off)]),nT)
              }
              Y.na.bagr.akl.dkl[(l+8),1,i.n,i,j,k] <- R.list$R.xy
              #Zugehöriger Stichprobenfehler
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
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14

  #Tabelle für BA-Gruppen
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
              Größen = c("Wert","Standardfehler"),
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
#'  \strong{Attribute} (Vektor mit berechneten Attributen), \strong{Größen} 
#'  (vektor mit berechneten Grroessen ("Wert", "Standardfehler")), \strong{BAGR} 
#'  (Labels fuer Baumartengruppen aus \code{ba.grupp}), \strong{AKL} (Labels der 
#'  Altersklassen), \strong{DKL} (Labels der Durchmesserklassen), 
#'  \strong{iVB.bagr.akl.dkl} (Zuwachs-Tabelle), \strong{VB.A.bagr.akl.dkl} 
#'  (Tabelle mit ausgeschiedenem Vorrat).
iVB.ew.bagrupp.akl.dkl.stratum.fun.2 <- function(baeume.23,baeume.3,
          BA.grupp,A.klass,D.klass,auswahl,A){
  #---
  #kä/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
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
      subset(ecken.3.s, select=c(TNr,ENr)),
      subset(ecken.2.s,select=c(TNr,ENr)),
      by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,subset(ecken.3,select=c(TNr,ENr,PL,PLkal)),
      by=c("TNr","ENr"))

  #-----------------------------------------------------------------------------
  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  
  #Erweiterung um "Alle BA"  kä/15.12.2014
  n.bagr <- length(BA.grupp[[1]]) + 1 #* +1
  BA.grupp$ba.grupp[[n.bagr]] <- c(10:299)    #*** eigentlich unnötig!!!
  BA.grupp$bagr.lab[[n.bagr]] <- "Alle BA"    #*** 
  bagr.list <- BA.grupp[[1]]
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)  #*** 
  #Aus Komaptibilitätsgründen wird Attribut-Name "bagr" auf "BaGr" geändert
  #(wegen Funktion <iVB.bilanz.bagr.akl.dkl.fun>
  names(bagr.tab)[3] <- "BaGr"

  #Baumartengruppen hinzufügen
  baeume.3.s <- merge(subset(baeume.3,select=c(TNr,ENr,STP,BNr,Entf,Pk,BA,
        Alt1,Alt2,BHD1,BHD2,D031,D032,H1,H2,VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,
        StFl2,NHa2)),subset(bagr.tab,select=c(ICode,BaGr)),by.x="BA",
        by.y="ICode",all.x=T)

  baeume.2.s <- merge(subset(baeume.23,select=c(TNr,ENr,STP,BNr,Entf,Pk,BA,
        Alt1,Alt2,BHD1,BHD2,D031,D032,H1,H2,VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,
        StFl1,NHa1)),subset(bagr.tab,select=c(ICode,BaGr)),by.x="BA",
        by.y="ICode",all.x=T)

  #Auf gemeinsames Netz reduzieren!
  baeume.3.s <- merge(baeume.3.s,ecken.23.hb,by=c("TNr","ENr"))
  baeume.2.s <- merge(baeume.2.s,ecken.23.hb,by=c("TNr","ENr"))

  #Zur Kontrolle
  sum(baeume.3.s$StFl2)/10000; sum(baeume.2.s$StFl1)/10000

  #Holzbodenfläche des Stratums
  #Nach TE
  hb.te <- aggregate(baeume.3.s$StFl2/10000,
                        by=list(baeume.3.s$TNr,baeume.3.s$ENr),sum)
  names(hb.te) <- c("TNr","ENr","m_HB_s")
  ecken <- merge(hb.te,subset(ecken.23.hb,select=c(TNr,ENr,PL,PLkal)),
                    by=c("TNr","ENr") )
  #Nach T
  hb.t <- aggregate(baeume.3.s$StFl2/10000,by=list(baeume.3.s$TNr),sum)
  names(hb.t) <- c("TNr","m_HB_s")
  hb.t.s <- merge(trakte,hb.t,by=c("TNr"),all.x=T)
  hb.t.s[is.na(hb.t.s)] <- 0
  r.list <- r.variance.fun(cbind(hb.t.s$m,hb.t.s$m_HB_s),nT)
  HBF <- r.list$R.xy*A
  se.HBF <- sqrt(r.list$V.R.xy)*A

  #-----------------------------------
  #Klassifizierung der Probebäume nach Alter und BHD zur Periodenmitte
  #BWI 2:
  #Alter und BHD zur Periodenmitte bestimmen (bei ausgeschiedenem Kollektiv
  #ist Alt2 und BHD2 in PM)
  baeume.2.s$bhd.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$BHD2,
      (baeume.2.s$BHD1+baeume.2.s$BHD2)/2)
  baeume.2.s$alt.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$Alt2,
      ifelse(baeume.2.s$BA<998,baeume.2.s$Alt1+baeume.2.s$PL/2,0))

  #BWI 3:
  #Alter und BHD zur Periodenmitte bestimmen (Hinweis nur relevant für
  #die bei der Zuwachsbilanzierung benötigten PB-Kategorien S- und E-Bäume)
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
  #Korrektur kä/09.02.2015
  #Aggregation alle Baumarten
  baeume.2.s$BaGr <- "AlleBA"
  baeume.3.s$BaGr <- "AlleBA"
  iv.es.a.t.alle.akl.dkl <- iVB.bilanz.bagr.akl.dkl.fun(baeume.2.s,baeume.3.s,
                                                       ecken)
  #++++

  #-----------------------------------------------------------------------------
  #Zuwachs
  #Tabelle anlegen mit den Dimensionen
  #1. 1-12: 4 Zielgrößen: V_DhmR, V_DhmR_HB, V_EoR, oiB in je 3 Varianten
  #     (1) Periodengesamtwert, (2) jährlicher Gesamtwert,
  #     (3) Ha-bezogener jährlicher Wert
  #     13: m. Baumartenfläche  mit Lückenkorrektur
  #     14: m. Baumartenfläche ohne Lückenkorr. für HB-Bäume
  #     15: m. Periodenlänge
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
  #1. 1-12: 4 Zielgrößen: V_A_DhmR, V_A_DhmR_HB, V_A_EoR, oiB_A in je
  #         3 Varianten
  #         (1) Periodengesamtwert, (2) jährlicher Gesamtwert,
  #         (3) Ha-bezogener jährlicher Wert  = 1:12
  #     13: m. kal. Periodenlänge
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

        #kä/15.12.2014
        if (i < n.bagr)  #*
        {
          iv.es.a.t <- subset(iv.es.a.t.bagr.akl.dkl,
                      BaGr==bagr.list[i]&Akl==akl.lab[j]&Dkl==dkl.lab[k])
        } else           #*
        { #Alle BA  09.02.2015: ...alle... für BaGr AlleBA
          iv.es.a.t <- subset(iv.es.a.t.alle.akl.dkl,
                      Akl==akl.lab[j]&Dkl==dkl.lab[k])
        } #*

        #Anzahl Trakte mit Beobachtungen
        iVB.bagr.akl.dkl[16,1,i,j,k] <- length(iv.es.a.t[,1])
        #-----------------------------------------------------------------------
        iv.bil.t <- subset(iv.es.a.t,select=
                c(TNr,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
                V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A))
        #Traktecken Wald/Nichtwald hinzufügen
        iv.bil.t <- merge(subset(trakte,select=c(TNr,m,m_HB)),iv.bil.t,
                          by=c("TNr"),all.x=T)
        #NA eliminieren
        iv.bil.t[is.na(iv.bil.t)] <- 0

        #head(iv.bil.t)
        #1,   2, 3,   4,   5,       6,  7,     8,      9,         10,    11,
        #TNr, m, m_HB,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
        #12,      13,         14,     15
        #V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A
        

        iii <- 0  #Index für Array <iVB.bagr.akl.dkl>
        iiii <- 0 #Index für Array <VB.A.bagr.akl.dkl>

        for (ii in 1:3)   #Aggregationsebenen: Gesamtperiodenwert,
                          #jährlicher Gesamtwert, Ha-bezogener jährl. Wert
        {
          for (jj in 8:11) #Spaltenpositionen der 4 Zielgrößen iV.DhmR,
                          #iV.DhmR.HB,
                          #iV.EoR, iB
          {
            iii <- iii + 1
            #Bezugsgröße x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: Größen
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==9) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielgöße y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPL
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,jj]/iv.bil.t[,6],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            iVB.bagr.akl.dkl[iii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            iVB.bagr.akl.dkl[iii,2,i,j,k]<- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
          #Auswertung für ausgeschiedenen Vorrat (kä/11.04.2014)

          for (jj in 12:15) #Spaltenpositionen der 4 Zielgrößen V.DhmR.A,
                            #V.DhmR.HB.A, V.EoR.A, B.A
          {
            iiii <- iiii + 1
            #Bezugsgröße x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: Größen
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==13) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielgöße y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPLkal
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
        #Mittlere BAF ohne Lückenkorrektur je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,5]),nT)
        iVB.bagr.akl.dkl[14,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[14,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere Periodenlänge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        #Hinweis: es sind weitere Mittelbildungen denkbar; z.B. mit der
        #mBAF gewogenen PL
        x <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,6]),nT)
        iVB.bagr.akl.dkl[15,1,i,j,k] <- R.list$R.xy
        iVB.bagr.akl.dkl[15,2,i,j,k] <- sqrt(R.list$V.R.xy)
        #Mittlere kalendarische Periodenlänge je BAGR, Akl, Dkl
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
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14
  
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
              Größen=c("Wert","Standardfehler"),
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
#'  \strong{Attribute} (Vektor mit berechneten Attributen), \strong{Größen} 
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
      subset(ecken.3.s, select=c(TNr,ENr)),
      subset(ecken.2.s,select=c(TNr,ENr)),
      by=c("TNr","ENr"))
  #Anpassung BWI 2    <ecken.2> statt <ecken.3>
  ecken.23.hb <- merge(ecken.23.hb,subset(ecken.2,select=c(TNr,ENr,PL,PLkal)),
      by=c("TNr","ENr"))

  #-----------------------------------------------------------------------------
  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  n.bagr <- length(BA.grupp[[1]])
  bagr.list <- BA.grupp[[1]]
  #Aus Komaptibilitätsgründen wird Attribut-Name "bagr" auf "BaGr" geändert
  #(wegen Funktion <iVB.bilanz.bagr.akl.dkl.fun>
  names(bagr.tab)[3] <- "BaGr"

  #Baumartengruppen hinzufügen
  baeume.3.s <- merge(subset(baeume.3,select=c(TNr,ENr,STP,BNr,Entf,Pk,BA,
        Alt1,Alt2,BHD1,BHD2,D031,D032,H1,H2,VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,
        StFl2,NHa2)),subset(bagr.tab,select=c(ICode,BaGr)),by.x="BA",
        by.y="ICode",all.x=T)

  baeume.2.s <- merge(subset(baeume.23,select=c(TNr,ENr,STP,BNr,Entf,Pk,BA,
        Alt1,Alt2,BHD1,BHD2,D031,D032,H1,H2,VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,
        StFl1,NHa1)),subset(bagr.tab,select=c(ICode,BaGr)),by.x="BA",
        by.y="ICode",all.x=T)

  #Auf gemeinsames Netz reduzieren!
  baeume.3.s <- merge(baeume.3.s,ecken.23.hb,by=c("TNr","ENr"))
  baeume.2.s <- merge(baeume.2.s,ecken.23.hb,by=c("TNr","ENr"))

  #Zur Kontrolle
  sum(baeume.3.s$StFl2)/10000; sum(baeume.2.s$StFl1)/10000

  #Holzbodenfläche des Stratums
  #Nach TE
  hb.te <- aggregate(baeume.3.s$StFl2/10000,
                        by=list(baeume.3.s$TNr,baeume.3.s$ENr),sum)
  names(hb.te) <- c("TNr","ENr","m_HB_s")
  ecken <- merge(hb.te,subset(ecken.23.hb,select=c(TNr,ENr,PL,PLkal)),
                    by=c("TNr","ENr") )
  #Nach T
  hb.t <- aggregate(baeume.3.s$StFl2/10000,by=list(baeume.3.s$TNr),sum)
  names(hb.t) <- c("TNr","m_HB_s")
  hb.t.s <- merge(trakte,hb.t,by=c("TNr"),all.x=T)
  hb.t.s[is.na(hb.t.s)] <- 0
  r.list <- r.variance.fun(cbind(hb.t.s$m,hb.t.s$m_HB_s),nT)
  HBF <- r.list$R.xy*A
  se.HBF <- sqrt(r.list$V.R.xy)*A

  #-----------------------------------
  #Klassifizierung der Probebäume nach Alter und BHD zur Periodenmitte
  #BWI 2:
  #Alter und BHD zur Periodenmitte bestimmen (bei ausgeschiedenem Kollektiv
  #ist Alt2 und BHD2 in PM)
  baeume.2.s$bhd.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$BHD2,
      (baeume.2.s$BHD1+baeume.2.s$BHD2)/2)
  baeume.2.s$alt.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$Alt2,
      ifelse(baeume.2.s$BA<998,baeume.2.s$Alt1+baeume.2.s$PL/2,0))

  #BWI 3:
  #Alter und BHD zur Periodenmitte bestimmen (Hinweis nur relevant für
  #die bei der Zuwachsbilanzierung benötigten PB-Kategorien S- und E-Bäume)
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
  #1. 1-12: 4 Zielgrößen: V_DhmR, V_DhmR_HB, V_EoR, oiB in je 3 Varianten
  #     (1) Periodengesamtwert, (2) jährlicher Gesamtwert,
  #     (3) Ha-bezogener jährlicher Wert
  #     13: m. Baumartenfläche  mit Lückenkorrektur
  #     14: m. Baumartenfläche ohne Lückenkorr. für HB-Bäume
  #     15: m. Periodenlänge
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
  #1. 1-12: 4 Zielgrößen: V_A_DhmR, V_A_DhmR_HB, V_A_EoR, oiB_A in je
  #         3 Varianten
  #         (1) Periodengesamtwert, (2) jährlicher Gesamtwert,
  #         (3) Ha-bezogener jährlicher Wert  = 1:12
  #     13: m. kal. Periodenlänge
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

        iv.es.a.t <- subset(iv.es.a.t.bagr.akl.dkl,
                      BaGr==bagr.list[i]&Akl==akl.lab[j]&Dkl==dkl.lab[k])

        #Anzahl Trakte mit Beobachtungen
        iVB.bagr.akl.dkl[16,1,i,j,k] <- length(iv.es.a.t[,1])
        #-----------------------------------------------------------------------
        iv.bil.t <- subset(iv.es.a.t,select=
                c(TNr,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
                V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A))
        #Traktecken Wald/Nichtwald hinzufügen
        iv.bil.t <- merge(subset(trakte,select=c(TNr,m,m_HB)),iv.bil.t,
                          by=c("TNr"),all.x=T)
        #NA eliminieren
        iv.bil.t[is.na(iv.bil.t)] <- 0

        #head(iv.bil.t)
        #1,   2, 3,   4,   5,       6,  7,     8,      9,         10,    11,
        #TNr, m, m_HB,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
        #12,      13,         14,     15
        #V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A
        

        iii <- 0  #Index für Array <iVB.bagr.akl.dkl>
        iiii <- 0 #Index für Array <VB.A.bagr.akl.dkl>

        for (ii in 1:3)   #Aggregationsebenen: Gesamtperiodenwert,
                          #jährlicher Gesamtwert, Ha-bezogener jährl. Wert
        {
          for (jj in 8:11) #Spaltenpositionen der 4 Zielgrößen iV.DhmR,
                          #iV.DhmR.HB,
                          #iV.EoR, iB
          {
            iii <- iii + 1
            #Bezugsgröße x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: Größen
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==9) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielgöße y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPL
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,jj]/iv.bil.t[,6],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            iVB.bagr.akl.dkl[iii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            iVB.bagr.akl.dkl[iii,2,i,j,k]<- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
          #Auswertung für ausgeschiedenen Vorrat (kä/11.04.2014)

          for (jj in 12:15) #Spaltenpositionen der 4 Zielgrößen V.DhmR.A,
                            #V.DhmR.HB.A, V.EoR.A, B.A
          {
            iiii <- iiii + 1
            #Bezugsgröße x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: Größen
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==13) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielgöße y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPLkal
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
        #Mittlere BAF ohne Lückenkorrektur je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,5]),nT)
        iVB.bagr.akl.dkl[14,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[14,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere Periodenlänge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        #Hinweis: es sind weitere Mittelbildungen denkbar; z.B. mit der
        #mBAF gewogenen PL
        x <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,6]),nT)
        iVB.bagr.akl.dkl[15,1,i,j,k] <- R.list$R.xy
        iVB.bagr.akl.dkl[15,2,i,j,k] <- sqrt(R.list$V.R.xy)
        #Mittlere kalendarische Periodenlänge je BAGR, Akl, Dkl
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
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14
  
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
              Größen=c("Wert","Standardfehler"),
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
#'  Echter Einwuchs wird getrennt ausgewiesen, ausschließlich nach dem Kriterium 
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
#'  \strong{Attribute} (Vektor mit berechneten Attributen), \strong{Größen} 
#'  (vektor mit berechneten Grroessen ("Wert", "Standardfehler")), \strong{BAGR} 
#'  (Labels fuer Baumartengruppen aus \code{ba.grupp}), \strong{AKL} (Labels der 
#'  Altersklassen), \strong{DKL} (Labels der Durchmesserklassen), 
#'  \strong{iVB.bagr.akl.dkl} (Zuwachs-Tabelle), \strong{VB.A.bagr.akl.dkl} 
#'  (Tabelle mit ausgeschiedenem Vorrat).
iVB.ew.bagrupp.akl.dkl.stratum.fun.2g <- function(baeume.23,baeume.3,
          BA.grupp,A.klass,D.klass,auswahl,A){
  #---
  #kä/24.08.2014: Bei Stratifikation anhand von Eckenmerkmalen gilt die
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
      subset(ecken.3.s, select=c(TNr,ENr)),
      subset(ecken.2.s,select=c(TNr,ENr)),
      by=c("TNr","ENr"))
  ecken.23.hb <- merge(ecken.23.hb,subset(ecken.3,select=c(TNr,ENr,PL,PLkal)),
      by=c("TNr","ENr"))

  #-----------------------------------------------------------------------------
  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)

  #Erweiterung um "Alle BA"  kä/15.12.2014
  n.bagr <- length(BA.grupp[[1]]) + 1 #* +1
  BA.grupp$ba.grupp[[n.bagr]] <- c(10:299)    #*** eigentlich unnötig!!!
  BA.grupp$bagr.lab[[n.bagr]] <- "Alle BA"    #***
  bagr.list <- BA.grupp[[1]]
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)  #***
  #Aus Komaptibilitätsgründen wird Attribut-Name "bagr" auf "BaGr" geändert
  #(wegen Funktion <iVB.bilanz.bagr.akl.dkl.fun.2g>
  names(bagr.tab)[3] <- "BaGr"

  #Baumartengruppen hinzufügen
  baeume.3.s <- merge(subset(baeume.3,select=c(TNr,ENr,STP,BNr,Entf,Pk,BA,
        Alt1,Alt2,BHD1,BHD2,D031,D032,H1,H2,VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,
        StFl2,NHa2)),subset(bagr.tab,select=c(ICode,BaGr)),by.x="BA",
        by.y="ICode",all.x=T)

  baeume.2.s <- merge(subset(baeume.23,select=c(TNr,ENr,STP,BNr,Entf,Pk,BA,
        Alt1,Alt2,BHD1,BHD2,D031,D032,H1,H2,VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,
        StFl1,NHa1)),subset(bagr.tab,select=c(ICode,BaGr)),by.x="BA",
        by.y="ICode",all.x=T)

  #Auf gemeinsames Netz reduzieren!
  baeume.3.s <- merge(baeume.3.s,ecken.23.hb,by=c("TNr","ENr"))
  baeume.2.s <- merge(baeume.2.s,ecken.23.hb,by=c("TNr","ENr"))

  #Zur Kontrolle
  sum(baeume.3.s$StFl2)/10000; sum(baeume.2.s$StFl1)/10000

  #Holzbodenfläche des Stratums
  #Nach TE
  hb.te <- aggregate(baeume.3.s$StFl2/10000,
                        by=list(baeume.3.s$TNr,baeume.3.s$ENr),sum)
  names(hb.te) <- c("TNr","ENr","m_HB_s")
  ecken <- merge(hb.te,subset(ecken.23.hb,select=c(TNr,ENr,PL,PLkal)),
                    by=c("TNr","ENr") )
  #Nach T
  hb.t <- aggregate(baeume.3.s$StFl2/10000,by=list(baeume.3.s$TNr),sum)
  names(hb.t) <- c("TNr","m_HB_s")
  hb.t.s <- merge(trakte,hb.t,by=c("TNr"),all.x=T)
  hb.t.s[is.na(hb.t.s)] <- 0
  r.list <- r.variance.fun(cbind(hb.t.s$m,hb.t.s$m_HB_s),nT)
  HBF <- r.list$R.xy*A
  se.HBF <- sqrt(r.list$V.R.xy)*A

  #-----------------------------------
  #Klassifizierung der Probebäume nach Alter und BHD zur Periodenmitte
  #BWI 2:
  #Alter und BHD zur Periodenmitte bestimmen (bei ausgeschiedenem Kollektiv
  #ist Alt2 und BHD2 in PM)
  baeume.2.s$bhd.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$BHD2,
      (baeume.2.s$BHD1+baeume.2.s$BHD2)/2)
  baeume.2.s$alt.pm <- ifelse(baeume.2.s$Pk%in%c(2:5,9),baeume.2.s$Alt2,
      ifelse(baeume.2.s$BA<998,baeume.2.s$Alt1+baeume.2.s$PL/2,0))

  #BWI 3:
  #Alter und BHD zur Periodenmitte bestimmen (Hinweis nur relevant für
  #die bei der Zuwachsbilanzierung benötigten PB-Kategorien S- und E-Bäume)
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
  #Korrektur kä/27.01.2015
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
  #1. 1-15: 5 Zielgrößen: V_DhmR, V_DhmR_HB, V_EoR, oiB, G in je 3 Varianten
  #     (1) Periodengesamtwert, (2) jährlicher Gesamtwert,
  #     (3) Ha-bezogener jährlicher Wert
  #     16: m. Baumartenfläche  mit Lückenkorrektur
  #     17: m. Baumartenfläche ohne Lückenkorr. für HB-Bäume
  #     18: m. Periodenlänge
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
  #1. 1-15: 4 Zielgrößen: V_A_DhmR, V_A_DhmR_HB, V_A_EoR, oiB_A, G_A in je
  #         3 Varianten
  #         (1) Periodengesamtwert, (2) jährlicher Gesamtwert,
  #         (3) Ha-bezogener jährlicher Wert  = 1:12
  #     16: m. kal. Periodenlänge
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

        #kä/15.12.2014
        if (i < n.bagr)  #*
        {
          iv.es.a.t <- subset(iv.es.a.t.bagr.akl.dkl,
                      BaGr==bagr.list[i]&Akl==akl.lab[j]&Dkl==dkl.lab[k])
        } else           #*
        { #Alle BA   kä/27.01.2015
          iv.es.a.t <- subset(iv.es.a.t.alle.akl.dkl,
                      Akl==akl.lab[j]&Dkl==dkl.lab[k])
        }

        #Anzahl Trakte mit Beobachtungen
        #+++ Dim 16 -> 19
        iVB.bagr.akl.dkl[19,1,i,j,k] <- length(iv.es.a.t[,1])
        #-----------------------------------------------------------------------
        iv.bil.t <- subset(iv.es.a.t,select=
                c(TNr,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,iG,
                V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A,G.A))
        #Traktecken Wald/Nichtwald hinzufügen
        iv.bil.t <- merge(subset(trakte,select=c(TNr,m,m_HB)),iv.bil.t,
                          by=c("TNr"),all.x=T)
        #NA eliminieren
        iv.bil.t[is.na(iv.bil.t)] <- 0

        #head(iv.bil.t)
        #1,   2, 3,   4,   5,       6,  7,     8,      9,         10,    11,
        #TNr, m, m_HB,mBAF,mBAF.oLK,mPL,mPLkal,iV.DhmR,iV.DhmR.HB,iV.EoR,iB,
        #12, 13,      14,         15      16   17
        #iG, V.DhmR.A,V.DhmR.HB.A,V.EoR.A,B.A, G.A


        iii <- 0  #Index für Array <iVB.bagr.akl.dkl>
        iiii <- 0 #Index für Array <VB.A.bagr.akl.dkl>

        for (ii in 1:3)   #Aggregationsebenen: Gesamtperiodenwert,
                          #jährlicher Gesamtwert, Ha-bezogener jährl. Wert
        {
          for (jj in 8:12) #Spaltenpositionen der 5 Zielgrößen iV.DhmR,
                          #iV.DhmR.HB,
                          #iV.EoR, iB
                          #iG
          {
            iii <- iii + 1
            #Bezugsgröße x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: Größen
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB, 12: iG: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            {if(jj==9) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielgöße y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPL
            if(ii < 2) {y <- iv.bil.t[,jj]} else
              {y <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,jj]/iv.bil.t[,6],0)}
            R.list <- r.variance.fun(cbind(x,y),nT)
            iVB.bagr.akl.dkl[iii,1,i,j,k] <- R.list$R.xy*ifelse(ii<3,A,1)
            iVB.bagr.akl.dkl[iii,2,i,j,k]<- sqrt(R.list$V.R.xy)*ifelse(ii<3,A,1)
          }
          #Auswertung für ausgeschiedenen Vorrat (kä/11.04.2014)

          for (jj in 13:17) #Spaltenpositionen der 5 Zielgrößen V.DhmR.A,
                            #V.DhmR.HB.A, V.EoR.A, B.A, G.A
          {
            iiii <- iiii + 1
            #Bezugsgröße x: bei ii = 1 oder 2 (Gesamtwerte): Anzahl Ecken je
            #Takt (m); bei ii = 3 (Hektar.Bezug) Fallunterscheidung: Größen
            #jj = 8: iV.DmR, 10:iV.Eor, 11: iB, 12: iG: m. BAF m. LK;
            #bei jj=9: iV.DmR.HB: m. BAF o. LK
            if(ii < 3) {x <- iv.bil.t[,2]}  else
            #+++ Dim 13 -> 14
            {if(jj==14) {x <- iv.bil.t[,5]} else {x <- iv.bil.t[,4]}}
            #Zielgöße y: ii = 1: Total; ii = 2 oder 3: Zeitbezug mPLkal
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
        #Mittlere BAF ohne Lückenkorrektur je BAGR, Akl, Dkl
        R.list <- r.variance.fun(cbind(iv.bil.t[,2],iv.bil.t[,5]),nT)
        #+++ Dim 14 -> 17
        iVB.bagr.akl.dkl[17,1,i,j,k] <- R.list$R.xy*A
        iVB.bagr.akl.dkl[17,2,i,j,k] <- sqrt(R.list$V.R.xy)*A
        #Mittlere Periodenlänge je BAGR, Akl, Dkl
        #Mit Anzahl der TE auf HB gewogenes Mittel
        #Hinweis: es sind weitere Mittelbildungen denkbar; z.B. mit der
        #mBAF gewogenen PL
        x <- ifelse(iv.bil.t[,6]>0,iv.bil.t[,3],0)
        R.list <- r.variance.fun(cbind(x,iv.bil.t[,3]*iv.bil.t[,6]),nT)
        #+++ Dim 15 -> 18
        iVB.bagr.akl.dkl[18,1,i,j,k] <- R.list$R.xy
        iVB.bagr.akl.dkl[18,2,i,j,k] <- sqrt(R.list$V.R.xy)
        #Mittlere kalendarische Periodenlänge je BAGR, Akl, Dkl
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
  dkl.lab <- dkl.lab.fun(D.klass,D.k) #kä/16.07.14

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
              Größen=c("Wert","Standardfehler"),
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
#' Auswertungskonvention als sogenannter Mean of Ratios (gemaeß Absprache mit TI 
#' Petra Henning bzw. Friedrich Schmitz, BMEL vom 11.08.14).
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 11.08.2014
#' @section Hinweis: Tabelle \code{bacode} muss geladen sein. \cr
#'  Folgende Verbiss-Kategorien werden ausgewertet: \cr 0- ohne Verbiss; \cr 1- 
#'  nur Verbiss der Terminalknospe (der letzten 12 Monate); \cr 2- mehrfacher 
#'  Verbiss (auch bei intakter Terminalknospe) ueber einen laengeren Zeitraum 
#'  (einschließlich der letzten 12 Monate); \cr 12- beide Verbiss-Kategorien 
#'  (1 oder 2).
#' @param verj Tabelle, welche die Kennwerte der Verjuengung mit Schadmerkmal 
#'  Verbiss <Biss> enthaelt.
#' @param ecken Tabelle mit den Stichproben (Ecken)-Merkmalen fuer die 
#'  Startifikation gemaeß der Auswahlkriterien in \code{auswahl} (gemaeß der 
#'  Konvention).
#' @param trakte Traktinformationen für die Hochrechnung.
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
  #Holzbodenfläche des Stratums
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der benötigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> auswählen
  verj.s.bis130 <- merge(subset(verj,h <1.3,select=c(tnr,enr,ba,nha,biss)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"))
                    
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  n.bagr <- 9
  #Baumartengruppen hinzufügen
  verj.s.bis130 <- merge(verj.s.bis130, subset(bacode,select=c(ICode,BaGr)),
                              by.x="ba",by.y="ICode",all.x=T)
                              
  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.0 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.1 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.2 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
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
    aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.a) <- c("TNr","BaGr","t")
  verbiss.t. <- aggregate(verbiss$vb0.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- aggregate(verbiss$vb1.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- aggregate(verbiss$vb2.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- aggregate(verbiss$vb12.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb12")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))

  m.vb.bagr <- array(dim=c(4,2,(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    verbiss.t.bagr <- subset(verbiss.t,BaGr==bagr.list[i])
    
    for (j in 1:4)
    {
     vb.bagr <- r.variance.fun(cbind(verbiss.t.bagr$t,verbiss.t.bagr[,(3+j)]),
                                length(trakte[,1]))
     m.vb.bagr[j,1,i] <- vb.bagr$R.xy*100         #in Prozent
     m.vb.bagr[j,2,i] <- sqrt(vb.bagr$V.R.xy)*100 #in Prozent

    }
  }
  
  #-------------------------------------------
  #Über alle Baumarten
  
  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.0 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.1 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.2 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
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

  #Vorkommen Verjüngung je Trakt
  verbiss.t.a <- aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr),sum)
  names(verbiss.t.a) <- c("TNr","t")
  #Verbiss-Kategorie 0
  verbiss.t. <- aggregate(verbiss$vb0.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1
  verbiss.t. <- aggregate(verbiss$vb1.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 2
  verbiss.t. <- aggregate(verbiss$vb2.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1|2
  verbiss.t. <- aggregate(verbiss$vb12.prz,by=list(verbiss$TNr),sum)
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
#'  Startifikation gemaeß der Auswahlkriterien in \code{auswahl} (gemaess der 
#'  Konvention).
#' @param trakte Traktinformationen für die Hochrechnung.
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
  #Holzbodenfläche des Stratums
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A

  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der benötigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> auswählen
  verj.s.bis130 <- merge(subset(verj,h <1.3,select=c(tnr,enr,ba,nha,biss)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"))

  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(ba.grupp)
  #Attribut <bagr> in <BaGr>
  names(bagr.tab)[3] <- "BaGr"
  #BA-Gruppe dazu spielen
  verj.s.bis130 <- merge(verj.s.bis130, subset(bagr.tab,select=c(ICode,BaGr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  verj.s.bis130[is.na(verj.s.bis130)] <- 0
  n.bagr <- length(ba.grupp[[1]])
  bagr.list <- ba.grupp[[1]]

  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.0 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.1 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr,verj.s.bis130$BaGr),sum)
  verbiss.2 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
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
    aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.a) <- c("TNr","BaGr","t")
  verbiss.t. <- aggregate(verbiss$vb0.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- aggregate(verbiss$vb1.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- aggregate(verbiss$vb2.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))
  verbiss.t. <- aggregate(verbiss$vb12.prz,by=list(verbiss$TNr,verbiss$BaGr),sum)
  names(verbiss.t.) <- c("TNr","BaGr","s.vb12")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr","BaGr"))

  m.vb.bagr <- array(dim=c(4,2,(n.bagr+1)))

  for (i in 1:n.bagr)
  {
    verbiss.t.bagr <- subset(verbiss.t,BaGr==bagr.list[i])

    for (j in 1:4)
    {
     vb.bagr <- r.variance.fun(cbind(verbiss.t.bagr$t,verbiss.t.bagr[,(3+j)]),
                                length(trakte[,1]))
     m.vb.bagr[j,1,i] <- vb.bagr$R.xy*100         #in Prozent
     m.vb.bagr[j,2,i] <- sqrt(vb.bagr$V.R.xy)*100 #in Prozent

    }
  }

  #-------------------------------------------
  #Über alle Baumarten

  #Verbiss auf Traktecke aggregieren, differenziert nach <Biss> 1 oder 2
  verbiss.a <- aggregate(verj.s.bis130$nha,
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.0 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==0,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.1 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==1,1,0),
            by=list(verj.s.bis130$tnr,verj.s.bis130$enr),sum)
  verbiss.2 <-aggregate(verj.s.bis130$nha*ifelse(verj.s.bis130$biss==2,1,0),
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

  #Vorkommen Verjüngung je Trakt
  verbiss.t.a <- aggregate(ifelse(verbiss$n.ges>0,1,0),by=list(verbiss$TNr),sum)
  names(verbiss.t.a) <- c("TNr","t")
  #Verbiss-Kategorie 0
  verbiss.t. <- aggregate(verbiss$vb0.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb0")
  verbiss.t <- merge(verbiss.t.a,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1
  verbiss.t. <- aggregate(verbiss$vb1.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb1")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 2
  verbiss.t. <- aggregate(verbiss$vb2.prz,by=list(verbiss$TNr),sum)
  names(verbiss.t.) <- c("TNr","s.vb2")
  verbiss.t <- merge(verbiss.t,verbiss.t.,by=c("TNr"))
  #Verbiss-Kategorie 1|2
  verbiss.t. <- aggregate(verbiss$vb12.prz,by=list(verbiss$TNr),sum)
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
#'  Startifikation gemaeß der Auswahlkriterien in \code{auswahl} (gemaeß der 
#'  Konvention).
#' @param trakte Traktinformationen für die Hochrechnung.
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
  #Holzbodenfläche des Stratums
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Atttributnahmen in <verj>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der benötigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> auswählen
  verj.s <- merge(subset(verj,select=c(tnr,enr,ba,h,oib,nha)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"))
                    
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  n.bagr <- 9
  #Baumartengruppen hinzufügen
  verj.s <- merge(verj.s, subset(bacode,select=c(ICode,BaGr)),
                              by.x="ba",by.y="ICode",all.x=T)
  verj.s[is.na(verj.s)] <- 0
  names(verj.s)[7]<- "bagr"                            
  #-----------------------------------------------------------------------------
  #Hochrechnen
  #-----------------------------
  #Verjüngung auf Trakt aggregieren
  #Nach Baumartengruppen  
  verj.t <-
    aggregate(verj.s$nha,by=list(verj.s$tnr,verj.s$bagr),sum)
  names(verj.t) <- c("tnr","bagr","n")
  #Alle Baumarten zusammen
  verj.t.a <-
    aggregate(verj.s$nha,by=list(verj.s$tnr),sum)
  names(verj.t.a) <- c("tnr","n.ges")
  
  #Array-Tabelle definieren:
  #1. Dimension: Totale je BAGR, Antielprozente
  #2. Dimension: Wert, Standardfehler 
  #3. Bauartengruppen + Gesamt
  n.verj.bagr <- array(dim=c(2,2,(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    verj.t.bagr <- subset(verj.t,bagr==bagr.list[i])
    #Gesamtzahlen nach BAGR
    xy <- merge(subset(trakte,select=c(tnr,m)),
                subset(verj.t.bagr, select=c(tnr,n)),by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    vj.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    n.verj.bagr[1,1,i] <- vj.bagr$R.xy * A        
    n.verj.bagr[1,2,i] <- sqrt(vj.bagr$V.R.xy)* A 
    #Baumartenanteile in Prozent (Stückzahl BAGR/Gesamtstückzahl)
    xy <- merge(verj.t.a,subset(xy,select=c(tnr,n)),by="tnr",all.y=T)
    xy[is.na(xy)] <- 0
    vj.proz.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1])) 
    n.verj.bagr[2,1,i] <- vj.proz.bagr$R.xy * 100        #Prozent        
    n.verj.bagr[2,2,i] <- sqrt(vj.proz.bagr$V.R.xy)* 100 #Prozent 
  }
  
  #-------------------------------------------
  #Über alle Baumarten
  
  verj.t.a <- merge(subset(trakte,select=c(tnr,m)),verj.t.a,by="tnr",all.x=T) 
  verj.t.a[is.na(verj.t.a)] <- 0 
  r.list <- r.variance.fun(cbind(verj.t.a$m,verj.t.a$n.ges),length(trakte[,1]))
  n.verj.bagr[1,1,(n.bagr+1)] <- r.list$R.xy*A
  n.verj.bagr[1,2,(n.bagr+1)] <- sqrt(r.list$V.R.xy)*A
  #Baumartenanteile in Prozent (Stückzahl BAGR/Gesamtstückzahl)
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
#'  Startifikation gemaeß der Auswahlkriterien in \code{auswahl} (gemaeß der 
#'  Konvention).
#' @param trakte Traktinformationen für die Hochrechnung.
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
  #Holzbodenfläche des Stratums
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(verj) <- tolower(names(verj))
  #"Neutralisierung" der benötigten Attributnamen
  inv <- ifelse(inv>1,2,inv)
  names(verj) <- sub(inv,names(verj),replacement="")
  #Attribute und Untermenge des Stratums aus <verj> auswählen
  verj.s <- merge(subset(verj,select=c(tnr,enr,ba,h,oib,nha)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"))
                    
  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(ba.grupp)
  n.bagr <- length(ba.grupp[[1]])
  bagr.list <- ba.grupp[[1]]
  #BA-Gruppe dazu spielen
  verj.s <- merge(verj.s, subset(bagr.tab,select=c(ICode,bagr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  verj.s[is.na(verj.s)] <- 0
  names(verj.s)[7]<- "bagr"                            
  #-----------------------------------------------------------------------------
  #Hochrechnen
  #-----------------------------
  #Verjüngung auf Trakt aggregieren
  #Nach Baumartengruppen  
  verj.t <-
    aggregate(verj.s$nha,by=list(verj.s$tnr,verj.s$bagr),sum)
  names(verj.t) <- c("tnr","bagr","n")
  #Alle Baumarten zusammen
  verj.t.a <-
    aggregate(verj.s$nha,by=list(verj.s$tnr),sum)
  names(verj.t.a) <- c("tnr","n.ges")
  
  #Array-Tabelle definieren:
  #1. Dimension: Totale je BAGR, Antielprozente
  #2. Dimension: Wert, Standardfehler 
  #3. Bauartengruppen + Gesamt
  n.verj.bagr <- array(dim=c(2,2,(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    verj.t.bagr <- subset(verj.t,bagr==bagr.list[i])
    #Gesamtzahlen nach BAGR
    xy <- merge(subset(trakte,select=c(tnr,m)),
                subset(verj.t.bagr, select=c(tnr,n)),by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    vj.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    n.verj.bagr[1,1,i] <- vj.bagr$R.xy * A        
    n.verj.bagr[1,2,i] <- sqrt(vj.bagr$V.R.xy)* A 
    #Baumartenanteile in Prozent (Stückzahl BAGR/Gesamtstückzahl)
    xy <- merge(verj.t.a,subset(xy,select=c(tnr,n)),by="tnr",all.y=T)
    xy[is.na(xy)] <- 0
    vj.proz.bagr <- r.variance.fun(xy[,2:3],length(trakte[,1])) 
    n.verj.bagr[2,1,i] <- vj.proz.bagr$R.xy * 100        #Prozent        
    n.verj.bagr[2,2,i] <- sqrt(vj.proz.bagr$V.R.xy)* 100 #Prozent 
  }
  
  #-------------------------------------------
  #Über alle Baumarten
  
  verj.t.a <- merge(subset(trakte,select=c(tnr,m)),verj.t.a,by="tnr",all.x=T) 
  verj.t.a[is.na(verj.t.a)] <- 0 
  r.list <- r.variance.fun(cbind(verj.t.a$m,verj.t.a$n.ges),length(trakte[,1]))
  n.verj.bagr[1,1,(n.bagr+1)] <- r.list$R.xy*A
  n.verj.bagr[1,2,(n.bagr+1)] <- sqrt(r.list$V.R.xy)*A
  #Baumartenanteile in Prozent (Stückzahl BAGR/Gesamtstückzahl)
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
  #Holzbodenfläche des Stratums
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Attributnahmen in <verj.kl4>
  names(verj.kl4) <- tolower(names(verj.kl4))
  
  #Baumartenflächen nach Baumart herleiten:
  #BAF = Deckungsgrad (Zehntel) * Anteil (Zehntel)/100
  verj.kl4$baf <- verj.kl4$dg*verj.kl4$anteil/100
  
  #Attribute und Untermenge des Stratums aus <verj.kl4> auswählen
  verj.kl4.s <- 
          merge(verj.kl4, subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"))
   
  #Baumartengruppe hinzufügen
  verj.kl4.s <- merge(verj.kl4.s,subset(bacode,select=c(ICode,BaGr)),by.x="ba",
                      by.y="ICode",all.x=T)
  names(verj.kl4.s)[11] <- "bagr"
  #Nach Trakt, BAGR und Verjüngungsart aggregieren
  vj.kl4.t <- aggregate(verj.kl4.s$baf,
                    by=list(verj.kl4.s$tnr,verj.kl4.s$bagr,verj.kl4.s$vjgart),
                    sum)
  names(vj.kl4.t) <- c("tnr","bagr","vart","baf")
  
  #Verjüngungs-Baumartenfläche nach BA-Gruppe und Verjüngungsart
  
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  vart.list <-
    c("Naturverjüngung","Pflanzung","Saat","Stockausschlag","nicht zuzuordnen")
  n.bagr <- length(bagr.list)
  n.vart <- length(vart.list)
  
  #Array definieren
  vj.kl4.baf.bagr.vart <- array(dim=c(2,(n.vart+1),(n.bagr+1)))
  
  for (i in 1:n.bagr)
  {
    #BAGR
    vj.kl4.t.bagr <- subset(vj.kl4.t,bagr==bagr.list[i])
    #Nach Verjüngungsarten
    for (j in 1:n.vart)
    {
      vj.kl4.t.vart <- subset(vj.kl4.t.bagr,vart==vart.list[j],select=c(tnr,baf))
      xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.vart,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,i] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,i] <- sqrt(r.list$V.R.xy) * A
      
      #Über alle Baumartengruppen einer Verjüngungsart
      vj.kl4.t.a <- aggregate(vj.kl4.t$baf[vj.kl4.t$vart==vart.list[j]],
                      by=list(vj.kl4.t$tnr[vj.kl4.t$vart==vart.list[j]]),sum)
      names(vj.kl4.t.a) <- c("tnr","baf")
      xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.a,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,(n.bagr+1)] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,(n.bagr+1)] <- sqrt(r.list$V.R.xy) * A
    }
    #Alle Verjüngungsarten zusammen nach BA-Gruppe
    vj.kl4.t.a <- aggregate(vj.kl4.t.bagr$baf,by=list(vj.kl4.t.bagr$tnr),sum)
    names(vj.kl4.t.a) <- c("tnr","baf")
    xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.a,by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    vj.kl4.baf.bagr.vart[1,(n.vart+1),i] <- r.list$R.xy * A
    vj.kl4.baf.bagr.vart[2,(n.vart+1),i] <- sqrt(r.list$V.R.xy) * A
  }
  
  
  #Alle Verjüngungsarten und alle Baumartengruppen
  vj.kl4.t.a <- aggregate(vj.kl4.t$baf,by=list(vj.kl4.t$tnr),sum)
  names(vj.kl4.t.a) <- c("tnr","baf")
  xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.a,by="tnr",all.x=T)
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
  #Holzbodenfläche des Stratums
  y <- tryCatch(aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum),
                error = function(e) return(data.frame(NA, 0))
                )
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  
  #Kleinschreibung aller Attributnahmen in <verj.kl4>
  names(verj.kl4) <- tolower(names(verj.kl4))
  
  #Baumartenflächen nach Baumart herleiten:
  #BAF = Deckungsgrad (Zehntel) * Anteil (Zehntel)/100
  verj.kl4$baf <- verj.kl4$dg*verj.kl4$anteil/100
  
  #Attribute und Untermenge des Stratums aus <verj.kl4> auswählen
  verj.kl4.s <- 
          merge(verj.kl4, subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"))
   
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(ba.grupp)
  #Attribut <bagr> in <BaGr>
  names(bagr.tab)[3] <- "BaGr"
  #BA-Gruppe dazu spielen
  verj.kl4.s <- merge(verj.kl4.s, subset(bagr.tab,select=c(ICode,BaGr)),
                                  by.x="ba",by.y="ICode",all.x=T)
  verj.kl4.s[is.na(verj.kl4.s)] <- 0
  n.bagr <- length(ba.grupp[[1]])
  bagr.list <- ba.grupp[[1]]
  names(verj.kl4.s)[11] <- "bagr"
    
  #Verjüngungsart
  vart.list <-
    c("Naturverjüngung","Pflanzung","Saat","Stockausschlag","nicht zuzuordnen")
  n.vart <- length(vart.list)
  
  #Nach Trakt, BAGR und Verjüngungsart aggregieren
  vj.kl4.t <- tryCatch(aggregate(verj.kl4.s$baf,
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
    vj.kl4.t.bagr <- subset(vj.kl4.t,bagr==bagr.list[i])
    #Nach Verjüngungsarten
    for (j in 1:n.vart)
    {
      vj.kl4.t.vart <- subset(vj.kl4.t.bagr,vart==vart.list[j],select=c(tnr,baf))
      xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.vart,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,i] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,i] <- sqrt(r.list$V.R.xy) * A
      
      #Über alle Baumartengruppen einer Verjüngungsart
      vj.kl4.t.a <- 
          tryCatch(aggregate(na.fail(vj.kl4.t$baf[vj.kl4.t$vart ==
                                     vart.list[j]]) ,
                             by = list(na.fail(vj.kl4.t$tnr[vj.kl4.t$vart == 
                                       vart.list[j]])), 
                             sum), 
                   error = function(e) return(data.frame(NA, 0))
                   )
      names(vj.kl4.t.a) <- c("tnr","baf")
      xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.a,by="tnr",all.x=T)
      xy[is.na(xy)] <- 0
      r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
      vj.kl4.baf.bagr.vart[1,j,(n.bagr+1)] <- r.list$R.xy * A
      vj.kl4.baf.bagr.vart[2,j,(n.bagr+1)] <- sqrt(r.list$V.R.xy) * A
    }
    #Alle Verjüngungsarten zusammen nach BA-Gruppe
    vj.kl4.t.a <-
        tryCatch(aggregate(vj.kl4.t.bagr$baf,by=list(vj.kl4.t.bagr$tnr),sum),
                   error = function(e) return(data.frame(NA, 0))
                   )
    names(vj.kl4.t.a) <- c("tnr","baf")
    xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.a,by="tnr",all.x=T)
    xy[is.na(xy)] <- 0
    r.list <- r.variance.fun(xy[,2:3],length(trakte[,1]))
    vj.kl4.baf.bagr.vart[1,(n.vart+1),i] <- r.list$R.xy * A
    vj.kl4.baf.bagr.vart[2,(n.vart+1),i] <- sqrt(r.list$V.R.xy) * A
  }
  
  #Alle Verjüngungsarten und alle Baumartengruppen
  vj.kl4.t.a <-
      tryCatch(aggregate(na.fail(vj.kl4.t$baf),by=list(na.fail(vj.kl4.t$tnr)),sum),
                error = function(e) return(data.frame(NA, 0))
                )
  names(vj.kl4.t.a) <- c("tnr","baf")
  xy <- merge(subset(trakte,select=c(tnr,m)),vj.kl4.t.a,by="tnr",all.x=T)
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
#' Funktion berechnet für das in \code{auswahl} definierte Stratum die Flaechen 
#' und Flaechenanteile (\%) der 5 Naturnaehestufen (ntns) fuer BWI 2 oder BWI 3
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 19.08.2014
#' @param ntns.te alle Traktecken die Naturnaehestufen in allen Varianten 
#'  (Schichten, Gesamtwert für Hauptbestockung) für BWI 2 und 3 sowie fuerr BWI 
#'  2 in den 2 Vaianten natWG BWI 2 und natWG BWI 3.
#' @param ecken Stichprobenmerkmale der jeweiligen BWI (2 oder 3).
#' @param trakte Traktmermale (m, m_HB, m_bHB, m_Wa).
#' @param A Gesamtflaeche des Inventurgebietes in ha.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param bwi Angabe welche BWI (2 oder 3).
#' @param natwg Differenzierung bei Auswertung für BWI 2: 2: nat. WG der 3
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
    stratum <- merge(stratum,subset(ntns.te,select=c(Tnr,Enr,NTNS_F_BaWue_BWI3)),
            by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
    NTNS <- "BWI 3"
  } else{ #BWI 2: Fallunterscheidung nach verwendeten nat. WG
    if (natwg==3){
        stratum <- merge(stratum,subset(ntns.te,
              select=c(Tnr,Enr,NTNS_F_BaWue_BWI2_natwgBWI3)),
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
        NTNS <- "BWI 2 mit nat. WG der BWI 3"
      } else{ #Nat. WG der BWI 2
        stratum <- merge(stratum,subset(ntns.te,
              select=c(Tnr,Enr,NTNS_F_BaWue_BWI2)),
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
        NTNS <- "BWI 2 mit nat. WG der BWI 2"
      }
  }
  names(stratum)[length(stratum)] <- "ntns"
  #Anzahl TE nach Trakt und NTNS aggregieren
  xy.ntns.t <- aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr,stratum$ntns),sum)
  names(xy.ntns.t) <- c("TNr","ntns","n.ntns")
  #Anzahl TE nach Trakt aggregieren
  xy.t <- aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr),sum)
  names(xy.t) <- c("TNr","n")

  #Array definieren
  #1. Dim (2): 1: Fläche ha HB; 2: Prozent-Anteil;
  #2. Dim (2): 1: Wert, 2: Standardfehler
  #3. Dim (6): 1-5 Naturnähestufen + "keine Angabe"
  
  ntns.f.ant <- array(dim=c(2,2,6))
  
  for (i in 1:6){
    if (i < 6){
      xy.ntns.i.t <- subset(xy.ntns.t,ntns==i,select=c(TNr,n.ntns))
    } else{  #OHNE NTNS-Angabe
      xy.ntns.i.t <- subset(xy.ntns.t,!ntns %in% c(1:5),select=c(TNr,n.ntns))
    }
    names(xy.ntns.i.t)[2] <- "n.ntns"
    xy.i.t <- merge(subset(trakte,select=c(TNr,m)),
                      xy.ntns.i.t,by="TNr",all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    #Fläche
    r.list <- r.variance.fun(xy.i.t[,2:3], length(trakte[,1]))
    ntns.f.ant[1,1,i] <- r.list$R.xy*A
    ntns.f.ant[1,2,i] <- sqrt(r.list$V.R.xy)*A
    #Flächenanteil
    xy.i.t <- merge(subset(xy.t,select=c(TNr,n)),xy.ntns.i.t,by=c("TNr"),all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    r.list <- r.variance.fun(xy.i.t[,2:3],length(trakte[,1]))
    ntns.f.ant[2,1,i] <- r.list$R.xy*100
    ntns.f.ant[2,2,i] <- sqrt(r.list$V.R.xy)*100
  }
  #HBF des Stratums
  xy.i.t <- merge(subset(trakte,select=c(TNr,m)),xy.t,by="TNr",all.x=T)
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
#' "kl 4m" oder "gr 4m" für BWI 2 oder BWI 3.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Erstellungsdatum: 27.10.2014
#' @section Version: 1.0 basierend auf \code{\link{ntns.stratum.fun}}
#' @param ntns.te alle Traktecken die Naturnaehestufen in allen Varianten 
#'  (Schichten, Gesamtwert für Hauptbestockung) für BWI 2 und 3 sowie fuer BWI 2 
#'  in den 2 Vaianten natWG BWI 2 und natWG BWI 3.
#' @param ecken Stichprobenmerkmale der jeweiligen BWI (2 oder 3).
#' @param trakte Traktmermale (m, m_HB, m_bHB, m_Wa).
#' @param A Gesamtflaeche des Inventurgebiets in ha.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: list(Wa=c(3,5),
#'  Begehbar=1).
#' @param bwi Angabe welche BWI (2 oder 3).
#' @param natwg Differenzierung bei Auswertung für BWI 2: 2: nat. WG der 3
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
      stratum <- merge(stratum,subset(ntns.te,
              select=c(Tnr,Enr,NTNS_F_kl4m_BA_Anteile_BWI3)),
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
      NTNS <- "BWI 3 Schicht < 4m"
    } else{
      stratum <- merge(stratum,subset(ntns.te,
              select=c(Tnr,Enr,NTNS_F_gr4m_alleBaeume_BWI3)),
              by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
      NTNS <- "BWI 3 Schicht > 4m"
    }
  } else{ #BWI 2: Fallunterscheidung nach verwendeten nat. WG
    if (natwg==3){
        if (schicht==1){
          stratum <- merge(stratum,subset(ntns.te,
                select=c(Tnr,Enr,NTNS_F_kl4m_BA_Anteile_BWI2_natwgBWI3)),
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht < 4 m mit nat. WG der BWI 3"
        } else{
          stratum <- merge(stratum,subset(ntns.te,
                select=c(Tnr,Enr,NTNS_F_gr4m_alleBaeume_BWI2_natwgBWI3)),
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht > 4 m mit nat. WG der BWI 3"
        }
      } else{
        #Nat. WG der BWI 2
        if (schicht==1){
          stratum <- merge(stratum,subset(ntns.te,
                select=c(Tnr,Enr,NTNS_F_kl4m_BA_Anteile_BWI2)),
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht < 4 m mit nat. WG der BWI 2"
        }else{
          stratum <- merge(stratum,subset(ntns.te,
                select=c(Tnr,Enr,NTNS_F_gr4m_alleBaeume_BWI2)),
                by.x=c("TNr","ENr"),by.y=c("Tnr","Enr"),all.x=T)
          NTNS <- "BWI 2 Schicht > 4 m mit nat. WG der BWI 2"
        }
      }
  }
  names(stratum)[length(stratum)] <- "ntns"
  #Anzahl TE nach Trakt und NTNS aggregieren
  xy.ntns.t <- aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr,stratum$ntns),sum)
  names(xy.ntns.t) <- c("TNr","ntns","n.ntns")
  #Anzahl TE nach Trakt aggregieren
  xy.t <- aggregate(rep(1,length(stratum[,1])),
                    by=list(stratum$TNr),sum)
  names(xy.t) <- c("TNr","n")

  #Array definieren
  #1. Dim (2): 1: Fläche ha HB; 2: Prozent-Anteil;
  #2. Dim (2): 1: Wert, 2: Standardfehler
  #3. Dim (6): 1-5 Naturnähestufen + "keine Angabe"
  
  ntns.f.ant <- array(dim=c(2,2,6))
  
  for (i in 1:6){
    if (i < 6){
      xy.ntns.i.t <- subset(xy.ntns.t,ntns==i,select=c(TNr,n.ntns))
    } else   {#OHNE NTNS-Angabe
      xy.ntns.i.t <- subset(xy.ntns.t,!ntns %in% c(1:5),select=c(TNr,n.ntns))
    }
    names(xy.ntns.i.t)[2] <- "n.ntns"
    xy.i.t <- merge(subset(trakte,select=c(TNr,m)),
                      xy.ntns.i.t,by="TNr",all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    #Fläche
    r.list <- r.variance.fun(xy.i.t[,2:3], length(trakte[,1]))
    ntns.f.ant[1,1,i] <- r.list$R.xy*A
    ntns.f.ant[1,2,i] <- sqrt(r.list$V.R.xy)*A
    #Flächenanteil
    xy.i.t <- merge(subset(xy.t,select=c(TNr,n)),xy.ntns.i.t,by=c("TNr"),all.x=T)
    xy.i.t[is.na(xy.i.t)] <- 0
    r.list <- r.variance.fun(xy.i.t[,2:3],length(trakte[,1]))
    ntns.f.ant[2,1,i] <- r.list$R.xy*100
    ntns.f.ant[2,2,i] <- sqrt(r.list$V.R.xy)*100
  }
  #HBF des Stratums
  xy.i.t <- merge(subset(trakte,select=c(TNr,m)),xy.t,by="TNr",all.x=T)
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
#' Funktion zur Auswertung der Biotop-Bäume
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
  #(1) Befundeinheit festlegen (Traktecken auswählen)
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  names(baeume) <- tolower(names(baeume))
  names(wzp4.merkmale) <- tolower(names(wzp4.merkmale))
  #"Neutralisierung" der benötigten Attributnamen in <baeume>: <2> entfernen
  names(baeume) <- sub(2,names(baeume),replacement="")

  #(2) Biotop-Bäume definieren
  wzp4.merkmale <- merge(subset(baeume,stp==0,
                        select=c(tnr,enr,bnr,ba,alt,bhd,h,volv,oib,nha,stfl)),
                        wzp4.merkmale, by=c("tnr","enr","bnr"),all.x=T)
  wzp4.merkmale[is.na(wzp4.merkmale)] <- 0
  #Biotop-Bäume
  #Merkmal: FaulKon, Hoehle, Bizarr, Uralt, Horst, MBiotop
  #Neues Merkmal <BiotopB> festlegen
  wzp4.merkmale$biotopb <-
      wzp4.merkmale$faulkon + wzp4.merkmale$hoehle + wzp4.merkmale$bizarr + wzp4.merkmale$uralt + wzp4.merkmale$horst + wzp4.merkmale$mbiotop

  #Probebäume im Stratum auswählen
  wzp4.merkmale.s <- merge(subset(wzp4.merkmale,select=c(tnr,enr,bnr,pk,biotopb,
                    ba,alt,bhd,volv,oib,nha,stfl)),
                    subset(stratum,select=c(tnr,enr)),by=c("tnr","enr"),all.y=T)
  wzp4.merkmale.s[is.na(wzp4.merkmale.s)] <- 0
  
  #---------------------
  #(3) Flächen
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  #------
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(
                  wzp4.merkmale.s$ba==999,wzp4.merkmale.s$stfl/10000,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(wzp4.merkmale.s$ba==998,wzp4.merkmale.s$stfl,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl

  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)

  #(4) Biotopbaum-Attribute auf Trakt aggregieren
  biotop.t <- aggregate(ifelse(wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$pk<=1,
                        wzp4.merkmale.s$nha,0),by=list(wzp4.merkmale.s$tnr),sum)
  #Vorrat
  biotop.t <- cbind(biotop.t,aggregate(ifelse(wzp4.merkmale.s$biotopb>0
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)
              
  #Oberirdische Biomasse
  biotop.t <- cbind(biotop.t,aggregate(ifelse(wzp4.merkmale.s$biotopb>0
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$oib,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)

  #Starkholzvorrat
  biotop.t <- cbind(biotop.t,aggregate(ifelse(
              wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$bhd>=50
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)

  #LB-Vorrat
  biotop.t <- cbind(biotop.t,aggregate(ifelse(
              wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$ba%in%c(100:299)
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)

  #NB-Vorrat
  biotop.t <- cbind(biotop.t,aggregate(ifelse(
              wzp4.merkmale.s$biotopb>0&wzp4.merkmale.s$ba%in%c(10:99)
              &wzp4.merkmale.s$pk<=1,
              wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
              by=list(wzp4.merkmale.s$tnr),sum)$x)



  names(biotop.t) <- c("tnr","N.BB","V.BB","oiB.BB","SthV.BB","V.BB.LB",
                        "V.BB.NB")
  head(biotop.t)

  biotop.t <- merge(subset(xy,select=c(tnr,m,hbf)),biotop.t,by="tnr",
                    all.x=T)
  biotop.t[is.na(biotop.t)] <- 0

  n <- length(trakte.3[,1])
  #Stückzahl
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

  #Stückvolumen
  R.xy <- r.variance.fun(cbind(biotop.t$N.BB,biotop.t$V.BB),n)
  (V.BB.Stck <- R.xy$R.xy)
  (se.V.BB.Stck <- R.xy$V.R.xy^0.5)

  #Vorrat der Laubbäume
  R.xy <- r.variance.fun(cbind(biotop.t$m,biotop.t$V.BB.LB),n)
  (T.V.BB.LB <- R.xy$R.xy*A)
  (se.T.V.BB.LB <- R.xy$V.R.xy^0.5*A)

  #je ha HBF
  R.xy <- r.variance.fun(cbind(biotop.t$hbf,biotop.t$V.BB.LB),n)
  (V.BB.LB.ha <- R.xy$R.xy)
  (se.V.BB.LB.ha <- R.xy$V.R.xy^0.5)

  #Vorrat der Nadelbäume
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
  #(1) Befundeinheit festlegen (Traktecken auswählen)
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  names(baeume) <- tolower(names(baeume))
  names(wzp4.merkmale) <- tolower(names(wzp4.merkmale))
  #"Neutralisierung" der benötigten Attributnamen in <baeume>: <2> entfernen
  names(baeume) <- sub(2,names(baeume),replacement="")

  #Aus Kompatibiltäts-Gründen werden die nur in der BWI 3 vorkommenden
  #Attribute <Ast> und <Ast_Hoe> im DS der BWI 3 entfernt
  ast.pos <- grep("ast",names(wzp4.merkmale))
  if (length(ast.pos)>0)
  {
    wzp4.merkmale <- subset(wzp4.merkmale,select=c(-ast.pos))
  }
  #(2) Merkmal-DS <wzp4.merkmale> mit Attribut-Auswahl aus <baeume> verknüpfen
  wzp4.merkmale <- merge(subset(baeume,stp==0,
                        select=c(tnr,enr,bnr,ba,alt,bhd,h,volv,oib,nha,stfl)),
                        wzp4.merkmale, by=c("tnr","enr","bnr"),all.x=T)
  wzp4.merkmale[is.na(wzp4.merkmale)] <- 0
  
  #Probebäume im Stratum mit den Merkmalen in  <merkmale> auswählen
  #Anzahl zu berücksichtigender Merkmale
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
  wzp4.merkmale.s <- merge(subset(wzp4.merkmale,
    select=c(1:3,12,mm.pos,mm.s.pos,4:11)),subset(stratum,select=c(tnr,enr)),
    by=c("tnr","enr"),all.y=T)
  wzp4.merkmale.s[is.na(wzp4.merkmale.s)] <- 0 
  
  #---------------------
  #(3) Flächen
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  #------
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(
                  wzp4.merkmale.s$ba==999,wzp4.merkmale.s$stfl/10000,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(wzp4.merkmale.s$ba==998,wzp4.merkmale.s$stfl,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl

  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)

  #(4) Attribute des Merkmals auf Trakt aggregieren
  
  #getrennt für die Baumarten(gruppen)

  Kennwerte=c("Gesamtzahl","Zahl_je_ha","Vorrat_m3_mR",
              "Vorrat_m3_mR_je_ha","Stueckvolumen_m3_mR","oi_Biom_t",
              "oi_Biom_t_je_ha","Anteil_Gesamtzahl_Proz",
              "Anteil_Gesamtvorrat_Proz")
  n.kw <- length(Kennwerte)
  #Anzahl BA-Gruppen
  n.bagr <- length(ba.grupp$ba.grupp)
  n.bagr <- n.bagr+1
  #Alle Baumarten ergänzen
  ba.grupp$bagr.lab[[n.bagr]] <- "Alle BA"
  ba.grupp$ba.grupp[[n.bagr]] <- c(10:299)
  #Array  für Ergebnisse je  BAGR (Dimension 2 steht für Wert und Standardfehler)
  attr.bagr.tab <- array(dim=c(n.kw,2,n.bagr))
  
  for (i in 1:n.bagr)
  {
    merkmal.t <- aggregate(ifelse(
      wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba 
      %in% ba.grupp$ba.grupp[[i]],wzp4.merkmale.s$nha,0),
      by=list(wzp4.merkmale.s$tnr),sum)
    #Vorrat
    merkmal.t <- cbind(merkmal.t,aggregate(ifelse(wzp4.merkmale.s$merkmal.s>0
      &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
                
    #Oberirdische Biomasse
    merkmal.t <- cbind(merkmal.t,aggregate(ifelse(wzp4.merkmale.s$merkmal.s>0
      &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$oib,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
  
    #Starkholzvorrat
    merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
      wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$bhd>=50
      &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
    
    #Gesamtzahl 
    merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
      wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha,0), by=list(wzp4.merkmale.s$tnr),sum)$x)
    #Gesamtvorrat 
    merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
      wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]],
      wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
      by=list(wzp4.merkmale.s$tnr),sum)$x)
    
  
  
    names(merkmal.t) <- c("tnr","N.MM","V.MM","oiB.MM","SthV.MM","N.ges","V.ges")
    head(merkmal.t)
  
    merkmal.t <- merge(subset(xy,select=c(tnr,m,hbf)),merkmal.t,by="tnr",
                      all.x=T)
    merkmal.t[is.na(merkmal.t)] <- 0
  
    n <- length(trakte.3[,1])
    #Stückzahl
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
  
    #Stückvolumen
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
#'  10%", 2: "häufig, > 10 bis 50%", 3:"flächig, > 50%". Diese Dichten werden in 
#'  Flaechenanteile umgerechnet: 0: 0; 1: 0.05; 2: 0.3; 3: 0.7; da jede 
#'  Stichprobe mit dem Repraesentationsfaktor (RF) hochgerechnet wird, ergeben 
#'  sich hieraus Flächenschaetzungen in ha
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
#'  \strong{FBA.Fläche}. Wobei FBA.Dichtevertlg ein Array mit den
#'  Dichteanteilen der FBA ist und FBA.Fläche ein Array ist, welches Wert und 
#'  Standardfehler fuer Flaeche (ha) und Flaechenanteile enthaelt.
fba.stratum.fun <- function(fba,fba.tab,auswahl,ecken,trakte,A){
  #Auswertungseinheit festlegen
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  #Holzbodenfläche des Stratums
  y <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(subset(trakte,select=c(tnr,m)),y,by=c("tnr"),all.x=T)
  y[is.na(y)] <- 0
  r.list <- r.variance.fun(y[,2:3],length(trakte[,1]))
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #-------------------
  nte.t <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(nte.t) <- c("tnr","nte")   
  #--------------------
  #FBA
  names(fba.tab) <- tolower(names(fba.tab))
  fba.tab <- merge(subset(stratum,select=c(tnr,enr)),
                   subset(fba.tab,select=c(tnr,enr,fba,dichte)),by=c("tnr","enr"),all.x=T)
  #fehlende Angaben werden als Nicht-Vorkommen interpretiert
  fba.tab$dichte[is.na(fba.tab$dichte)] <- 0
  #Falls aus Originaldaten Code -1 für fehlende Angabe vorkommt
  fba.tab$dichte[fba.tab$dichte==-1] <- 0
  
  fba.lab <- c("Adlerfarn", "Brennessel",  "Riedgras", "Honiggras",  "Reitgras",
               "Heidekraut", "Heidelbeere","Brombeere", "Riesenbärenklau",
               "Riesenknöterich", "Drüsiges Springkraut","Kleinblütiges Springkraut",
               "Kermesbeere")
  fba.code <- c(11:23)
  
  if (is.na(fba[1]) | tolower(fba[1])=="alle")
  {fba <- c(11:23) }
  
  #d.lab <- c("k.A.","fehlt","selten","häufig","flächig")
  #entsprechend den Codes -1, 0, 1, 2, 3
  d.lab <- c("nicht vorhanden","selten, bis 10%","häufig, > 10 bis 50%","flächig, > 50%")
  #entsprechend den Codes 0, 1, 2, 3
  #entsprechende Flächenanteile für 0,1,2,3
  d.fl.ant <- data.frame(dichte=0:3,fl.ant=c(0,0.05,0.3,0.7))
  fba.tab <- merge(fba.tab,d.fl.ant,by="dichte",all.x=T)
  fba.tab <- subset(fba.tab,select=c(2:4,1,5))
  #Auf überschießende Fläche prüfen
  n.fba.te <- aggregate(cbind(ifelse(fba.tab$dichte<=0,0,1),fba.tab$fl.ant),
                        by=list(fba.tab$tnr,fba.tab$enr),sum)
  names(n.fba.te) <- c("tnr","enr","n.fba","sum.fl.ant")
  #Überschießende Fläche auf eins korrigieren
  fba.tab <- merge(fba.tab,n.fba.te,by=c("tnr","enr"),all.x=T)
  #fba.tab$fl.ant.0 <- fba.tab$fl.ant
  fba.tab$fl.ant <- ifelse(fba.tab$sum.fl.ant>1,fba.tab$fl.ant/fba.tab$sum.fl.ant,fba.tab$fl.ant)
  
  #---------------------
  #Ausgabe-Tabelle als Array
  #Dichte-Anteile
  #jeweils Wert und Fehler für 4 Dichte-Stufen nach FBA
  fba.d <-array(dim=c(2,4,length(fba)))
  #Flächen und Flächenanteile
  #jeweils Wert und Fehler für FBA für Fläche (ha) und Flächenanteil
  fba.fl <-array(dim=c(2,length(fba),2))
  
  ii <- 0
  for (i in fba)
  {
    
    ii <- ii+1
    fba.i <- merge(stratum,subset(fba.tab,fba==i,select=c(tnr,enr,fba,dichte,fl.ant)),by=c("tnr","enr"),
                   all.x=T)
    fba.i$dichte[is.na(fba.i$dichte)] <- 0
    fba.i$fl.ant[is.na(fba.i$fl.ant)] <- 0
    
    for (j in 0:3)
    {
      nte.d.j.t <- aggregate(ifelse(fba.i$dichte==j,1,0),by=list(fba.i$tnr),sum)
      names(nte.d.j.t) <- c("tnr","nd")
      nte.d.j.t <- merge(nte.t,nte.d.j.t,by=c("tnr"),all.x=T)
      nte.d.j.t[is.na(nte.d.j.t)] <- 0
      r.list <-  r.variance.fun(nte.d.j.t[,2:3],length(trakte[,1]))
      fba.d[1,(j+1),ii] <- round(r.list$R.xy,4)
      fba.d[2,(j+1),ii] <- round(sqrt(r.list$V.R.xy),5)      
    }
    #Fläche schätzen als Total
    fl.i.t <- aggregate(fba.i$fl.ant,by=list(fba.i$tnr),sum)
    names(fl.i.t) <- c("tnr","fl")
    #daher Verknüpfung mit <y>
    fl.i.t <- merge(y[,1:2],fl.i.t,by="tnr",all.x=T)
    fl.i.t[is.na(fl.i.t)] <- 0
    r.list <-  r.variance.fun(fl.i.t[,2:3],length(trakte[,1]))
    fba.fl[1,ii,1] <- round(r.list$R.xy*A,1)
    fba.fl[2,ii,1] <- round(sqrt(r.list$V.R.xy)*A,2)
    
    #Fläche schätzen als Anteil
    fl.i.t <- aggregate(fba.i$fl.ant,by=list(fba.i$tnr),sum)
    names(fl.i.t) <- c("tnr","fl")
    #Verknüpfung mit <nte.t>
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
              FBA.Fläche = fba.fl))
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
  nte.t <- aggregate(rep(1,length(te[,1])),by=list(te$TNr),sum)
  names(nte.t) <- c("TNr","nte")
  head(nte.t)
  nte.t <- merge(subset(trakte,select=c(TNr,m)),nte.t,by="TNr",all.x=T)
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
  nte.t <- aggregate(rep(1,length(te[,1])),by=list(te$TNr),sum)
  names(nte.t) <- c("TNr","nte")
  substratum.1 <- stratum
  k <- length(names(substratum))
  for (i in 1:k){
    substratum.1[[names(substratum)[i]]] <- substratum[[i]]
  }
  te.s <- stratum.fun(substratum.1,ecken)
  nte.s.t <- aggregate(rep(1,length(te.s[,1])),by=list(te.s$TNr),sum)
  names(nte.s.t) <- c("TNr","nte.s")
  head(nte.s.t)
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
#'  Werte-Mengen als Untermenge (subset) heraus gefiltert wird.
#' @param auswahl Liste, welche die Eckenmerkmale mit den Werten enthaelt, 
#'  anhand derer die Auswahl erfolgt.
#' @param ecken Tabelle mit allen zur Selektion dienenden Eckenmerkmalen.
#' @return Untermenge der Ecken, die der Auswahl entsprechen.
stratum.fun <- function(auswahl,ecken){
  ecken <- as.data.frame(ecken)
  #Attribute für Auswahl
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
    #exaktes "matching"  kä/23.01.2015
    pos[i] <- which(names(ecken)==attribute[i])
    stratum <- subset(stratum,stratum[,pos[i]]%in%auswahl[[i]])
    if (is.factor(stratum[,pos[i]])) stratum[,pos[i]] <-
              as.numeric(stratum[,pos[i]])
  }
  stratum <- subset(stratum,select=c(1,2,pos))
  n.stratum <- length(stratum[,1])
  stratum[is.na(stratum)] <- 0
  
  return(stratum)
}

#-------------------------------------------------------------------------------
#' Klassifiziert und aggegiert die mittlere Baumartenfläche
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
#' @param A.klass Liste mit den Klassifizierungsparametern fürs Alter: z.B. 
#'  list(A.ob=160,A.b=20).
#' @param D.klass Liste mit den Klassifizierungsparametern für Durchmesser z.B. 
#'  list(D.unt=0,D.ob=70,D.b=10,Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet, dass 
#'  zusätzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt \code{D.unt} 
#'  als unterste Schwelle.
#' @return Dataframe-Tabelle mit folgenden Attributen: tnr, bagr, akl.pm,
#'  dkl.pm, baf1, baf2, mbaf, mbaf.hb. Wobei mbaf die mittlere Baumartenflaeche 
#'  und mbaf.hb die mittlere Baumartenflaeche im Hauptbestand enthaelt.
mbaf.bagr.alt.bhd.pm.fun <- function(baeume.vor,baeume.folg,A.klass,D.klass){
  names(baeume.vor) <- tolower(names(baeume.vor))
  names(baeume.folg) <- tolower(names(baeume.folg))
  #Vorinventur (aus Sicht Folgeinventur):
  #pk == 0; Bäume mit BHD1 < 7 cm! (STP=1,2)
  baeume.vor$bhd.pm <- ifelse(baeume.vor$pk%in%c(2:5,9),baeume.vor$bhd2,
                        (baeume.vor$bhd1+baeume.vor$bhd2)/2)
  baeume.vor$alt.pm <- ifelse(baeume.vor$pk%in%c(2:5,9),baeume.vor$alt2,
                ifelse(baeume.vor$ba < 998,baeume.vor$alt1+baeume.vor$pl/2,0))
  #Folgeinventur
  #Differenzieren nach Kollektiven: STP == 0, STP > 0
  baeume.folg$bhd.pm <- (baeume.folg$bhd1+baeume.folg$bhd2)/2
  baeume.folg$alt.pm <- ifelse(baeume.folg$ba<998,baeume.folg$alt2-baeume.folg$pl/2,
                0)
  #Klassifizierung durchführen
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

  #Lückenkorrekturfaktoren
  #Vorinventur
  lk.v <- sum(baeume.vor$stfl1)/sum(baeume.vor$stfl1[baeume.vor$ba<998])
  lk.f <- sum(baeume.folg$stfl2)/sum(baeume.folg$stfl2[baeume.folg$ba<998])
  baeume.vor  <- subset(baeume.vor, ba < 998)
  baeume.folg <- subset(baeume.folg,ba < 998)
  #Umrechnen in ha
  baf1.tnr <- aggregate(baeume.vor$stfl1*lk.v/10000,
    by=list(baeume.vor$tnr,baeume.vor$bagr,baeume.vor$akl.pm,baeume.vor$dkl.pm),
    sum)
  names(baf1.tnr) <- c("tnr","bagr","akl.pm","dkl.pm","baf1")
  baf2.tnr <- aggregate(baeume.folg$stfl2*lk.f/10000,
    by=list(baeume.folg$tnr,baeume.folg$bagr,baeume.folg$akl.pm,baeume.folg$dkl.pm),
    sum)
  names(baf2.tnr) <- c("tnr","bagr","akl.pm","dkl.pm","baf2")
  baf12.tnr <- merge(baf1.tnr,baf2.tnr,by=c("tnr","bagr","akl.pm","dkl.pm"),
                all.x=T, all.y=T)
  #NA entfernen
  baf12.tnr$baf1[is.na(baf12.tnr$baf1)] <- 0
  baf12.tnr$baf2[is.na(baf12.tnr$baf2)] <- 0
  #Mittlere Baumartenfläche
  baf12.tnr$mbaf <- (baf12.tnr$baf1+baf12.tnr$baf2)/2
  #Mittlere Baumartenfläche im Hauptbestand
  baf12.tnr$mbaf.hb <- (baf12.tnr$baf1/lk.v+baf12.tnr$baf2/lk.f)/2
  return(baf12.tnr)
}
#Ende <mbaf.bagr.alt.bhd.pm.fun>

#-------------------------------------------------------------------------------
#' Berechnet Ratio-Schaetzer und seine Varianz
#' 
#' Funktion berechnet Ratio-Schaetzer und seine Varianz über Fehlerfortpflanzung.
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
  #Ratio-Schätzer
  S.y <- sum(xy[,2]); S.x <- sum(xy[,1])
  R.xy <- S.y/S.x
  #Jacobi-Vektor: dR/dS.x/n = -n*S.y/S.x^2, dR/dS.y/n = n/S.x
  J.xy <- c(-n*S.y/S.x^2,n/S.x)
  #Varianz-Covarianz-Matrix von xy ("reduziert")
  xy <- as.matrix(xy)
  VC.xy <- (t(xy)%*%xy)/n/(n-1)
  #Varianz des Ratio-Schätzers
  V.R.xy <- (t(J.xy)%*%VC.xy)%*%J.xy
  return(list(R.xy = R.xy, V.R.xy=as.numeric(V.R.xy)))
}
#Ende <r.variance.fun>

#-------------------------------------------------------------------------------
#' Berechnet einen "kombinierten" Ratio-Schaetzer und seine Varianz
#' 
#' Funktion berechnet einen "kombinierten" Ratio-Schaetzer mit 2 x-Groeßen im 
#' Nenner und seine Varianz über Fehlerfortpflanzung.
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
  #Indikator-Variable t für x2 > 0
  ti <- ifelse(xy[,2]>0,1,0)
  #in Datensatz einfügen (an 3. Stelle)
  xy <- cbind(xy[,1:2],ti,y=xy[,3])    #TODO
  #Summenterme = Komponenten des Ratio-Schätzers
  S.x1 <- sum(xy[,1]); S.x2 <- sum(xy[,2])
  S.ti <- sum(xy[,3]); S.y  <- sum(xy[,4])
  #Ratio-Schätzer
  R.xxy <- S.y/S.x1*S.ti/S.x2
  #Jacobi-Vektor:
  #dR/dS.x1/n = -n*S.y*S.ti/S.x2/S.x1^2, dR/dS.x2  = -S.y*S.ti/S.x1/S.x2^2
  #dR/dS.ti   = S.y/S.x1/S.x2,           dR/dS.y/n = n*S.ti/S.x1/S.x2
  #lässt sich mit R.xxy = S.y*S.t/S.x1/S.x2 umformen in
  #dR/dS.x1/n = -R.xxy*n/S.x1, dR/dS.x2/n  = -R.xxy*n/S.x2
  #dR/dS.ti/n   =  R.xxy*n/S.ti,   dR/dS.y/n = R.xxy*n/S.y
  J.xxy <- n*R.xxy*c(-1/S.x1,-1/S.x2,1/S.ti,1/S.y)
  #Varianz-Covarianz-Matrix von xy ("reduziert")
  xy <- as.matrix(xy)
  VC.xxy <- (t(xy)%*%xy)/n/(n-1)
  #Varianz des Ratio-Schätzers
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
#'  09.02.2015 Biomassezuwachs auf Derbholz-Kollektiv beschränkt (echter 
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
  baeume.ba <- subset(baeume.folg,
            select=c(TNr,ENr,STP,BNr,Pk,BHD1,BHD2,BaGr,akl,dkl,
                      VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,NHa2,StFl2))

  #(1.1) S-Baeume
  #(1.111) Derbholz m.R.
  iv.es.t <- aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(1.112) Derbholz m.R. im Hauptbestand (kä/11.04.2014)
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0)* ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.12) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.13) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.2) Es-Baeume (Kriterium: modellierter BHD1 >= 7 cm)
  #(1.211) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.212) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0)*ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.22) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.23) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.3) Ew-Baeume (echter Einwuchs)
  #(1.311) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.312) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7,1,0)*ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.32) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolE2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.33) oberird. Biomasse (nur Derbholzkollektiv kä/09.02.2015)
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #BA-Fläche (über alle PB der BAGr)
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$StFl2,
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)
  #Endvorrat (S2,Es)
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk<=1 & baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  names(iv.es.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.S","iV.DhmR.HB.S",
          "iV.EoR.S","iB.S","iV.DhmR.Es","iV.DhmR.HB.Es","iV.EoR.Es",
          "iB.Es","V.DhmR.E","V.DhmR.HB.E","V.EoR.E","B.E","BAF2","V.DhmR.S2.Es")
  head(iv.es.t)

  #(2) Berechnung der Bilanzkomponenten iV.A aus baeume.vor
  baeume.ba <- subset(baeume.vor,
          select=c(TNr,ENr,STP,BNr,Pk,BHD1,BHD2,BaGr,akl,dkl,
                    VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,NHa1,StFl1))
  #(2.1) A-Baeume
  #(2.111) Derbholz m.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(2.112) Derbholz m.R. im Hauptbestand Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.113) Derbholz m.R. ausgeschiedener Vorrat (insgesamt zur PM)
  iv.a.t <- cbind(iv.a.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.114) Derbholz m.R. im Hauptbestand ausgeschiedener Vorrat (insgesamt zur PM) 
  iv.a.t <- cbind(iv.a.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(2.121) Erntevolumen o.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.122) Erntevolumen o.R. ausgeschiedene Vorrat
  iv.a.t <- cbind(iv.a.t,
          aggregate(baeume.ba$VolE2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.131) oberird. Biomasse  Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.132) oberird. Biomasse  ausgeschiedener Vorrat
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$oiB2*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.14) BA-Fläche (über alle PB der BAGr)
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$StFl1,
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)

  #Anfangsvorrat (S1
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$VolV1*baeume.ba$NHa1*ifelse(baeume.ba$Pk==1,1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$VolV1*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk%in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)


  names(iv.a.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.A","iV.DhmR.HB.A",
    "V.DhmR.A","V.DhmR.HB.A","iV.EoR.A","V.EoR.A", "iB.A","B.A","BAF1",
    "V.DhmR.S1","V.DhmR.A1")
  head(iv.a.t)

  iv.es.a.t <- merge(iv.es.t,iv.a.t, by=c("TNr","BaGr","Akl","Dkl"),all.x=T,all.y=T)
  #NA-Werte auf 0 setzen, um traktweise zu bilanzieren
  iv.es.a.t[is.na(iv.es.a.t)] <- 0
  length(iv.es.a.t[,1])
 
  #Periodenlänge je Trakt hinzufügen
  nte.pl.t <- aggregate(rep(1,length(ecken[,1])),by=list(ecken$TNr),sum)
  nte.pl.t <- cbind(nte.pl.t,
                     aggregate(ecken$PL,by=list(ecken$TNr),mean)$x,
                     aggregate(ecken$PLkal,by=list(ecken$TNr),mean)$x)
  names(nte.pl.t) <- c("TNr","nTE","mPL","mPLkal")
  iv.es.a.t <- merge(iv.es.a.t,nte.pl.t,by=c("TNr"),all.x=T)

  #Zuwachsbilanz auf Traktebene (PSU) zu erstellen
  #Ausgangsdatensatz ist <iv.es.a.t>

  #  iv.es.a.t[is.na(iv.es.a.t)] <- 0
  
  head(iv.es.a.t)

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

  #Mittlere Baumartenfläche
  hbf <- length(ecken[,1]) #ohne REF!!!
  lk.2 <- hbf/sum(baeume.folg$StFl2[baeume.folg$Pk!=8])*10000
  lk.1 <- hbf/sum(baeume.vor$StFl1[baeume.vor$Pk!=8])*10000
  iv.es.a.t$mBAF <- (iv.es.a.t$BAF1*lk.1+iv.es.a.t$BAF2*lk.2)/2
  #Mitt. Baumartenfläche OHNE Lückenkorrektur (Bezugsfläche für Hauptbestand)
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
  #nach Trakt, BAGR, akl, dkl       + kä/09.02.2015: <bhd.pm>
  baeume.ba <- subset(baeume.folg,
            select=c(TNr,ENr,STP,BNr,Pk,BHD1,BHD2,bhd.pm,BaGr,akl,dkl,
                      VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,NHa2,StFl2))

  #(1.1) S-Baeume
  #(1.111) Derbholz m.R.
  iv.es.t <- aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(1.112) Derbholz m.R. im Hauptbestand (kä/11.04.2014)
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0)* ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.12) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.13) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(1.14) Grundfläche (Derbholz)
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$BHD2^2-baeume.ba$BHD1^2)*pi/40000*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk ==1,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(1.2) Es-Baeume (Kriterium: modellierter BHD1 >= 7 cm)
  #(1.211) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.212) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0)*ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.22) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.23) oberird. Biomasse
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(1.24) Grundfläche (Derbholz)
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$BHD2^2-baeume.ba$BHD1^2)*pi/40000*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0&baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(1.3) Ew-Baeume (echter Einwuchs)   
  
  #(1.311) Derbholz m.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2 >=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.312) Derbholz m.R. im Hauptbestand
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2 >=7,1,0)
          *ifelse(baeume.ba$StFl2>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.32) Erntevolumen o.R.
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolE2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2 >=7 ,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(1.33) oberird. Biomasse: nur Derbholzkollektiv kä/26.01.2015
  #kä/09.02.2015: hier wird die Differenz gebildet, da auch der Anfangswert mit
  #BHD < 7 cm eine Biomasse hat (im Unterschied zum Derbholzvolumen!)
  #Das Erreichen der Derbholzschwelle wird ebenfalls berücksichtigt!
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(1.34) Grundfläche (Derbholz) +kä/09.02.2015: Derbholz-Zuwachs: 7[cm]²abziehen!!  
  iv.es.t <- cbind(iv.es.t,
          aggregate((baeume.ba$BHD2^2-7^2)*pi/40000*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk==0 & baeume.ba$BHD1<7 & baeume.ba$BHD2>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
          
  #BA-Fläche (über alle PB der BAGr)
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$StFl2,
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)
  #Endvorrat (S2,Es)
  iv.es.t <- cbind(iv.es.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa2
          *ifelse(baeume.ba$Pk<=1 & baeume.ba$BHD1>=7,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  names(iv.es.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.S","iV.DhmR.HB.S",
          "iV.EoR.S","iB.S","iG.S","iV.DhmR.Es","iV.DhmR.HB.Es","iV.EoR.Es",
          "iB.Es","iG.Es","V.DhmR.E","V.DhmR.HB.E","V.EoR.E","B.E","G.E","BAF2",
          "V.DhmR.S2.Es")
  head(iv.es.t)

  #(2) Berechnung der Bilanzkomponenten iV.A aus baeume.vor
  baeume.ba <- subset(baeume.vor,
          select=c(TNr,ENr,STP,BNr,Pk,BHD1,BHD2,BaGr,akl,dkl,
                    VolV1,VolV2,VolE1,VolE2,oiB1,oiB2,NHa1,StFl1))
  #(2.1) A-Baeume
  #(2.111) Derbholz m.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)
  #(2.112) Derbholz m.R. im Hauptbestand Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$VolV2-baeume.ba$VolV1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.113) Derbholz m.R. ausgeschiedener Vorrat (insgesamt zur PM)
  iv.a.t <- cbind(iv.a.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.114) Derbholz m.R. im Hauptbestand ausgeschiedener Vorrat (insgesamt zur PM)
  iv.a.t <- cbind(iv.a.t,
          aggregate(baeume.ba$VolV2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0)*ifelse(baeume.ba$StFl1>0,1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(2.121) Erntevolumen o.R. Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$VolE2-baeume.ba$VolE1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.122) Erntevolumen o.R. ausgeschiedene Vorrat
  iv.a.t <- cbind(iv.a.t,
          aggregate(baeume.ba$VolE2*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.131) oberird. Biomasse  Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$oiB2-baeume.ba$oiB1)*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.132) oberird. Biomasse  ausgeschiedener Vorrat
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$oiB2*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #+++
  #(2.141) Grundfläche Zuwachs des ausgeschiedenen Vorrats
  iv.a.t <- cbind(iv.a.t,
          aggregate((baeume.ba$BHD2^2-baeume.ba$BHD1^2)*pi/40000*baeume.ba$NHa1
          *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
          by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  #(2.142) Grundfläche  ausgeschiedener Vorrat
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$BHD2^2*pi/40000*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk %in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)

  #(2.14) BA-Fläche (über alle PB der BAGr)
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$StFl1,
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x/10000)

  #Anfangsvorrat (S1
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$VolV1*baeume.ba$NHa1*ifelse(baeume.ba$Pk==1,1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)
  iv.a.t <- cbind(iv.a.t,
      aggregate(baeume.ba$VolV1*baeume.ba$NHa1
      *ifelse(baeume.ba$Pk%in%c(2:5,9),1,0),
      by=list(baeume.ba$TNr,baeume.ba$BaGr,baeume.ba$akl,baeume.ba$dkl),sum)$x)


  names(iv.a.t) <- c("TNr","BaGr","Akl","Dkl","iV.DhmR.A","iV.DhmR.HB.A",
    "V.DhmR.A","V.DhmR.HB.A","iV.EoR.A","V.EoR.A", "iB.A","B.A","iG.A","G.A",
    "BAF1","V.DhmR.S1","V.DhmR.A1")
  head(iv.a.t)

  iv.es.a.t <- merge(iv.es.t,iv.a.t, by=c("TNr","BaGr","Akl","Dkl"),all.x=T,all.y=T)
  #NA-Werte auf 0 setzen, um traktweise zu bilanzieren
  iv.es.a.t[is.na(iv.es.a.t)] <- 0
  length(iv.es.a.t[,1])

  #Periodenlänge je Trakt hinzufügen
  nte.pl.t <- aggregate(rep(1,length(ecken[,1])),by=list(ecken$TNr),sum)
  nte.pl.t <- cbind(nte.pl.t,
                     aggregate(ecken$PL,by=list(ecken$TNr),mean)$x,
                     aggregate(ecken$PLkal,by=list(ecken$TNr),mean)$x)
  names(nte.pl.t) <- c("TNr","nTE","mPL","mPLkal")
  iv.es.a.t <- merge(iv.es.a.t,nte.pl.t,by=c("TNr"),all.x=T)

  #Zuwachsbilanz auf Traktebene (PSU) zu erstellen
  #Ausgangsdatensatz ist <iv.es.a.t>

  #  iv.es.a.t[is.na(iv.es.a.t)] <- 0

  head(iv.es.a.t)

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
  #Grundfläche in m²
  iv.es.a.t$iG <-
  (iv.es.a.t$iG.S + iv.es.a.t$iG.Es + iv.es.a.t$G.E  + iv.es.a.t$iG.A)

  #Mittlere Baumartenfläche
  hbf <- length(ecken[,1]) #ohne REF!!!
  lk.2 <- hbf/sum(baeume.folg$StFl2[baeume.folg$Pk!=8])*10000
  lk.1 <- hbf/sum(baeume.vor$StFl1[baeume.vor$Pk!=8])*10000
  iv.es.a.t$mBAF <- (iv.es.a.t$BAF1*lk.1+iv.es.a.t$BAF2*lk.2)/2
  #Mitt. Baumartenfläche OHNE Lückenkorrektur (Bezugsfläche für Hauptbestand)
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
#'  wurde, wird NA zurück gegeben.
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
#' @section Note: benoetigt Tabelle \code{bacode}.
#' @param ba.lab.klass Liste mit Angaben zu Baumartenlabel und Baumartencode \cr
#'  Beispiel: ba.lab.klass <- list(ba.lab = c("FiTa","DglKiLae","Bu","Ei","BLb",
#'  "WLb"), ba.code = list(c(10:19,30:39,90:99), c(20:29,40,50,51), c(100),
#'  c(110,111), c(112:199), c(200:299))).
#' @return Dataframe-Tabelle mit Baumartengruppen-Nummer und dem entspechenden 
#'  Baumartengruppen-Label.
ba.klass.lab.tab.fun <- function(ba.lab.klass){
   n <-  length(bacode[,1])
   ba.klass.tab <- data.frame(ICode=bacode$ICode,bagr.nr=rep(0,n))
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
#' @param D.K Anzahl der Durchmesserklassen.
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
  {dkl.lab[D.k] <- paste(D.klass[[1]],"-",D.klass[[2]],sep="")} #kä/07.02.2015
  
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
    Stückzahl_Verj_Mio=round(vj.tab$n.Verjg.BAGR[1,1,]/1e6,3),
    SE_Stückzahl_Verj_Mio=round(vj.tab$n.Verjg.BAGR[1,2,]/1e6,3),
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
#nur für BWI 2 und 3!
  #(1) Befundeinheit festlegen (Traktecken auswählen)
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  names(baeume) <- tolower(names(baeume))
  names(wzp4.merkmale) <- tolower(names(wzp4.merkmale))
  #"Neutralisierung" der benötigten Attributnamen in <baeume>: <2> entfernen
  names(baeume) <- sub(2,names(baeume),replacement="")
  
  #Aus Kompatibiltäts-Gründen werden die nur in der BWI 3 vorkommenden
  #Attribute <Ast> und <Ast_Hoe> im DS der BWI 3 entfernt
  ast.pos <- grep("ast",names(wzp4.merkmale))
  if (length(ast.pos)>0)
  {
    wzp4.merkmale <- subset(wzp4.merkmale,select=c(-ast.pos))
  }
  #(2) Merkmal-DS <wzp4.merkmale> mit Attribut-Auswahl aus <baeume> verknüpfen
  wzp4.merkmale <- merge(subset(baeume,stp==0,
                                select=c(tnr,enr,bnr,ba,alt,bhd,h,volv,oib,nha,stfl)),
                         wzp4.merkmale, by=c("tnr","enr","bnr"),all.x=T)
  wzp4.merkmale[is.na(wzp4.merkmale)] <- 0
  
  #Probebäume im Stratum mit den Merkmalen in  <merkmale> auswählen
  #Anzahl zu berücksichtigender Merkmale
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
  wzp4.merkmale.s <- merge(subset(wzp4.merkmale,
                                  select=c(1:3,12,mm.pos,mm.s.pos,4:11)),subset(stratum,select=c(tnr,enr)),
                           by=c("tnr","enr"),all.y=T)
  wzp4.merkmale.s[is.na(wzp4.merkmale.s)] <- 0 
  
  #---------------------
  #(3) Flächen
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #TE im Stratum
  n.te.s <- length(stratum[,1])
  #------
  #HBF nach Trakt im Stratum
  xy <- aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,aggregate(ifelse(
    wzp4.merkmale.s$ba==999,wzp4.merkmale.s$stfl/10000,0),
    by=list(wzp4.merkmale.s$tnr),sum)$x)
  xy <- cbind(xy,aggregate(ifelse(wzp4.merkmale.s$ba==998,wzp4.merkmale.s$stfl,0),
                           by=list(wzp4.merkmale.s$tnr),sum)$x/10000)
  names(xy) <- c("tnr","hbf","bl","ibl")
  n.t.s <- length(xy[,1])
  xy <- merge(subset(trakte,select=c(tnr,m)),xy,by=c("tnr"),all.x=T)
  xy[is.na(xy)] <- 0
  #Nur die HBF der realen Baumarten (d,h. OHNE BL bzw. iBL)
  xy$hbf.ba <- xy$hbf-xy$bl-xy$ibl
  
  #HBFl. [ha]
  r.list= r.variance.fun(subset(xy,select=c(m,hbf)),nT)
  T.hbf <- r.list$R.xy*A
  se.T.hbf <- sqrt(r.list$V.R.xy)*A
  #Blößen [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,bl)),nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(subset(xy,select=c(m,ibl)),nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(subset(xy,select=c(hbf.ba,hbf)),nT)
  lk <- r.list$R.xy
  se.lk <- sqrt(r.list$V.R.xy)
  
  #Altersklassifikation
  
  A.max <- 999
  #----- kä/15.02.
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
  
  #getrennt für die Baumarten(gruppen)
  
  Kennwerte=c("Gesamtzahl","Zahl_je_ha","Vorrat_m3_mR",
              "Vorrat_m3_mR_je_ha","Stueckvolumen_m3_mR","oi_Biom_t",
              "oi_Biom_t_je_ha","Anteil_Gesamtzahl_Proz",
              "Anteil_Gesamtvorrat_Proz")
  n.kw <- length(Kennwerte)
  #Anzahl BA-Gruppen
  n.bagr <- length(ba.grupp$ba.grupp)
  n.bagr <- n.bagr+1
  #Alle Baumarten ergänzen
  ba.grupp$bagr.lab[[n.bagr]] <- "Alle BA"
  ba.grupp$ba.grupp[[n.bagr]] <- c(10:299)
  #Array  für Ergebnisse je  BAGR und AKL (Dimension 2 steht für Wert und Standardfehler)
  attr.bagr.akl.tab <- array(dim=c(n.kw,2,n.bagr,A.k))
  
  for (j in 1:A.k)
  {
    for (i in 1:n.bagr)
    {
      merkmal.t <- aggregate(ifelse(
        wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba 
        %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,wzp4.merkmale.s$nha,0),
        by=list(wzp4.merkmale.s$tnr),sum)
      #Vorrat
      merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
                  wzp4.merkmale.s$merkmal.s>0
                  &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
                  wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Oberirdische Biomasse
      merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
                  wzp4.merkmale.s$merkmal.s>0
                  &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
                  wzp4.merkmale.s$nha*wzp4.merkmale.s$oib,0),
                  by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Starkholzvorrat
      merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
        wzp4.merkmale.s$merkmal.s>0&wzp4.merkmale.s$bhd>=50
        &wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
        wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
        by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Gesamtzahl 
      merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
        wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
        wzp4.merkmale.s$nha,0), by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      #Gesamtvorrat 
      merkmal.t <- cbind(merkmal.t,aggregate(ifelse(
        wzp4.merkmale.s$pk<=1&wzp4.merkmale.s$ba %in% ba.grupp$ba.grupp[[i]]&wzp4.merkmale.s$akl==j,
        wzp4.merkmale.s$nha*wzp4.merkmale.s$volv,0),
        by=list(wzp4.merkmale.s$tnr),sum)$x)
      
      
      
      names(merkmal.t) <- c("tnr","N.MM","V.MM","oiB.MM","SthV.MM","N.ges","V.ges")
      head(merkmal.t)
      
      merkmal.t <- merge(subset(xy,select=c(tnr,m,hbf)),merkmal.t,by="tnr",
                         all.x=T)
      merkmal.t[is.na(merkmal.t)] <- 0
      
      n <- length(trakte.3[,1])
      #Stückzahl
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
      
      #Stückvolumen
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
