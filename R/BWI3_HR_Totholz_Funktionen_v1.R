#===============================================================================
#BWI 3; Funktionen zur Totholz-Auswertung
#k\u00e4/17.02.2014
#-------------------------------------------------------------------------------
#Maximalvariante
#-------------------------------------------------------------------------------
Totholz.bagr.art.zg.stratum.fun <-
          function(totholz,ecken,trakte,bwi,A,D.klass,auswahl)

#-------------------------------------------------------------------------------
#Funktion wertet nach Totholz-BA-Gruppen (Tbagr>, Totholzart <Tart> und
#Totholzzersetzungsgrad <Tzg> im Stratum aus.
#Verallgemeinerte Version f\u00fcr die Auswertung unterschiedlicher Inventurzeit-
#punkte, also BWI 2, BWI 3:
#daher werden die Datentabellen <totholz>, die Eckenmerkmale <ecken> und die
#<trakte> als Argumente \u00fcbergeben.
#Die Tabelle <totholz> muss mindestens die Attribute
#Tnr, Enr, Nr, Tbagr, Tart, Tzg, Tbd, Tsd,  Tl, Tvol, Anz, Thf enthalten;
#Hinweis BWI 3: Durchmesser: Tbd Tsd; L\u00e4nge: Tl; BWI 2: Dm, Lge
#---------------------
#"Harmonisieren" der Totholzaufnahme BWI 2 und 3
#Basis-Attribute (BWI 2)
#<TBAGr>, <TArt>, <TZg>, <TVol>, <Dm>, <Lge>, <THf>
#neu bei BWI 3
#<Tbd>: Durchmesser am st\u00e4rkeren Ende bzw. BHD
#<Tsd>: Durchmesser am d\u00fcnnen Ende (nur bei Tart 1 und 13),
#f\u00fcr Klassifikation relevant ist bei Bruchst\u00fccken der Mitten-Durchmesser,
#bei St\u00fccken mit Wurzelanlauf (stehend oder liegend) ist es der BHD
#Mittendurchmesser berechnen, wenn Tsd > 0, aus Volumen und l\u00e4nge
#<Dm> = sqrt(tvol/tl/pi)*200; die Bezeichnung <Dm> entspricht auch derjenigen
#der BWI 2;
#Wegen der Einheitlichkeit mit BWI 2wird der Bezeichner <tl> in <lge>
#umgewandelt.

#Vergleichbarkeit mit BWI 2
#Wenn(([Tart]=4 Und ([Tbd]>=60 Oder [Tl]>=0,5)) Oder ([Tart]<>4 Und [Tbd]>=20)
#---------------

#<bwi> (2 oder 3): um mit denselben Algorithmen f\u00fcr beide Zust\u00e4nde arbeiten zu
#k\u00f6nnen
#<A> ist die Fl\u00e4che in ha des Inventurgebiets zum jeweiligen Inventurzeitpunkt
#(sollte eigentlich konstant sein)
#<D.klass>: Liste mit den Klassifizierungsparametern f\u00fcr Durchmesser
#z.B. list(D.unt=0,D.ob=70,D.b=10,Ndh=T), Ndh (Nicht-Derbholz) = T bedeutet,
#dass zus\u00e4tzlich Nicht-Dh (unter 7 cm) ausgewiesen wird, sonst gilt D.unt als
#unterste Schwelle.
#<auswahl> definiert das auszuwertende Stratum entsprechend Funktion <stratum.fun>
#Die Funktion ben\u00f6tigt desweiteren die Tabelle <bacode>;

#Version 1.0 (k\u00e4/17.02.2014)
#Hinweis: um Konfliktze mit unterschiedlicher Gro\u00df-/Kleinschreibung bei den
#Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle Attribut.
#Namen auf Kleinschreibung umgestellt
#-------------------------------------------------------------------------------
{
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  n.t.s <- length(y[,1])
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(y,trakte[TRUE, c("tnr", "m")],by=c("tnr"))
  #Alle Tratecken im Inventurgebiet
  x <- trakte$m
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #----------------
  #HBFl. [ha]
  T.hbf <- sum(y$y)/sum(x)*A
  var.T.hbf <-  nT/(nT-1)*T.hbf^2*(sum(y$y^2)/sum(y$y)^2+sum(x^2)/sum(x)^2
                      -2*sum(y$y*y$m)/sum(y$y)/sum(x))
  #var.T.hbf^0.5
  se.T.hbf <- var.T.hbf^0.5 #Standardfehler
  #----------------
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(totholz) <- tolower(names(totholz))
  #Harmonisierung der Bezeichnungen zwischen BWI 2 und 3
  if(bwi==3) {
    #Bezeichnung <tl> durch <lge> (wie bei BWI 2 ersetzen
    names(totholz) <- sub("tl",names(totholz),replacement="lge")
    #Mittendurchmesser wird bei Bruchst\u00fccken (mit 2 Durchmessern als mittlerer
    #Walzedurchmesser ermittelt
    totholz$dm <- ifelse(totholz$tsd>0&!is.na(totholz$tsd),
              round(200*sqrt(totholz$tvol/totholz$lge/pi),1),totholz$tbd)
  }else #BWI 2
  {
    #Attribut <anz> einf\u00fcgen
    totholz$anz <- rep(1,length(totholz[,1]))
  }
  #Attribute und Untermenge des Stratums aus <baeume> ausw\u00e4hlen
  totholz.s <- merge(totholz[TRUE, c("tnr", "enr", "nr", "tbagr", "tart", "tzg", "tvol", "dm", "lge", "anz", "thf")],
                     stratum[TRUE, c("tnr", "enr")],
                     by=c("tnr","enr"),all.y=T)

  #BA-Gruppen-Bezeichner
  tbagr.list <- c("NB","LB","EI")
  #Klassifizierung durchf\u00fchren
  #Durchmesser
  D.max <- 999
  brks <- c(seq(D.klass[[1]],D.klass[[2]],D.klass[[3]]),D.max)
  totholz.s$dkl <- cut(totholz.s$dm, breaks=brks, right=F)
  dkl.lab <- unique(totholz.s$dkl)
  dkl.lab <- as.character(dkl.lab[order(dkl.lab)])
  D.k <- length(dkl.lab)
  #Maximale Anzahl D-klassen aus D-Kl-Parametern <D.klass>
  if(D.klass[[2]]>D.klass[[3]])
  {
    max.D.k <- (D.klass[[2]]-D.klass[[1]])/D.klass[[2]] + 2
    if (D.k < max.D.k) {D.k <- max.D.k }
  }

  tbagr.k <- length(tbagr.list)
  tart.k <- length(unique(totholz$tart))
  tart.code <- unique(totholz$tart)
  tart.code <- tart.code[order(tart.code)]
  tzg.k <- 4
  #Array f\u00fcr Ergebnisse (Totals und SE jeweils nach tbagr, tart, tzg)
  #Es gibt 2 Zielgr\u00f6\u00dfen <Y>: V [m^3], N (Gesamtzahl)
  #F\u00fcr diese 2 "Gr\u00f6\u00dfen" werden jeweils der Gesamtwert ("Total") und der Stichproben-
  #fehler (SE) berechnet, und zwar jeweils f\u00fcr die 3 Baumartengruppen <tbagr>,
  #7 Totholzarten <tart>, 4 Zersetzungsgrade <tzg> sowie D.k Durchmesserklassen
  
  Y.bagr.dkl    <- array(dim=c(2,2,tbagr.k,tart.k,tzg.k,D.k))
  nT.bagr.dkl   <- array(dim=c(tbagr.k,tart.k,tzg.k,D.k))
  #Hektarbezogene Kennwerte. 2 Zielgr\u00f6\u00dfen:  Th-Vol je ha, N Th je ha,
  Yha.bagr.dkl  <- array(dim=c(2,2,tbagr.k,tart.k,tzg.k,D.k))
  #----------------

  for (i in 1:tbagr.k)  #Baumartengruppen
  {
    for (j in 1:tart.k) #Totholzart
    {
      for (k in 1:tzg.k)  #Zersetzungsgrad
      {
          for (l in 1:D.k)  #Durchmesserklassen
          {
              ## Gerald's original was:
              #    totholz.ba <- subset(totholz.s,
              #                         tbagr==i&tart==tart.code[j]&tzg==k&dkl==dkl.lab[l],
              #                         select=c(tnr,enr,tbagr,tart,tzg,dm,lge,tvol,anz,thf))
              ## this returns an empty data.frame if the subset is not found.
              totholz.ba <- totholz.s[totholz.s[["tbagr"]] == i & 
                                      totholz.s[["tart"]] == tart.code[j] &
                                      totholz.s[["tzg"]] == k & 
                                      totholz.s[["dkl"]] == dkl.lab[l],
                                  c("tnr", "enr", "tbagr", "tart", "tzg", "dm", "lge", "tvol", "anz", "thf")]
              ## So I empty all rows if there's only NA in it.
              if(all(is.na(totholz.ba))) totholz.ba <- totholz.ba[FALSE, TRUE] 
          if (length(totholz.ba[,1])== 0)
          {
             Y.bagr.dkl[1:2,1,i,j,k,l]   <- rep(0,2)  #Zielgr\u00f6\u00dfe Total
             Y.bagr.dkl[1:2,2,i,j,k,l]   <- rep(0,2)  #Stichprobenfehler (SE)
             Yha.bagr.dkl[1:2,1,i,j,k,l] <- rep(0,2)
             Yha.bagr.dkl[1:2,1,i,j,k,l] <- rep(0,2)
             nT.bagr.dkl[i,j,k,l]    <- 0  #n PSU (Trakte)
          }else
          {
            #Nach Trakt aggregieren
            #Vol der BA-Gruppe [m²] als "v"
            xy <- stats::aggregate(totholz.ba$tvol*totholz.ba$thf*totholz.ba$anz,
                          by=list(totholz.ba$tnr),sum)
            names(xy) <- c("tnr","v")
            #Anzahl Totholzst\u00fccke als "n"
            xy <- cbind(xy,stats::aggregate(totholz.ba$thf*totholz.ba$anz,
                                      by=list(totholz.ba$tnr),sum)$x)
            names(xy)[3] <- "n"

            #Hbf-Ecken (Bezugsfl\u00e4che ha HB) und Anzahl Ecken je Trakt
            #hinzuf\u00fcgen (in <y> enthalten: y$y, y$m)
            xy <- merge(xy,y,by=c("tnr"),all.x=T)
            #Umbennen von xy$y in xy$x (Symbol f\u00fcr Bezugsfl\u00e4che Holzboden)
            names(xy)[4] <- "x"
            #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzuf\u00fcgen
            #xy <- merge(xy,trakte[TRUE, c("tnr", "m"),by=c("tnr")]
            #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
            nT.bagr.dkl[i,j,k,l] <- length(xy[,1])

            for (m in 1:2)
            {
              #Zielgr\u00f6\u00dfen Y {Vol, N)
              #Total
              Y.bagr.dkl[m,1,i,j,k,l] <-  sum(xy[,(1+m)])/sum(x)*A
              #Zugeh\u00f6riger Stichprobenfehler
              Y.bagr.dkl[m,2,i,j,k,l] <- Y.bagr.dkl[m,1,i,j,k,l]*sqrt(
              nT/(nT-1)*(sum(xy[,(1+m)]^2)/sum(xy[,(1+m)])^2+sum(x^2)/sum(x)^2
                                -2*sum(xy[,(1+m)]*xy$m)/sum(xy[,(1+m)])/sum(x)))

              #Ratio-Sch\u00e4tzer (Th-Vol/ha, Th-N/ha)
              #Bezugsfl\u00e4che ist die HBF des Stratums (also keine BA-Fl\u00e4che!)
              Yha.bagr.dkl[m,1,i,j,k,l] <- Y.bagr.dkl[m,1,i,j,k,l]/T.hbf
              tmp <- (sum(xy[,(1+m)]^2)/sum(xy[,(1+m)])^2+sum(xy$x^2)/sum(xy$x)^2
                      -2*sum(xy[,(1+m)]*xy$x)/sum(xy[,(1+m)])/sum(xy$x))
              if (isTRUE(all.equal(tmp, 0))) tmp <- 0
              Yha.bagr.dkl[m,2,i,j,k,l] <- Yha.bagr.dkl[m,1,i,j,k,l] * 
                  sqrt( nT/(nT-1)* tmp)

            }#End for l (Zielgr\u00f6\u00dfen)
          }#End if ... else
        }#End for l (D-Klassen)
      }#End for k (Zersetzungsgrad)
    }#End for j (Totholzart)
  }#End for i (Th-BAGR)
  #-----------------------
  #Tabelle f\u00fcr BA-Gruppen

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",totholz$bemerk[1],fixed=T)
  b <- nchar(as.character(totholz$bemerk[1]))
  version.totholz.b  <- substr(as.character(totholz$bemerk[1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.totholz.b=substr(as.character(totholz$bemerk[1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              BAGR.Liste=tbagr.list,
              DKL = dkl.lab,
              T.ThVN.Bagr.Tart.Tzg.Dkl=Y.bagr.dkl,
              ThVN.ha.Bagr.Tart.Tzg.Dkl=Yha.bagr.dkl,
              nT.Bagr.Tart.Tzg.Dkl=nT.bagr.dkl))
}#End <Toth.Tbagr.Tart.Tzg.stratum.fun>

#-------------------------------------------------------------------------------
Totholz.Tart.stratum.fun <-
          function(totholz,ecken,trakte,bwi,krit,tart.grupp,A,auswahl)

#-------------------------------------------------------------------------------
#Funktion wertet nach  Totholzart <Tart>, Aufnahmekriterium <krit> [2,3]
#im Stratum <auswahl> aus. Bez\u00fcglich
#der Totholz-Aufnahmeschwellen gibt es 2 Varianten:
#(1) die BWI 2-Kriterien mit Schwellendurchmessern: 20 cm am schw\u00e4cheren Ende
#bei liegenden St\u00fccken  bzw. BHD sowie 60 cm Schnittfl\u00e4chendurchmesser bei St\u00f6cken
#(2) die BWI 3-Kriterien mit Schwellendurchmessern: 10 cm am schw\u00e4cheren Ende
#bei liegenden St\u00fccken  bzw. BHD  20 cm Schnittfl\u00e4chendurchmesser bei St\u00f6cken.
#BWI 2-Kriterien bei BWI 3-Aufnahme:
#Wenn(([Tart]=4 Und ([Tbd]>=60 Oder [Tl]>=0,5)) Oder ([Tart]<>4 Und [Tbd]>=20)
#<tart.grupp> ist eine Liste, mit der Th-Arten-Gruppen definiert werden k\u00f6nnen
#z. B. tart.grupp = list(g1=c(11,12,13),g2=c(2,3),g3=c(4,5))
#Wird f\u00fcr tart.grupp NA \u00fcbergeben, werden die Original-tart-Kategorien verwendet,
#also bei BWI 3: 11,12,13,2,3,4,5; BWI 2: 1,2,3,4,5

#Verallgemeinerte Version f\u00fcr die Auswertung unterschiedlicher Inventurzeit-
#punkte, also BWI 2, BWI 3:
#daher werden die Datentabellen <totholz>, die Eckenmerkmale <ecken> und die
#<trakte> als Argumente \u00fcbergeben.
#Die Tabelle <totholz> muss mindestens die Attribute
#Tnr, Enr, Nr, Tbagr, Tart, Tzg, Tbd, Tsd,  Tl, Tvol, Anz, Thf enthalten;
#Hinweis BWI 3: Durchmesser: Tbd Tsd; L\u00e4nge: Tl; BWI 2: Dm, Lge, kein <Anz>
#---------------------
#TODO: "harmonisieren"
#Basis-Attribute (BWI 2)
#<TBAGr>, <TArt>, <TZg>, <TVol>, <Dm>, <Lge>, <THf>
#neu bei BWI 3
#<Tbd>: Durchmesser am st\u00e4rkeren Ende bzw. BHD
#<Tsd>: Durchmesser am d\u00fcnnen Ende (nur bei Tart 1 und 13),
#f\u00fcr Klassifikation relevant ist bei Bruchst\u00fccken der Mitten-Durchmesser,
#bei St\u00fccken mit Wurzelanlauf (stehend oder liegend) ist es der BHD
#Mittendurchmesser berechnen, wenn Tsd > 0, aus Volumen und l\u00e4nge
#<Dm> = sqrt(tvol/tl/pi)*200; die Bezeichnung <Dm> entspricht auch derjenigen
#der BWI 2;
#Wegen der Einheitlichkeit mit BWI 2wird der Bezeichner <tl> in <lge>
#umgewandelt.

#<bwi> (2 oder 3): um mit denselben Algorithmen f\u00fcr beide Zust\u00e4nde arbeiten zu
#k\u00f6nnen
#<A> ist die Fl\u00e4che in ha des Inventurgebiets zum jeweiligen Inventurzeitpunkt
#(sollte eigentlich konstant sein)

#<auswahl> definiert das auszuwertende Stratum entsprechend Funktion <stratum.fun>

#Version 1.0 (k\u00e4/17.02.2014)
#Hinweis: um Konfliktze mit unterschiedlicher Gro\u00df-/Kleinschreibung bei den
#Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle Attribut.
#Namen auf Kleinschreibung umgestellt
#-------------------------------------------------------------------------------
{
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  n.t.s <- length(y[,1])
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(y,trakte[TRUE, c("tnr", "m")],by=c("tnr"))
  #Alle Tratecken im Inventurgebiet
  x <- trakte$m
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #----------------
  #HBFl. [ha]
  T.hbf <- sum(y$y)/sum(x)*A
  var.T.hbf <-  nT/(nT-1)*T.hbf^2*(sum(y$y^2)/sum(y$y)^2+sum(x^2)/sum(x)^2
                      -2*sum(y$y*y$m)/sum(y$y)/sum(x))
  #var.T.hbf^0.5
  se.T.hbf <- var.T.hbf^0.5 #Standardfehler
  #----------------
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(totholz) <- tolower(names(totholz))
  #Harmonisierung der Bezeichnungen zwischen BWI 2 und 3
  if(bwi==3) {
    #Bezeichnung <tl> durch <lge> (wie bei BWI 2 ersetzen
    names(totholz) <- sub("tl",names(totholz),replacement="lge")
    #Mittendurchmesser wird bei Bruchst\u00fccken (mit 2 Durchmessern als mittlerer
    #Walzedurchmesser ermittelt
    totholz$dm <- ifelse(totholz$tsd>0&!is.na(totholz$tsd),
              round(200*sqrt(totholz$tvol/totholz$lge/pi),1),totholz$tbd)
  }else #BWI 2
  {
    #Attribut <anz> einf\u00fcgen
    totholz$anz <- rep(1,length(totholz[,1]))
  }
  #Auswahl nach Aufnahme-Kriterium:
  #nur sinnvoll, wenn Totholz-Aufnahme der BWI 3 vorliegt, dann kann entweder
  #nachden Totholfaufnahmekriterien der BWI 3 oder BWI 2 ausgewertet werden
  krit.bwi2 <- ifelse(bwi==3 & krit==2,T,F)

  if(krit.bwi2)
  #Wenn(([Tart]=4 Und ([Tbd]>=60 Oder [Tl]>=0,5)) Oder ([Tart]<>4 Und [Tbd]>=20)
  {
     totholz <- totholz[(totholz$tart!=4&totholz$tbd>=20)|(totholz$tart==4&(totholz$tbd>=60|totholz$lge>=0.5)), TRUE]
  }

  #Attribute und Untermenge des Stratums aus <totholz> ausw\u00e4hlen
  totholz.s <- merge(totholz[TRUE, c("tnr", "enr", "nr", "tart", "tvol", "dm", "lge", "anz", "thf")],
                    stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"),all.y=T)
  if(is.na(tart.grupp[[1]][1]))
  {
    tart.lab <- unique(totholz$tart)
    tart.lab <- tart.lab[order(tart.lab)]
    tart.grupp=list()
    tart.k <- length(tart.lab)
    for (i in 1:tart.k){tart.grupp[[i]]=tart.lab[i]}
  }else
  {
    tart.k <- length(tart.grupp)
    tart.lab <- "Gesamt"
  }
  
  #Array f\u00fcr Ergebnisse (Totals und SE jeweils nach tbagr, tart, tzg)
  #Es gibt 2 Zielgr\u00f6\u00dfen <Y>: V [m^3], N (Gesamtzahl)
  #F\u00fcr diese 2 "Gr\u00f6\u00dfen" werden jeweils der Gesamtwert ("Total") und der Stichproben-
  #fehler (SE) berechnet, und zwar jeweils f\u00fcr die 3 Baumartengruppen <tbagr>,
  #7 Totholzarten <tart>, 4 Zersetzungsgrade <tzg> sowie D.k Durchmesserklassen

  Y.tart    <- array(dim=c(2,2,tart.k))
  nT.tart   <- array(dim=c(tart.k,1))
  #Hektarbezogene Kennwerte. 2 Zielgr\u00f6\u00dfen:  Th-Vol je ha, N Th je ha,
  Yha.tart  <- array(dim=c(2,2,tart.k))
  #----------------

  for (i in 1:tart.k)  #Totholzart
  {
    totholz.tart <- totholz.s[totholz.s$tart%in%tart.grupp[[i]],
                        c("tnr", "enr", "tart", "dm", "lge", "tvol", "anz", "thf")]
    if (length(totholz.tart[,1])== 0)
    {
      Y.tart[1:2,1,i]   <- rep(0,2)  #Zielgr\u00f6\u00dfe Total
      Y.tart[1:2,2,i]   <- rep(0,2)  #Stichprobenfehler (SE)
      Yha.tart[1:2,1,i] <- rep(0,2)
      Yha.tart[1:2,1,i] <- rep(0,2)
      nT.tart[i,]    <- 0  #n PSU (Trakte)
    }else
    {
      #Nach Trakt aggregieren
      #Vol der BA-Gruppe [m²] als "v"
      xy <- stats::aggregate(totholz.tart$tvol*totholz.tart$thf*totholz.tart$anz,
              by=list(totholz.tart$tnr),sum)
      names(xy) <- c("tnr","v")
      #Anzahl Totholzst\u00fccke als "n"
      xy <- cbind(xy,stats::aggregate(totholz.tart$thf*totholz.tart$anz,
                                      by=list(totholz.tart$tnr),sum)$x)
      names(xy)[3] <- "n"
      #Hbf-Ecken (Bezugsfl\u00e4che ha HB) und Anzahl Ecken je Trakt
      #hinzuf\u00fcgen (in <y> enthalten: y$y, y$m)
      xy <- merge(xy,y,by=c("tnr"),all.x=T)
      #Umbennen von xy$y in xy$x (Symbol f\u00fcr Bezugsfl\u00e4che Holzboden)
      names(xy)[4] <- "x"
      #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
      nT.tart[i,] <- length(xy[,1])
      for (j in 1:2)
      {
        #Zielgr\u00f6\u00dfen Y {Vol, N)
        #Total
        Y.tart[j,1,i] <-  sum(xy[,(1+j)])/sum(x)*A
        #Zugeh\u00f6riger Stichprobenfehler
        Y.tart[j,2,i] <- Y.tart[j,1,i]*sqrt(
          nT/(nT-1)*(sum(xy[,(1+j)]^2)/sum(xy[,(1+j)])^2+sum(x^2)/sum(x)^2
                                -2*sum(xy[,(1+j)]*xy$m)/sum(xy[,(1+j)])/sum(x)))

        #Ratio-Sch\u00e4tzer (Th-Vol/ha, Th-N/ha)
        #Bezugsfl\u00e4che ist die HBF des Stratums (also keine BA-Fl\u00e4che!)
        Yha.tart[j,1,i] <- Y.tart[j,1,i]/T.hbf
        Yha.tart[j,2,i] <- Yha.tart[j,1,i]*sqrt(
          nT/(nT-1)*(sum(xy[,(1+j)]^2)/sum(xy[,(1+j)])^2+sum(xy$x^2)/sum(xy$x)^2
                             -2*sum(xy[,(1+j)]*xy$x)/sum(xy[,(1+j)])/sum(xy$x)))

      }#End for l (Zielgr\u00f6\u00dfen)
    }#End if ... else
  }#End for i (Th-Art)
  #-----------------------
  #Tabelle f\u00fcr BA-Gruppen

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",totholz$bemerk[1],fixed=T)
  b <- nchar(as.character(totholz$bemerk[1]))
  version.totholz.b  <- substr(as.character(totholz$bemerk[1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.totholz.b=substr(as.character(totholz$bemerk[1]),a,b))

  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              TArt.Liste=tart.lab,
              T.ThVN.Tart=Y.tart,
              ThVN.ha.Tart=Yha.tart,
              nT.Tart=nT.tart))
}#End <Totholz.Tart.stratum.fun>

#-------------------------------------------------------------------------------
Totholz.klass.stratum.fun <-
          function(totholz,ecken,trakte,bwi,krit,klass,A,auswahl)

#aktualisiert 10.04.2014
#-------------------------------------------------------------------------------
#Funktion wertet nach Klassifikation <klass>, Aufnahmekriterium <krit> [2,3]
#im Stratum <auswahl> aus.
#Bez\u00fcglich der Totholz-Aufnahmeschwellen <krit> gibt es 2 Varianten:
#(1) die BWI 2-Kriterien mit Schwellendurchmessern: 20 cm am schw\u00e4cheren Ende
#bei liegenden St\u00fccken  bzw. BHD sowie 60 cm Schnittfl\u00e4chendurchmesser bei St\u00f6cken
#(2) die BWI 3-Kriterien mit Schwellendurchmessern: 10 cm am schw\u00e4cheren Ende
#bei liegenden St\u00fccken  bzw. BHD  20 cm Schnittfl\u00e4chendurchmesser bei St\u00f6cken.
#BWI 2-Kriterien bei BWI 3-Aufnahme:
#Wenn(([Tart]=4 Und ([Tbd]>=60 Oder [Tl]>=0,5)) Oder ([Tart]<>4 Und [Tbd]>=20)

#<klass> ist eine Liste, mit der die Klassifikationsvariable (Attribut) und ihre
#Kategorien definiert werden k\u00f6nnen; m\u00f6gliche Klassifikationsattribute sind
#Totholzbaumartengruppe <tbagr>, Totholzart <tart>, Zersetzungsgrad <tzg>,
#Durchmesser <dm> (Mittendurchmesser, BHD, Stockschnittfl\u00e4chendurchmessser) oder
#L\u00e4nge <lge>
#z. B. klass = list(attr="tart", kat=list(c(11,12,13),c(2,3),c(4,5)))
#Wird f\u00fcr klass NA \u00fcbergeben, erfolgt keine Klassifikation.

#Verallgemeinerte Version f\u00fcr die Auswertung unterschiedlicher Inventurzeit-
#punkte, also BWI 2, BWI 3:
#daher werden die Datentabellen <totholz>, die Eckenmerkmale <ecken> und die
#<trakte> als Argumente \u00fcbergeben.
#Die Tabelle <totholz> muss mindestens die Attribute
#Tnr, Enr, Nr, Tbagr, Tart, Tzg, Tbd, Tsd,  Tl, Tvol, Anz, Thf enthalten;
#Hinweis BWI 3: Durchmesser: Tbd Tsd; L\u00e4nge: Tl; BWI 2: Dm, Lge, kein <Anz>
#---------------------
#TODO: "harmonisieren"
#Basis-Attribute (BWI 2)
#<TBAGr>, <TArt>, <TZg>, <TVol>, <Dm>, <Lge>, <THf>
#neu bei BWI 3
#<Tbd>: Durchmesser am st\u00e4rkeren Ende bzw. BHD
#<Tsd>: Durchmesser am d\u00fcnnen Ende (nur bei Tart 1 und 13),
#f\u00fcr Klassifikation relevant ist bei Bruchst\u00fccken der Mitten-Durchmesser,
#bei St\u00fccken mit Wurzelanlauf (stehend oder liegend) ist es der BHD
#Mittendurchmesser berechnen, wenn Tsd > 0, aus Volumen und L\u00e4nge
#<Dm> = sqrt(tvol/tl/pi)*200; die Bezeichnung <Dm> entspricht auch derjenigen
#der BWI 2;
#Wegen der Einheitlichkeit mit BWI 2wird der Bezeichner <tl> in <lge>
#umgewandelt.

#<bwi> (2 oder 3): um mit denselben Algorithmen f\u00fcr beide Zust\u00e4nde arbeiten zu
#k\u00f6nnen
#<A> ist die Fl\u00e4che in ha des Inventurgebiets zum jeweiligen Inventurzeitpunkt
#(sollte eigentlich konstant sein)

#<auswahl> definiert das auszuwertende Stratum entsprechend Funktion <stratum.fun>

#Version 1.0 (k\u00e4/17.02.2014)
#Hinweis: um Konfliktze mit unterschiedlicher Gro\u00df-/Kleinschreibung bei den
#Attributnamen zu vermeiden, werden innerhalb dieser Funktion alle Attribut.
#Namen auf Kleinschreibung umgestellt
#-------------------------------------------------------------------------------
{
  stratum <- stratum.fun(auswahl,ecken)
  #Kleinschreibung
  names(stratum) <- tolower(names(stratum))
  names(trakte)  <- tolower(names(trakte))
  n.te.s <- length(stratum[,1])
  y <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  names(y) <- c("tnr","y")
  n.t.s <- length(y[,1])
  #Teilmenge der Trakte im Auswertungsstratum
  y <- merge(y,trakte[TRUE, c("tnr", "m")],by=c("tnr"))
  #Alle Tratecken im Inventurgebiet
  x <- trakte$m
  #n Trakte im Inventurgebiet ist konstant
  nT <- length(trakte[,1])
  #----------------
  #HBFl. [ha]
  T.hbf <- sum(y$y)/sum(x)*A
  var.T.hbf <-  nT/(nT-1)*T.hbf^2*(sum(y$y^2)/sum(y$y)^2+sum(x^2)/sum(x)^2
                      -2*sum(y$y*y$m)/sum(y$y)/sum(x))
  #var.T.hbf^0.5
  se.T.hbf <- var.T.hbf^0.5 #Standardfehler
  #----------------
  #Kleinschreibung aller Atttributnahmen in <baeume>
  names(totholz) <- tolower(names(totholz))
  #Harmonisierung der Bezeichnungen zwischen BWI 2 und 3
  if(bwi==3) {
    #Bezeichnung <tl> durch <lge> (wie bei BWI 2 ersetzen
    names(totholz) <- sub("tl",names(totholz),replacement="lge")
    #Mittendurchmesser wird bei Bruchst\u00fccken (mit 2 Durchmessern als mittlerer
    #Walzedurchmesser ermittelt
    totholz$dm <- ifelse(totholz$tsd>0&!is.na(totholz$tsd),
              round(200*sqrt(totholz$tvol/totholz$lge/pi),1),totholz$tbd)
  }else #BWI 2
  {
    #Attribut <anz> einf\u00fcgen
    totholz$anz <- rep(1,length(totholz[,1]))
  }
  #Auswahl nach Aufnahme-Kriterium:
  #nur sinnvoll, wenn Totholz-Aufnahme der BWI 3 vorliegt, dann kann entweder
  #nachden Totholfaufnahmekriterien der BWI 3 oder BWI 2 ausgewertet werden
  krit.bwi2 <- ifelse(bwi==3 & krit==2,T,F)

  if(krit.bwi2)
  #Wenn(([Tart]=4 Und ([Tbd]>=60 Oder [Tl]>=0,5)) Oder ([Tart]<>4 Und [Tbd]>=20)
  {
     totholz <- totholz[(totholz$tart!=4&totholz$tbd>=20)|(totholz$tart==4&(totholz$tbd>=60|totholz$lge>=0.5))]
  }

  #Attribute und Untermenge des Stratums aus <totholz> ausw\u00e4hlen
  totholz.s <- merge(totholz[TRUE, c("tnr", "enr", "nr", "tbagr", "tart", "tzg", "dm", "lge", "tvol", "anz", "thf")],
                     stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"),all.y=T)

  if(is.na(klass[[1]][1]))
  {
    klass.k <- 1
  }else
  {
    #Position der m\u00f6glichen Klassifizierungsattribute im Datensatz <totholz.s>
    #tbagr: 4; tart: 5; tzg: 6; dm: 7; lge: 8
    klass.attr.list <- c("tbagr","tart","tzg","dm","lge")
    pos <- 3 + grep(klass$attr,klass.attr.list)
    #Kategorien
    #bei tbagr, tart, tzg liegen die m\u00f6glichen Kategorien fest
    #bei dm und lge  werden die Klassen aufgrund von Klassifikationsparametern
    #gebildet: unterer Schwellenwert, oberer Schwellenwert, Klassenbreite
    if (pos%in%c(4:6))
    {
      klass.k <- length(klass$kat)
    }else
    {
      klass.k <- (klass$kat[2]-klass$kat[1])/klass$kat[3]+1
      #totholz.s$kl <- cut(totholz.s[,pos],
      #      breaks=c(seq(klass$kat[1],klass$kat[2],klass$kat[3]),
      #                  (max(totholz.s[,pos],na.rm=T)+1)),right=F)
      #Hinweis: die Klassifizierungskategorien k\u00f6nnten auch dem Attribut <pos>
      #zugewiesen werden
      totholz.s[,pos] <- cut(totholz.s[,pos],
            breaks=c(seq(klass$kat[1],klass$kat[2],klass$kat[3]),
                        (max(totholz.s[,pos],na.rm=T)+1)),right=F)
      #kat <- unique(totholz.s$kl)
      kat <- unique(totholz.s[,pos])
      kat <- kat[order(kat)]
      #Hinweis: der Vektor <kat> wird an <klass$kat> \u00fcbergeben:
      klass$kat <- kat
    }
  }

  #Array f\u00fcr Ergebnisse (Totals und SE jeweils nach tbagr, tart, tzg)
  #Es gibt 2 Zielgr\u00f6\u00dfen <Y>: V [m^3], N (Gesamtzahl)
  #F\u00fcr diese 2 "Gr\u00f6\u00dfen" werden jeweils der Gesamtwert ("Total") und der Stichproben-
  #fehler (SE) berechnet, und zwar jeweils f\u00fcr die 3 Baumartengruppen <tbagr>,
  #7 Totholzarten <tart>, 4 Zersetzungsgrade <tzg> sowie D.k Durchmesserklassen

  Y.klass    <- array(dim=c(2,2,klass.k))
  nT.klass   <- array(dim=c(klass.k,1))
  #Hektarbezogene Kennwerte. 2 Zielgr\u00f6\u00dfen:  Th-Vol je ha, N Th je ha,
  Yha.klass  <- array(dim=c(2,2,klass.k))
  #----------------

  
  for (i in 1:klass.k)  #Kategorie der Klassifikation
  {
    if (klass.k>1)
    {
      totholz.klass <- totholz.s[totholz.s[TRUE, pos] %in% klass[["kat"]][[i]],
                                 c(1,2,9,10,11)] #TODO: Use named index.
    }else
    {
      totholz.klass <- totholz.s[TRUE, c(1,2,9,10,11)] #TODO: Use named index.
    }
    if (length(totholz.klass[,1])== 0)
    {
      Y.klass[1:2,1,i]   <- rep(0,2)  #Zielgr\u00f6\u00dfe Total
      Y.klass[1:2,2,i]   <- rep(0,2)  #Stichprobenfehler (SE)
      Yha.klass[1:2,1,i] <- rep(0,2)
      Yha.klass[1:2,1,i] <- rep(0,2)
      nT.klass[i,]    <- 0  #n PSU (Trakte)
    }else
    {
      #Nach Trakt aggregieren
      #Vol der BA-Gruppe [m²] als "v"
      xy <- stats::aggregate(totholz.klass$tvol*totholz.klass$thf*totholz.klass$anz,
              by=list(totholz.klass$tnr),sum,na.rm=T)
      names(xy) <- c("tnr","v")
      #Anzahl Totholzst\u00fccke als "n"
      xy <- cbind(xy,stats::aggregate(totholz.klass$thf*totholz.klass$anz,
                                      by=list(totholz.klass$tnr),sum,na.rm=T)$x)
      names(xy)[3] <- "n"
      #Hbf-Ecken (Bezugsfl\u00e4che ha HB) und Anzahl Ecken je Trakt
      #hinzuf\u00fcgen (in <y> enthalten: y$y, y$m)
      xy <- merge(xy,y,by=c("tnr"),all.x=T)
      #Umbennen von xy$y in xy$x (Symbol f\u00fcr Bezugsfl\u00e4che Holzboden)
      names(xy)[4] <- "x"
      xy[is.na(xy)] <- 0
      #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
      nT.klass[i,] <- length(xy[,1])
      for (j in 1:2)
      {
        #Zielgr\u00f6\u00dfen Y {Vol, N)
        #Total
        Y.klass[j,1,i] <-  sum(xy[,(1+j)])/sum(x)*A
        #Zugeh\u00f6riger Stichprobenfehler
        Y.klass[j,2,i] <- Y.klass[j,1,i]*sqrt(
          nT/(nT-1)*(sum(xy[,(1+j)]^2)/sum(xy[,(1+j)])^2+sum(x^2)/sum(x)^2
                                -2*sum(xy[,(1+j)]*xy$m)/sum(xy[,(1+j)])/sum(x)))

        #Ratio-Sch\u00e4tzer (Th-Vol/ha, Th-N/ha)
        #Bezugsfl\u00e4che ist die HBF des Stratums (also keine BA-Fl\u00e4che!)
        Yha.klass[j,1,i] <- Y.klass[j,1,i]/T.hbf
        Yha.klass[j,2,i] <- Yha.klass[j,1,i]*sqrt(
          nT/(nT-1)*(sum(xy[,(1+j)]^2)/sum(xy[,(1+j)])^2+sum(xy$x^2)/sum(xy$x)^2
                             -2*sum(xy[,(1+j)]*xy$x)/sum(xy[,(1+j)])/sum(xy$x)))

      }#End for j (Zielgr\u00f6\u00dfen)
    }#End if ... else
  }#End for i 
  #-----------------------
  #Tabelle f\u00fcr BA-Gruppen

  #Dokumentation der Grunddaten und Auswertungsdatum der HR
  a <- regexpr("/",totholz$bemerk[1],fixed=T)
  b <- nchar(as.character(totholz$bemerk[1]))
  version.totholz.b  <- substr(as.character(totholz$bemerk[1]),a,b)
  Log <- list(Datum=Sys.time(),
      Version.totholz.b=substr(as.character(totholz$bemerk[1]),a,b))
  if (klass.k==1)
  {
    Klass.Kat <- "alle"
  }else
  {
    Klass.Kat <- rep(0,klass.k)
    for (i in 1:klass.k)
    {
      Klass.Kat[i] <- paste(unlist(klass[[2]][i]),collapse=",")
    }
  }
  return(list(Log=Log, Stratum=auswahl, nTE=n.te.s, HBF=T.hbf, se.HBF=se.T.hbf,
              Klass.Attr = ifelse(is.na(klass[[1]][1]),"Gesamt",klass$attr),
              Klass.Kat=Klass.Kat,
              T.ThVN.klass=Y.klass,
              ThVN.ha.klass=Yha.klass,
              nT.klass=nT.klass))
}#End <Totholz.klass.stratum.fun>

#-------------------------------------------------------------------------------


#Test
if(F)
{
  A <- 3575148;
  auswahl <- list(Wa=c(3,5),Begehbar=1)
  D.klass <- list(D.unt=0,D.ob=500,D.b=500)
  th.gw.3 <-
    Totholz.bagr.art.zg.stratum.fun(totholz.3,ecken.3,trakte.3,3,A,D.klass,auswahl)
    
  th.gw.2 <-
    Totholz.bagr.art.zg.stratum.fun(totholz.2,ecken.2,trakte.2,2,A.12,D.klass,
          list(Wa=c(1:3),Begehbar=1))
      

  th.gw.ges.3 <- Totholz.Tart.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(c(11:13,2:5)),A, auswahl)

  th.gw.lg.st.so.3 <- Totholz.Tart.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(c(11:13),c(2,3),c(4,5)),A, auswahl)

  th.gw.tart.3 <- Totholz.Tart.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(c(11),c(12),c(13),c(2),c(3),c(4),c(5)),A, auswahl)
        
  th.gw.tzg.3 <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(attr="tzg",kat=list(c(1),c(2),c(3),c(4))),A, auswahl)
        
  th.gw.tbagr.3 <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(attr="tbagr",kat=list(c(1),c(2),c(3))),A, auswahl)
        
  th.gw.tartgrp.ges.3 <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(attr="tart", kat=list(c(11,12,13),c(2,3),c(4,5))),A, auswahl)
        
  th.gw.ges.3.n <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        NA,A, auswahl)
        
  th.gw.ges.3.n.bwi2krit <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,
            trakte.3,3,2, NA,A, auswahl)
  
  th.stw.ges.3.n.bwi2krit <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,
            trakte.3,3,2, NA,A, list(Wa=c(3,5),Begehbar=1, EigArt="StW"))
            
  th.stw.ges.3.n <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        NA,A, list(Wa=c(3,5),Begehbar=1, EigArt="StW"))
        
  th.kw.ges.3.n <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        NA,A, list(Wa=c(3,5),Begehbar=1, EigArt="KW"))
        
  th.kw.ges.3.n.bwi2krit <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,
        trakte.3,3,2, NA,A, list(Wa=c(3,5),Begehbar=1, EigArt="KW"))
        
  th.pw.ges.3.n <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        NA,A, list(Wa=c(3,5),Begehbar=1, EigArt="PW"))
        
  th.pw.ges.3.n.bwi2krit <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,
        trakte.3,3,2, NA,A, list(Wa=c(3,5),Begehbar=1, EigArt="PW"))
        
  th.gw.ges.2 <-  Totholz.klass.stratum.fun(totholz.2,ecken.2,trakte.2,2,2,
        NA,A.12, list(Wa=c(1:3),Begehbar=1))
        
  th.stw.ges.2 <-  Totholz.klass.stratum.fun(totholz.2,ecken.2,trakte.2,2,2,
        NA,A.12, list(Wa=c(1:3),Begehbar=1,EigArt="StW"))
  
  th.kw.ges.2 <-  Totholz.klass.stratum.fun(totholz.2,ecken.2,trakte.2,2,2,
        NA,A.12, list(Wa=c(1:3),Begehbar=1,EigArt="KW"))
  
  th.pw.ges.2 <-  Totholz.klass.stratum.fun(totholz.2,ecken.2,trakte.2,2,2,
        NA,A.12, list(Wa=c(1:3),Begehbar=1,EigArt="PW"))
        
  th.gw.tbagr.3.bwi2krit <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,
        3,2,list(attr="tbagr",kat=list(c(1),c(2),c(3))),A, auswahl)
        
  th.gw.dkl.3 <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        list(attr="dm",kat=c(0,50,10)),A, auswahl)
        
  #nur stehende ganze oder abgebrochene Totholzb\u00e4ume (Tart 2 oder 3)
  th.gw.dkl.stth.3 <-  Totholz.klass.stratum.fun(totholz.3[totholz.3$Tart%in%c(2,3), TRUE],
        ecken.3,trakte.3,3,3, list(attr="dm",kat=c(10,50,10)),A, auswahl)
  #nur liegendes Totholz (BWI 3: Tart 11,12,13)
  th.gw.dkl.lgth.3 <-  Totholz.klass.stratum.fun(totholz.3[totholz.3$Tart%in%c(11:13), TRUE],
        ecken.3,trakte.3,3,3, list(attr="dm",kat=c(0,50,10)),A, auswahl)
        
  #Alternative Berechnung des Volumens bei liegnedem Tothol (Tart = 13)
  #entsprechend TI: Berechnung als Zylinder mit gemittelten Enddurchmessern
  totholz.3$Tvol.ti <- ifelse(totholz.3$Tart==13,
    pi*((totholz.3$Tbd+totholz.3$Tsd)/400)^2*totholz.3$Tl,totholz.3$Tvol)
  utils::head(totholz.3)
  #Volumen austauschen
  totholz.3$Tvol.fva <- totholz.3$Tvol
  totholz.3$Tvol <- totholz.3$Tvol.ti 
  th.gw.ges.3.tvol.ti <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        NA,A, auswahl)
  th.gw.ges.3.tvol.ti.n <-  Totholz.klass.stratum.fun(totholz.3,ecken.3,trakte.3,3,3,
        NA,A, auswahl)
        
}
