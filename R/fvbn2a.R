#' @describeIn FVBN.bagrupp.akl.dkl.stratum.fun.2e (version 2a)
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
  baeume.s <- merge(baeume[TRUE, c("tnr", "enr", "ba", "alt", "bhd", "volv", "oib", "nha", "stfl")],
                    stratum[TRUE, c("tnr", "enr")],by=c("tnr","enr"),all.y=T)

  #Klassifizierung durchführen
  #Baumartengruppen-Zuordnungstabelle für BWI-BA-Code erzeugen
  #(Tab. <bacode> muss geladen sein)
  bagr.tab <- ba.klass.lab.tab.fun(BA.grupp)
  #BA-Gruppe dazu spielen
  baeume.s <- merge(baeume.s,bagr.tab[TRUE, c("ICode", "bagr")],
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
  xy <- stats::aggregate(rep(1,length(stratum[,1])),by=list(stratum$tnr),sum)
  #Blößen (BL): BA=999, Lücken (iBL): BA=998
  xy <- cbind(xy,stats::aggregate(ifelse(baeume.s$ba==999,baeume.s$stfl/10000,0),
                  by=list(baeume.s$tnr),sum)$x)
  xy <- cbind(xy,stats::aggregate(ifelse(baeume.s$ba==998,baeume.s$stfl,0),
                  by=list(baeume.s$tnr),sum)$x/10000)
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
  #Blößen [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "bl")],nT)
  T.bl <- r.list$R.xy*A
  se.T.bl <- sqrt(r.list$V.R.xy)*A
  #Ideelle Blößen ("Lücken") [ha]
  r.list <- r.variance.fun(xy[TRUE, c("m", "ibl")],nT)
  T.ibl <- r.list$R.xy*A
  se.T.ibl <- sqrt(r.list$V.R.xy)*A
  #Lückenkorrekturfaktor
  r.list <- r.variance.fun(xy[TRUE, c("hbf.ba", "hbf")],nT)
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
        baeume.ba <- baeume.s[
                      baeume.s[["bagr"]]==bagr.list[i]&baeume.s[["akl"]]==akl.lab[j]&baeume.s[["dkl"]]==dkl.lab[k],
                      c("tnr", "enr", "bhd", "dkl", "volv", "oib", "nha", "stfl")]
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
          xy <- stats::aggregate(baeume.ba$stfl,by=list(baeume.ba$tnr),sum)
          names(xy) <- c("tnr","x")
          xy$x <- xy$x/10000*lk  #Umrechnung in ha  und Lückenkorrektur
          #Derbholz-Vorrat [m³ mR] als "v"
          xy <- cbind(xy,stats::aggregate(baeume.ba$volv*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[3] <- "v"
          #Derbholz-Vorrat [m³ mR] im Hauptbestand als "v.hb"
          xy <- cbind(xy,
            stats::aggregate(baeume.ba$volv*baeume.ba$nha*ifelse(baeume.ba$stfl>0,1,0),
                                            by=list(baeume.ba$tnr),sum)$x )
          names(xy)[4] <- "v.hb"
          #oberird. Biomasse [t] als "b"
          xy <- cbind(xy,stats::aggregate(baeume.ba$oib*baeume.ba$nha,
                                            by=list(baeume.ba$tnr),sum)$x/1000)
          names(xy)[5] <- "b"
          #Anzahl Bäume als "n"
          xy <- cbind(xy,stats::aggregate(baeume.ba$nha,by=list(baeume.ba$tnr),sum)$x)
          names(xy)[6] <- "n"
          #Anzahl nur Derbholz-Bäume (bhd>=7)  als "ndh"
          #sofern in der Klassifizierung solche vorkommen!
          x.ndh <- try(stats::aggregate(baeume.ba$nha*ifelse(baeume.ba$bhd>=7,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh)
          names(xy)[7] <- "ndh"

          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,trakte[TRUE, c("tnr", "m"),by=c("tnr")]
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---
          xy <- merge(trakte[TRUE, c("tnr", "m")],xy,by=c("tnr"),all.x=T)
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
              "Gr\u00f6\u00dfen" = c("Wert","Standardfehler"),
              BAGR=bagr.list,AKL = akl.lab[1:A.k], DKL = dkl.lab,
              T.FVBN.Bagr.Akl.Dkl=Y.bagr.akl.dkl,
              FVBN.ha.Bagr.Akl.Dkl=Yha.bagr.akl.dkl,
              nT.Bagr.Akl.Dkl=nT.bagr.akl.dkl))
}#End <FVBN.bagrupp.akl.dkl.stratum.fun.2a>

