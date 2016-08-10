#' @describeIn FVBN.bagrupp.akl.dkl.stratum.fun.2e (version 2d)
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
  #kä/11.10.2014  wegen BA-Anteilen muss hbf je Trakt im Stratum erhalten bleiben
  xy.s <- xy
  #----
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
          x.ndh.hb <- try(stats::aggregate(baeume.ba$nha*ifelse(
              baeume.ba$bhd>=7 & baeume.ba$stfl>0,1,0),
              by=list(baeume.ba$tnr),sum)$x, silent=T )
          if (length(grep("7)",dkl.lab[k]))>0)
          {
            x.ndh.hb <- rep(0,length(xy[,1]))
          }
          xy <- cbind(xy,x.ndh.hb)
          names(xy)[8] <- "ndh.hb"

          #Anzahl Traktecken je Trakt (Wald- und Nichtwald) hinzufügen
          #xy <- merge(xy,trakte[TRUE, c("tnr", "m"),by=c("tnr")]
          #Anzahl Trakte (i.S. von PSU) im Teilkollektiv ijk
          nT.bagr.akl.dkl[i,j,k] <- length(xy[,1])
          #---  Ergänzug 11.10. <m_bhb>    Hinweis: Offset für Indizierung ange-
          #passt!!
          #des Weiteren merge mit <xy.s> wegen <hbf> (begehbare HBF) für
          #Berechnung der BA-Anteile
          xy <- merge(xy.s[TRUE, c("tnr", "m", "hbf")],xy,by=c("tnr"),all.x=T)
          #xy <- merge(trakte[TRUE, c("tnr", "m,m_bhb")],xy,by=c("tnr"),all.x=T)
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

