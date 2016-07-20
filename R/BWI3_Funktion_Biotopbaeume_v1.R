#===============================================================================
#Funktion zur Auswertung der Biotop-Bäume
#kä/10.10.2014
#-------------

#Funktion wertet Biotop-Bäume aus, die bei der 3. BWI aufgenommen worden sind
#als Merkmal der Probebäume in der WZP 4
#als Biotop-Bäume werden die Probebäume erfasst, welche mindestens eines der
#Merkmale: FaulKon, Hoehle, Bizarr, Uralt, Horst, MBiotop erfüllen

biotop.baeume.fun <- function(wzp4.merkmale,baeume,ecken,trakte,auswahl)
#Version 1, 10.10.14
#funktioniert nicht für <baume.1> der BWI 1!!!
#<wzp4.merkmale> ist die Tabelle, welche die besonderen Biotop-Baum-Merkmale
#enthält u.a. FaulKon, Hoehle, Bizarr, Uralt, Horst, MBiotop
#<baeume> enthält die Probebaum-Attribute
#<ecken> enthält die Trakteckenmerkmale
#<trakte> entält traktbezogene Informationen (u.a. m, m_HB, ...)
#<auswahl> ist die Liste, welche bezogen auf die Tabelle <ecken> die
#Befundeinheit über die Funktion <stratum.fun>  festlegt, z.B.
#list(Wa=c(3,5),Begehbar=1)
{

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
              
}  #Ende <biotop.baeume.fun>
