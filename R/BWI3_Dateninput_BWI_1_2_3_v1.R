#===============================================================================
#BWI 3 Daten-Input und Ergänzung
#BWI 1, 2, 3
#Zweck: Festlegung der für die BWI-3-Auswertung zu verwendenden Daten
#einschließlich Vorinventuren

#Autor: Gerald Kändler mailto:gerald.kaendler@forst.bwl.de
#Stand: 01.05.2014
#-------------------------------------------------------------------------------
#ACCESS-DB "BWI3_HR_BW.mdb" auf Laufwerk H (vorläufig),
#später Integration in SQL-Server (Auftrag Jürgen Kayser)
#Absimmung mit Dominik Cullmann
#Korrektur 15.07.2014: Totholztabelle BWI 2 in <BWI3_HR_BW.mdb> umbenannt in
#<Totholz_B2>
#Ursprüngliche Fassung; zurückgestellt, kä/21.07.2014
#-------------------------------------------------------------------------------

library(RODBC)

channel <- odbcConnectAccess("C:/BWI3/Programme/HR/BWI3_HR_BW.mdb")
baeume.3 <- sqlFetch(channel,"BWI_3_Baeume_B")
baeume.23 <- sqlFetch(channel,"BWI_23_Baeume_B") #BWI 2 (aus Sicht BWI 3 "Status BWI 3)
baeume.2 <- sqlFetch(channel,"BWI_2_Baeume_B") #BWI 2 ("Original")
baeume.12 <- sqlFetch(channel,"BWI_12_Baeume_B") #BWI 1 (aus Sicht BWI 2 "Status BWI 1)
baeume.1 <- sqlFetch(channel,"BWI_1_Baeume_B") #BWI 1 ("Original Status BWI 1")
#Neu kä/30.09.2014
wzp4.merkmale.3 <- sqlFetch(channel,"BWI_3_WZP_sonstige_Merkmale")
#kä/14.12.2014
wzp4.merkmale.2 <- sqlFetch(channel,"BWI_2_WZP_sonstige_Merkmale")
#Neu kä/11.08.2014
verj.3   <- sqlFetch(channel,"Verjuengung_B3")
verj.2   <- sqlFetch(channel,"Verjuengung_B2")
#Neu kä/13.08.2014
verj.kl4m.3   <- sqlFetch(channel,"Verjuengung_kl_4m_B3")
verj.kl4m.2   <- sqlFetch(channel,"Verjuengung_kl_4m_B2")
#-----
#neu kä/18.08.2014
#Tabelle mit Naturnähestufen
ntns.te   <-  sqlFetch(channel,"dbo_BWI_ECKE_Naturnaehe")
#------

ecken.3  <-  sqlFetch(channel,"Eckenmerkmale_BWI3")
ecken.2  <-  sqlFetch(channel,"Eckenmerkmale_BWI2")
ecken.23 <-  sqlFetch(channel,"Eckenmerkmale_BWI23")
ecken.1  <-  sqlFetch(channel,"Eckenmerkmale_BWI1")
trakte.3 <-  sqlFetch(channel,"Trakte_BWI3")
trakte.2 <-  sqlFetch(channel,"Trakte_BWI2")
trakte.1 <-  sqlFetch(channel,"Trakte_BWI1")
totholz.3 <- sqlFetch(channel,"Totholz_B3")
totholz.2 <- sqlFetch(channel,"Totholz_B2")
#28.04.2015
#fba.3     <- sqlFetch(channel,"B3v_fba")
fba.3     <- sqlQuery(channel,
                "SELECT B3v_fba.Tnr, B3v_fba.Enr, B3v_fba.FBA, B3v_fba.Dichte
                  FROM B3v_fba ORDER BY B3v_fba.Tnr, B3v_fba.Enr;")
x_fba.3   <- sqlFetch(channel,"x3_fba")
x_dichte  <- sqlFetch(channel,"x3_dichte")
#01.05.2015
fba.2     <- sqlFetch(channel,"B2_fba")
#---------------------------------------------------------
schutzgebiete.3 <- sqlFetch(channel,"Schutzgebiete_BWI_3")
bacode  <-  sqlFetch(channel,"BACode_BWI_3")
gk.koord <- sqlFetch(channel,"GK_Koord_Ist_TE")
ecke.feld.3 <- sqlFetch(channel, "ecke_feld_3")
kreise <- sqlFetch(channel,"xbw_Landkreise_Flaechen")
odbcClose(channel)   

head(baeume.3);head(baeume.23);head(baeume.2);head(baeume.1)
head(ecken.3); head(ecken.2); head(ecken.23); head(ecken.1)
head(trakte.3); head(trakte.2); head(trakte.1)
head(totholz.3); head(totholz.2)
head(bacode)
bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
ecke.feld.3[is.na(ecke.feld.3)] <- 0
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Parameter
#A: Fläche des Inventurgebiets (Land BW)[ha]; nTE: Anzahl Ecken im Inventurgebiet;
#RF "Repräsentationsfaktor einer Ecke [ha] (=A/nTE)
A <- 3575148;	nTE <- 35731;	RF <- 100.0573172
#aktuell: 3575136 gemäß:
#http://www.statistik-portal.de/statistik-portal/de_jb01_jahrtab1.asp
#Zugriff am 17.04.2014
#Anzahl Trakte (Gesamtfläche BW)
nT <- 8970
#BWI 1 und 2:
A.12 <- 3575163; nTE.12 <- 35743;  RF.12 = 100.024144587751
#-------------------------------------------------------------------------------



#Bezeichner TNr, ENr einheitlich (seit Version 2)
#Wegen Kompatibiliät mit BWI 3
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Funktionen aktivieren
source("C:/BWI3/Programme/HR/BWI3_HR_Funktionen_v3.r")
#Totholz
source("C:/BWI3/Programme/HR/BWI3_HR_Totholz_Funktionen_v1.r")
#Grafik-Funktionen
source("C:/BWI3/Programme/HR/BWI3_HR_Grafikfunktionen_v2.r")
#Formigkeitstarif
#source("C:/BWI3/Programme/GR/Formigkeitstarif/q03_Formigkeitstarif_Funktionen_v2.r")
#oberird. Biomasse für BWI 1 und BWI 2 berechnen
#source("C:/BWI3/Methodenentwicklung/BMELV_Vertrag/Modul3/Analysen/Integrierte_analytische_Biomassefunktion_Zusammenfassung.r")
#Voraussetzung: d03
#source("C:/BWI3/Programme/GR/Formigkeitstarif/q03_Formigkeitstarif_Funktionen_v2.r")
#Baumholvolumen
#source("C:/BWI3/Methodenentwicklung/BMELV_Vertrag/Modul3/Analysen/Baumholz_Expansions_Funktion_segm_allom_mod_v1.r")
#-------------------------------------------------------------------------------

if (F)
{
#BWI 1
#BDAT-BANr
baeume.1$bdat.ba <- bdat.ba.bwi2.code(baeume.1$BA)
#Untermenge der
daten <- subset(baeume.1,STP==0&!is.na(bdat.ba),select=
                        c(TNr,ENr,STP,BNr,BA,bdat.ba,BHD1,D71,H1))
#d03
n.1 <-length(daten[,1])
daten$d031 <- round(dxmR.hx.x(daten$bdat.ba,daten$BHD1,rep(1.3,n.1),
      daten$D71,rep(7,n.1),daten$H1,0.3*daten$H1),1)
length(baeume.1[,1])

baeume.1 <- merge(baeume.1,subset(daten,select=c(TNr,ENr,STP,BNr,d031),
                          by=c("TNr","ENr","STP","BNr")),all.x=T)
baeume.1$d031[is.na(baeume.1$d031)] <- 0
baeume.1$oiB1 <- ifelse(!is.na(baeume.1$bdat.ba),
        int.analyt.oib.fun(baeume.1$BA,cbind(baeume.1$BHD1,baeume.1$d031,
                            baeume.1$H1)),0)
head(baeume.1)

if (F)
{
write.table(file="C:/BWI3/Programme/HR/BWI_1_Baeume_B_Biomasse_neu.csv",
    subset(baeume.1,select=c(TNr,ENr,STP,BNr,BA,Alt1,BHD1,D71,d031,H1,
    VolV1,oiB1,StFl1,Nha1)),row.names = FALSE, col.names = TRUE, dec=",",sep=";")
}

#-----------------------------

#BWI 2
#BDAT-BANr
baeume.2$bdat.ba <- bdat.ba.bwi2.code(baeume.2$BA)
#Untermenge der
daten <- subset(baeume.2,STP==0&!is.na(bdat.ba),select=
                        c(TNr,ENr,STP,BNr,BA,bdat.ba,BHD2,D72,H2))
#d03
n.1 <-length(daten[,1])
daten$d032 <- round(dxmR.hx.x(daten$bdat.ba,daten$BHD2,rep(1.3,n.1),
    daten$D72,rep(7,n.1), daten$H2,0.3*daten$H2),1)
length(baeume.2[,1])

baeume.2 <- merge(baeume.2,subset(daten,select=c(TNr,ENr,STP,BNr,d032),
                          by=c("TNr","ENr","STP","BNr")),all.x=T)
baeume.2$d032[is.na(baeume.2$d032)] <- 0
baeume.2$oiB2 <- ifelse(!is.na(baeume.2$bdat.ba),
        int.analyt.oib.fun(baeume.2$BA,cbind(baeume.2$BHD2,baeume.2$d032,
                            baeume.2$H2)),0)
head(baeume.2)

#Ausgabe der Dendrometrie einschl. Biomasse

if (F)
{
write.table(file="C:/BWI3/Programme/HR/BWI_2_Baeume_B_Biomasse_neu.csv",
    subset(baeume.2,select=c(TNr,ENr,STP,BNr,BA,Alt2,BHD2,D72,d032,H2,
    VolV2,oiB2,StFl2,NHa2)),row.names = FALSE, col.names = TRUE, dec=",",sep=";")
}

#-------------------------------------------------------------------------------
#Baumholzvolumen
#BWI 3
baeume.3$VolBh2[baeume.3$BA<998&baeume.3$STP==0] <- round(bh.exp.ba.dh.fun(
              baeume.3$BA[baeume.3$BA<998&baeume.3$STP==0],
              baeume.3$VolV2[baeume.3$BA<998&baeume.3$STP==0]),6)
baeume.3$VolBh2[is.na(baeume.3$VolBh2)] <- 0
vbh.t <- aggregate(baeume.3$VolBh2*baeume.3$NHa2,by=list(baeume.3$TNr),sum)
names(vbh.t) <- c("TNr","Vbh")
vbh.t <- merge(trakte.3,vbh.t,by=c("TNr"),all.x=T)
head(vbh.t)
length(vbh.t[,1])
vbh.t[is.na(vbh.t)] <- 0
r.list <- r.variance.fun(subset(vbh.t,select=c(m,Vbh)),length(vbh.t[,1]))
(T.Vbh <- A*r.list$R.xy)
(se.T.Vbh <- A*sqrt(r.list$V.R.xy))

#Alternative Berechnung des Erntevolumens (entsprechend TI)
#BDAT-BA-Nr. zufügen
baeume.3$bdat.ba <- bdat.ba.bwi2.code(baeume.3$BA)
ds <- subset(baeume.3,STP==0&NHa2>0,select=c(TNr,ENr,STP,BNr,bdat.ba,BHD2,D032,
              H2,Kh2,Kst2,HSt2))

ds$VolE2.ti <- VolEoR.dob.hkz.ti.x(
    ds$bdat.ba,ds$BHD2, ds$D032, 0.3*ds$H2,ds$H2,ds$HSt2,ds$Kh2,ds$Kst2)
ds$VolE2.fva <- VolEoR.dob.hkz.x(
    ds$bdat.ba,ds$BHD2, ds$D032, 0.3*ds$H2,ds$H2,ds$HSt2,ds$Kh2,ds$Kst2)
baeume.3 <- merge(baeume.3,subset(ds,select=c(TNr,ENr,STP,BNr,VolE2.ti,VolE2.fva)),
        by=c("TNr","ENr","STP","BNr"),all.x=T)
#-------------------------------------------------------------------------------
#kä/04.04.2014
#Koeffizienten von Dieter & Elsasser geprüft (Ei und Bu identisch!)
#----------------
#Unterirdische Baumbiomasse über Expansionsfunktion nach Dieter und Elsasser
#Koeffiziententabelle einlesen  (vorläufige Lösung zur näherunsgweisen Abschätzung
#der Wurzelbiomasse
uiB.koeff.DE.tab <-
  read.table(file="C:/BWI3/Programme/HR/Dieter_Elsasser_Wurzel.csv",
  header=T,sep=";",dec=",")
rownames(uiB.koeff.DE.tab) <- uiB.koeff.DE.tab$uib_bagr
#Zuordnungstabelle erzeugen
#BWI-BA zuordnen
#Picea       Abies       Pseudotsuga Pinus       Fagus       Quercus     ALN
uiB.ba.grupp <- list(uib.bagr=as.character(uiB.koeff.DE.tab$Baumartengruppe),
    bacode=list(c(10:19,50,51,90:99),c(30:39),c(40),c(20:29),c(100,120:199),
                c(100:114),c(200:299)))
uiB.ba.tab  <- ba.klass.lab.tab.fun(uiB.ba.grupp)
rownames(uiB.ba.tab) <- uiB.ba.tab$ICode

#-------------------------------------------------------------------------------
uiB.fun <- function(ba,oib.ha)
#Funktion nach Dieter und Elsasser zur Berechnung der Wurzelbiomasse auf der
#Basis von ha-Werten oberirdischer Biomasse
#<ba> ist die BWI-Baumart
#<oib.ha> ist die oberirdische Biomasse t je ha
{
  return((uiB.koeff.DE.tab[as.character(uiB.ba.tab[as.character(ba),2]),3]*
  sqrt(oib.ha)+uiB.koeff.DE.tab[as.character(uiB.ba.tab[as.character(ba),2]),4])^2)
}
#-------------------------------------------------------------------------------

#Ergänzen der unterird. Biomasse in <baeume.3>
head(baeume.3)

baeume.3$uiB2 <-
  ifelse(baeume.3$NHa2>0,uiB.fun(baeume.3$BA,
        baeume.3$oiB2*baeume.3$NHa2)/baeume.3$NHa2,0)

sum(baeume.3$oiB2*baeume.3$NHa2); sum(baeume.3$uiB2*baeume.3$NHa2)
length(baeume.3$uiB2[!is.na(baeume.3$uiB2)&baeume.3$NHa2>0])
length(baeume.3$oiB2[baeume.3$oiB2>0&baeume.3$NHa2>0])
head(baeume.3[is.na(baeume.3$uiB2),],20)

#BWI 2
baeume.2$uiB2 <-
  ifelse(baeume.2$NHa2>0,uiB.fun(baeume.2$BA,
          baeume.2$oiB2*baeume.2$NHa2)/baeume.2$NHa2,0)

sum(baeume.2$oiB2*baeume.2$NHa2); sum(baeume.2$uiB2*baeume.2$NHa2)

#BWI 1
baeume.1$uiB1 <-
  ifelse(baeume.1$Nha1>0,uiB.fun(baeume.1$BA,
  baeume.1$oiB1*baeume.1$Nha1)/baeume.1$Nha1,0)

sum(baeume.1$oiB1*baeume.1$Nha1); sum(baeume.1$uiB1*baeume.1$Nha1)

#-------------------------------------------------------------------------------
}