fvbn.kreis.fun.1 <- function(kreiscode,eig.list,bwi.list,bagr)
#Funktion führt Standard-FVBN-Auswertung für den Landkreis mit <kreiscode>
#für die in der <eig.list> aufgeführten Eigentumskategorien
#{gw,stw,kw,oew,pw,gpw,mpw,kpw} für die in
#<bwi.list> aufgeführten BWI-Aufnahmen {1,2,3}
#Es wird die Funktion <FVBN.bagrupp.akl.dkl.stratum.fun.2d> verwendet, welche
#neben Baumartengruppen auch die Summenwerte für alle Baumarten liefert
#kä/02.12.14)

{
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

#Funktion wertet Flächen für Landkreis aus
flaechen.kreis.fun.1 <- function(kreiscode)
#Funktion berechnet verschiedene Flächen nach Eigentumskategorien
{
  i <- as.numeric(rownames(kreise[kreise$codeKreis==kreiscode,]))
  #Waldfläche insgesamt
  (wfl.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i]),ecken.3,trakte.3,A))
  (wfl.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i]),ecken.2,trakte.2,A.12))
  (wfl.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i]),ecken.1,trakte.1,A.12))

  #nach Eigentumsarten:
  #StW  (mit Bundeswald)
  (wfl.stw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt=c("BW","StW")),
                      ecken.3,trakte.3,A))
  (wfl.stw.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt=c("BW","StW")),
                      ecken.2,trakte.2,A.12))
  (wfl.stw.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt=c("BW","StW")),
                      ecken.1,trakte.1,A.12))

  #BW
  (wfl.bw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt="BW"),
                      ecken.3,trakte.3,A))
  #KW
  (wfl.kw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt="KW"),
                      ecken.3,trakte.3,A))
  (wfl.kw.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt="KW"),
                      ecken.2,trakte.2,A.12))
  (wfl.kw.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt="KW"),
                      ecken.1,trakte.1,A.12))
  #OEW
  (wfl.oew.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],
                EigArt=c("BW","StW","KW")), ecken.3,trakte.3,A))
  (wfl.oew.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],
                EigArt=c("BW","StW","KW")), ecken.2,trakte.2,A.12))
  (wfl.oew.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],
                EigArt=c("BW","StW","KW")), ecken.1,trakte.1,A.12))

  #PW
  (wfl.pw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt="PW"),
                      ecken.3,trakte.3,A))
  (wfl.pw.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt="PW"),
                      ecken.2,trakte.2,A.12))
  (wfl.pw.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt="PW"),
                      ecken.1,trakte.1,A.12))

  #GPW
  (wfl.gpw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt2="GPW"),
                      ecken.3,trakte.3,A))
  (wfl.gpw.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt2="GPW"),
                      ecken.2,trakte.2,A.12))
  (wfl.gpw.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt2="GPW"),
                      ecken.1,trakte.1,A.12))
  #MPW
  (wfl.mpw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt2="MPW"),
                      ecken.3,trakte.3,A))
  (wfl.mpw.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt2="MPW"),
                      ecken.2,trakte.2,A.12))
  (wfl.mpw.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt2="MPW"),
                      ecken.1,trakte.1,A.12))

  #KPW
  (wfl.kpw.3 <-
        fl.stratum.fun(list(Wa=c(3,4,5),Kreis=krs.list[i],EigArt2="KPW"),
                      ecken.3,trakte.3,A))
  (wfl.kpw.2 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt2="KPW"),
                      ecken.2,trakte.2,A.12))
  (wfl.kpw.1 <-
        fl.stratum.fun(list(Wa=c(1:4),Kreis=krs.list[i],EigArt2="KPW"),
                      ecken.1,trakte.1,A.12))

  #--------------------
  #Eigentumsart Prozent der Waldfläche  nur für aktuelle BWi 3 2012
  #Staatswald (mit Bundeswald)
  fl.ant.stw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt=c("BW","StW")),
                      ecken.3)
  #Körperschaftswald
  fl.ant.kw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt="KW"),ecken.3)
  #Körperschaftswald
  fl.ant.oew.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),
                          list(EigArt=c("BW","StW","KW")),ecken.3)
  #Privatwald
  fl.ant.pw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt="PW"),ecken.3)
  #Groß-PW
  fl.ant.gpw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt2="GPW"),ecken.3)
  #Mittlerer PW
  fl.ant.mpw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt2="MPW"),ecken.3)
  #Klein-PW
  fl.ant.kpw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt2="KPW"),ecken.3)

  #begehbare Holzbodenfläche
  (bhbf.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i]),
                      ecken.3,trakte.3,A))
  (bhbf.stw.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt="StW"),ecken.3,trakte.3,A))
  (bhbf.kw.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt="KW"),ecken.3,trakte.3,A))
  (bhbf.oew.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt=c("BW","StW","KW")),ecken.3,trakte.3,A))
  (bhbf.pw.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt="PW"),ecken.3,trakte.3,A))
  (bhbf.gpw.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt2="GPW"),ecken.3,trakte.3,A))
  (bhbf.mpw.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt2="MPW"),ecken.3,trakte.3,A))
  (bhbf.kpw.3 <- fl.stratum.fun(list(Wa=c(3,5),Begehbar=1,Kreis=krs.list[i],
                      EigArt2="KPW"),ecken.3,trakte.3,A))

  return(list(Kreis = as.character(kreise[kreise$codeKreis==kreiscode,3]),
    KreisCode = kreiscode,
    Kreisfläche_gesamt_ha = kreise[kreise$codeKreis==kreiscode,5]*100,
    Waldanteil=data.frame(BWI=c(1987,2002,2012),
    Waldanteil= round(c(wfl.3$Flaeche/kreise[kreise$codeKreis==kreiscode,5],
                  wfl.2$Flaeche/kreise[kreise$codeKreis==kreiscode,5],
                  wfl.1$Flaeche/kreise[kreise$codeKreis==kreiscode,5]),1)),
    Waldflaeche=data.frame(BWI=c(1987,2002,2012),
    Waldfl_GW   = round(c(wfl.1$Flaeche,wfl.2$Flaeche,wfl.3$Flaeche),0),
    SE_Waldfl_GW= round(c(wfl.1$SE_Flaeche,wfl.2$SE_Flaeche, wfl.3$SE_Flaeche),0),
    Waldfl_StW  = round(c(wfl.stw.1$Flaeche, wfl.stw.2$Flaeche,wfl.stw.3$Flaeche),0),
    SE_Waldfl_StW=round(c(wfl.stw.1$SE_Flaeche,wfl.stw.2$SE_Flaeche,
                        wfl.stw.3$SE_Flaeche),0),
    Waldfl_KW = round(c(wfl.kw.1$Flaeche,wfl.kw.2$Flaeche,wfl.kw.3$Flaeche),0),
    SE_Waldfl_KW = round(c(wfl.kw.1$SE_Flaeche,wfl.kw.2$SE_Flaeche,
                        wfl.kw.3$SE_Flaeche),0),
    Waldfl_OEW = round(c(wfl.oew.1$Flaeche,wfl.oew.2$Flaeche,wfl.oew.3$Flaeche),0),
    SE_Waldfl_OEW = round(c(wfl.oew.1$SE_Flaeche,wfl.oew.2$SE_Flaeche,
                        wfl.oew.3$SE_Flaeche),0),
    Waldfl_PW = round(c(wfl.pw.1$Flaeche,wfl.pw.2$Flaeche,wfl.pw.3$Flaeche),0),
    SE_Waldfl_PW = round(c(wfl.pw.1$SE_Flaeche,wfl.pw.2$SE_Flaeche,
                        wfl.pw.3$SE_Flaeche),0),
    Waldfl_GPW = round(c(wfl.gpw.1$Flaeche,wfl.gpw.2$Flaeche,wfl.gpw.3$Flaeche),0),
    SE_Waldfl_GPW = round(c(wfl.gpw.1$SE_Flaeche,wfl.gpw.2$SE_Flaeche,
                        wfl.gpw.3$SE_Flaeche),0),
    Waldfl_MPW = round(c(wfl.mpw.1$Flaeche,wfl.mpw.2$Flaeche,wfl.mpw.3$Flaeche),0),
    SE_Waldfl_MPW = round(c(wfl.mpw.1$SE_Flaeche,wfl.mpw.2$SE_Flaeche,
                        wfl.mpw.3$SE_Flaeche),0),
    Waldfl_KPW = round(c(wfl.kpw.1$Flaeche,wfl.kpw.2$Flaeche,wfl.kpw.3$Flaeche),0),
    SE_Waldfl_KPW = round(c(wfl.kpw.1$SE_Flaeche,wfl.kpw.2$SE_Flaeche,
                        wfl.kpw.3$SE_Flaeche),0)),
    Waldfl_Proz = data.frame(Eigentum=c("StW(BW)","KW","OEW","PW","GPW","MPW","KPW"),
        Waldfl_Proz = round(c(fl.ant.stw.3$Fl_Proz, fl.ant.kw.3$Fl_Proz,
              fl.ant.oew.3$Fl_Proz,fl.ant.pw.3$Fl_Proz,fl.ant.gpw.3$Fl_Proz,
              fl.ant.mpw.3$Fl_Proz, fl.ant.kpw.3$Fl_Proz),1),
        SE_Waldfl_Proz = round(c(fl.ant.stw.3$SE_Fl_Proz,fl.ant.kw.3$SE_Fl_Proz,
              fl.ant.oew.3$SE_Fl_Proz,fl.ant.pw.3$SE_Fl_Proz,
              fl.ant.gpw.3$SE_Fl_Proz,
              fl.ant.mpw.3$SE_Fl_Proz,fl.ant.kpw.3$SE_Fl_Proz),1)),
    begehb_Holzbfl = data.frame(Eigentum=
          c("GW","StW(BW)","KW","OEW","PW","GPW","MPW","KPW"),
        b_HBF = round(c(bhbf.3$Flaeche,bhbf.stw.3$Flaeche,bhbf.kw.3$Flaeche,
            bhbf.oew.3$Flaeche,bhbf.pw.3$Flaeche,bhbf.gpw.3$Flaeche,
            bhbf.mpw.3$Flaeche,bhbf.kpw.3$Flaeche),0),
        SE_begehb_Holzbfl = round(c(bhbf.3$SE_Flaeche,bhbf.stw.3$SE_Flaeche,
                bhbf.kw.3$SE_Flaeche, bhbf.oew.3$SE_Flaeche,
                bhbf.pw.3$SE_Flaeche,bhbf.gpw.3$SE_Flaeche,
                bhbf.mpw.3$SE_Flaeche,bhbf.kpw.3$SE_Flaeche),0))))
} #Ende <flaechen.kreis.fun.1>
#-------------------------------------------------------------------------------

flaechen.stratum.fun.1 <- function(auswahl)
#Funktion berechnet verschiedene Flächen nach Eigentumskategorien
{
  auswahl.o <- auswahl
  auswahl.oo <- auswahl
  #Waldfläche insgesamt
  auswahl$Wa <- c(3:5)
  (wfl.3 <-
        fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.2 <-
        fl.stratum.fun(auswahl,ecken.2,trakte.2,A.12))
  (wfl.1 <-
        fl.stratum.fun(auswahl,ecken.1,trakte.1,A.12))

  #nach Eigentumsarten:
  #StW  (mit Bundeswald)
  auswahl$EigArt=c("BW","StW")
  auswahl$Wa <- c(3:5)
  (wfl.stw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.stw.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.stw.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))

  #BW
  auswahl$EigArt="BW"
  (wfl.bw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  #KW
  auswahl$EigArt="KW"
  auswahl$Wa <- c(3:5)
  (wfl.kw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.kw.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.kw.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))
  #OEW
  auswahl$EigArt=c("BW","StW","KW")
  auswahl$Wa <- c(3:5)
  (wfl.oew.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.oew.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.oew.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))

  #PW
  auswahl$EigArt="PW"
  auswahl$Wa <- c(3:5)
  (wfl.pw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.pw.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.pw.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))

  #GPW
  auswahl <- auswahl.o
  auswahl$EigArt2="GPW"
  auswahl$Wa <- c(3:5)
  (wfl.gpw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.gpw.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.gpw.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))
  #MPW
  auswahl$EigArt2="MPW"
  auswahl$Wa <- c(3:5)
  (wfl.mpw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.mpw.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.mpw.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))

  #KPW
  auswahl$EigArt2="KPW"
  auswahl$Wa <- c(3:5)
  (wfl.kpw.3 <-
        fl.stratum.fun(auswahl, ecken.3,trakte.3,A))
  auswahl$Wa <- c(1:4)
  (wfl.kpw.2 <-
        fl.stratum.fun(auswahl, ecken.2,trakte.2,A.12))
  (wfl.kpw.1 <-
        fl.stratum.fun(auswahl, ecken.1,trakte.1,A.12))

  #--------------------
  #Eigentumsart Prozent der Waldfläche  nur für aktuelle BWI 3 2012
  #Staatswald (mit Bundeswald)

  auswahl.o$Wa <- c(3:5)
  fl.ant.stw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt=c("BW","StW")),
                      ecken.3)
  #Körperschaftswald
  fl.ant.kw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt="KW"),ecken.3)
  #Körperschaftswald
  fl.ant.oew.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt=c("BW","StW","KW")),ecken.3)
  #Privatwald
  fl.ant.pw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt="PW"),ecken.3)
  #Groß-PW
  fl.ant.gpw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt2="GPW"),ecken.3)
  #Mittlerer PW
  fl.ant.mpw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt2="MPW"),ecken.3)
  #Klein-PW
  fl.ant.kpw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt2="KPW"),ecken.3)

  #begehbare Holzbodenfläche

  auswahl.o$Wa=c(3,5)
  auswahl.o$Begehbar=1
  auswahl <- auswahl.o
  (bhbf.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$EigArt="StW"
  (bhbf.stw.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$EigArt="KW"
  (bhbf.kw.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$EigArt=c("BW","StW","KW")
  (bhbf.oew.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$EigArt="PW"
  (bhbf.pw.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl <- auswahl.o
  auswahl$EigArt2="GPW"
  (bhbf.gpw.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$EigArt2="MPW"
  (bhbf.mpw.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))
  auswahl$EigArt2="KPW"
  (bhbf.kpw.3 <- fl.stratum.fun(auswahl,ecken.3,trakte.3,A))

  return(list(Stratum = auswahl.oo,
    Waldflaeche=data.frame(BWI=c(1987,2002,2012),
    Waldfl_GW   = round(c(wfl.1$Flaeche,wfl.2$Flaeche,wfl.3$Flaeche),0),
    SE_Waldfl_GW= round(c(wfl.1$SE_Flaeche,wfl.2$SE_Flaeche, wfl.3$SE_Flaeche),0),
    Waldfl_StW  = round(c(wfl.stw.1$Flaeche, wfl.stw.2$Flaeche,wfl.stw.3$Flaeche),0),
    SE_Waldfl_StW=round(c(wfl.stw.1$SE_Flaeche,wfl.stw.2$SE_Flaeche,
                        wfl.stw.3$SE_Flaeche),0),
    Waldfl_KW = round(c(wfl.kw.1$Flaeche,wfl.kw.2$Flaeche,wfl.kw.3$Flaeche),0),
    SE_Waldfl_KW = round(c(wfl.kw.1$SE_Flaeche,wfl.kw.2$SE_Flaeche,
                        wfl.kw.3$SE_Flaeche),0),
    Waldfl_OEW = round(c(wfl.oew.1$Flaeche,wfl.oew.2$Flaeche,wfl.oew.3$Flaeche),0),
    SE_Waldfl_OEW = round(c(wfl.oew.1$SE_Flaeche,wfl.oew.2$SE_Flaeche,
                        wfl.oew.3$SE_Flaeche),0),
    Waldfl_PW = round(c(wfl.pw.1$Flaeche,wfl.pw.2$Flaeche,wfl.pw.3$Flaeche),0),
    SE_Waldfl_PW = round(c(wfl.pw.1$SE_Flaeche,wfl.pw.2$SE_Flaeche,
                        wfl.pw.3$SE_Flaeche),0),
    Waldfl_GPW = round(c(wfl.gpw.1$Flaeche,wfl.gpw.2$Flaeche,wfl.gpw.3$Flaeche),0),
    SE_Waldfl_GPW = round(c(wfl.gpw.1$SE_Flaeche,wfl.gpw.2$SE_Flaeche,
                        wfl.gpw.3$SE_Flaeche),0),
    Waldfl_MPW = round(c(wfl.mpw.1$Flaeche,wfl.mpw.2$Flaeche,wfl.mpw.3$Flaeche),0),
    SE_Waldfl_MPW = round(c(wfl.mpw.1$SE_Flaeche,wfl.mpw.2$SE_Flaeche,
                        wfl.mpw.3$SE_Flaeche),0),
    Waldfl_KPW = round(c(wfl.kpw.1$Flaeche,wfl.kpw.2$Flaeche,wfl.kpw.3$Flaeche),0),
    SE_Waldfl_KPW = round(c(wfl.kpw.1$SE_Flaeche,wfl.kpw.2$SE_Flaeche,
                        wfl.kpw.3$SE_Flaeche),0)),
    Waldfl_Proz = data.frame(Eigentum=c("StW(BW)","KW","OEW","PW","GPW","MPW","KPW"),
        Waldfl_Proz = round(c(fl.ant.stw.3$Fl_Proz, fl.ant.kw.3$Fl_Proz,
              fl.ant.oew.3$Fl_Proz,fl.ant.pw.3$Fl_Proz,fl.ant.gpw.3$Fl_Proz,
              fl.ant.mpw.3$Fl_Proz, fl.ant.kpw.3$Fl_Proz),1),
        SE_Waldfl_Proz = round(c(fl.ant.stw.3$SE_Fl_Proz,fl.ant.kw.3$SE_Fl_Proz,
              fl.ant.oew.3$SE_Fl_Proz,fl.ant.pw.3$SE_Fl_Proz,
              fl.ant.gpw.3$SE_Fl_Proz,
              fl.ant.mpw.3$SE_Fl_Proz,fl.ant.kpw.3$SE_Fl_Proz),1)),
    begehb_Holzbfl = data.frame(Eigentum=
          c("GW","StW(BW)","KW","OEW","PW","GPW","MPW","KPW"),
        b_HBF = round(c(bhbf.3$Flaeche,bhbf.stw.3$Flaeche,bhbf.kw.3$Flaeche,
            bhbf.oew.3$Flaeche,bhbf.pw.3$Flaeche,bhbf.gpw.3$Flaeche,
            bhbf.mpw.3$Flaeche,bhbf.kpw.3$Flaeche),0),
        SE_begehb_Holzbfl = round(c(bhbf.3$SE_Flaeche,bhbf.stw.3$SE_Flaeche,
                bhbf.kw.3$SE_Flaeche, bhbf.oew.3$SE_Flaeche,
                bhbf.pw.3$SE_Flaeche,bhbf.gpw.3$SE_Flaeche,
                bhbf.mpw.3$SE_Flaeche,bhbf.kpw.3$SE_Flaeche),0))))
} #Ende <flaechen.stratum.fun.1>
#-------------------------------------------------------------------------------

