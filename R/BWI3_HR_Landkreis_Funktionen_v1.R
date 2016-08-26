#' Aggregiert Flaechen fuer den Landkreis
#' 
#' Funktion wertet verschiedene Flaechen fuer den Landkreis aus, unterteilt fuer 
#' die Eigentumskategorien.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @section Hinweis: \code{krs.list} muss geladen sein
#' @param kreise A data.frame (see \code{\link{kreise}}
#' @param A A global variable  (see \code{\link{A}}
#' @param A.12 A global variable  (see \code{\link{A.12}}
#' @param trakte.3 A data.frame (see \code{\link{trakte.3}}
#' @param ecken.3 A data.frame (see \code{\link{ecken.3}}
#' @param trakte.2 A data.frame (see \code{\link{trakte.2}}
#' @param ecken.2 A data.frame (see \code{\link{ecken.2}}
#' @param trakte.1 A data.frame (see \code{\link{trakte.1}}
#' @param ecken.1 A data.frame (see \code{\link{ecken.1}}
#' @param kreiscode Code fuer den Landkreis.   kreiscode
#' @export
#' @return Liste mit folgenden Komponenten: \strong{Kreis} (Label fuer den 
#'  Kreis), \strong{KreisCode} (\code{kreiscode}), 
#'  \strong{Kreisflaeche_gesamt_ha} (Groesse des Kreises in ha), 
#'  \strong{Waldanteil} (waldanteil des Kreises an den verschiedenen BWIs),
#'  \strong{Waldflaeche} (Waldflaeche und Standardfehler fuer verschiedene 
#'  Eigentumsklassen und BWIs), \strong{Waldfl_Proz} (Waldflaeche in Prozent und 
#'  Standardfehler fuer verschiedene Eigentumsklassen), \strong{begehb_Holzbfl} 
#'  (Holzbodenflaeche und Standardfehler fuer verschiedene Eigentumsklassen)
flaechen.kreis.fun.1 <- function(kreiscode, 
                                 A = get_data("A"),
                                 A.12 = get_data("A.12"),
                                 kreise = get_data("kreise"),
                                 trakte.3 = get_data("trakte.3"),
                                 ecken.3 = get_data("ecken.3"),
                                 trakte.2 = get_data("trakte.2"),
                                 ecken.2 = get_data("ecken.2"),
                                 trakte.1 = get_data("trakte.1"),
                                 ecken.1 = get_data("ecken.1")
                                 ) {
  #Input Check
    checkmate::assertInt(kreiscode, lower = 0)
  #End Input Check
    krs.list <- unique(kreise$codeKreis)
  i <- as.numeric(rownames(kreise[kreise$codeKreis==kreiscode,]))
  #Waldfl\u00e4che insgesamt
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
  #Eigentumsart Prozent der Waldfl\u00e4che  nur f\u00fcr aktuelle BWi 3 2012
  #Staatswald (mit Bundeswald)
  fl.ant.stw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt=c("BW","StW")),
                      ecken.3)
  #K\u00f6rperschaftswald
  fl.ant.kw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt="KW"),ecken.3)
  #K\u00f6rperschaftswald
  fl.ant.oew.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),
                          list(EigArt=c("BW","StW","KW")),ecken.3)
  #Privatwald
  fl.ant.pw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt="PW"),ecken.3)
  #Gro\u00df-PW
  fl.ant.gpw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt2="GPW"),ecken.3)
  #Mittlerer PW
  fl.ant.mpw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt2="MPW"),ecken.3)
  #Klein-PW
  fl.ant.kpw.3 <-
  fl.proz.stratum.fun(list(Wa=c(3:5),Kreis=krs.list[i]),list(EigArt2="KPW"),ecken.3)

  #begehbare Holzbodenfl\u00e4che
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
    'Kreisfl\u00e4che_gesamt_ha' = kreise[kreise$codeKreis==kreiscode,5]*100,
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

#' Aggregiert Flaechen fuer ein Stratum
#' 
#' Funktion berechnet verschiedene Flaechen nach Eigentumskategorien.
#' 
#' @author Gerald Kaendler \email{gerald.kaendler@@forst.bwl.de}
#' @param auswahl auswahl Liste, welche die Eckenmerkmale mit den Werten 
#'  enthaelt, anhand derer die Auswahl fuer das Stratum erfolgt. Bsp.: 
#'  list(Wa=c(3,5), Begehbar=1).
#' @param A A global variable  (see \code{\link{A}}
#' @param A.12 A global variable  (see \code{\link{A.12}}
#' @param trakte.3 A data.frame (see \code{\link{trakte.3}}
#' @param ecken.3 A data.frame (see \code{\link{ecken.3}}
#' @param trakte.2 A data.frame (see \code{\link{trakte.2}}
#' @param ecken.2 A data.frame (see \code{\link{ecken.2}}
#' @param trakte.1 A data.frame (see \code{\link{trakte.1}}
#' @param ecken.1 A data.frame (see \code{\link{ecken.1}}
#' @export
#' @return Liste mit folgenden Komponenten: \strong{Stratum} (\code{auswahl}), 
#'  \strong{Waldflaeche} (Waldflaeche und Standardfehler fuer verschiedene 
#'  Eigentumsklassen und BWIs), \strong{Waldfl_Proz} (Waldflaeche in Prozent und 
#'  Standardfehler fuer verschiedene Eigentumsklassen), \strong{begehb_Holzbfl} 
#'  (Holzbodenflaeche und Standardfehler fuer verschiedene Eigentumsklassen)
flaechen.stratum.fun.1 <- function(auswahl,
                                 A = get_data("A"),
                                 A.12 = get_data("A.12"),
                                 trakte.3 = get_data("trakte.3"),
                                 ecken.3 = get_data("ecken.3"),
                                 trakte.2 = get_data("trakte.2"),
                                 ecken.2 = get_data("ecken.2"),
                                 trakte.1 = get_data("trakte.1"),
                                 ecken.1 = get_data("ecken.1")
                                 ){
  #Input Check
    checkmate::assertList(auswahl, min.len = 1, names = "named")
  #Output check
  auswahl.o <- auswahl
  auswahl.oo <- auswahl
  #Waldfl\u00e4che insgesamt
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
  #Eigentumsart Prozent der Waldfl\u00e4che  nur f\u00fcr aktuelle BWI 3 2012
  #Staatswald (mit Bundeswald)

  auswahl.o$Wa <- c(3:5)
  fl.ant.stw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt=c("BW","StW")),
                      ecken.3)
  #K\u00f6rperschaftswald
  fl.ant.kw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt="KW"),ecken.3)
  #K\u00f6rperschaftswald
  fl.ant.oew.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt=c("BW","StW","KW")),ecken.3)
  #Privatwald
  fl.ant.pw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt="PW"),ecken.3)
  #Gro\u00df-PW
  fl.ant.gpw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt2="GPW"),ecken.3)
  #Mittlerer PW
  fl.ant.mpw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt2="MPW"),ecken.3)
  #Klein-PW
  fl.ant.kpw.3 <-
  fl.proz.stratum.fun(auswahl.o,list(EigArt2="KPW"),ecken.3)

  #begehbare Holzbodenfl\u00e4che

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

