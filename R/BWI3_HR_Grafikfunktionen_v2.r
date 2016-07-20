#===============================================================================
#BWI 3 Hochrechnung
#kä/14.03.2014 Version 2
#-------------------------------------------------------------------------------
#Funktionen für graphische Darstellungen
#Aktualisierung: BA-Farben korrigiert (Ki, Lä, Ei, Alh)
#-------------------------------------------------------------------------------
require(gplots)
#-------------------------------------------------------------------------------
plot.bar.bagr <- function(Y,se.Y,titel,y.lab)
#Funktion erzeugt Säulendiagramm für die BWI-Standard-Baumartengruppen
#kä/25.09.13
{
  #Baumartenfarben
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  clrs <- c("grey30","red","violet","orange","yellow","green","darkblue","lightblue",
            "brown")
  dx <- 0.4
  k <- length(clrs)
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  p <- 5*10^(floor(log(max(Y)+max(se.Y))/log(10))-1)
  plot(x=1:k,Y,"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,ceiling((max(Y)+max(se.Y))/p)*p),
        main=titel,xlab="Baumartengruppe", ylab=y.lab,
        sub=Sys.time(),cex.sub=0.5)
  axis(side=1,at=1:k,labels=bagr.list)

  #Säulen mit Fehlerbalken einzeichnen
  for (i in 1:k)
  {
    #Säulen-Polygon
    y <- c(0,Y[i],Y[i],0)
    x <- c(i-dx,i-dx,i+dx,i+dx)
    polygon(x,y, col=clrs[i])
    #Fehlerbalken
    y0 <- Y[i]-se.Y[i]
    y1 <- Y[i]+se.Y[i]
    x0 <- i; x1 <- i
    segments(x0,y0,x1,y1,col="grey",lwd=2)
    x0 <- i-0.1; x1 <- i+0.1;
    segments(x0,y0,x1,y0,col="grey",lwd=2)
    segments(x0,y1,x1,y1,col="grey",lwd=2)
  }
}
#-------------------------------------------------------------------------------

plot.mult.bar.bagr <- function(Y,se.Y,titel,bar.lab,y.lab)
#Funktion erzeugt Säulendiagramm für die BWI-Standard-Baumartengruppen
#mit der Möglichkeit, mehrere Datenreihen als Säulen je Kategorie nebeneinander
#zu platzieren
#kä/30.09.13
#<Y> bzw. <se.Y> muss hier eine Matrix sein mit den Spalten als Datenreihen
#<bar.lab> enthält die Bezeichnungen je Datenreihe
{
  #Baumartenfarben
  bagr.list <- c("FI","TA","DGL","KI","LAE","BU","EI","ALH","ALN")
  clrs <- c("grey30","red","violet","orange","yellow","green","darkblue","lightblue",
            "brown")

  k <- length(clrs)
  n.r <- length(Y[1,])  #Anzahl Datenreihen
  dx <- 0.4
  dx.r <- 2*dx/n.r
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  p <- 5*10^(floor(log(max(Y)+max(se.Y))/log(10))-1)
  plot(x=1:k,Y[,1],"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,ceiling((max(Y)+max(se.Y))/p)*p),
        main=titel,xlab="Baumartengruppe", ylab=y.lab,
        sub=Sys.time(),cex.sub=0.5)
  axis(side=1,at=1:k,labels=bagr.list)

  #Säulen mit Fehlerbalken einzeichnen

  for (r in 1:n.r)
  {
    for (i in 1:k)
    {
      dx.i <- i-dx+(r-1)*dx.r             #neu
      #Säulen-Polygon
      y <- c(0,Y[i,r],Y[i,r],0)
      x <- c(dx.i,dx.i,dx.i+dx.r,dx.i+dx.r) #neu
      polygon(x,y, col=clrs[i])
      #Fehlerbalken
      y0 <- Y[i,r]-se.Y[i,r]
      y1 <- Y[i,r]+se.Y[i,r]
      x0 <- dx.i+dx.r/2; x1 <- x0
      segments(x0,y0,x1,y1,col="grey",lwd=2)
      x0 <- x0-0.1/n.r; x1 <- x1+0.1/n.r;
      segments(x0,y0,x1,y0,col="grey",lwd=2)
      segments(x0,y1,x1,y1,col="grey",lwd=2)
    }
  }
  #Legende
  legend("topright",legend=paste(1:n.r,". Säule: ",bar.lab,sep=""))

}
#-------------------------------------------------------------------------------


plot.bar.kl <- function(Y,se.Y,kl.lab,titel,x.lab,y.lab,clr)
#<Y>: Zielgröße
#<se.Y>: Stichprobenfehler der Zielgröße
#<kl.lab>: Klassen (x-Achse)
#<titel>
#<x.lab>: Beschriftung der x-Achse (Klassifizierte Größe)
#<y.lab>: Einheit der Zielgröße
#<clr>: Farbe für Säulen
#Funktion erzeugt Säulendiagramm für eine klassifizierte Zielgröße (AKl oder DKl)
#kä/25.09.13
{

  dx <- 0.4
  k <- length(kl.lab)
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  #NA aus Vektoren entfernen
  se.Y[is.na(se.Y)]  <- 0
  p <- 5*10^(floor(log(max(Y)+max(se.Y))/log(10))-1)
  plot(x=1:k,Y,"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,ceiling((max(Y)+max(se.Y))/p)*p),
        main=titel,xlab=x.lab, ylab=y.lab,
        sub=Sys.time(), cex.sub=0.5)
  axis(side=1,at=1:k,labels=kl.lab)

  #Säulen mit Fehlerbalken einzeichnen
  for (i in 1:k)
  {
    #Säulen-Polygon
    y <- c(0,Y[i],Y[i],0)
    x <- c(i-dx,i-dx,i+dx,i+dx)
    polygon(x,y, col=clr)
    #Fehlerbalken, wenn SE >0
    if (se.Y[i] > 0)
    {
      y0 <- Y[i]-se.Y[i]
      y1 <- Y[i]+se.Y[i]
      x0 <- i; x1 <- i
      segments(x0,y0,x1,y1,col="grey",lwd=2)
      x0 <- i-0.1; x1 <- i+0.1;
      segments(x0,y0,x1,y0,col="grey",lwd=2)
      segments(x0,y1,x1,y1,col="grey",lwd=2)
    }
  }
}
#-------------------------------------------------------------------------------

plot.bar.kl.2 <- function(Y,se.Y,kl.lab,titel,x.lab,y.lab,kl.clrs)
#Variante: für jede Klasse kann eine Farbe festgelegt werden
#<Y>: Zielgröße
#<se.Y>: Stichprobenfehler der Zielgröße
#<kl.lab>: Klassen (x-Achse)
#<titel>
#<x.lab>: Beschriftung der x-Achse (Klassifizierte Größe)
#<y.lab>: Einheit der Zielgröße
#<clr>: Farbe für Säulen
#Funktion erzeugt Säulendiagramm für eine klassifizierte Zielgröße (AKl oder DKl)
#kä/25.09.13
{

  dx <- 0.4
  k <- length(kl.lab)
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  #NA aus Vektoren entfernen
  se.Y[is.na(se.Y)]  <- 0
  p <- 5*10^(floor(log(max(Y)+max(se.Y))/log(10))-1)
  plot(x=1:k,Y,"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,ceiling((max(Y)+max(se.Y))/p)*p),
        main=titel,xlab=x.lab, ylab=y.lab,
        sub=Sys.time(), cex.sub=0.5)
  axis(side=1,at=1:k,labels=kl.lab)

  #Säulen mit Fehlerbalken einzeichnen
  for (i in 1:k)
  {
    #Säulen-Polygon
    y <- c(0,Y[i],Y[i],0)
    x <- c(i-dx,i-dx,i+dx,i+dx)
    polygon(x,y, col=kl.clrs[i])
    #Fehlerbalken, wenn SE >0
    if (se.Y[i] > 0)
    {
      y0 <- Y[i]-se.Y[i]
      y1 <- Y[i]+se.Y[i]
      x0 <- i; x1 <- i
      segments(x0,y0,x1,y1,col="grey",lwd=2)
      x0 <- i-0.1; x1 <- i+0.1;
      segments(x0,y0,x1,y0,col="grey",lwd=2)
      segments(x0,y1,x1,y1,col="grey",lwd=2)
    }
  }
}
#-------------------------------------------------------------------------------

plot.mult.bar.kl <- function(Y,se.Y,bar.lab,kl.lab,titel,x.lab,y.lab,bar.clr)
#<Y>: Zielgrößen-Matrix mit Datenreihen in den Spalten
#<se.Y>: Stichprobenfehler der Zielgrößen-Matrix mit Datenreihen in den Spalten
#<bar.lab>: Bezeichnung der Datenreihen
#<kl.lab>: Klassen (x-Achse)
#<titel>
#<x.lab>: Beschriftung der x-Achse (Klassifizierte Größe)
#<y.lab>: Einheit der Zielgröße
#<bar.clr>: Farben für Säulen der jeweiligen Dataenreihe
#Funktion erzeugt Säulendiagramm (nebeneinander liegende Säu,en)
#für eine klassifizierte Zielgröße (AKl oder DKl) kä/30.09.13
{

  n.r <- length(Y[1,])  #Anzahl Datenreihen
  dx <- 0.45
  dx.r <- 2*dx/n.r
  k <- length(kl.lab)
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  Y[is.nan(Y)]  <- 0
  #NA aus Vektoren entfernen
  se.Y[is.na(se.Y)]  <- 0
  Y[is.na(Y)]  <- 0
  Y[Y==Inf] <- 0
  se.Y[se.Y==Inf] <- 0
  p <- 5*10^(floor(log(max(Y)+max(se.Y))/log(10))-1)
  p <- ifelse(is.nan(p),1,p)
  plot(x=1:k,Y[,1],"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,ceiling((max(Y)+max(se.Y))/p)*p),
        main=titel,xlab=x.lab, ylab=y.lab,
        sub=Sys.time(), cex.sub=0.5)
  axis(side=1,at=1:k,labels=kl.lab)

  #Säulen mit Fehlerbalken einzeichnen
  for (r in 1:n.r)
  {
    for (i in 1:k)
    {
      dx.i <- i-dx+(r-1)*dx.r
      #Säulen-Polygon
      y <- c(0,Y[i,r],Y[i,r],0)
      x <- c(dx.i,dx.i,dx.i+dx.r,dx.i+dx.r)
      polygon(x,y, col=bar.clr[r])
      #Fehlerbalken, wenn SE >0
      if (se.Y[i] > 0)
      {
        y0 <- Y[i,r]-se.Y[i,r]
        y1 <- Y[i,r]+se.Y[i,r]
        x0 <- dx.i+dx.r/2; x1 <- x0
        segments(x0,y0,x1,y1,col="grey",lwd=2)
        x0 <- x0-0.1/n.r; x1 <- x1+0.1/n.r;
        segments(x0,y0,x1,y0,col="grey",lwd=2)
        segments(x0,y1,x1,y1,col="grey",lwd=2)
      }
    }
  }
  #Legende
  legend("topleft",legend=bar.lab,fill=bar.clr)

}

#-------------------------------------------------------------------------------

plot.line.bar.kl <- function(Y,se.Y,bar.lab,kl.lab,titel,x.lab,y.lab,y.max,
                              line.clr, bar.clr, l.wd)
#Funktion kombiniert Linien und Säulen-Darstellung
#nur für 2 Datenreihen
#<Y>: Zielgrößen-Matrix mit den 2 Datenreihen in den Spalten; die erste wird
#als Linie, die zweite in Säulen dargestellt
#<se.Y>: Stichprobenfehler der Zielgrößen-Matrix mit Datenreihen in den Spalten
#<bar.lab>: Bezeichnung der Datenreihen
#<kl.lab>: Klassen (x-Achse)
#<titel>
#<x.lab>: Beschriftung der x-Achse (Klassifizierte Größe)
#<y.lab>: Einheit der Zielgröße
#<y.max>: maximaler Wert der Y-Achse ( wenn NA, dann wird es von der Funktion
#         selbst bestimmt
#<line.clr>: Farbe der Linie
#<bar.clr>: Farbe für Säulen der Datenreihe
#<l.wd>: Liniendicke (=lwd)
#für eine klassifizierte Zielgröße (AKl oder DKl) kä/16.03.2014
{
  n.r <- 2
  dx <- 0.33
  dx.r <- dx
  k <- length(kl.lab)
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  Y[is.nan(Y)]  <- 0
  #NA aus Vektoren entfernen
  #se.Y[is.na(se.Y)]  <- 0
  #Y[is.na(Y)]  <- 0
  Y[Y==Inf] <- 0
  se.Y[se.Y==Inf] <- 0
  p <- 5*10^(floor(log(max(Y,na.rm=T)+max(se.Y,na.rm=T))/log(10))-1)
  p <- ifelse(is.nan(p),1,p)
  y.max <-ifelse(is.na(y.max),ceiling((max(Y,na.rm=T)+max(se.Y,na.rm=T))/p)*p,y.max)
  plot(x=1:k,Y[,1],"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,y.max),
        main=titel,xlab=x.lab, ylab=y.lab,
        sub=Sys.time(), cex.sub=0.5)
  axis(side=1,at=1:k,labels=kl.lab)


  #1. Säulen mit Fehlerbalken einzeichnen
  r <- 2
  for (i in 1:k)
  {
    dx.i <- i-dx
    #Säulen-Polygon
    y <- c(0,Y[i,r],Y[i,r],0)
    x <- c(dx.i,dx.i,dx.i+2*dx,dx.i+2*dx.r)
    polygon(x,y, col=bar.clr)
    #Fehlerbalken, wenn SE >0
    if (!is.na(se.Y[i]))
    {
      y0 <- Y[i,r]-se.Y[i,r]
      y1 <- Y[i,r]+se.Y[i,r]
      x0 <- i; x1 <- x0
      segments(x0,y0,x1,y1,col="grey10",lwd=2)
      x0 <- x0-0.1; x1 <- x1+0.1;
      segments(x0,y0,x1,y0,col="grey30",lwd=2)
      segments(x0,y1,x1,y1,col="grey30",lwd=2)
    }
  }
  
  #2. Linie einzeichnen

  r <- 1
  lines(1:k,Y[,r],col=line.clr,lwd=l.wd)
  #Fehlerbalken
  for (i in 1:k)
  {
    if (!is.na(se.Y[i,r]))
    {
      y0 <- Y[i,r]-se.Y[i,r]
      y1 <- Y[i,r]+se.Y[i,r]
      x0 <- i; x1 <- x0
      segments(x0,y0,x1,y1,col=line.clr,lwd=2)
      x0 <- x0-0.1; x1 <- x1+0.1;
      segments(x0,y0,x1,y0,col=line.clr,lwd=2)
      segments(x0,y1,x1,y1,col=line.clr,lwd=2)
    }
  }

  #Legende
  legend("topright",legend=bar.lab,fill=c(line.clr,bar.clr))

} #Ende <plot.line.bar.kl>

#-------------------------------------------------------------------------------

plot.mult.line.kl <- function(Y,se.Y,line.lab,kl.lab,titel,x.lab,y.lab,y.max,
                              line.clr, l.wd)
#Funktion erstellt mehrfache Linien-Darstellung für n.r Datenreihen
#<Y>: Zielgrößen-Matrix mit den n.r Datenreihen in den Spalten
#<se.Y>: Stichprobenfehler der Zielgrößen-Matrix mit n.r Datenreihen in den Spalten
#<bar.lab>: Vektor mit Bezeichnungen der Datenreihen
#<kl.lab>: Vektor der Klassen bzw. Werte der x-Achse
#<titel>
#<x.lab>: Beschriftung der x-Achse
#<y.lab>: Einheit der Zielgröße auf der y-Achse
#<y.max>: maximaler Wert der Y-Achse ( wenn NA, dann wird es von der Funktion
#         selbst bestimmt
#<line.clr>: Vektor der Linienfarben (=col)
#<l.wd>: Vektor der Liniendicken (=lwd)
#kä/16.03.2014
{
  n.r <- length(Y[1,])
  k <- length(kl.lab)
  dx <- 0.33
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  Y[is.nan(Y)]  <- 0
  #NA aus Vektoren entfernen
  #se.Y[is.na(se.Y)]  <- 0
  #Y[is.na(Y)]  <- 0
  Y[Y==Inf] <- 0
  se.Y[se.Y==Inf] <- 0
  p <- 5*10^(floor(log(max(Y,na.rm=T)+max(se.Y,na.rm=T))/log(10))-1)
  p <- ifelse(is.nan(p),1,p)
  y.max <-ifelse(is.na(y.max),ceiling((max(Y,na.rm=T)+max(se.Y,na.rm=T))/p)*p,y.max)
  plot(x=1:k,Y[,1],"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,y.max),
        main=titel,xlab=x.lab, ylab=y.lab,
        sub=Sys.time(), cex.sub=0.5)
  axis(side=1,at=1:k,labels=kl.lab)


  #Linien einzeichnen

  for (r in 1:n.r)
  {
    lines(1:k,Y[,r],col=line.clr[r],lwd=l.wd[r])
    #Fehlerbalken
    for (i in 1:k)
    {
      if (!is.na(se.Y[i,r]))
      {
        y0 <- Y[i,r]-se.Y[i,r]
        y1 <- Y[i,r]+se.Y[i,r]
        x0 <- i; x1 <- x0
        segments(x0,y0,x1,y1,col=line.clr[r],lwd=2)
        x0 <- x0-0.1; x1 <- x1+0.1;
        segments(x0,y0,x1,y0,col=line.clr[r],lwd=2)
        segments(x0,y1,x1,y1,col=line.clr[r],lwd=2)
      }
    }
  }

  #Legende
  legend("topright",legend=line.lab,lwd=l.wd,col=line.clr)

} #Ende <plot.mult.line.kl>

#-------------------------------------------------------------------------------

plot.mult.line.kl.2 <- function(Y,se.Y,line.lab,kl.lab,titel,x.lab,y.lab,y.max,
                              line.clr, l.wd)
#Funktion erstellt mehrfache Linien-Darstellung für n.r Datenreihen
#Variante OHNE Fehlerbalken, statt dessen Linien
#<Y>: Zielgrößen-Matrix mit den n.r Datenreihen in den Spalten
#<se.Y>: Stichprobenfehler der Zielgrößen-Matrix mit n.r Datenreihen in den Spalten
#<bar.lab>: Vektor mit Bezeichnungen der Datenreihen
#<kl.lab>: Vektor der Klassen bzw. Werte der x-Achse
#<titel>
#<x.lab>: Beschriftung der x-Achse
#<y.lab>: Einheit der Zielgröße auf der y-Achse
#<y.max>: maximaler Wert der Y-Achse ( wenn NA, dann wird es von der Funktion
#         selbst bestimmt
#<line.clr>: Vektor der Linienfarben (=col)
#<l.wd>: Vektor der Liniendicken (=lwd)
#kä/16.03.2014
{
  n.r <- length(Y[1,])
  k <- length(kl.lab)
  dx <- 0.33
  #NaN aus Vektoren entfernen
  se.Y[is.nan(se.Y)]  <- 0
  Y[is.nan(Y)]  <- 0
  #NA aus Vektoren entfernen
  #se.Y[is.na(se.Y)]  <- 0
  #Y[is.na(Y)]  <- 0
  Y[Y==Inf] <- 0
  se.Y[se.Y==Inf] <- 0
  p <- 5*10^(floor(log(max(Y,na.rm=T)+max(se.Y,na.rm=T))/log(10))-1)
  p <- ifelse(is.nan(p),1,p)
  y.max <-ifelse(is.na(y.max),ceiling((max(Y,na.rm=T)+max(se.Y,na.rm=T))/p)*p,y.max)
  plot(x=1:k,Y[,1],"n",lwd=20,col="grey70",xaxt="n",
        xlim=c(dx,k+dx),ylim=c(0,y.max),
        main=titel,xlab=x.lab, ylab=y.lab,
        sub=Sys.time(), cex.sub=0.5)
  axis(side=1,at=1:k,labels=kl.lab)


  #Linien einzeichnen

  for (r in 1:n.r)
  {
    lines(1:k,Y[,r],col=line.clr[r],lwd=l.wd[r])
    #Fehlerlinien
    lines(1:k,(Y[,r]-se.Y[,r]),col=line.clr[r],lwd=1,lty=2)
    lines(1:k,(Y[,r]+se.Y[,r]),col=line.clr[r],lwd=1,lty=2)
  }

  #Legende
  legend("topright",legend=line.lab,lwd=l.wd,col=line.clr)

} #Ende <plot.mult.line.kl.2>

#-------------------------------------------------------------------------------
plot.mult.stacked.bar <- function(y.mat,bar.lab,kat.lab,titel,x.lab,y.lab,kat.col)
#Basis ist <barplot2> aus <gplots>
#<y.mat> entspricht <height> als Matrix
#<bar.lab> entspricht <names.arg>   (z.B. Inventur bzw. Referenzjahr)
#<kat.lab> enstpricht den Bezeichnungen der Kategorien der gestapelten Säulen
#(z.B. Baumarten(gruppen)) = <legend.text>
#<titel> = <main>; <x.lab> = X-Achsen-Beschriftung; <y.lab> = Y-Achsen-Beschr.
#<kat.col>: Farben für die Kategorien innerhalb der gestapelten Säulen
{
 x.max <- length(y.mat[1,])*1.4
 p <- barplot2(y.mat,names.arg=bar.lab,legend.text=kat.lab,xlab=x.lab,ylab=y.lab,
    main=titel,axes=T,col=kat.col, xlim=c(0,x.max),width=0.9)

 for (j in 1:length(y.mat[1,]))
 {
  y <- 0; y.max <- sum(y.mat[,j])
  for (i in 1:length(y.mat[,1]))
  {
    #print(paste(y.mat[i,j]))
    y <- y + y.mat[i,j]
    text(p[j],y-y.mat[i,j]/2+0.03*y.max,paste(y.mat[i,j]),pos=1)
  }
 }

}#Ende <plot.mult.stacked.bar>
#-------------------------------------------------------------------------------

#Tests
if(F)
{

 y.mat <- cbind(
    round( FVBN.Gw.1$T.FVBN.Bagr.Akl.Dkl[1,1,,,]/sum( FVBN.Gw.1$T.FVBN.Bagr.Akl.Dkl[1,1,,,])*100,1),
    round( FVBN.Gw.2$T.FVBN.Bagr.Akl.Dkl[1,1,,,]/sum( FVBN.Gw.2$T.FVBN.Bagr.Akl.Dkl[1,1,,,])*100,1),
    round( FVBN.Gw.3$T.FVBN.Bagr.Akl.Dkl[1,1,,,]/sum( FVBN.Gw.3$T.FVBN.Bagr.Akl.Dkl[1,1,,,])*100,1))
 bar.lab <- c("1987","2002","2012")
 kat.lab <- FVBN.Gw.1$BAGR.Liste
 titel <- "Entwicklung der Baumartenanteile im Land Baden-Württemberg"
 x.lab="";y.lab="";
 kat.col <- c("grey30","red","violet","yellow","orange","green","lightblue","darkblue",
            "brown")

 #------------
 
 plot.mult.stacked.bar(y.mat,bar.lab,kat.lab,titel,x.lab,y.lab,kat.col)
 #---------
 x.max <- length(y.mat[1,])+1
 
 p <- barplot2(y.mat,names.arg=bar.lab,legend.text=kat.lab,xlab=x.lab,ylab=y.lab,
    main=titel,axes=T,col=kat.col, xlim=c(0,x.max),width=0.9)

 for (j in 1:length(y.mat[1,]))
 {
  y <- 0
  for (i in 1:length(y.mat[,1]))
  {
    #print(paste(y.mat[i,j]))
    y <- y + y.mat[i,j]
    text(p[j],y-y.mat[i,j]/2+3,paste(y.mat[i,j]),pos=1)
  }
 }
 points(1,10,pch=20)
 
 # Test von <balloonplot>: bringt nicht viel!
 y <- FVBN.Gw.3$BAGR.Liste
 x <- c(1987,2002,2012)
 z <- cbind(FVBN.Gw.1$T.FVBN.Bagr.Akl.Dkl[1,1,,,],
            FVBN.Gw.2$T.FVBN.Bagr.Akl.Dkl[1,1,,,],
            FVBN.Gw.3$T.FVBN.Bagr.Akl.Dkl[1,1,,,])
 ds <- data.frame(bwi=c(rep(1987,9),rep(2002,9),rep(2012,9)),bagr=rep(y,3),
        baf=c(FVBN.Gw.1$T.FVBN.Bagr.Akl.Dkl[1,1,,,],
            FVBN.Gw.2$T.FVBN.Bagr.Akl.Dkl[1,1,,,],
            FVBN.Gw.3$T.FVBN.Bagr.Akl.Dkl[1,1,,,]))
 bagr.col <- c("grey30","red","violet","yellow","orange","green","lightblue","darkblue",
            "brown")
 balloonplot(ds$bwi,ds$bagr,ds$baf, dotcolor=bagr.col,sorted=F,cum.margins=F,
            show.margins=F,label=F,xlab="",ylab="", scale.method="volume",
            label.lines=F)
 ds <- data.frame(bwi=c(rep(1987,9),rep(2002,9),rep(2012,9)),bagr=rep(y,3),
        baf=c(FVBN.Gw.1$T.FVBN.Bagr.Akl.Dkl[2,1,,,],
            FVBN.Gw.2$T.FVBN.Bagr.Akl.Dkl[2,1,,,],
            FVBN.Gw.3$T.FVBN.Bagr.Akl.Dkl[2,1,,,]))
 balloonplot(ds$bwi,ds$bagr,ds$baf, dotcolor=bagr.col,sorted=F,cum.margins=F,
            show.margins=F,label=F,xlab="",ylab="", scale.method="volume",
            label.lines=F)
}