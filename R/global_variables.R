#!/usr/bin/Rscript --vanilla
## global_variables.R
##
## define global variables used by most scripts.
##
## Author: Dominik Cullmann, <dominik.cullmann@forst.bwl.de>
## Version: $Id: 227df1eb6407be22b6e08805af4874bd292734c3 $
HR_FUNCTIONS_DIRECTORY <- file.path(PROJECT_ROOT, 'HR_Funktionen')
FUNCTIONS_DIRECTORY <- file.path(PROJECT_ROOT, 'functions')
DATA_DIRECTORY <- file.path(PROJECT_ROOT, 'data')
DATA_WORKSPACE  <-  file.path(DATA_DIRECTORY, 'mdb_output.Rdata')
STATISTICS_WORKSPACE  <-  file.path(DATA_DIRECTORY, 'statistics_output.Rdata')

MOUNT_POINT <- switch(.Platform$OS.type,
        'windows' = "H:",
        'unix' = "/trans/h",
        stop(paste('unkown operation system', .Platform$OS.type))
       )
PROJECT_ROOT_SERVER <- file.path(MOUNT_POINT,
				 'BuI', 'Projekte', 'BWI3_P932', 
				 'BWI_3_Inventur_P932', '07_Auswertungen', 
				 'LandBW', 'Hochrechnungen'
				 )

OUTPUT_DIRECTORY  <- file.path(PROJECT_ROOT_SERVER, 'Ergebnisse')
GRAPHICS_DIRECTORY  <- file.path(OUTPUT_DIRECTORY, 'Grafiken')
TABLES_DIRECTORY  <- file.path(OUTPUT_DIRECTORY, 'Tabellen')
COLORS_BWI <- c(
		rgb(117, 112, 179, maxColorValue = 255),
		rgb(217,  95,   2,    maxColorValue = 255),
		rgb( 27, 158, 119, maxColorValue = 255)
		)

COLORS_BWI <- c(rgb(154,205,50, maxColorValue = 255),
		rgb(124,252,0, maxColorValue = 255), 
		rgb(0,100,0, maxColorValue = 255)
		
		)
# Old stuff from BWI3_Dateninput_BWI_1_2_3_v1.r
##Parameter
##A: Fläche des Inventurgebiets (Land BW)[ha]; nTE: Anzahl Ecken im Inventurgebiet;
##RF "Repräsentationsfaktor einer Ecke [ha] (=A/nTE)
A <- 3575148;	nTE <- 35731;	RF <- A/nTE
##aktuell: 3575136 gemäß:
##http://www.statistik-portal.de/statistik-portal/de_jb01_jahrtab1.asp
##Zugriff am 17.04.2014
##Anzahl Trakte (Gesamtfläche BW)
nT <- 8970
##BWI 1 und 2:
A.12 <- 3575163; nTE.12 <- 35743;  RF.12 = A.12/nTE.12
##
# Definition der BWI-Baumartengruppen
assign('BAGR.BWI', 
       list(bagr.lab = c("FI", "TA", 
			 "DGL", "KI", "LAE", 
			 "BU", "EI",
			 "ALH", "ALN"),
	    ba.grupp = list(c(10:19, 90:99), c(30:39), 
			    c(40), c(20:29), c(50, 51),
			    c(100), c(110:114),
			    c(120:199), c(200:299)),
	    ba.colors = c("grey30", "red", 
			  "violet", "orange", "yellow", 
			  "green", "darkblue",
			  rgb(100,190, 150, maxColorValue = 255), "brown"),
	    ba.text = c("Fichte", "Tanne", 
			"Douglasie", "Kiefer", "Lärche",
			"Buche", "Eiche", 
			"ALH", "ALN")
			)
)
assign('bagr.bwi', BAGR.BWI) # for compabilitiy 
BAGR.LIST <- BAGR.BWI$bagr.lab # for compabilitiy
bagr.list <- BAGR.LIST # for compabilitiy 

