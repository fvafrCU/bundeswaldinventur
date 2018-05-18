all: usage.log diff_git.log
	cat diff_git.log || echo "no changes"

usage.log: usage.R regional.R
	./usage.R


diff_git.log: Auswertung_Kreis_Ostalbkreis.tex
	./diff_git.cl

Auswertung_Kreis_Ostalbkreis.tex: district_inloop.r regional.R devel.R
	./devel.R


pdfdiff:
	diffpdf Auswertung_Kreis_Ostalbkreis.pdf  ~/git/os/fva/hochrechnungen/regional/Auswertung_Kreis_Ostalbkreis.pdf
