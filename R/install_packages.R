#!/usr/bin/Rscript --vanilla
## install_packages.R
##
## This script installs all R packages needed for our reports.
##
## Author: Dominik Cullmann, <dominik.cullmann@forst.bwl.de>
## Version: $Id: f12add996c94051c434e4c520ab5fce30445c1a4 $
install.packages(c("xtable", "plyr", "reshape", "reshape2", "ggplot2",
		   "colorspace", "munsell", "scales", "gplots", "Hmisc",
		   "RODBC"
		   )
)
