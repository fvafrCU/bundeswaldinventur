#!/usr/bin/Rscript --vanilla
## customization: master control 
##
## This script loads a bunch of customization scripts, it should usually be the 
## only customization script that needs to be source()d in R oder Rnw files.
##
## Author: Dominik Cullmann, <dominik.cullmann@forst.bwl.de>
## Version: $Id: 17c73a399686b55e3b180a1d26707411e231cb4e $
PROJECT_ROOT  <-  '..'
CUSTOMIZATION_DIRECTORY <- file.path(PROJECT_ROOT, 'customization')
## load common libraries
library(xtable)
library(plyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(scales)
# get rid of quotes that might interfere with Sweave (see Ripley's answer on
# http://r.789695.n4.nabble.com/Sweave-font-problems-with-Signif-codes-lines-td977346.html)
options(useFancyQuotes = FALSE)
scripts <-  c('global_variables.R', 'create_output_directories.R',
	      'load_functions.R')
for (script in scripts) {
    source(file.path(CUSTOMIZATION_DIRECTORY, script))
}


