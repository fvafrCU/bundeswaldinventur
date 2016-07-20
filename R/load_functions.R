#!/usr/bin/Rscript --vanilla
## load_functions.R
##
## source all files in FUNCTIONS_DIRECTORY and HR_FUNCTIONS_DIRECTORY.
##
## Author: Dominik Cullmann, <dominik.cullmann@forst.bwl.de>
## Version: $Id: 3b95f8a321fc99bb24077745b082595592b06c43 $
for (file in c('BWI3_HR_Funktionen_v3.r', 'BWI3_HR_Totholz_Funktionen_v1.r',
	       'BWI3_HR_Grafikfunktionen_v2.r')) {
    source(file.path(HR_FUNCTIONS_DIRECTORY, file), encoding = 'Latin1')
}
for (file in list.files(FUNCTIONS_DIRECTORY, pattern = '.R$')) {
    source(file.path(FUNCTIONS_DIRECTORY, file))
}
