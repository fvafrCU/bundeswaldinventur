#!/usr/bin/Rscript --vanilla
## create_output_directories.R
##
## create standard output directories defined in global_variables.R
##
## Author: Dominik Cullmann, <dominik.cullmann@forst.bwl.de>
## Version: $Id: 6396ebbdbc2c41f26a8d68bc73baae32c3bef39f $
dir.create(showWarnings = FALSE, OUTPUT_DIRECTORY)  
dir.create(showWarnings = FALSE, GRAPHICS_DIRECTORY)
dir.create(showWarnings = FALSE, TABLES_DIRECTORY)
