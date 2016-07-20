#!/usr/bin/Rscript --vanilla
if (! require("devtools")) install.packages("devtools")
devtools::install_github("hadley/devtools")
devtools::create("bundeswaldinventur")
