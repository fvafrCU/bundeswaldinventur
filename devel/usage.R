#!/usr/bin/env Rscript
devtools::load_all()
source("regional.R")
sink("usage.Rout")
codetools::checkUsageEnv(.GlobalEnv)
sink()
