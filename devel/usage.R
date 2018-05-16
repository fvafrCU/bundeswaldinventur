#!/usr/bin/env Rscript
devtools::load_all()
source("regional.R")
sink("usage.log")
codetools::checkUsageEnv(.GlobalEnv)
sink()
