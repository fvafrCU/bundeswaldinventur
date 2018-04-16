library(testthat)
library(bundeswaldinventur)
probably_me <- function() {
    me <- Sys.info()[["nodename"]] %in% c("h6", "fvafrdebianCU") &&
        .Platform[["OS.type"]] == "unix"
    return(me)
}

if (probably_me()) test_check("bundeswaldinventur")

