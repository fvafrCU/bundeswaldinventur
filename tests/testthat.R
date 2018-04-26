library(testthat)
library(bundeswaldinventur)
if (isTRUE(as.logical(Sys.getenv("_R_COVR_RUN_")))) {
    # covr::package_coverage runs this file, but not through R CMD check.
    IS_R_CMD_CHECK <- FALSE
} else {
    IS_R_CMD_CHECK <- TRUE
}
testthat::test_check("bundeswaldinventur", filter = "^local")
