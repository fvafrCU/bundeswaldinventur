name <- "adapt_script.R"
root <- rprojroot::find_root(rprojroot::is_r_package)
source_file <- file.path(root, "R", name)
is_runit <- TRUE

if (isTRUE(is_runit)) {
    test_file <- file.path(root, "inst", "runit_tests", paste0("runit-", name))
} else {
    test_file <- file.path(root, "tests", "testthat", paste0("test-local_", name))
}

if (FALSE) {
    dump("result")
}
devtools::load_all()

if (isTRUE(is_runit)) {
    runit <- RUnit::runTestFile(test_file)
    RUnit::printTextProtocol(runit)
    message("coverage somehow doesn't work with single files...")
} else {
    testthat::test_file(test_file)
    cov <- covr::file_coverage(source_file, test_file)
    print(covr::zero_coverage(cov))
    print(cov)
}
