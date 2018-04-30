name <- "options.R"
root <- rprojroot::find_root(rprojroot::is_r_package)
source_file <- file.path(root, "R", name)
test_file <- file.path(root, "tests", "testthat", paste0("test-local_", name))

if (FALSE) {
    dump("result")
}
devtools::load_all()

testthat::test_file(test_file)
cov <- covr::file_coverage(source_file, test_file)
print(covr::zero_coverage(cov))
print(cov)
