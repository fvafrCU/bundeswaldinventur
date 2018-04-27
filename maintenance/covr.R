name <- "fvbn2e.R"
devtools::load_all()
root <- rprojroot::find_root(rprojroot::is_r_package)
source_file <- file.path(root, "R", name)
test_file <- file.path(root, "tests", "testthat", paste0("test-local_", name))
cov <- covr::file_coverage(source_file, test_file)
print(covr::zero_coverage(cov))
print(cov)
