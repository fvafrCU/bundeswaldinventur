if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    dput(result)
}
testthat::context("batch.R")

# testthat::test_that("batch", {
#   if (exists("IS_R_CMD_CHECK") && isTRUE(IS_R_CMD_CHECK)) {
#   }
# #  result <- get_R_CMD_BATCH_script_path()
# #  print(get_Rscript_script_path())
# #  print(get_R_CMD_BATCH_script_path())
# #  stop(get_script_path())
# #  reference <- character(0)
# #  testthat::expect_identical(result, reference)
# #  result <- get_Rscript_script_path()
# #  testthat::expect_identical(result, reference)
# #  result <- get_script_path()
# #  testthat::expect_identical(result, reference)
# #  result <- get_script_name()
# #  reference <- "interactive_R_session"
# #  testthat::expect_identical(result, reference)
# #  result <- is_batch()
# #  reference <- ! interactive()
# #  testthat::expect_identical(result, reference)
# #
# #  result <- provide_output_directory("graphics", path = tempdir())
# #  testthat::expect_identical(graphics_directory, result)
# #  testthat::expect_true(dir.exists(result))
# 
# 
# })
