if (interactive()) devtools::load_all()
testthat::context("golden ratio")

testthat::test_that("golden ratio", {

  result <- golden_ratio(1)  
  reference <- structure(list(a = 0.618033988749895, b = 0.381966011250105), 
                         .Names = c("a", "b"))
  result <- lapply(result, round, 10)
  reference <- lapply(reference, round, 10)
  testthat::expect_equal(result, reference)
})

testthat::context("find files")

testthat::test_that("find files", {
  root <- file.path(tempdir(), "find_files")
  dir.create(root)
  cat("qwerasdf", file = file.path(root, "foo.R"))
  cat("qwerasdf", file = file.path(root, "foo.txt"))
  cat("qwerasdf", file = file.path(root, "bar.R"))

  result <- find_files(path = root)  
  reference <- c("/tmp/RtmpxFMwW1/find_files/bar.R", 
                 "/tmp/RtmpxFMwW1/find_files/foo.R")
  for (obj in c("reference", "result" )) {
           new <- sub("Rtmp[[:alnum:]]*/", "", get(obj))
           assign(obj, new)
  }
  testthat::expect_equal(result, reference)

  paths <- file.path(root, c("foo.txt", "not_there.txt"))
  msg <- paste("You specified both arguments `file_names`and `path`,",
               "we're ignoring the latter.")
  testthat::expect_warning(result <- find_files(file_names = paths, path = root),
                           msg, all = FALSE)

  paths <- file.path(root, c("foo.R", "foo.txt", "not_there.txt"))
  testthat::expect_warning(result <- find_files(file_names = paths))

  paths <- file.path(root, c("foo.R", "foo.txt", "not_there.txt"))
  testthat::expect_warning(result <- find_files(file_names = paths, path = "."))
  
  reference <- c("/tmp/RtmpxFMwW1/find_files/foo.R", 
                 "/tmp/RtmpxFMwW1/find_files/foo.txt")
  for (obj in c("reference", "result" )) {
           new <- sub("Rtmp[[:alnum:]]*/", "", get(obj))
           assign(obj, new)
  }
  testthat::expect_equal(result, reference)

  paths <- file.path(root, "not_there.txt")
  testthat::expect_warning(testthat::expect_error(result <- 
      find_files(file_names = paths, path = ".")
  ))

  unlink(root, recursive = TRUE)
})

