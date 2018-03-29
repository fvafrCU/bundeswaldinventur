#' Tests the outputs of the functions of the file deadwood.R.
#'
#' @author Franziska Berg
#' @section Version: 19.10.2015
#' @name A Header for
NULL

context("deadwood.R")

test_that("harmonize_deadwood", {
  output <- head(harmonize_deadwood(totholz.3))

  output <- output[-13]

  reference <- structure(list(
    tnr = c(2062L, 2062L, 2062L, 2062L, 2062L, 2062L),
    enr = c(4L, 4L, 4L, 4L, 1L, 1L),
    nr = c(1L, 2L, 3L, 4L, 1L, 2L),
    tbagr = c(1L, 1L, 1L, 1L, 1L, 1L),
    tart = c(4, 4, 4, 4, 4, 4),
    tzg = c(3L, 1L, 1L, 2L, 1L, 4L),
    tbd = c(24L, 25L, 24L, 30L, 21L, 42L),
    tsd = c(0L, 0L, 0L, 0L, 0L, 0L),
    lge = c(
      0.400000006, 0.200000003, 0.200000003, 0.200000003, 0.100000001,
      0.100000001
    ),
    tvol = c(
      0.0180955753, 0.00981747825, 0.00904778764, 0.0141371684,
      0.00346360635, 0.0138544254
    ),
    anz = c(1L, 1L, 1L, 1L, 1L, 1L),
    thf = c(
      127.323944, 127.323944, 127.323944, 127.323944, 127.323944,
      127.323944
    ), dm = c(24, 25, 24, 30, 21, 42)
  ),
  .Names = c(
    "tnr", "enr", "nr", "tbagr", "tart", "tzg", "tbd", "tsd", "lge",
    "tvol", "anz", "thf", "dm"
  ), row.names = c(NA, 6L),
  class = "data.frame"
  )

  expect_equal(output, reference)
})

test_that("purge_deadwood_to_2", {
  input <- harmonize_deadwood(totholz.3)

  output <- head(purge_deadwood_to_2(input))

  output <- output[-13]

  reference <- structure(list(
    tnr = c(2062L, 2062L, 2062L, 2062L, 2210L, 2210L),
    enr = c(3L, 3L, 3L, 3L, 3L, 3L),
    nr = c(1L, 2L, 3L, 4L, 1L, 5L),
    tbagr = c(1L, 1L, 1L, 1L, 1L, 1L),
    tart = c(1, 1, 1, 1, 1, 1),
    tzg = c(2L, 2L, 2L, 2L, 2L, 3L),
    tbd = c(44L, 31L, 31L, 36L, 36L, 36L),
    tsd = c(39L, 0L, 27L, 26L, 32L, 27L),
    lge = c(0.600000024, 12, 0.800000012, 0.899999976, 0.600000024, 0.300000012),
    tvol = c(
      0.0811591297, 0.652817607, 0.0528415963, 0.0679290965,
      0.0544752255, 0.0233793426
    ),
    anz = c(1L, 1L, 1L, 1L, 1L, 1L),
    thf = c(
      127.323944, 127.323944, 127.323944, 127.323944, 127.323944,
      127.323944
    ),
    dm = c(41.5, 31, 29, 31, 34, 31.5)
  ),
  .Names = c(
    "tnr", "enr", "nr", "tbagr", "tart", "tzg", "tbd", "tsd", "lge",
    "tvol", "anz", "thf", "dm"
  ),
  row.names = c(13L, 14L, 15L, 16L, 27L, 31L),
  class = "data.frame"
  )

  expect_equal(output, reference)
})

test_that("drop_variables", {
  input <- head(harmonize_deadwood(totholz.3))

  output <- drop_variables(input)

  reference <- structure(list(
    tnr = c(2062L, 2062L, 2062L, 2062L, 2062L, 2062L), tvol = c(
      0.0180955753, 0.00981747825, 0.00904778764, 0.0141371684,
      0.00346360635, 0.0138544254
    ),
    thf = c(
      127.323944, 127.323944, 127.323944, 127.323944, 127.323944,
      127.323944
    ),
    anz = c(1L, 1L, 1L, 1L, 1L, 1L),
    tart = c(4, 4, 4, 4, 4, 4),
    tbagr = c(1L, 1L, 1L, 1L, 1L, 1L), tzg = c(3L, 1L, 1L, 2L, 1L, 4L)
  ),
  .Names = c("tnr", "tvol", "thf", "anz", "tart", "tbagr", "tzg"),
  class = "data.frame", row.names = c(NA, 6L)
  )

  expect_equal(output, reference)
})

test_that("deadwood", {
  input <- harmonize_deadwood(totholz.3)

  output <- deadwood(input[0:50, ])

  reference <- structure(list(
    tnr = c(2062L, 2210L, 2210L, 2212L),
    enr = c(3L, 3L, 4L, 1L), x = c(
      108.829813847802, 41.4107525970386,
      8.45000009341954, 43.013250806386
    )
  ),
  .Names = c("tnr", "enr", "x"),
  row.names = c(2L, 3L, 4L, 1L), class = "data.frame"
  )

  expect_equal(output, reference)
})
