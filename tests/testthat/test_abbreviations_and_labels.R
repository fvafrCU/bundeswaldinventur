#' Testing of abbrevations_and_labels.R
#'
#' Tests the outputs of the functions of the file abbreviations_and_labels.R.
#'
#' @author Franziska Berg
#' @section Version: 21.08.2015
#' @section TODO: Fix Encoding problems
#' @name get_design("a", 3) Header for
NULL

#' Test get_abbreviation_for_label
#'
#' Tests wheter the output of get_abbreviation_for_label is of the kind
#' character and with a length greater zero. And whether wrong inputs will raise
#' an error.
#'
#' @author Franziska Berg
#' @section Version: 21.08.2015
#' @param get_abbreviation_for_label Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("get_abbreviation_for_label")

test_that("abbreviation is character", {
  expect_is(get_abbreviation_for_label("Stammzahl"), "character")
  expect_is(get_abbreviation_for_label("Baumartenfläche"), "character")
})

test_that("abbreviation is longer than 0", {
  expect_gt(nchar(get_abbreviation_for_label("Stammzahl")), 0)
  expect_gt(nchar(get_abbreviation_for_label("Baumartenfläche")), 0)
})

test_that("output is error", {
  expect_that(get_abbreviation_for_label("test"), throws_error())
  expect_that(get_abbreviation_for_label(""), throws_error())
  expect_that(get_abbreviation_for_label(100), throws_error())
})

#' Test get_axis_label_for_abbreviation
#'
#' Tests wheter the output of get_axis_label_for_abbreviation is of the kind
#' character and with a length greater zero. And whether wrong inputs will raise
#' an error.
#'
#' @author Franziska Berg
#' @section Version: 21.08.2015
#' @param get_axis_label_for_abbreviation Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("get_axis_label_for_abbreviation")

test_that("label is character", {
  expect_is(get_axis_label_for_abbreviation("N"), "character")
  expect_is(get_axis_label_for_abbreviation("BAF"), "character")
})

test_that("label is longer than 0", {
  expect_gt(nchar(get_axis_label_for_abbreviation("N")), 0)
  expect_gt(nchar(get_axis_label_for_abbreviation("BAF")), 0)
})

test_that("output is error", {
  expect_that(get_axis_label_for_abbreviation("test"), throws_error())
  expect_that(get_axis_label_for_abbreviation(""), throws_error())
  expect_that(get_axis_label_for_abbreviation(100), throws_error())
})

#' Test get_text_label_for_abbreviation
#'
#' Tests wheter the output of get_text_label_for_abbreviation is of the kind
#' character and with a length greater zero. And whether wrong inputs will be
#' returned unchanged. Exception: if the input is a number, NULL will be
#' returned.
#'
#' @author Franziska Berg
#' @section Version: 21.08.2015
#' @param get_text_label_for_abbreviation Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("get_text_label_for_abbreviation")

test_that("label is character", {
  expect_is(get_text_label_for_abbreviation("N"), "character")
  expect_is(get_text_label_for_abbreviation("BAF"), "character")
})

test_that("label is longer than 0", {
  expect_gt(nchar(get_text_label_for_abbreviation("N")), 0)
  expect_gt(nchar(get_text_label_for_abbreviation("BAF")), 0)
})

test_that("unknown input given back", {
  expect_equal(get_text_label_for_abbreviation("test"), "test")
  expect_equal(get_text_label_for_abbreviation(""), "")
})


#' Test get_label_for_abbreviation
#'
#' Tests wheter the output of get_label_for_abbreviation is of the kind
#' character and with a length greater zero. And whether wrong inputs will be
#' returned unchanged if the label type = text label (Exception: if the input is
#' a number, NULL will be returned.) and an error if the label type = axis
#' label.
#'
#' @author Franziska Berg
#' @section Version: 21.08.2015
#' @param get_text_label_for_abbreviation Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("get_label_for_abbreviation")

test_that("label is character", {
  expect_is(get_label_for_abbreviation("N"), "character")
  expect_is(get_label_for_abbreviation("N", "text_label"), "character")
  expect_is(get_label_for_abbreviation("N", "axis_label"), "character")
  expect_is(get_label_for_abbreviation("Baumartenfläche"), "character")
  expect_is(get_label_for_abbreviation("Baumartenfläche", "text_label"), "character")
  expect_is(get_label_for_abbreviation("Baumartenfläche", "axis_label"), "character")
})

test_that("label is longer than 0", {
  expect_gt(nchar(get_label_for_abbreviation("N")), 0)
  expect_gt(nchar(get_label_for_abbreviation("N", "text_label")), 0)
  expect_gt(nchar(get_label_for_abbreviation("N", "axis_label")), 0)
  expect_gt(nchar(get_label_for_abbreviation("Baumartenfläche")), 0)
  expect_gt(nchar(get_label_for_abbreviation("Baumartenfläche", "text_label")), 0)
  expect_gt(nchar(get_label_for_abbreviation("Baumartenfläche", "axis_label")), 0)
})

test_that("unknown output given back when label_type='text_label'", {
  expect_equal(get_label_for_abbreviation("test"), "test")
  expect_equal(get_label_for_abbreviation("test", "text_label"), "test")
  expect_equal(get_label_for_abbreviation(""), "")
  expect_equal(get_label_for_abbreviation("", "text_label"), "")
})

test_that("unknown output will raise error when label_type='axis_label", {
  expect_that(get_label_for_abbreviation("test", "axis_label"), throws_error())
  expect_that(get_label_for_abbreviation("", "axis_label"), throws_error())
})


#' Test map_abbreviations_to_label
#'
#' Tests wheter the output of map_abbreviations_to_label is of the correct kind
#' and with a length greater zero. And whether wrong inputs will be returned
#' unchanged.
#'
#' @author Franziska Berg
#' @section Version: 25.08.2015
#' @param map_abbreviations_to_label Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("map_abbreviations_to_label")

test_that("correct assignment", {
  input_vector <- c("N", "BAF")
  output_vector <- c("Stammzahl", "Baumartengruppenfläche")
  input_factor <- factor(input_vector)
  output_factor <- factor(output_vector)
  names(output_vector) <- input_vector
  expect_identical(map_abbreviations_to_labels(input_vector), output_vector)
  expect_identical(map_abbreviations_to_labels(input_factor), output_factor)
})

test_that("label is longer than 0", {
  input_vector <- c("N", "BAF")
  output_vector <- map_abbreviations_to_labels(input_vector)
  for (i in 1:length(output_vector)) {
    expect_gt(nchar(output_vector[i]), 0)
  }
  input_factor <- factor(input_vector)
  output_factor <- map_abbreviations_to_labels(input_factor)
  for (i in 1:length(output_factor)) {
    expect_gt(nchar(as.character(output_factor[i])), 0)
  }
})

test_that("unknown output is given back", {
  input_vector <- c("1", 100, "", "unknown")
  output_vector <- c("1", "100", "", "unknown")
  input_factor <- factor(input_vector)
  output_factor <- factor(output_vector)
  names(output_vector) <- input_vector
  expect_identical(map_abbreviations_to_labels(input_vector), output_vector)
  expect_identical(map_abbreviations_to_labels(input_factor), output_factor)
})

#' Test map_labels_to_abbreviations
#'
#' Tests wheter the output of map_labels_to_abbreviations is of the correct kind
#' and with a length greater zero. And whether wrong inputs will be returned
#' unchanged.
#'
#' @author Franziska Berg
#' @section Version: 25.08.2015
#' @param map_labels_to_abbreviations Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("map_labels_to_abbreviations")

test_that("correct assignment", {
  input_vector <- c("Stammzahl", "Baumartengruppenfläche")
  output_vector <- c("N", "BAF")
  input_factor <- factor(input_vector)
  output_factor <- factor(output_vector)
  names(output_vector) <- input_vector
  expect_identical(map_labels_to_abbreviations(input_vector), output_vector)
  expect_identical(map_labels_to_abbreviations(input_factor), output_factor)
})

test_that("label is longer than 0", {
  input_vector <- c("Stammzahl", "Baumartengruppenfläche")
  output_vector <- map_labels_to_abbreviations(input_vector)
  for (i in 1:length(output_vector)) {
    expect_gt(nchar(output_vector[i]), 0)
  }
  input_factor <- factor(input_vector)
  output_factor <- map_labels_to_abbreviations(input_factor)
  for (i in 1:length(output_factor)) {
    expect_gt(nchar(as.character(output_factor[i])), 0)
  }
})

test_that("unknown output is given back", {
  input_vector <- c("Stammzahl", 100)
  input_factor <- factor(input_vector)
  expect_that(map_labels_to_abbreviations(input_vector), throws_error())
  expect_that(map_labels_to_abbreviations(input_factor), throws_error())
  input_vector <- c("Stammzahl", "100")
  input_factor <- factor(input_vector)
  expect_that(map_labels_to_abbreviations(input_vector), throws_error())
  expect_that(map_labels_to_abbreviations(input_factor), throws_error())
  input_vector <- c("Stammzahl", "test")
  input_factor <- factor(input_vector)
  expect_that(map_labels_to_abbreviations(input_vector), throws_error())
  expect_that(map_labels_to_abbreviations(input_factor), throws_error())
})

#' Test revalue_data
#'
#' Tests wheter the output of revalue_data is of the correct kind. And whether
#' wrong inputs will be returned unchanged.
#'
#' @author Franziska Berg
#' @section Version: 26.08.2015
#' @param revalue_data Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("revalue_data")

test_that("correct assignment", {
  label <- c("Stammzahl", "N", "BaumartenflÃ€che")
  # BAF because of Encoding removed
  df_input <- data.frame(label)
  label <- c("Stammzahl", "Stammzahl", "BaumartenflÃ€che")
  # BaumartengruppenflÃ€che because of Encoding problems removed
  df_output <- data.frame(label)
  revalue_data(df_input[, "label"])
  expect_identical(df_input, df_output)
})

test_that("unknown output is given back", {
  label <- c("N", "", "test", "1", 100)
  df_input <- data.frame(label)
  label <- c("Stammzahl", "", "test", "1", 100)
  df_output <- data.frame(label)
  revalue_data(df_input[, "label"])
  expect_identical(df_input, df_output)
})

#' Test get_color_for_ownership
#'
#' Tests wheter the output of get_color_for_ownership is of the kind character
#' and with a length greater zero. And whether wrong inputs will raise an error.
#'
#' @author Franziska Berg
#' @section Version: 26.08.2015
#' @param get_color_for_ownership Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("get_color_for_ownership")

test_that("label is character", {
  expect_is(get_color_for_ownership("Mittlerer Privatwald"), "character")
  expect_is(get_color_for_ownership("mpw"), "character")
})

test_that("label is longer than 0", {
  expect_gt(nchar(get_color_for_ownership("Mittlerer Privatwald")), 0)
  expect_gt(nchar(get_color_for_ownership("mpw")), 0)
})

test_that("unknown ownership throws error", {
  expect_that(get_color_for_ownership(""), throws_error())
  expect_that(get_color_for_ownership("test"), throws_error())
  expect_that(get_color_for_ownership("100"), throws_error())
  expect_that(get_color_for_ownership(100), throws_error())
})

#' Test get_colors_for_ownership
#'
#' Tests wheter the output of get_colors_for_ownership is of the kind character
#' and with a length greater zero. And whether wrong inputs will raise an error.
#'
#' @author Franziska Berg
#' @section Version: 26.08.2015
#' @param get_colors_for_ownership Funktionsname
#' @return Will raise error message if not all expectations are fullfilled.
context("get_colors_for_ownership")

test_that("label is character", {
  ownership <- ownership <- c("Kleinprivatwald", "Mittlerer Privatwald", "mpw")
  output <- get_colors_for_ownership(ownership)
  for (i in 1:length(output)) {
    expect_is(output[i], "character")
  }
})

test_that("label is longer than 0", {
  ownership <- ownership <- c("Kleinprivatwald", "Mittlerer Privatwald", "mpw")
  output <- get_colors_for_ownership(ownership)
  for (i in 1:length(output)) {
    expect_gt(nchar(output[i]), 0)
  }
})

test_that("unknown ownership throws error", {
  ownership <- ownership <- c("Kleinprivatwald", "")
  expect_that(get_colors_for_ownership(ownership), throws_error())
  ownership <- ownership <- c("Kleinprivatwald", "test")
  expect_that(get_colors_for_ownership(ownership), throws_error())
  ownership <- ownership <- c("Kleinprivatwald", "100")
  expect_that(get_colors_for_ownership("ownership"), throws_error())
  ownership <- ownership <- c("Kleinprivatwald", 100)
  expect_that(get_colors_for_ownership(ownership), throws_error())
})
