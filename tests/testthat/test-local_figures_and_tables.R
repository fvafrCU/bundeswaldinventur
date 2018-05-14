if (interactive()) devtools::load_all()
if (FALSE) {
    dump("result")
    debugonce(Totholz.klass.stratum.fun)
}

testthat::context("figures_and_tables.R")

testthat::test_that("bold themes", {
  result <- theme_bold_axes()
reference <-
structure(list(axis.title = structure(list(family = NULL, face = NULL, 
    colour = "black", size = 20, hjust = NULL, vjust = NULL, 
    angle = NULL, lineheight = NULL, margin = NULL, debug = NULL, 
    inherit.blank = FALSE), .Names = c("family", "face", "colour", 
"size", "hjust", "vjust", "angle", "lineheight", "margin", "debug", 
"inherit.blank"), class = c("element_text", "element")), axis.text = structure(list(
    family = NULL, face = NULL, colour = "black", size = 15, 
    hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, 
    margin = NULL, debug = NULL, inherit.blank = FALSE), .Names = c("family", 
"face", "colour", "size", "hjust", "vjust", "angle", "lineheight", 
"margin", "debug", "inherit.blank"), class = c("element_text", 
"element"))), .Names = c("axis.title", "axis.text"), class = c("theme", 
"gg"), complete = FALSE, validate = TRUE)
  
  testthat::expect_equal(reference, result)


  result <- theme_bold_title()
reference <-
structure(list(plot.title = structure(list(family = NULL, face = NULL, 
    colour = "black", size = 14, hjust = NULL, vjust = NULL, 
    angle = NULL, lineheight = NULL, margin = NULL, debug = NULL, 
    inherit.blank = FALSE), .Names = c("family", "face", "colour", 
"size", "hjust", "vjust", "angle", "lineheight", "margin", "debug", 
"inherit.blank"), class = c("element_text", "element"))), .Names = "plot.title", class = c("theme", 
"gg"), complete = FALSE, validate = TRUE)
  
  testthat::expect_equal(reference, result)


  result <- theme_bold_legend()
reference <-
structure(list(legend.text = structure(list(family = NULL, face = NULL, 
    colour = "black", size = 20, hjust = NULL, vjust = NULL, 
    angle = NULL, lineheight = NULL, margin = NULL, debug = NULL, 
    inherit.blank = FALSE), .Names = c("family", "face", "colour", 
"size", "hjust", "vjust", "angle", "lineheight", "margin", "debug", 
"inherit.blank"), class = c("element_text", "element")), legend.title = structure(list(
    family = NULL, face = NULL, colour = "black", size = 20, 
    hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, 
    margin = NULL, debug = NULL, inherit.blank = FALSE), .Names = c("family", 
"face", "colour", "size", "hjust", "vjust", "angle", "lineheight", 
"margin", "debug", "inherit.blank"), class = c("element_text", 
"element"))), .Names = c("legend.text", "legend.title"), class = c("theme", 
"gg"), complete = FALSE, validate = TRUE)
  testthat::expect_equal(reference, result)
}
)
