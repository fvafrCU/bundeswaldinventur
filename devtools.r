if (! require("devtools")) install.packages("devtools")
if (FALSE)
    devtools::install_github("hadley/devtools")
devtools::load_all()
devtools::use_testthat()
devtools::test()
devtools::document()
if (FALSE) {
    check_log <- devtools::check()
    foo <-    evaluate::evaluate(devtools::check())
}


devtools::dev_example("set_options")
