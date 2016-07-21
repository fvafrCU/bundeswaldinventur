if (! require("devtools")) install.packages("devtools")
if (FALSE)
    devtools::install_github("hadley/devtools")
devtools::load_all()
devtools::document()
devtools::check()
devtools::use_testthat()
devtools::test()

