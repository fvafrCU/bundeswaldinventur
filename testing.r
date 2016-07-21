if (! require("devtools")) install.packages("devtools")
devtools::load_all()
devtools::document()
devtools::check()
devtools::use_testthat()
devtools::test()

