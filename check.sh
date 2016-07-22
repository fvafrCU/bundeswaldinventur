echo "devtools::check()" > check.R
R CMD BATCH --vanilla check.R
