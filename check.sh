file=tmp_check.R
echo "devtools::check()" > $file
R CMD BATCH --vanilla $file
