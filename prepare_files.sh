for file in $(ls R/*.r)
do
    output=${file%.r}.R
    git mv $file $output
done
for file in $(ls R/BWI3_*)
do
    iconv -f ISO-8859-15 -t UTF-8 $file -o tmp.R
    mv tmp.R $file
    fromdos $file
done
for file in $(ls tests/testthat/*.r)
do
    output=${file%.r}.R
    git mv $file $output
done
for file in $(file  tests/testthat/* | grep ISO| cut -f1 -d':')
do
    iconv -f ISO-8859-15 -t UTF-8 $file -o tmp.R
    mv tmp.R $file
done
