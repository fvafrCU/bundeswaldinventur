for file in $(ls R/*.r)
do
    output=${file%.r}.R
    git mv $file $output
done
for file in $(ls R/BWI3_*)
do
    iconv -f ISO-8859-15 -t UTF-8 $file -o tmp.R
    git mv tmp.R $file
    fromdos $file
done
