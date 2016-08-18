file="R/global_variables.R"
tempfile=$(tempfile)
sed \
    -e "s/\(.*\)Ä/\1\\\u00c4/g" \
    -e "s/\(.*\)ä/\1\\\u00e4/g" \
    -e "s/\(.*\)Ö/\1\\\u00d6/g" \
    -e "s/\(.*\)ö/\1\\\u00f6/g" \
    -e "s/\(.*\)Ü/\1\\\u00dc/g" \
    -e "s/\(.*\)ü/\1\\\u00fc/g" \
    -e "s/\(.*\)ß/\1\\\u00df/g" \
    < $file >  $tempfile
mv $tempfile $file
