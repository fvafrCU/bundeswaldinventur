# whoa! this is ugly! but I can't get non_ascii_code.r to run.
# Fuck the encodings. Why did Gerald ever code with non-ascii characters?!?!
files=$(find R/ -name "*.R" -print)
times=5
for file in $files;
do
    tempfile=$(tempfile)
    for i in seq $times
    do
        sed \
            -e "s/\(#'.*\)³/\1^3/g" \
            -e "s/\(#'.*\)Ä/\1Ae/g" \
            -e "s/\(#'.*\)ä/\1ae/g" \
            -e "s/\(#'.*\)ä/\1ae/g" \
            -e "s/\(#'.*\)Ö/\1Oe/g" \
            -e "s/\(#'.*\)ö/\1oe/g" \
            -e "s/\(#'.*\)Ü/\1Ue/g" \
            -e "s/\(#'.*\)ü/\1ue/g" \
            -e "s/\(#'.*\)ß/\1ss/g" \
            < $file >  $tempfile
        mv $tempfile $file
    done
    for i in seq $times
    do
        sed \
            -e "s/\(.*\)³/\1^3/g" \
            -e "s#\(.*\)Größen#\1\"Gr\\\u00f6\\\u00dfen\"#g" \
            -e "s#\(.*\)FBA.Fläche#\1\"FBA.Fl\\\u00e4che\"#g" \
            -e "s#\(.*\)\<Stückzahl_Verj_Mio\>#\1\"St\\\u00fcckzahl_Verj_Mio\"#g" \
            -e "s#\(.*\)SE_Stückzahl_Verj_Mio#\1\"SE_St\\\u00fcckzahl_Verj_Mio\"#g" \
            -e "s/\(.*\)Ä/\1\\\u00c4/g" \
            -e "s/\(.*\)ä/\1\\\u00e4/g" \
            -e "s/\(.*\)ä/\1\\\u00e4/g" \
            -e "s/\(.*\)Ö/\1\\\u00d6/g" \
            -e "s/\(.*\)ö/\1\\\u00f6/g" \
            -e "s/\(.*\)Ü/\1\\\u00dc/g" \
            -e "s/\(.*\)ü/\1\\\u00fc/g" \
            -e "s/\(.*\)ß/\1\\\u00df/g" \
            < $file >  $tempfile
        mv $tempfile $file
    done
done
