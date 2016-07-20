echo "1,/NULL/d" > sedscr
for file in $(grep -l "@name.*Header" R/*)
do
    runsed $file
done
