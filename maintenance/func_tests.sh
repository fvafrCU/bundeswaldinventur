functions=$(grep -h "<- function(" R/* | cut -f1 -d"<")
for function in $functions
do
    grep -q "${function}(" tests/testthat/* || echo $function not tested!
done

