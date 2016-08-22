for file in $( find R/ -type f -print)
do
    num_functions=$(grep -F "function(" $file | wc -l)
    num_exported=$(grep -F "#' @export" $file | wc -l)
    echo $file $num_exported of $num_functions exported.
done
