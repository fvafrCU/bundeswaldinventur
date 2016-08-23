for file in $(find R/ -type f -print | sort)
do
    num_functions=$(grep "function([^.]" $file | wc -l)
    num_exported=$(grep -F "#' @export" $file | wc -l)
    echo $num_exported of $num_functions exported in $file.
done
