git diff --  '*.tex' -U0 | \
    grep '^[+-]' | \
    grep -Ev '^(--- a/|\+\+\+ b/)' | \
    grep -v '[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\} 2018' \
    > diff_git.log 2>&1
