echo "s/result <- result\[-1\]/result\[\["Log"\]\] <- NULL/" > sedscr
runsed tests/testthat/*
echo 's/result\[\[Log\]\]/result\[\["Log"\]\]/' > sedscr
runsed tests/testthat/*

