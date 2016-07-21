echo "s/result <- result\[-1\]/result\[\["Log"\]\] <- NULL/" > sedscr
runsed tests/testthat/*
