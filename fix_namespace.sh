cat > sedscr << EOF
s/\<axis\>(/graphics::axis(/g
s/\<legend\>(/graphics::legend(/g
s/\<lines\>(/graphics::lines(/g
s/\<plot\>(/graphics::plot(/g
s/\<polygon\>(/graphics::polygon(/g
s/\<segments\>(/graphics::segments(/g
s/\<text\>(/graphics::text(/g
s/\<aggregate\>(/stats::aggregate(/g
s/\<na.fail\>(/stats::na.fail(/g
s/\<head\>(/utils::head(/g
EOF
cat > sedscr << EOF
s/\<xtable\>(/xtable::xtable(/g
s/\<dcast\>(/reshape2::dcast(/g
s/\<barplot2\>(/gplots::barplot2(/g
EOF


runsed R/*
