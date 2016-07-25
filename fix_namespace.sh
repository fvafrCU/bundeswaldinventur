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
cat > sedscr << EOF
s#\(\<aes_string\>(\)#ggplot2::\1#g
s#\(\<geom_bar\>(\)#ggplot2::\1#g
s#\(\<ggplot\>(\)#ggplot2::\1#g
s#\(\<ggtitle\>(\)#ggplot2::\1#g
s#\(\<xlab\>(\)#ggplot2::\1#g
s#\(\<ylab\>(\)#ggplot2::\1#g
EOF

cat > sedscr << EOF
s#\(\<theme\>(\)#ggplot2::\1#g
s#\(\<element_text\>(\)#ggplot2::\1#g
s#\(\<position_dodge\>(\)#ggplot2::\1#g
s#\(\<standard_error\>(\)#ggplot2::\1#g
s#\(\<geom_errorbar\>(\)#ggplot2::\1#g
s#\(\<scale_y_continuous\>(\)#ggplot2::\1#g
s#\(\<scale_fill_manual\>(\)#ggplot2::\1#g
s#\(\<aes\>(\)#ggplot2::\1#g
EOF

runsed R/*
