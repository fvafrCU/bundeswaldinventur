cat > sedscr <<EOF
s/ä/\\\u00e4/g
s/Ä/\\\u00c4/g
s/ö/\\\u00f6/g
s/Ö/\\\u00d6/g
s/ü/\\\u00fc/g
s/Ü/\\\u00dc/g
s/ß/\\\u00df/g
EOF
runsed R/*
