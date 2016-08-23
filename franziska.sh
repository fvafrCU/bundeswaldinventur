dir=$(pwd)
cd  ../hochrechnungen
git checkout devel
cd $dir
tfa=/tmp/franzi
tfb=/tmp/local
ls -1 ../hochrechnungen/HR_Funktionen/BWI3/R/ > $tfa
ls -1 R > $tfb
sdiff $tfa $tfb
file=${1:-R/bits_and_pieces.R}
ffile=../hochrechnungen/HR_Funktionen/BWI3/$file
file $ffile
diffuse $file $ffile
sed -e "s/[[:blank:]]\(assert.*(.*)\)/ checkmate::\1/g" < $file > $tfb
mv $tfb $file
gvim -p $file $ffile

