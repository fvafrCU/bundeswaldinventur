cp ../hochrechnungen/functions/*.R R/
cp ../hochrechnungen/functions/*.r R/
cp ../hochrechnungen/HR_Funktionen/*.r R/
cp ../hochrechnungen/customization/*.R R/


dir=$(pwd)
cd  ../hochrechnungen/
git checkout devel
git checkout f457c53e6bd850669dffcd46e62a7c20eedba66f
cd ${dir}
mkdir tests
cp -r ../hochrechnungen/HR_Funktionen/BWI3/tests/testthat/ tests
cd  ../hochrechnungen/
git checkout master
cd ${dir}

