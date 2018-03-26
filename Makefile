# header
rpackage=bundeswaldinventur
rpversion=0.3.1.9000
roxy_code=tmp_roxy.r
temp_file=/tmp/filejlwtiJ
test_changes_file=tmp_test_change.R
# devtools
dev_all: dev_test dev_check
dev: dev_test_change dev_check
dev_test:
	rm ${temp_file} || TRUE; \
	Rscript --vanilla -e 'devtools::test()' >  ${temp_file} 2>&1; \
	sed -n -e '/^DONE.*/q;p' < ${temp_file} > dev_test.Rout 
dev_test_change:
	rm ${temp_file} || TRUE; \
	Rscript --vanilla ${test_changes_file} >  ${temp_file} 2>&1; \
	sed -n -e '/^DONE.*/q;p' < ${temp_file} > dev_test_change.Rout 
dev_check:
	rm ${temp_file} || TRUE; \
	Rscript --vanilla -e 'devtools::check()' > ${temp_file} 2>&1; \
	grep -v ".*'/" ${temp_file} | grep -v ".*/tmp/R.*" > dev_check.Rout 
# R CMD 
craninstall: crancheck
	R --vanilla CMD INSTALL  ${rpackage}_${rpversion}.tar.gz
crancheck: check 
	export _R_CHECK_FORCE_SUGGESTS_=FALSE && \
        R CMD check --as-cran ${rpackage}_${rpversion}.tar.gz 
install: check 
	R --vanilla CMD INSTALL  ${rpackage}_${rpversion}.tar.gz && \
        printf '===== have you run\n\tmake check_demo && ' && \
        printf 'make package_tools && make runit && make coldr\n?!\n' 
install_bare: build_bare 
	R --vanilla CMD INSTALL  ${rpackage}_${rpversion}.tar.gz 
check_bare: build_bare 
	export _R_CHECK_FORCE_SUGGESTS_=FALSE && \
        R --vanilla CMD check --no-examples ${rpackage}_${rpversion}.tar.gz && \
        printf '===== run\n\tmake install\n!!\n'
check: build 
	export _R_CHECK_FORCE_SUGGESTS_=FALSE && \
        R --vanilla CMD check ${rpackage}_${rpversion}.tar.gz && \
        printf '===== run\n\tmake install\n!!\n'
build_bare: 
	R --vanilla CMD build ../${rpackage}
build: roxy 
	R --vanilla CMD build ../${rpackage}
direct_check:  
	R --vanilla CMD check ../${rpackage} ## check without build -- not recommended
roxy:
	rm man/* || true
	printf "devtools::load_all()\n" > ${roxy_code}
	printf "roxygen2::roxygenize('.', roclets = c('rd'))\n" >> ${roxy_code}
	R --vanilla CMD BATCH --vanilla ${roxy_code}

