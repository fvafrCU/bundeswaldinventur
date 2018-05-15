# bundeswaldinventur 0.15.0.9000

* FIXME

# bundeswaldinventur 0.15.0

* Finished test cases for all functions except the ones in R/batch.R and
  R/regional.R
* Changed return value of add\_colSums\_prettify\_and\_print\_xtable() from TRUE
  to a string of html code.
* Fixed bug in  get\_bwi\_species\_groups("bc").
* Passed package argument through get\_global\_objects() to get\_package\_data()
  to enable testing (without package bwibw).

# bundeswaldinventur 0.14.0

* Fixed argument tests functions for deadwood for BWI 2 (former tests forced BWI 3)
* Added handling of empty selections from stratification for stats::aggregate().

# bundeswaldinventur 0.13.1

* Improved on testing.
* Added workaround for evaluating tests/testthat.R via R CMD check and covr.
* Fixed Astmerkmale in stamm.merkmale.bagr.akl.t fun().

# bundeswaldinventur 0.13.0

* Fixed Astmerkmale in stamm.merkmale.bagr.fun().

# bundeswaldinventur 0.12.2

* Fix data aquistion for historical analysis scripts.
* Improved on testing with local data.

# bundeswaldinventur 0.12.1

* Fixed get\_data()

# bundeswaldinventur 0.12.0

* Added resampled data, disabled tests an on data from the bwibw data package.

# bundeswaldinventur 0.11.2

* Fix 0.11.1

# bundeswaldinventur 0.11.1

* Exclude testthat runs by R CMD check on external machines and disable data
  getter if the bwibw data package is not installed.

# bundeswaldinventur 0.11.0

* Added functions to load data and global variables into .GlobalEnv for
  historical analysis scripts.

# bundeswaldinventur 0.10.1

* Added man files to git.

# bundeswaldinventur 0.10.0

* Purged data files from git's history.

# bundeswaldinventur 0.9.0

* Get rid of data and global variables.
  Both can be found in hochrechnungen/functions/ and hochrechnungen/data.

# bundeswaldinventur 0.8.0

* Got rid of statistics data.
* Added imports from ggplot2.
* Exported functions from R/regional.R.
* Introduced argument `have_title` to plotting functions to get rid of 
  global object `TITLE\_PLOT`.

# bundeswaldinventur 0.7.3

* Fixed subsetting/indexing bug in FVBN...().

# bundeswaldinventur 0.7.2

* Fixed subset / indexing bug in verjg.kl4.bagrupp.fun(),
  see R/BWI\_HR.R lines 3502ff.

* Fixed fix 0.7.1 using 1L instead of 1 in testing length with
  identical().

# bundeswaldinventur 0.7.1

* Fixed subset / indexing bug in verjg.kl4.bagr.fun(),
  see R/BWI\_HR.R lines 3346ff.

# bundeswaldinventur 0.7.0

* Exported Objects defined R/global\_variables.R this makes the scripts in 
  hochrechnungen/regional/ run.

# bundeswaldinventur 0.6.0

* Renamed function get\_species\_groups() to group\_district\_species() in
  regional.R to avoid two functions of the same name (get\_species\_groups is 
  already assigned in variable_getters.R).

# bundeswaldinventur 0.5.0

* Replaced global\_variables.RData with assignments in global\_variables.R

# bundeswaldinventur 0.4.0

* Replaced stratum.fun with refactored version for speed.
* Added a `NEWS.md` file to track changes to the package.



