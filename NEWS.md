# bundeswaldinventur 0.10.1.9000

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



