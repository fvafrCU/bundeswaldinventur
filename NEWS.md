# bundeswaldinventur 0.7.3

* Fixed fixes 0.7.1 and 0.7.2 using 1L instead of 1 in testing length with
  identical().
  
# bundeswaldinventur 0.7.2

* Fixed subset / indexing bug in verjg.kl4.bagrupp.fun(),
  see R/BWI\_HR.R lines 3502ff.

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



