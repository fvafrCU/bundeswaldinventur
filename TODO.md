* get rid of RData history following https://blog.ostermiller.org/git-remove-from-history
* Fix global bindings in log/check.Rout
* clean, lint, spell!
* consider pulling documentation from deleted files 
  R/data\_documentation.R and
  R/global\_variables.R
* Write data input checks to make sure BWI4 input data will conform to the tables in BWI3.
* use a set_options(use_global_variables) and move graphics\_[width|height] to
  getter functions reading that option.
* reconsider the package_name-option
* Improve test coverage
