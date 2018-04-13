* Fix global bindings in log/check.Rout
* clean, lint, spell!
* pull documentation from deleted files 
  R/data\_documentation.R and
  R/global\_variables.R
  AND:
  provide a function to load all these global objects (for the hochrechnungen scripts)

* Write data input checks to make sure BWI4 input data will conform to the tables in BWI3.
* use a set_options(use_global_variables) and move graphics\_[width|height] to
  getter functions reading that option.
* reconsider the package_name-option
* Improve test coverage
