* have a dedicated data package.
* Improve test coverage
* Fix global bindings in log/check.Rout
* Check which of the data in the RData files are needed. Especially the
  statistics data.
* Sample and anonymise bwi.RData and rewrite all testthat test according to new data.
* Write data input checks to make sure BWI4 input data will conform to the tables in BWI3.
* use a set_options(use_global_variables) and move graphics\_[width|height] to
  getter functions reading that option.
* reconsider the package_name-option
