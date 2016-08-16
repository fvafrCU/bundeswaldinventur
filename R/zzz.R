

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.package <- list(
    package_name = "bundeswaldinventur"
  )
  toset <- !(names(op.package) %in% names(op))
  if(any(toset)) options(op.package[toset])

  set_options(overwrite = FALSE)

  return(invisible(NULL))
}
