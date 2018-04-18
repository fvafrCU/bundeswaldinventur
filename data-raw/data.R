unlink(file.path(devtools::as.package(".")[["path"]], "data", "*"))
rm(list = ls(all.names = TRUE))
devtools::load_all(".")
bundeswaldinventur::get_package_data("bwibw")
ls()
N <- 100
set.seed(42)
ref <- ecken.3[sample(nrow(ecken.3), N), c("TNr", "ENr")]
rm(N)

to_be_sampled <- NULL
for (object in ls()) {
    if (any(grepl("TNr", names(get(object)))))
        to_be_sampled <- c(to_be_sampled, object)
}

for (object in to_be_sampled) assign(object, merge(get(object), ref))
rm(ref, to_be_sampled, object)
trakte.1$ENr <-trakte.2$ENr <-  trakte.3$ENr <- NULL

clean_column <- function(colname) {
    to_be_cleaned <- NULL
    for (object in ls(envir = .GlobalEnv)) {
        if (any(grepl("Bemerk", names(get(object))))) {
            tmp <- get(object, envir = .GlobalEnv)
            tmp[[colname]] <- NA
            assign(object, tmp, envir = .GlobalEnv)
        }
    }
}
clean_column("Bemerk")
clean_column("Bemerkung")
clean_column("raumkat")
rm(clean_column)


assign("names", ls())
names <- names[names != "names"]
names <- paste(names, collapse = ", ")

cmd <- paste('devtools::use_data(', names, 
             ', pkg = ".", internal = FALSE, overwrite = TRUE)')
eval(parse(text = cmd))
