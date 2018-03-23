test_exception <- function() {
    RUnit::checkException(bundeswaldinventur:::throw("Hello, error!"))
}
