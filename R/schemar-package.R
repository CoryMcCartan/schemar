#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
    rlang::run_on_load() # nocov
}

rlang::on_load(rlang::local_use_cli(format = TRUE, inline = TRUE))

assert <- function(condition, message) {
    x = rlang::enquo(condition)
    if (!isTRUE(rlang::eval_tidy(x))) {
        if (missing(message) || is.null(message)) {
            message = paste("Expected", rlang::expr_deparse(rlang::quo_squash(x))) # nocov
        }
        rlang::abort(message, call = parent.frame(), .envir = parent.frame())
    }
}
