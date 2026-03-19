#' Coerce a data frame to conform to a schema
#'
#' Attempts to coerce each column of `data` to the type expected by `schema`,
#' using the coercion method defined for each column type. After coercion,
#' optionally validates the result with [sch_validate()].
#'
#' Coercion is applied column-by-column using the `coerce` function registered
#' for each type in the internal `type_fns` registry. For example, a column
#' specified as `sch_integer()` will be coerced with [as.integer()]. Nested
#' schemas (created with [sch_nest()]) are handled by recursing into each
#' element data frame, and grouped columns (created with [sch_multiple()]) have
#' each member column coerced individually.
#'
#' Columns present in `data` but not named in `schema` (i.e., those covered by
#' [sch_others()]) are left untouched.
#'
#' @param schema A schema object created by [sch_schema()].
#' @param data A data frame to coerce.
#' @param validate If `TRUE` (default), [sch_validate()] is called on the
#'   coerced data after all columns have been processed. Set to `FALSE` to skip
#'   validation, which can be useful when you want to inspect the coerced result
#'   before checking constraints.
#' @param call The environment or call used for error reporting, passed to
#'   [rlang::abort()]. Useful when wrapping `sch_coerce()` inside another
#'   function so that errors point to the right place.
#'
#' @returns `data` with columns coerced to their schema types, invisibly, if
#'   coercion succeeds. If any column cannot be coerced, an error of class
#'   `sch_coercion_error` is raised with a summary of all failures. When
#'   `validate = TRUE`, a subsequent [sch_validate()] call may also raise a
#'   `sch_validation_error` if the coerced data still violates schema
#'   constraints (e.g., out-of-bounds values or uniqueness violations).
#'
#' @examples
#' schema <- sch_schema(
#'     id = sch_integer(distinct = TRUE),
#'     name = sch_character(missing = FALSE),
#'     score = sch_numeric()
#' )
#'
#' # Coerce a data frame with character columns
#' df <- data.frame(id = c("1", "2", "3"), name = c("Alice", "Bob", "Carol"), score = 1:3)
#' str(sch_coerce(schema, df))
#'
#' # Nested schema coercion
#' nested_schema <- sch_schema(
#'     group = sch_factor(),
#'     info = sch_nest(x = sch_numeric(), y = sch_integer())
#' )
#' nested_df <- data.frame(group = "A")
#' nested_df$info <- list(data.frame(x = "1.5", y = "2"))
#' str(sch_coerce(nested_schema, nested_df))
#' @export
sch_coerce <- function(schema, data, validate = TRUE, call = rlang::caller_env()) {
    assert(
        inherits(schema, "sch_schema"),
        "{.arg schema} must be an {.cls sch_schema} object."
    )
    assert(is.data.frame(data), "{.arg data} must be a data frame.")

    result <- coerce_cols(schema$cols, data, path = character(0))
    data <- result$data
    issues <- result$issues

    if (length(issues) > 0) {
        print_coercion_issues(issues, call = call)
    }

    if (isTRUE(validate)) {
        sch_validate(schema, data, call = call)
    }

    data
}


# Internal helpers --------------------------------------------------------

# Coerce all schema columns in `data`, returning list(data, issues).
coerce_cols <- function(cols, data, path = character(0)) {
    col_info <- classify_columns(cols)
    issues <- list()

    for (i in seq_along(cols)) {
        tt <- cols[[i]]

        if (col_info$is_other[i]) {
            next
        }

        if (col_info$is_named_nest[i]) {
            col_nm <- col_info$nms[i]
            if (col_nm %in% names(data)) {
                result <- coerce_nest_col(tt, data[[col_nm]], col_nm, path)
                data[[col_nm]] <- result$value
                issues <- c(issues, result$issues)
            }
            next
        }

        if (col_info$is_multiple[i]) {
            groups_attr <- attr(data, "sch_groups")
            if (!is.null(groups_attr) && tt$name %in% names(groups_attr)) {
                for (col_nm in groups_attr[[tt$name]]) {
                    if (col_nm %in% names(data)) {
                        result <- coerce_one(data[[col_nm]], tt$inner, c(path, col_nm))
                        data[[col_nm]] <- result$value
                        issues <- c(issues, result$issues)
                    }
                }
            }
            next
        }

        col_nm <- col_info$nms[i]
        if (col_nm %in% names(data)) {
            result <- coerce_one(data[[col_nm]], tt, c(path, col_nm))
            data[[col_nm]] <- result$value
            issues <- c(issues, result$issues)
        }
    }

    list(data = data, issues = issues)
}


# Coerce a single column vector; return list(value, issues).
coerce_one <- function(x, tt, col_path) {
    coerce_fn <- type_fns[[tt$type]]$coerce
    tryCatch(
        list(value = coerce_fn(x, tt), issues = list()),
        error = function(e) {
            list(
                value = x,
                issues = list(make_coercion_issue(col_path, conditionMessage(e)))
            )
        }
    )
}


# Recursively coerce a list-column of data frames according to a nest spec.
coerce_nest_col <- function(nest, list_col, col_nm, path) {
    issues <- list()
    col_path <- c(path, col_nm)

    for (idx in seq_along(list_col)) {
        elem <- list_col[[idx]]
        if (!is.data.frame(elem)) {
            next
        }
        result <- coerce_cols(nest$cols, elem, path = col_path)
        list_col[[idx]] <- result$data
        elem_issues <- result$issues
        for (j in seq_along(elem_issues)) {
            elem_issues[[j]]$element <- idx
        }
        issues <- c(issues, elem_issues)
    }

    list(value = list_col, issues = issues)
}


make_coercion_issue <- function(path, message) {
    list(path = path, message = message)
}


print_coercion_issues <- function(issues, call = NULL) {
    n <- length(issues)
    bullets <- vapply(
        issues,
        function(issue) {
            p <- paste(issue$path, collapse = "$")
            elem_note <- if (!is.null(issue$element)) {
                paste0(" (element ", issue$element, ")")
            } else {
                ""
            }
            cli::format_inline("Column {.field {p}}{elem_note}: {issue$message}")
        },
        character(1)
    )
    names(bullets) <- rep("x", n)
    cli::cli_abort(
        c("Coercion failed for {n} column{?s}:", bullets),
        class = "sch_coercion_error",
        issues = issues,
        call = call
    )
}
