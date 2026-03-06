#' Validate a data frame against a schema
#'
#' Checks that a data frame conforms to a schema, validating column presence,
#' types, missing values, uniqueness constraints, and nesting structure.
#'
#' @param schema A schema object created by [sch_schema()].
#' @param data A data frame to validate.
#' @param check A character vector specifying which checks to perform. The
#'   default runs all checks. Possible values:
#'   - `"names"`: check for missing required columns and unexpected extra columns.
#'   - `"types"`: check column types and missing-value (`NA`) constraints.
#'   - `"distinct"`: check uniqueness constraints for columns marked
#'     `distinct = TRUE`. Omit this for large data frames where the check is
#'     expensive.
#'   - `"nesting"`: validate nested schema columns (flat and named nests),
#'     including key consistency.
#' @param call The environment or call used for error reporting, passed to
#'   [rlang::abort()]. Useful when wrapping `sch_validate()` inside another
#'   function so that the error points to the right place.
#'
#' @returns `data`, invisibly, if validation succeeds. Otherwise, an error of
#'   class `sch_validation_error` is raised with a formatted summary of all
#'   issues found.
#'
#' @examples
#' # Basic validation: valid data passes silently
#' schema <- sch_schema(
#'     id = sch_integer(distinct = TRUE),
#'     name = sch_character(missing = FALSE),
#'     age = sch_numeric(required = FALSE)
#' )
#' df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Carol"), age = c(25, NA, 30))
#' sch_validate(schema, df)
#'
#' # Invalid, throw validation errors
#' \dontrun{
#' # missing required columns
#' sch_validate(schema, data.frame(id = 1:2))
#'
#' # Validation error: type constrains
#' sch_validate(schema, data.frame(id = c(1L, 1L), name = c("Alice", NA)))
#' }
#' @export
sch_validate <- function(
    schema,
    data,
    check = c("names", "types", "distinct", "nesting"),
    call = rlang::caller_env()
) {
    assert(
        inherits(schema, "sch_schema"),
        "{.arg schema} must be an {.cls sch_schema} object."
    )
    assert(is.data.frame(data), "{.arg data} must be a data frame.")

    check = rlang::arg_match(check, multiple = TRUE)
    col_info <- classify_columns(schema$cols)
    issues <- list()
    if ("names" %in% check) {
        issues <- c(issues, validate_names(schema$cols, data, col_info))
    }
    if ("types" %in% check) {
        issues <- c(issues, validate_types_missing(schema$cols, data, col_info))
    }
    if ("distinct" %in% check) {
        issues <- c(issues, validate_distinct(schema$cols, data, col_info))
    }
    if ("nesting" %in% check) {
        issues <- c(issues, validate_nests(schema$cols, data, col_info, check))
    }

    if (length(issues) > 0) {
        classes = setdiff(class(data), c("sch_df", "tbl", "tbl_df", "data.frame", "data.table"))
        print_validation_issues(issues, class = classes, call = call)
    }

    invisible(data)
}


# Validators -------------------------------------------

classify_columns <- function(cols) {
    is_other <- vapply(cols, function(x) x$type == "other", FALSE)
    is_nest <- vapply(cols, function(x) x$type == "schema_nest", FALSE)
    nms <- names(cols) %||% rep("", length(cols))
    is_flat_nest <- is_nest & !nzchar(nms)
    is_named_nest <- is_nest & nzchar(nms)
    current_col_names <- nms[!is_nest & !is_other]

    list(
        nms = nms,
        is_other = is_other,
        is_nest = is_nest,
        is_flat_nest = is_flat_nest,
        is_named_nest = is_named_nest,
        current_col_names = current_col_names
    )
}


validate_names <- function(cols, data, col_info, path, group_by) {
    if (missing(path)) {
        path <- character(0)
    }
    if (missing(group_by)) {
        group_by <- character(0)
    }
    issues <- list()

    # Gather expected column names
    expected_names <- character(0)
    for (i in seq_along(cols)) {
        if (col_info$is_other[i]) {
            next
        }
        if (col_info$is_flat_nest[i]) {
            expected_names <- c(expected_names, get_all_flat_col_names(cols[[i]]$cols))
        } else {
            expected_names <- c(expected_names, col_info$nms[i])
        }
    }

    # Check for extra columns
    extra <- setdiff(names(data), c(expected_names, group_by))
    has_others <- any(col_info$is_other)
    if (length(extra) > 0 && !has_others) {
        issues <- c(issues, list(make_issue("extra_columns", path, columns = extra)))
    }

    issues
}


validate_types_missing <- function(cols, data, col_info, path) {
    if (missing(path)) {
        path <- character(0)
    }
    issues <- list()

    for (i in seq_along(cols)) {
        if (col_info$is_other[i] || col_info$is_nest[i]) {
            next
        }
        tt <- cols[[i]]
        col_nm <- col_info$nms[i]
        col_path <- c(path, col_nm)

        # Check if column exists
        if (!col_nm %in% names(data)) {
            if (attr(tt, "required")) {
                expected <- type_fns[[tt$type]]$msg(tt)
                issues <- c(
                    issues,
                    list(make_issue("missing_column", col_path, expected = expected))
                )
            }
            next
        }

        x <- data[[col_nm]]

        # Check type
        if (!type_fns[[tt$type]]$check(x, tt)) {
            expected <- type_fns[[tt$type]]$msg(tt)
            a_an = if (grepl("^[aeiou]", expected)) "An " else "A "
            issues <- c(
                issues,
                list(make_issue("wrong_type", col_path, expected = paste0(a_an, expected)))
            )
        }

        # Check missing values
        if (!attr(tt, "missing") && anyNA(x)) {
            issues <- c(issues, list(make_issue("has_na", col_path)))
        }
    }

    issues
}


validate_distinct <- function(cols, data, col_info, path, group_by) {
    if (missing(path)) {
        path <- character(0)
    }
    if (missing(group_by)) {
        group_by <- character(0)
    }
    issues <- list()

    # When flat nests exist, collapse to unique outer rows first
    if (any(col_info$is_flat_nest)) {
        all_outer <- intersect(c(group_by, col_info$current_col_names), names(data))
        effective <- vctrs::vec_unique(data[all_outer])
    } else {
        effective <- data
    }

    for (i in seq_along(cols)) {
        if (col_info$is_other[i] || col_info$is_nest[i]) {
            next
        }
        tt <- cols[[i]]
        if (!attr(tt, "distinct")) {
            next
        }
        col_nm <- col_info$nms[i]
        if (!col_nm %in% names(effective)) {
            next
        }
        col_path <- c(path, col_nm)

        issues <- c(issues, check_col_distinct(effective, col_nm, col_path, group_by))
    }

    issues
}


validate_nests <- function(cols, data, col_info, check, path, group_by) {
    if (missing(path)) {
        path <- character(0)
    }
    if (missing(group_by)) {
        group_by <- character(0)
    }
    issues <- list()

    for (i in seq_along(cols)) {
        if (col_info$is_named_nest[i]) {
            issues <- c(
                issues,
                validate_named_nest(cols[[i]], col_info$nms[i], data, check, path)
            )
        } else if (col_info$is_flat_nest[i]) {
            inner_group_by <- c(group_by, col_info$current_col_names)
            issues <- c(
                issues,
                validate_flat_nest(cols[[i]], data, check, path, inner_group_by)
            )
        }
    }

    issues
}


validate_flat_nest <- function(nest, data, check, path, group_by) {
    if (missing(path)) {
        path <- character(0)
    }
    if (missing(group_by)) {
        group_by <- character(0)
    }
    issues <- list()
    keys <- nest$keys

    # Check keys consistency across groups
    if ("names" %in% check && length(keys) > 0 && length(group_by) > 0) {
        group_cols_present <- intersect(group_by, names(data))
        keys_present <- intersect(keys, names(data))

        if (length(group_cols_present) > 0 && length(keys_present) == length(keys)) {
            issues <- c(
                issues,
                check_keys_consistent(data, group_cols_present, keys_present, path)
            )
        }
    }

    # Recurse into inner columns
    col_info <- classify_columns(nest$cols)
    if ("names" %in% check) {
        issues <- c(issues, validate_names(nest$cols, data, col_info, path, group_by))
    }
    if ("types" %in% check) {
        issues <- c(issues, validate_types_missing(nest$cols, data, col_info, path))
    }
    if ("distinct" %in% check) {
        issues <- c(issues, validate_distinct(nest$cols, data, col_info, path, group_by))
    }
    if ("nesting" %in% check) {
        issues <- c(issues, validate_nests(nest$cols, data, col_info, check, path, group_by))
    }

    issues
}


validate_named_nest <- function(nest, col_nm, data, check, path) {
    issues <- list()
    col_path <- c(path, col_nm)

    if (!col_nm %in% names(data)) {
        if ("names" %in% check && attr(nest, "required")) {
            issues <- c(
                issues,
                list(make_issue("missing_column", col_path, expected = "nested data frame"))
            )
        }
        return(issues)
    }

    x <- data[[col_nm]]

    if (!is.list(x)) {
        issues <- c(issues, list(make_issue("not_nested_df", col_path)))
        return(issues)
    }

    # Validate each nested data frame element
    for (idx in seq_along(x)) {
        elem <- x[[idx]]
        if (!is.data.frame(elem)) {
            issues <- c(
                issues,
                list(make_issue("not_data_frame_element", col_path, index = idx))
            )
            next
        }

        col_info <- classify_columns(nest$cols)
        inner_issues <- list()
        if ("names" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_names(nest$cols, elem, col_info, character(0), col_path)
            )
        }
        if ("types" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_types_missing(nest$cols, elem, col_info, col_path)
            )
        }
        if ("distinct" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_distinct(nest$cols, elem, col_info, character(0), col_path)
            )
        }
        if ("nesting" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_nests(nest$cols, elem, col_info, check, character(0), col_path)
            )
        }

        for (j in seq_along(inner_issues)) {
            inner_issues[[j]]$element <- idx
        }
        issues <- c(issues, inner_issues)
    }

    # Check key consistency: every element must have the same set of key values
    if ("names" %in% check && length(nest$keys) > 0) {
        dfs <- x[vapply(x, is.data.frame, logical(1))]
        issues <- c(issues, check_named_nest_keys(dfs, nest$keys, col_path))
    }

    issues
}


# Returns an issue if the key columns in `dfs` do not contain the same set of
# values in every element. Elements missing a key column are skipped.
check_named_nest_keys <- function(dfs, keys, col_path) {
    if (length(dfs) <= 1) {
        return(list())
    }

    keys_present <- intersect(keys, names(dfs[[1]]))
    if (length(keys_present) < length(keys)) {
        return(list())
    }

    key_vals <- function(df) vctrs::vec_sort(vctrs::vec_unique(df[keys_present]))
    ref <- key_vals(dfs[[1]])

    for (df in dfs[-1]) {
        if (!all(keys_present %in% names(df))) {
            next
        }
        if (!identical(ref, key_vals(df))) {
            return(list(make_issue("inconsistent_keys", col_path, keys = keys_present)))
        }
    }
    list()
}


# Check that a column has distinct values, optionally within groups.
# The caller is responsible for pre-collapsing the data when flat nests
# create structural row repetition (see `validate_impl`).
check_col_distinct <- function(data, col_nm, col_path, group_by) {
    if (missing(group_by)) {
        group_by <- character(0)
    }
    x <- data[[col_nm]]
    if (is.null(x)) {
        return(list())
    }

    grp <- intersect(group_by, names(data))

    if (length(grp) == 0) {
        if (vctrs::vec_unique_count(x) != vctrs::vec_size(x)) {
            return(list(make_issue("not_distinct", col_path)))
        }
    } else {
        locs <- vctrs::vec_group_loc(data[grp])
        for (loc in locs$loc) {
            vals <- vctrs::vec_slice(x, loc)
            if (vctrs::vec_unique_count(vals) != vctrs::vec_size(vals)) {
                return(list(make_issue("not_distinct", col_path)))
            }
        }
    }

    list()
}


check_keys_consistent <- function(data, group_cols, key_cols, path) {
    group_data <- data[group_cols]
    locs <- vctrs::vec_group_loc(group_data)

    if (nrow(locs) <= 1) {
        return(list())
    }

    key_data <- data[key_cols]
    ref_keys <- vctrs::vec_sort(vctrs::vec_unique(
        vctrs::vec_slice(key_data, locs$loc[[1]])
    ))

    for (i in seq_along(locs$loc)[-1]) {
        this_keys <- vctrs::vec_sort(vctrs::vec_unique(
            vctrs::vec_slice(key_data, locs$loc[[i]])
        ))
        if (!identical(ref_keys, this_keys)) {
            return(list(make_issue("inconsistent_keys", path, keys = key_cols)))
        }
    }

    list()
}


# Printing ---------------------------------------------------------------

print_validation_issues <- function(issues, class = NULL, call = NULL) {
    n <- length(issues)

    bullets <- vapply(issues, print_validation_issue, character(1))
    names(bullets) <- rep("x", n)
    if (is.null(class) || length(class) == 0) {
        class <- "Data"
    } else {
        class = paste0("{.cls ", class, "}")
    }

    cli::cli_abort(
        c(paste(class, "validation failed with {n} issue{?s}:"), bullets),
        class = "sch_validation_error",
        issues = issues,
        call = call
    )
}

print_validation_issue <- function(issue) {
    p <- paste(issue$path, collapse = "$")
    elem_note <- if (!is.null(issue$element)) {
        paste0(" (element ", issue$element, ")")
    } else {
        ""
    }

    switch(
        issue$type,
        missing_column = cli::format_inline(
            "Required column {.field {p}}{elem_note} is missing. Expected {issue$expected}."
        ),
        wrong_type = cli::format_inline(
            "Column {.field {p}}{elem_note} has wrong type. Expected {issue$expected}."
        ),
        has_na = cli::format_inline(
            "Column {.field {p}}{elem_note} must not contain missing values."
        ),
        not_distinct = cli::format_inline(
            "Column {.field {p}}{elem_note} must not contain duplicate values."
        ),
        extra_columns = {
            nms <- cli::cli_vec(issue$columns, list("vec-trunc" = 5))
            cli::format_inline("Unexpected column{?s}: {.field {nms}}.")
        },
        inconsistent_keys = {
            keys_str <- paste(issue$keys, collapse = ", ")
            cli::format_inline(
                "Key columns ({.field {keys_str}}) have inconsistent values across groups{elem_note}."
            )
        },
        not_nested_df = cli::format_inline(
            "Column {.field {p}} should be a list of data frames."
        ),
        not_data_frame_element = cli::format_inline(
            "Column {.field {p}} element {issue$index} is not a data frame."
        ),
        paste0("Unknown issue: ", issue$type)
    )
}


# Helpers ----------

make_issue <- function(type, path, ...) {
    c(list(type = type, path = path), list(...))
}

get_all_flat_col_names <- function(cols) {
    nms <- character(0)
    for (i in seq_along(cols)) {
        if (cols[[i]]$type == "other") {
            next
        }
        nm <- names(cols)[i]
        if (cols[[i]]$type == "schema_nest") {
            is_named <- !is.null(nm) && nzchar(nm)
            if (is_named) {
                nms <- c(nms, nm)
            } else {
                nms <- c(nms, get_all_flat_col_names(cols[[i]]$cols))
            }
        } else {
            nms <- c(nms, nm)
        }
    }
    nms
}
