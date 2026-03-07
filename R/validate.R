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
#'     `distinct = TRUE`. Relatively expensive.
#'   - `"relationships"`: validate relationship formulas (primary-key uniqueness
#'     and crossing/nesting completeness). Relatively expensive.
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
#' # type constrains not satisfied
#' sch_validate(schema, data.frame(id = c(1L, 1L), name = c("Alice", NA)))
#' }
#' @export
sch_validate <- function(
    schema,
    data,
    check = c("names", "types", "distinct", "relationships"),
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
    if (any(c("names", "types", "distinct") %in% check)) {
        issues <- c(issues, validate_nests(schema$cols, data, col_info, check))
    }
    if ("relationships" %in% check) {
        issues <- c(issues, validate_relationships(schema, data))
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
    is_multiple <- vapply(cols, function(x) x$type == "schema_multiple", FALSE)
    nms <- names(cols) %||% rep("", length(cols))
    is_named_nest <- is_nest & nzchar(nms)

    list(
        nms = nms,
        is_other = is_other,
        is_nest = is_nest,
        is_multiple = is_multiple,
        is_named_nest = is_named_nest
    )
}


validate_names <- function(cols, data, col_info, path = character(0)) {
    issues <- list()
    groups_attr <- attr(data, "sch_groups")

    if (any(col_info$is_multiple) && is.null(groups_attr)) {
        issues <- c(issues, list(make_issue("missing_sch_groups", path)))
    }

    expected_names <- character(0)
    for (i in which(!col_info$is_other)) {
        if (col_info$is_multiple[i]) {
            if (!is.null(groups_attr)) {
                res <- check_multiple_names(cols[[i]], data, groups_attr, path)
                issues <- c(issues, res$issues)
                expected_names <- c(expected_names, res$expected_nms)
            }
        } else {
            col_nm <- col_info$nms[i]
            expected_names <- c(expected_names, col_nm)
            if (
                !col_info$is_named_nest[i] &&
                    isTRUE(attr(cols[[i]], "required")) &&
                    !col_nm %in% names(data)
            ) {
                new_issue = make_issue(
                    "missing_column",
                    c(path, col_nm),
                    expected = a_an_msg(cols[[i]])
                )
                issues <- c(issues, list(new_issue))
            }
        }
    }

    if (!any(col_info$is_other)) {
        extra <- setdiff(names(data), expected_names)
        if (length(extra) > 0) {
            issues <- c(issues, list(make_issue("extra_columns", path, columns = extra)))
        }
    }

    issues
}


check_multiple_names <- function(tt, data, groups_attr, path) {
    if (!tt$name %in% names(groups_attr)) {
        return(list(
            issues = list(make_issue("missing_group", path, name = tt$name)),
            expected_nms = character(0)
        ))
    }

    col_nms <- groups_attr[[tt$name]]
    issues <- list()
    if (length(col_nms) == 0 && isTRUE(attr(tt, "required"))) {
        issues <- c(issues, list(make_issue("empty_group", path, name = tt$name)))
    }
    for (col_nm in col_nms[!col_nms %in% names(data)]) {
        new_issue <- make_issue(
            "missing_column",
            c(path, col_nm),
            expected = a_an_msg(tt$inner)
        )
        issues <- c(issues, list(new_issue))
    }

    list(issues = issues, expected_nms = col_nms)
}


validate_types_missing <- function(cols, data, col_info, path = character(0)) {
    issues <- list()
    groups_attr <- attr(data, "sch_groups")

    for (i in which(!col_info$is_other & !col_info$is_nest)) {
        if (col_info$is_multiple[i]) {
            issues <- c(issues, check_multiple_types(cols[[i]], data, groups_attr, path))
        } else {
            col_nm <- col_info$nms[i]
            if (col_nm %in% names(data)) {
                issues <- c(
                    issues,
                    check_col_type_and_na(data[[col_nm]], cols[[i]], c(path, col_nm))
                )
            }
        }
    }

    issues
}


check_multiple_types <- function(tt, data, groups_attr, path) {
    if (is.null(groups_attr) || !tt$name %in% names(groups_attr)) {
        return(list())
    }
    col_nms <- groups_attr[[tt$name]]
    if (length(col_nms) == 0) {
        return(list())
    }

    issues <- list()
    per_col_ok <- TRUE
    for (col_nm in col_nms) {
        if (!col_nm %in% names(data)) {
            per_col_ok <- FALSE
            next
        }
        col_issues <- check_col_type_and_na(data[[col_nm]], tt$inner, c(path, col_nm))
        issues <- c(issues, col_issues)
        if (length(col_issues) > 0) per_col_ok <- FALSE
    }

    if (per_col_ok && !is.null(tt$cross_check)) {
        present_nms <- intersect(col_nms, names(data))
        col_list <- stats::setNames(lapply(present_nms, function(nm) data[[nm]]), present_nms)
        if (!isTRUE(tt$cross_check(col_list, tt))) {
            new_issue <- make_issue(
                "cross_check_failed",
                path,
                name = tt$name,
                expected = tt$cross_msg(tt)
            )
            issues <- c(issues, list(new_issue))
        }
    }

    issues
}


validate_distinct <- function(cols, data, col_info, path = character(0)) {
    issues <- list()

    for (i in seq_along(cols)) {
        if (col_info$is_other[i] || col_info$is_nest[i]) {
            next
        }

        if (col_info$is_multiple[i]) {
            tt <- cols[[i]]
            if (!isTRUE(attr(tt$inner, "distinct"))) {
                next
            }
            groups_attr <- attr(data, "sch_groups")
            if (is.null(groups_attr) || !tt$name %in% names(groups_attr)) {
                next
            }
            for (col_nm in groups_attr[[tt$name]]) {
                if (!col_nm %in% names(data)) {
                    next
                }
                x <- data[[col_nm]]
                x_obs <- x[!is.na(x)]
                if (vctrs::vec_unique_count(x_obs) != vctrs::vec_size(x_obs)) {
                    issues <- c(issues, list(make_issue("not_distinct", c(path, col_nm))))
                }
            }
            next
        }

        tt <- cols[[i]]
        if (!attr(tt, "distinct")) {
            next
        }
        col_nm <- col_info$nms[i]
        if (!col_nm %in% names(data)) {
            next
        }
        col_path <- c(path, col_nm)

        x <- data[[col_nm]]
        x_obs <- x[!is.na(x)]
        if (vctrs::vec_unique_count(x_obs) != vctrs::vec_size(x_obs)) {
            issues <- c(issues, list(make_issue("not_distinct", col_path)))
        }
    }

    issues
}


validate_nests <- function(cols, data, col_info, check, path = character(0)) {
    issues <- list()

    for (i in which(col_info$is_named_nest)) {
        issues <- c(
            issues,
            validate_named_nest(cols[[i]], col_info$nms[i], data, check, path)
        )
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
    inner_col_info <- classify_columns(nest$cols)
    for (idx in seq_along(x)) {
        elem <- x[[idx]]
        if (!is.data.frame(elem)) {
            issues <- c(
                issues,
                list(make_issue("not_data_frame_element", col_path, index = idx))
            )
            next
        }

        inner_issues <- list()
        if ("names" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_names(nest$cols, elem, inner_col_info, col_path)
            )
        }
        if ("types" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_types_missing(nest$cols, elem, inner_col_info, col_path)
            )
        }
        if ("distinct" %in% check) {
            inner_issues <- c(
                inner_issues,
                validate_distinct(nest$cols, elem, inner_col_info, col_path)
            )
        }

        for (j in seq_along(inner_issues)) {
            inner_issues[[j]]$element <- idx
        }
        issues <- c(issues, inner_issues)
    }

    issues
}


# Relationship validation -------------------------------------------------

validate_relationships <- function(schema, data) {
    tree <- schema$relationships
    if (is.null(tree)) {
        return(list())
    }

    # All formula columns
    all_cols <- relationship_columns(tree)

    # Skip if any formula column missing from data (names check catches it)
    if (!all(all_cols %in% names(data))) {
        return(list())
    }

    # Skip empty data frames
    if (nrow(data) == 0L) {
        return(list())
    }

    issues <- list()

    # 1. Uniqueness: all formula columns form a primary key
    key_data <- data[all_cols]
    n_unique <- nrow(vctrs::vec_unique(key_data))
    if (n_unique != nrow(data)) {
        n_dup <- nrow(data) - n_unique
        new_issue <- make_issue(
            "duplicate_key",
            character(0),
            columns = all_cols,
            n_duplicates = n_dup
        )
        issues <- c(issues, list(new_issue))
    }

    # 2. Recursively check crossing and nesting
    issues <- c(issues, validate_rel_node(tree, data, group_cols = character(0)))

    issues
}


# Recursively validate a relationship tree node
validate_rel_node <- function(node, data, group_cols) {
    switch(
        node$type,
        var = list(),
        compound = list(),
        cross = validate_rel_cross(node, data, group_cols),
        nest = validate_rel_nest(node, data, group_cols),
        rlang::abort(paste0("Unknown relationship node type: ", node$type))
    )
}


# Check crossing completeness: within each group defined by group_cols,
# the unique combos of all children = product of unique combos per child
validate_rel_cross <- function(node, data, group_cols) {
    issues <- list()
    children <- node$children

    # Get column names per child
    child_cols <- lapply(children, relationship_columns)

    if (length(group_cols) == 0) {
        issues <- c(issues, check_cross_completeness(data, child_cols))
    } else {
        grp_data <- data[group_cols]
        locs <- vctrs::vec_group_loc(grp_data)

        for (loc in locs$loc) {
            sub <- vctrs::vec_slice(data, loc)
            issues <- c(issues, check_cross_completeness(sub, child_cols))
        }
    }

    # Recurse: each child gets siblings' columns as context
    for (k in seq_along(children)) {
        sibling_cols <- unique(unlist(child_cols[-k]))
        new_group <- c(group_cols, sibling_cols)
        issues <- c(issues, validate_rel_node(children[[k]], data, new_group))
    }

    issues
}


# Check that unique(all child cols) == product of unique(each child's cols)
check_cross_completeness <- function(data, child_cols) {
    if (length(child_cols) < 2) {
        return(list())
    }

    all_cols <- unique(unlist(child_cols))
    actual <- nrow(vctrs::vec_unique(data[all_cols]))
    expected <- prod(vapply(
        child_cols,
        function(cc) {
            nrow(vctrs::vec_unique(data[cc]))
        },
        0L
    ))

    if (actual < expected) {
        label <- paste(
            vapply(child_cols, function(cc) paste(cc, collapse = " + "), ""),
            collapse = " \u00d7 "
        )
        return(list(make_issue(
            "incomplete_crossing",
            character(0),
            label = label,
            actual = actual,
            expected = expected
        )))
    }

    list()
}


# Check nesting: inner scoped within outer groups; (outer, inner) unique
validate_rel_nest <- function(node, data, group_cols) {
    issues <- list()
    outer_cols <- relationship_columns(node$outer)

    # Inner is scoped within outer: just add outer to group_cols and recurse
    new_group <- c(group_cols, outer_cols)
    issues <- c(issues, validate_rel_node(node$inner, data, new_group))
    # Also recurse into outer with original group_cols
    issues <- c(issues, validate_rel_node(node$outer, data, group_cols))

    issues
}


# Printing ---------------------------------------------------------------

print_validation_issues <- function(issues, class = NULL, call = NULL) {
    n <- length(issues)

    bullets <- vapply(issues, print_validation_issue, character(1))
    names(bullets) <- rep("x", n)
    if (is.null(class) || length(class) == 0) {
        class <- "Data"
    } else {
        class = paste0("{.cls ", paste(class, collapse = "}/{.cls "), "}")
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
        not_nested_df = cli::format_inline(
            "Column {.field {p}} should be a list of data frames."
        ),
        not_data_frame_element = cli::format_inline(
            "Column {.field {p}} element {issue$index} is not a data frame."
        ),
        missing_sch_groups = cli::format_inline(
            "Data frame is missing the {.field sch_groups} attribute (required by {.fn sch_multiple})."
        ),
        missing_group = cli::format_inline(
            "Malformed attributes: group {.field {issue$name}} not found in {.field sch_groups}."
        ),
        empty_group = cli::format_inline(
            "Malformed attributes: group {.field {issue$name}} in {.field sch_groups} is emptpy but {.code required = TRUE})."
        ),
        cross_check_failed = cli::format_inline(
            "Column group {.field {issue$name}} failed cross-column check. Expected {issue$expected}."
        ),
        duplicate_key = {
            cols_str <- paste(issue$columns, collapse = ", ")
            cli::format_inline(
                "Columns ({.field {cols_str}}) should uniquely identify each row. Found {issue$n_duplicates} duplicate combination{?s}."
            )
        },
        incomplete_crossing = cli::format_inline(
            "Incomplete crossing of {issue$label}: found {issue$actual} of {issue$expected} expected combinations."
        ),
        paste0("Unknown issue: ", issue$type)
    )
}


# Helpers ----------

make_issue <- function(type, path, ...) {
    c(list(type = type, path = path), list(...))
}

# Build "a <type description>" / "an <type description>" for a type object
a_an_msg <- function(tt) {
    msg <- type_fns[[tt$type]]$msg(tt)
    paste0(if (grepl("^[aeiou]", msg)) "an " else "a ", msg)
}

# TRUE if a vector or list-column contains any NA / NULL elements
has_missing <- function(x) {
    if (is.list(x)) anyNA(x) || any(vapply(x, is.null, logical(1))) else anyNA(x)
}

# Type check + (gated) NA check for a single column vector
check_col_type_and_na <- function(x, tt, col_path) {
    issues <- list()
    type_ok <- isTRUE(type_fns[[tt$type]]$check(x, tt))
    if (!type_ok) {
        issues <- c(issues, list(make_issue("wrong_type", col_path, expected = a_an_msg(tt))))
    }
    if (type_ok && !attr(tt, "missing") && has_missing(x)) {
        issues <- c(issues, list(make_issue("has_na", col_path)))
    }
    issues
}
