#' Define a structured data type
#'
#' Defines the structure of a single 'observation' for a structured data frame.
#' Each column has type restrictions and may be required or optional.
#' Schemas can be nested.
#'
#' @param ... Column specifications, in the form of `col_name = col_type` pairs,
#'   where `col_type` is a call to a column type constructor listed here, such
#'   as `sch_numeric()`. Every type must be a kind of vector, i.e.,
#'   [vctrs::obj_is_vector()] must return `TRUE`.
#'
#'   All columns must be named, except for `sch_others()`, as described below,
#'   and `sch_flat()`, which describes a set of columns which are logically
#'   nested within the outer columns but are not nested in the actual data frame.
#'
#'   The special function `sch_others()` indicates the preferred location of
#'   other columns not explicitly mentioned in the schema. If no `sch_others()`
#'   appears, then other columns are not allowed.
#'   Trailing commas are permitted.
#' @param desc,.desc A description of the column for consumers of the schema.
#'   The type contraints will be described separately and do not need to be
#'   included in the description.  For example for "age", the descriptoin might
#'   be "Age of the patient in years", not "Non-negative integer representing
#'   the age of the patient in years".
#' @param missing If `TRUE`, the column may be contain missing values. Otherwise,
#'   any missing values result in an error.
#' @param required If `TRUE`, the column must be present. If `FALSE`, the column
#'   is optional.
#'
#' @returns An object of class `sch_schema`,
#' @examples
#' sch_schema(
#'     .desc = "Student data",
#'     age = sch_integer("Age in years", bounds = c(0, 130)),
#'     birthday = sch_date("Date of birth", required = FALSE),
#'     height = sch_numeric(
#'         "Height in inches",
#'         bounds = c(0, 108),
#'         closed = c(FALSE, TRUE)
#'     ),
#'     teacher = sch_factor(levels = c("Jones", "Smith", "Hernandez")),
#'     enrolled = sch_logical(missing = FALSE),
#'     sch_others()
#' )
#'
#' sch_schema(
#'     .desc = "MCMC draws",
#'     draw = sch_integer("Draw number", bounds = c(1, Inf), closed = c(TRUE, FALSE)),
#'     contents = sch_schema(
#'        param = sch_factor("Parameter name", levels = c("mu", "sigma", "log_lik")),
#'        value = sch_numeric("Parameter value"),
#'     )
#' )
#'
#' sch_custom(
#'    name = "even",
#'    check = function(x, type) is.integer(x) && all(x %% 2 == 0),
#'    msg = function(type) "vector of even integers",
#'    coerce = function(x, type) (as.integer(x) %/% 2) * 2
#' )
#' @export
sch_schema <- function(..., .desc = NULL) {
    cols = rlang::dots_list(..., .homonyms = "error", .check_assign = TRUE)

    if (!all(vapply(cols, inherits, FALSE, what = "sch_type"))) {
        rlang::abort("All columns must be specified using a column type constructor.")
    }

    is_other = vapply(cols, function(x) x$type == "other", FALSE)
    if (sum(is_other) > 1) {
        rlang::abort("Only one {.fn sch_others()} is allowed in a schema.")
    }
    if (any(is_other) && rlang::is_named(cols[is_other])) {
        rlang::abort("{.fn sch_others()} must not be named.")
    }

    named_cols = cols[!is_other]
    if (length(named_cols) > 0 && !rlang::is_named(named_cols)) {
        rlang::abort("All columns must be named.")
    }
    nms = names(named_cols)
    if (anyDuplicated(nms[nzchar(nms)]) > 0) {
        rlang::abort("Column names must be unique.")
    }

    structure(
        list(type = "schema_flat", cols = cols),
        desc = check_desc(.desc),
        missing = FALSE,
        required = TRUE,
        class = c("sch_schema", "sch_type")
    )
}

#' @export
format.sch_schema <- function(x, ansi = FALSE, ...) {
    out = character(length(x$cols))
    names(out) = names(x$cols)
    for (i in seq_along(x$cols)) {
        tt = x$cols[[i]]
        fmt = format(tt, ansi = ansi)
        if (tt$type == "other") {
            out[i] = fmt
            names(out)[i] = "..."
            next
        }
        fmt = paste0(
            fmt,
            ".",
            if (!attr(tt, "missing")) " No NAs allowed.",
            if (!attr(tt, "required")) " [Optional]"
        )
        if (!is.null(names(fmt))) {
            fmt = paste0(names(fmt), ": ", fmt)
        }
        out[i] = unname(fmt)
    }
    attr(out, "desc") = attr(x, "desc")
    out
}
#' @export
print.sch_schema <- function(x, ...) {
    if (!is.null(attr(x, "desc"))) {
        cat(cli::style_bold(attr(x, "desc")), "\n")
    }
    n_req = sum(vapply(x$cols, function(y) attr(y, "required"), FALSE))
    cat(cli::col_grey("A schema with ", n_req, " required columns:"), "\n", sep = "")
    fmt = format(x, ansi = TRUE)
    nms = names(fmt)
    w_nms = max(nchar(nms))
    lbls = cli::ansi_align(cli::col_green(nms), width = w_nms, align = "right")
    cat(paste0(lbls, "  ", fmt), sep = "\n")
}

#' @export
format.sch_type <- function(x, ansi = FALSE, ...) {
    if (x$type == "other") {
        if (isTRUE(ansi)) {
            return(cli::style_italic("Other columns"))
        } else {
            return("Other columns")
        }
    }
    assert(x$type %in% names(type_fns), paste0("Unknown type: ", x$type))
    msg = type_fns[[x$type]]$msg(x)
    if (!isTRUE(ansi)) {
        msg = cli::ansi_strip(msg)
    }
    a_an = if (grepl("^[aeiou]", msg)) "An " else "A "
    out = paste0(a_an, msg)
    names(out) = attr(x, "desc")
    out
}
#' @export
print.sch_type <- function(x, ...) {
    fmt = format(x, ansi = TRUE)
    if (!is.null(names(fmt))) {
        cat(names(fmt), "\n", fmt, "\n", sep = "")
    } else {
        cat(fmt, "\n", sep = "")
    }
}


#' @describeIn sch_schema A placeholder for other non-required columns in a schema.
#' @export
sch_others <- function() {
    structure(list(type = "other"), required = FALSE, class = "sch_type")
}

#' @describeIn sch_schema A numeric vector that is optionally constrained to be
#'   within a certain range.
#'
#' @param bounds, Length-two vector `c(min, max)` specifying the allowed range of values.
#' @param closed Length-two logical vector specifying whether the bounds are
#'   closed (inclusive) or open (exclusive).
#' @export
sch_numeric <- function(
    desc = NULL,
    bounds = c(-Inf, Inf),
    closed = c(TRUE, TRUE),
    missing = TRUE,
    required = TRUE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "numeric", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}


#' @describeIn sch_schema An integer vector that is optionally constrained to be
#'   within a certain range.
#' @export
sch_integer <- function(
    desc = NULL,
    bounds = c(-Inf, Inf),
    closed = c(TRUE, TRUE),
    missing = TRUE,
    required = TRUE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "integer", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A logical vector.
#' @export
sch_logical <- function(desc = NULL, missing = TRUE, required = TRUE) {
    structure(
        list(type = "logical"),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A character vector.
#' @export
sch_character <- function(desc = NULL, missing = TRUE, required = TRUE) {
    structure(
        list(type = "character"),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A factor with specified levels.
#' @param levels A character vector of factor levels.
#' @param strict If `TRUE`, only factors with the specified levels are accepted.
#'   If `FALSE`, character vectors with the specified levels are also accepted.
#' @export
sch_factor <- function(
    desc = NULL,
    levels,
    strict = TRUE,
    missing = TRUE,
    required = TRUE
) {
    if (!is.character(levels)) {
        rlang::abort("`levels` must be a character vector.")
    }
    structure(
        list(type = "factor", levels = levels),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A Date vector that is optionally constrained to be
#'   within a certain range.
#' @export
sch_date <- function(
    desc = NULL,
    bounds = c(as.Date(-Inf), as.Date(Inf)),
    closed = c(FALSE, FALSE),
    missing = TRUE,
    required = TRUE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "date", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}
#' @describeIn sch_schema A POSIXct vector that is optionally constrained to be
#'   within a certain range.
#' @export
sch_datetime <- function(
    desc = NULL,
    bounds = c(as.POSIXct(-Inf), as.POSIXct(Inf)),
    closed = c(FALSE, FALSE),
    missing = TRUE,
    required = TRUE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "datetime", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A list-column whose elements satisfy `inherits(_, class)`.
#' @param class A character vector of class names.
#'
#' @export
sch_inherits <- function(desc = NULL, class, missing = TRUE, required = TRUE) {
    structure(
        list(type = "inherits", class = as.character(class)),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A vector satisfying `inherits(_, class)`.
#' @export
sch_list_of <- function(desc = NULL, class, missing = TRUE, required = TRUE) {
    structure(
        list(type = "list_of", class = as.character(class)),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A custom type defined by user-provided check, error,
#'   and coercion functions. Additional Additional named values to be stored
#'   along with the type specification may be passed via `...` and will be available
#'   to the check, error, and coercion functions.
#' @param name A name for the custom type.
#' @param check A two-argument function that checks whether an object satisfies
#'   the type. The first argument is the object to check, and the second is the
#'   full type specification.
#' @param msg A one-argument function that generates a descriptive message
#'   about the type when passed the type object itself. Should not end with a period.
#' @param coerce A two-argument function that attempts to coerce an object to the
#'   type. The first argument is the object to coerce, and the second is the full
#'   type specification.
#'
#' @export
sch_custom <- function(
    name,
    desc = NULL,
    check,
    msg,
    coerce,
    ...,
    missing = TRUE,
    required = TRUE
) {
    if (!is.character(name) || length(name) != 1) {
        rlang::abort("{.arg name} must be a single string.")
    }
    reserved_nms = c("other", names(type_fns))
    if (name %in% reserved_nms) {
        rlang::abort(
            "{.arg name} must not be one of the reserved type names: {.field {reserved_nms}}."
        )
    }
    err_fn = function(arg) {
        rlang::abort("{.arg {arg}} must be a function with two arguments: `x` and `type`.")
    }
    if (!is.function(check) || length(formals(check)) != 2) {
        err_fn("check")
    }
    if (!is.function(msg) || length(formals(msg)) != 1) {
        err_fn("msg")
    }
    if (!is.function(coerce) || length(formals(coerce)) != 2) {
        err_fn("coerce")
    }

    extras = rlang::list2(...)
    if (length(extras) > 0 && !rlang::is_named(extras)) {
        rlang::abort("All additional arguments must be named.")
    }

    structure(
        rlang::list2(
            type = "custom",
            name = name,
            check = check,
            msg = msg,
            coerce = coerce,
            !!!extras
        ),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        class = "sch_type"
    )
}

check_bounds_closed = function(bounds, closed) {
    assert(length(bounds) == 2, "{.arg bounds} must be a length-two vector.")
    assert(
        length(closed) == 2 && is.logical(closed),
        "{.arg closed} must be a length-two logical vector."
    )
}
check_desc = function(desc) {
    if (is.null(desc)) {
        NULL
    } else if (is.character(desc) && length(desc) == 1) {
        desc
    } else {
        rlang::abort("{.arg desc} must be NULL or a single string.", call = parent.frame())
    }
}
