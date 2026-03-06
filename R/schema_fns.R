sch_validate <- function(x, type) {
    stopifnot(inherits(type, "sch_type"))

    if (type$type == "object") {}
}


check_num <- function(x, type) {
    switch(
        type$type,
        numeric = is.numeric(x),
        integer = is.integer(x),
        date = inherits(x, "Date"),
        datetime = inherits(x, "POSIXct")
    ) &&
        (if (type$closed[1]) {
            all(x >= type$bounds[1], na.rm = TRUE)
        } else {
            all(x > type$bounds[1], na.rm = TRUE)
        }) &&
        (if (type$closed[2]) {
            all(x <= type$bounds[2], na.rm = TRUE)
        } else {
            all(x < type$bounds[2], na.rm = TRUE)
        })
}
msg_num <- function(type) {
    out = paste0(type$type, " vector")
    if (!all(is.infinite(type$bounds))) {
        out = paste0(
            out,
            " with values in ",
            if (type$closed[1]) "[" else "(",
            type$bounds[1],
            ", ",
            type$bounds[2],
            if (type$closed[2]) "]" else ")"
        )
    }
    out
}


type_fns = list(
    numeric = list(check = check_num, msg = msg_num, coerce = as.numeric),
    integer = list(check = check_num, msg = msg_num, coerce = as.integer),
    date = list(check = check_num, msg = msg_num, coerce = as.Date),
    datetime = list(check = check_num, msg = msg_num, coerce = as.POSIXct),
    logical = list(
        check = function(x, type) is.logical(x),
        msg = function(type) "logical vector",
        coerce = as.logical
    ),

    factor = list(
        check = function(x, type) {
            as.factor(x) ||
                (!type$strict && is.character(x) && all(x %in% type$levels))
        },
        msg = function(type) {
            levs = cli::cli_vec(
                type$levels,
                list(
                    "vec-trunc" = 10,
                    "vec-last" = ", or "
                )
            )
            cli::format_inline("factor; one of {.strong {levs}}")
        },
        coerce = function(x, type) {
            factor(x, levels = type$levels)
        }
    ),

    character = list(
        check = function(x, type) is.character(x),
        msg = function(type) "character vector",
        coerce = as.character
    ),

    inherits = list(
        check = function(x, type) {
            inherits(x, type$class)
        },
        msg = function(type) {
            cli::format_inline("vector inheriting from {.cls {type$class}}")
        },
        coerce = function(x, type) {
            methods::as(x, type$class)
        }
    ),
    list_of = list(
        check = function(x, type) {
            is.list(x) &&
                all(
                    vapply(x, inherits, logical(1), what = type$class)
                )
        },
        msg = function(type) {
            cli::format_inline("list-column with elements of type {.cls {type$class}}")
        },
        coerce = function(x, type) {
            lapply(x, function(y) {
                methods::as(y, type$class)
            })
        }
    ),

    custom = list(
        check = function(x, type) {
            type$check(x, type)
        },
        msg = function(type) {
            type$msg(type)
        },
        coerce = function(x, type) {
            type$coerce(x, type)
        }
    )
)
