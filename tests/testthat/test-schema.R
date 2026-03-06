test_that("sch_schema() returns correct class and structure", {
    x <- sch_schema(
        age = sch_integer(),
        name = sch_character()
    )
    expect_s3_class(x, "sch_schema")
    expect_length(x$cols, 2)
    expect_named(x$cols, c("age", "name"))
})

test_that("sch_schema() allows trailing commas", {
    expect_no_error(
        sch_schema(
            age = sch_integer(),
        )
    )
})

test_that("sch_schema() allows sch_others()", {
    x <- sch_schema(a = sch_numeric(), sch_others())
    expect_length(x$cols, 2)
    # sch_others() has no name
    expect_equal(names(x$cols)[2], "")
})

test_that("sch_schema() errors on non-sch_type columns", {
    expect_error(
        sch_schema(a = 1),
        "column type constructor"
    )
})

test_that("sch_schema() errors on multiple sch_others()", {
    expect_error(
        sch_schema(a = sch_numeric(), sch_others(), sch_others()),
        "Only one"
    )
})

test_that("sch_schema() errors on unnamed columns", {
    expect_error(
        sch_schema(sch_numeric()),
        "must be named"
    )
})

test_that("sch_schema() errors on named sch_others()", {
    expect_error(
        sch_schema(a = sch_numeric(), other = sch_others()),
        "must not be named"
    )
})

test_that("sch_schema() errors on duplicate column names", {
    expect_error(
        sch_schema(a = sch_numeric(), a = sch_integer())
    )
})

# sch_others() -----------------------------------------------------------

test_that("sch_others() returns sch_type with type = 'other'", {
    x <- sch_others()
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "other")
    expect_false(attr(x, "required"))
})

# Column type constructors -----------------------------------------------

test_that("sch_numeric() returns correct structure", {
    x <- sch_numeric("A number", bounds = c(0, 10), closed = c(TRUE, FALSE))
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "numeric")
    expect_equal(x$bounds, c(0, 10))
    expect_equal(x$closed, c(TRUE, FALSE))
    expect_equal(attr(x, "desc"), "A number")
    expect_true(attr(x, "missing"))
    expect_true(attr(x, "required"))
})

test_that("sch_numeric() defaults: infinite bounds, both closed, missing/required TRUE", {
    x <- sch_numeric()
    expect_equal(x$bounds, c(-Inf, Inf))
    expect_equal(x$closed, c(TRUE, TRUE))
    expect_null(attr(x, "desc"))
    expect_true(attr(x, "missing"))
    expect_true(attr(x, "required"))
})

test_that("sch_integer() returns correct structure", {
    x <- sch_integer("Count", bounds = c(0L, 100L), missing = FALSE, required = FALSE)
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "integer")
    expect_false(attr(x, "missing"))
    expect_false(attr(x, "required"))
})

test_that("sch_logical() returns correct structure", {
    x <- sch_logical("Flag", missing = FALSE)
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "logical")
    expect_equal(attr(x, "desc"), "Flag")
    expect_false(attr(x, "missing"))
    expect_true(attr(x, "required"))
})

test_that("sch_character() returns correct structure", {
    x <- sch_character()
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "character")
})

test_that("sch_factor() returns correct structure", {
    levs <- c("a", "b", "c")
    x <- sch_factor(levels = levs)
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "factor")
    expect_equal(x$levels, levs)
})

test_that("sch_factor() errors when levels is not character", {
    expect_error(sch_factor(levels = 1:3), "`levels` must be a character vector")
})

test_that("sch_date() returns correct structure", {
    x <- sch_date("Birthday", required = FALSE)
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "date")
    expect_false(attr(x, "required"))
})

test_that("sch_datetime() returns correct structure", {
    x <- sch_datetime()
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "datetime")
    expect_equal(x$closed, c(FALSE, FALSE))
})

test_that("sch_inherits() returns correct structure", {
    x <- sch_inherits(class = "myClass")
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "inherits")
    expect_equal(x$class, "myClass")
})

test_that("sch_list_of() returns correct structure", {
    x <- sch_list_of(class = "data.frame")
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "list_of")
    expect_equal(x$class, "data.frame")
})

# Bounds/closed validation -----------------------------------------------

test_that("bounds must be length two", {
    expect_error(sch_numeric(bounds = c(1, 2, 3)), "length-two")
    expect_error(sch_integer(bounds = 1), "length-two")
})

test_that("closed must be a length-two logical vector", {
    expect_error(sch_numeric(closed = c(1, 0)), "length-two logical")
    expect_error(sch_numeric(closed = TRUE), "length-two logical")
})

# sch_custom() -----------------------------------------------------------

test_that("sch_custom() creates a valid custom type", {
    x <- sch_custom(
        name = "mytype",
        check = function(x, type) is.numeric(x),
        msg = function(type) "a custom numeric",
        coerce = function(x, type) as.numeric(x)
    )
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "custom")
    expect_equal(x$name, "mytype")
    expect_true(is.function(x$check))
    expect_true(is.function(x$msg))
    expect_true(is.function(x$coerce))
})

test_that("sch_custom() errors on reserved names", {
    expect_error(
        sch_custom(
            "numeric",
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "reserved"
    )
    expect_error(
        sch_custom(
            "other",
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "reserved"
    )
})

test_that("sch_custom() errors when name is not a single string", {
    expect_error(
        sch_custom(
            c("a", "b"),
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        )
    )
})

test_that("sch_custom() errors on wrong function signatures", {
    # check must have 2 args
    expect_error(
        sch_custom(
            "mytype",
            check = function(x) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "check"
    )
    # msg must have 1 arg
    expect_error(
        sch_custom(
            "mytype",
            check = function(x, type) TRUE,
            msg = function(type, extra) "x",
            coerce = function(x, type) x
        ),
        "msg"
    )
    # coerce must have 2 args
    expect_error(
        sch_custom(
            "mytype",
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x) x
        ),
        "coerce"
    )
})

test_that("sch_custom() errors on unnamed extra arguments", {
    # description must be explicitly named so the unnamed value lands in ...
    expect_error(
        sch_custom(
            "mytype",
            desc = NULL,
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x,
            "unnamed_extra"
        ),
        "named"
    )
})

test_that("sch_custom() stores extra named arguments", {
    x <- sch_custom(
        "mytype",
        check = function(x, type) TRUE,
        msg = function(type) "x",
        coerce = function(x, type) x,
        threshold = 42
    )
    expect_equal(x$threshold, 42)
})

# description validation -------------------------------------------------

test_that("description must be NULL or a single string", {
    expect_no_error(sch_numeric(desc = NULL))
    expect_no_error(sch_numeric(desc = "some text"))
    expect_error(sch_numeric(desc = c("a", "b")))
    expect_error(sch_numeric(desc = 123))
})

# format methods ---------------------------------------------------------

test_that("format.sch_type() returns a character string", {
    expect_type(format(sch_numeric()), "character")
    expect_type(format(sch_integer()), "character")
    expect_type(format(sch_logical()), "character")
    expect_type(format(sch_character()), "character")
    expect_type(format(sch_factor(levels = c("a", "b"))), "character")
    expect_type(format(sch_date()), "character")
    expect_type(format(sch_datetime()), "character")
})

test_that("format.sch_type() for sch_others() returns 'Other columns'", {
    expect_equal(format(sch_others()), "Other columns")
})

test_that("format.sch_type() description becomes name attribute", {
    x <- sch_numeric("Height")
    fmt <- format(x)
    expect_equal(names(fmt), "Height")
})

test_that("format.sch_type() with no description has no name", {
    x <- sch_numeric()
    fmt <- format(x)
    expect_null(names(fmt))
})

test_that("format.sch_type() includes range for bounded numeric", {
    x <- sch_numeric(bounds = c(0, 100))
    fmt <- format(x)
    expect_match(fmt, "0")
    expect_match(fmt, "100")
})

test_that("format.sch_schema() returns named character vector", {
    x <- sch_schema(
        age = sch_integer(),
        name = sch_character()
    )
    fmt <- format(x)
    expect_type(fmt, "character")
    expect_named(fmt, c("age", "name"))
})

test_that("format.sch_schema() labels sch_others() as '...'", {
    x <- sch_schema(a = sch_numeric(), sch_others())
    fmt <- format(x)
    expect_true("..." %in% names(fmt))
})

test_that("format.sch_schema() appends optional/no-NA notes", {
    x <- sch_schema(
        a = sch_numeric(missing = FALSE),
        b = sch_numeric(required = FALSE)
    )
    fmt <- format(x)
    expect_match(fmt["a"], "No NAs allowed")
    expect_match(fmt["b"], "Optional")
})

test_that("print.sch_schema() runs without error", {
    x <- sch_schema(age = sch_integer(), name = sch_character())
    expect_output(print(x), "schema")
})

test_that("print.sch_type() runs without error", {
    expect_output(print(sch_numeric("Height")), "Height")
    expect_output(print(sch_others()))
})

# sch_nest() (named only — for list-column nested data frames) -----------

test_that("sch_nest() returns sch_schema class with type schema_nest", {
    x <- sch_nest(
        param = sch_character(),
        value = sch_numeric()
    )
    expect_s3_class(x, "sch_schema")
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "schema_nest")
})

test_that("sch_nest() stores columns correctly", {
    x <- sch_nest(
        a = sch_integer(),
        b = sch_numeric()
    )
    expect_named(x$cols, c("a", "b"))
    expect_s3_class(x$cols$a, "sch_type")
    expect_s3_class(x$cols$b, "sch_type")
})

test_that("sch_nest() accepts .desc", {
    x <- sch_nest(a = sch_integer(), .desc = "My group")
    expect_equal(attr(x, "desc"), "My group")
})

test_that("sch_nest() errors on non-sch_type columns", {
    expect_error(
        sch_nest(a = 1),
        "column type constructor"
    )
})

test_that("sch_nest() errors on unnamed columns", {
    expect_error(
        sch_nest(sch_integer()),
        "named"
    )
})

test_that("sch_nest() errors when sch_others() is included", {
    expect_error(
        sch_nest(a = sch_integer(), sch_others()),
        "sch_others"
    )
})

test_that("sch_nest() errors on duplicate column names", {
    expect_error(
        sch_nest(a = sch_integer(), a = sch_numeric())
    )
})

# sch_nest() inside sch_schema() -----------------------------------------

test_that("named sch_nest() works in sch_schema()", {
    x <- sch_schema(
        id = sch_integer(),
        draws = sch_nest(
            param = sch_character(),
            value = sch_numeric()
        )
    )
    expect_s3_class(x, "sch_schema")
    expect_equal(x$cols$draws$type, "schema_nest")
})

test_that("unnamed sch_nest() is not allowed in sch_schema()", {
    expect_error(
        sch_schema(
            id = sch_integer(),
            sch_nest(
                param = sch_character(),
                value = sch_numeric()
            )
        ),
        "named"
    )
})

# Print/format with sch_nest() ------------------------------------------

test_that("format.sch_schema() with named sch_nest() shows (nested) header", {
    x <- sch_schema(
        age = sch_integer(),
        scores = sch_nest(
            subject = sch_character(),
            score = sch_numeric(),
            .desc = "exam scores"
        )
    )
    fmt <- format(x)
    header_line <- fmt[grep("nested", fmt)]
    expect_length(header_line, 1)
    expect_match(header_line, "exam scores")
    expect_match(header_line, "\\(nested\\)")
    # named nest has the column name in names(fmt)
    expect_true("scores" %in% names(fmt))
})

test_that("print.sch_schema() with named sch_nest() runs without error", {
    x <- sch_schema(
        id = sch_integer(),
        draws = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .desc = "MCMC draws"
        )
    )
    expect_output(print(x), "schema")
})

# Distinct argument tests -----------------------------------------------

test_that("All column types default distinct to FALSE", {
    expect_false(attr(sch_numeric(), "distinct"))
    expect_false(attr(sch_integer(), "distinct"))
    expect_false(attr(sch_logical(), "distinct"))
    expect_false(attr(sch_character(), "distinct"))
    expect_false(attr(sch_factor(levels = c("a", "b")), "distinct"))
    expect_false(attr(sch_date(), "distinct"))
    expect_false(attr(sch_datetime(), "distinct"))
    expect_false(attr(sch_inherits(class = "myClass"), "distinct"))
    expect_false(attr(sch_list_of(class = "data.frame"), "distinct"))
    expect_false(attr(
        sch_custom(
            name = "test",
            check = function(x, type) TRUE,
            msg = function(type) "test",
            coerce = function(x, type) x
        ),
        "distinct"
    ))
    expect_false(attr(sch_others(), "distinct"))
})

test_that("distinct = TRUE is stored as attribute", {
    x <- sch_numeric(distinct = TRUE)
    expect_true(attr(x, "distinct"))

    y <- sch_character(distinct = TRUE)
    expect_true(attr(y, "distinct"))

    z <- sch_factor(levels = c("a", "b"), distinct = TRUE)
    expect_true(attr(z, "distinct"))
})

test_that("distinct = FALSE explicitly is stored as attribute", {
    x <- sch_numeric(distinct = FALSE)
    expect_false(attr(x, "distinct"))
})

test_that("[Distinct] flag appears in schema print output for distinct columns", {
    x <- sch_schema(
        id = sch_integer(distinct = TRUE),
        name = sch_character(),
        email = sch_character(distinct = TRUE)
    )
    output <- capture.output(print(x))
    output_text <- paste(output, collapse = "\n")

    # Check that [Distinct] flag appears in output
    expect_match(output_text, "\\[Distinct\\]")
})

test_that("[Distinct] flag does not appear for non-distinct columns", {
    x <- sch_schema(
        id = sch_integer(),
        name = sch_character()
    )
    output <- capture.output(print(x))
    output_text <- paste(output, collapse = "\n")

    # [Distinct] flag should not appear
    expect_false(grepl("\\[Distinct\\]", output_text))
})

test_that("[Distinct] and [Optional] flags work together", {
    x <- sch_schema(
        id = sch_integer(distinct = TRUE, required = FALSE),
        name = sch_character(distinct = TRUE)
    )
    output <- capture.output(print(x))
    output_text <- paste(output, collapse = "\n")

    # Both flags should appear
    expect_match(output_text, "\\[Distinct\\]")
    expect_match(output_text, "\\[Optional\\]")
})

test_that("Nested schema columns show [Distinct] flag", {
    x <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            name = sch_character(distinct = TRUE),
            value = sch_numeric()
        )
    )
    output <- capture.output(print(x))
    output_text <- paste(output, collapse = "\n")

    expect_match(output_text, "\\[Distinct\\]")
})

# Regression tests: strict factor level validation ----

test_that("strict factor with wrong levels attribute raises error", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = TRUE))
    df <- data.frame(x = factor(c("x", "y"), levels = c("x", "y")))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("strict factor with extra levels in attribute raises error", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = TRUE))
    df <- data.frame(x = factor(c("a", "b"), levels = c("a", "b", "c")))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("strict factor with exact schema levels passes", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = TRUE))
    df <- data.frame(x = factor(c("a", "b"), levels = c("a", "b")))
    expect_no_error(sch_validate(schema, df))
})

test_that("strict factor with values outside schema levels raises error", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = TRUE))
    df <- data.frame(x = factor(c("a", "z"), levels = c("a", "b", "z")))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("non-strict factor ignores extra entries in levels attribute", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = FALSE))
    df <- data.frame(x = factor(c("a", "b"), levels = c("a", "b", "extra")))
    expect_no_error(sch_validate(schema, df))
})

# Regression tests: non-strict factor with NAs ----

test_that("non-strict factor with character and NA allowed when missing=TRUE", {
    schema <- sch_schema(
        x = sch_factor(levels = c("a", "b"), strict = FALSE, missing = TRUE)
    )
    df <- data.frame(x = c("a", NA), stringsAsFactors = FALSE)
    expect_no_error(sch_validate(schema, df))
})

test_that("non-strict factor with character and NA raises 'missing values' when missing=FALSE", {
    schema <- sch_schema(
        x = sch_factor(levels = c("a", "b"), strict = FALSE, missing = FALSE)
    )
    df <- data.frame(x = c("a", NA), stringsAsFactors = FALSE)
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing values")
})

test_that("non-strict factor with factor input and NA allowed when missing=TRUE", {
    schema <- sch_schema(
        x = sch_factor(levels = c("a", "b"), strict = FALSE, missing = TRUE)
    )
    df <- data.frame(x = factor(c("a", NA), levels = c("a", "b")))
    expect_no_error(sch_validate(schema, df))
})

# Regression tests: sch_list_of with NULL elements ----

test_that("list_of with NULL element and missing=TRUE passes", {
    schema <- sch_schema(x = sch_list_of(class = "data.frame", missing = TRUE))
    df <- data.frame(x = I(list(data.frame(a = 1), NULL)))
    expect_no_error(sch_validate(schema, df))
})

test_that("list_of with NULL element and missing=FALSE raises error", {
    schema <- sch_schema(x = sch_list_of(class = "data.frame", missing = FALSE))
    df <- data.frame(x = I(list(data.frame(a = 1), NULL)))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing values")
})

test_that("list_of with all valid elements passes", {
    schema <- sch_schema(x = sch_list_of(class = "data.frame"))
    df <- data.frame(x = I(list(data.frame(a = 1), data.frame(a = 2))))
    expect_no_error(sch_validate(schema, df))
})

# sch_multiple() ---------------------------------------------------------

test_that("sch_multiple() returns sch_type with type schema_multiple", {
    x <- sch_multiple(name = "trt", type = sch_numeric())
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "schema_multiple")
})

test_that("sch_multiple() stores name, inner type, and required", {
    inner <- sch_numeric("A share", bounds = c(0, 1))
    x <- sch_multiple(name = "demo", type = inner, desc = "Demographics")
    expect_equal(x$name, "demo")
    expect_identical(x$inner, inner)
    expect_true(attr(x, "required"))
    expect_equal(attr(x, "desc"), "Demographics")
})

test_that("sch_multiple() required=FALSE is stored correctly", {
    x <- sch_multiple(name = "grp", type = sch_integer(), required = FALSE)
    expect_false(attr(x, "required"))
})

test_that("sch_multiple() without cross-column fns stores NULLs", {
    x <- sch_multiple(name = "grp", type = sch_integer())
    expect_null(x$cross_check)
    expect_null(x$cross_msg)
    expect_null(x$cross_coerce)
})

test_that("sch_multiple() stores cross-column functions", {
    chk <- function(x, type) TRUE
    msg <- function(type) "ok"
    crc <- function(x, type) x
    x <- sch_multiple(name = "grp", type = sch_numeric(), check = chk, msg = msg, coerce = crc)
    expect_identical(x$cross_check, chk)
    expect_identical(x$cross_msg, msg)
    expect_identical(x$cross_coerce, crc)
})

test_that("sch_multiple() errors when name is missing", {
    expect_error(sch_multiple(type = sch_numeric()), "name")
})

test_that("sch_multiple() errors when name is not a single string", {
    expect_error(sch_multiple(name = c("a", "b"), type = sch_numeric()), "name")
    expect_error(sch_multiple(name = 1L, type = sch_numeric()), "name")
})

test_that("sch_multiple() errors when type is not sch_type", {
    expect_error(sch_multiple(name = "grp", type = list()), "sch_type")
    expect_error(sch_multiple(name = "grp", type = "numeric"), "sch_type")
})

test_that("sch_multiple() errors when type is sch_others()", {
    expect_error(sch_multiple(name = "grp", type = sch_others()), "sch_type")
})

test_that("sch_multiple() errors when type is a schema_nest", {
    expect_error(
        sch_multiple(name = "grp", type = sch_nest(a = sch_numeric())),
        "sch_type"
    )
})

test_that("sch_multiple() errors when only some of check/msg/coerce are provided", {
    expect_error(
        sch_multiple(
            name = "grp",
            type = sch_numeric(),
            check = function(x, type) TRUE
        ),
        "check.*msg.*coerce|all.*provided"
    )
    expect_error(
        sch_multiple(
            name = "grp",
            type = sch_numeric(),
            msg = function(type) "x"
        ),
        "check.*msg.*coerce|all.*provided"
    )
})

test_that("sch_multiple() errors on wrong check arity (must have 2 args)", {
    expect_error(
        sch_multiple(
            name = "grp",
            type = sch_numeric(),
            check = function(x) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "check"
    )
})

test_that("sch_multiple() errors on wrong msg arity (must have 1 arg)", {
    expect_error(
        sch_multiple(
            name = "grp",
            type = sch_numeric(),
            check = function(x, type) TRUE,
            msg = function(type, extra) "x",
            coerce = function(x, type) x
        ),
        "msg"
    )
})

test_that("sch_multiple() errors on wrong coerce arity (must have 2 args)", {
    expect_error(
        sch_multiple(
            name = "grp",
            type = sch_numeric(),
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x) x
        ),
        "coerce"
    )
})

# sch_multiple() inside sch_schema() ------------------------------------

test_that("sch_schema() allows unnamed sch_multiple()", {
    x <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    expect_s3_class(x, "sch_schema")
    expect_length(x$cols, 2)
    expect_equal(x$cols[[2]]$type, "schema_multiple")
})

test_that("sch_schema() errors on named sch_multiple()", {
    expect_error(
        sch_schema(
            id = sch_integer(),
            grp = sch_multiple(name = "trt", type = sch_numeric())
        ),
        "must not be named"
    )
})

test_that("sch_schema() allows sch_multiple() alongside sch_others()", {
    x <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric()),
        sch_others()
    )
    expect_length(x$cols, 3)
})

# format/print for sch_multiple() ---------------------------------------

test_that("format.sch_type() handles schema_multiple", {
    x <- sch_multiple(name = "trt", type = sch_numeric(bounds = c(0, 1)))
    fmt <- format(x)
    expect_type(fmt, "character")
    expect_match(fmt, "trt")
})

test_that("format.sch_schema() includes multiple group entry", {
    x <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric(), desc = "Treatments")
    )
    fmt <- format(x)
    # Should have an entry for the multiple group
    expect_true(any(grepl("trt|multiple|Treatments", fmt, ignore.case = TRUE)))
})

test_that("print.sch_schema() runs without error when sch_multiple() present", {
    x <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    expect_output(print(x))
})

# .relationships formula: parsing ----------------------------------------

test_that("parse_relationship() parses ~ a into Var", {
    tree <- parse_relationship(~a)
    expect_equal(tree$type, "var")
    expect_equal(tree$name, "a")
})

test_that("parse_relationship() parses ~ a * b into Cross", {
    tree <- parse_relationship(~ a * b)
    expect_equal(tree$type, "cross")
    expect_length(tree$children, 2)
    expect_equal(tree$children[[1]]$name, "a")
    expect_equal(tree$children[[2]]$name, "b")
})

test_that("parse_relationship() flattens ~ a * b * c into a single Cross", {
    tree <- parse_relationship(~ a * b * c)
    expect_equal(tree$type, "cross")
    expect_length(tree$children, 3)
    expect_equal(tree$children[[1]]$name, "a")
    expect_equal(tree$children[[2]]$name, "b")
    expect_equal(tree$children[[3]]$name, "c")
})

test_that("parse_relationship() parses ~ a / b into Nest", {
    tree <- parse_relationship(~ a / b)
    expect_equal(tree$type, "nest")
    expect_equal(tree$outer$name, "a")
    expect_equal(tree$inner$name, "b")
})

test_that("parse_relationship() parses ~ a / b / c as left-associative nesting", {
    tree <- parse_relationship(~ a / b / c)
    expect_equal(tree$type, "nest")
    expect_equal(tree$outer$type, "nest")
    expect_equal(tree$outer$outer$name, "a")
    expect_equal(tree$outer$inner$name, "b")
    expect_equal(tree$inner$name, "c")
})

test_that("parse_relationship() parses ~ a + b into Compound", {
    tree <- parse_relationship(~ a + b)
    expect_equal(tree$type, "compound")
    expect_length(tree$children, 2)
})

test_that("parse_relationship() flattens ~ a + b + c into a single Compound", {
    tree <- parse_relationship(~ a + b + c)
    expect_equal(tree$type, "compound")
    expect_length(tree$children, 3)
})

test_that("parse_relationship() parses ~ (a + b) * c correctly", {
    tree <- parse_relationship(~ (a + b) * c)
    expect_equal(tree$type, "cross")
    expect_equal(tree$children[[1]]$type, "compound")
    expect_equal(tree$children[[2]]$name, "c")
})

test_that("parse_relationship() parses ~ a * (b / c) correctly", {
    tree <- parse_relationship(~ a * (b / c))
    expect_equal(tree$type, "cross")
    expect_equal(tree$children[[1]]$name, "a")
    expect_equal(tree$children[[2]]$type, "nest")
    expect_equal(tree$children[[2]]$outer$name, "b")
    expect_equal(tree$children[[2]]$inner$name, "c")
})

test_that("parse_relationship() parses ~ (a * b) / c correctly", {
    tree <- parse_relationship(~ (a * b) / c)
    expect_equal(tree$type, "nest")
    expect_equal(tree$outer$type, "cross")
    expect_equal(tree$inner$name, "c")
})

test_that("parse_relationship() treats : as * (synonym)", {
    tree <- parse_relationship(~ a:b)
    expect_equal(tree$type, "cross")
    expect_length(tree$children, 2)
})

test_that("parse_relationship() precedence: * and / are left-associative", {
    # ~ a * b / c  parses as  ~ (a * b) / c  in R
    tree <- parse_relationship(~ a * b / c)
    expect_equal(tree$type, "nest")
    expect_equal(tree$outer$type, "cross")
    expect_equal(tree$inner$name, "c")
})

test_that("parse_relationship() precedence: * binds tighter than +", {
    # ~ a + b * c  should parse as  ~ a + (b * c)
    tree <- parse_relationship(~ a + b * c)
    expect_equal(tree$type, "compound")
    expect_equal(tree$children[[1]]$name, "a")
    expect_equal(tree$children[[2]]$type, "cross")
})

test_that("parse_relationship() complex election-style formula", {
    tree <- parse_relationship(
        ~ (state + election + contest) / ((party / candidate) * time * (geo / method))
    )
    expect_equal(tree$type, "nest")
    expect_equal(tree$outer$type, "compound")
    expect_length(tree$outer$children, 3) # state, election, contest
    expect_equal(tree$inner$type, "cross")
    expect_length(tree$inner$children, 3) # (party/cand), time, (geo/method)
})

test_that("parse_relationship() errors on non-formula input", {
    expect_error(parse_relationship("not a formula"), "formula")
})

test_that("parse_relationship() errors on two-sided formula", {
    expect_error(parse_relationship(y ~ x), "one-sided")
})

# .relationships in sch_schema() -----------------------------------------

test_that("sch_schema() accepts .relationships formula", {
    x <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    expect_s3_class(x, "sch_schema")
    expect_false(is.null(x$relationships))
    expect_equal(x$relationships$type, "cross")
})

test_that("sch_schema() with .relationships = NULL stores no relationships", {
    x <- sch_schema(a = sch_integer())
    expect_null(x$relationships)
})

test_that("sch_schema() errors when .relationships references non-existent column", {
    expect_error(
        sch_schema(
            .relationships = ~ a * b,
            a = sch_integer()
        ),
        "b"
    )
})

test_that("sch_schema() errors when .relationships references distinct=TRUE column", {
    expect_error(
        sch_schema(
            .relationships = ~ a * b,
            a = sch_integer(distinct = TRUE),
            b = sch_character()
        ),
        "distinct"
    )
})

test_that("sch_schema() errors when .relationships is not a formula", {
    expect_error(
        sch_schema(
            .relationships = "not a formula",
            a = sch_integer()
        ),
        "formula"
    )
})

test_that("sch_schema() warns when .relationships root is + (compound at top level)", {
    expect_warning(
        sch_schema(
            .relationships = ~ a + b,
            a = sch_integer(),
            b = sch_character()
        ),
        "Top-level"
    )
})

# format/print with .relationships ---------------------------------------

test_that("print.sch_schema() shows relationships when present", {
    x <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    output <- capture.output(print(x))
    output_text <- paste(output, collapse = "\n")
    expect_match(output_text, "Relationships|relationships")
})

test_that("format.sch_schema() includes formula columns in flat list", {
    x <- sch_schema(
        .relationships = ~ chain * draw * param,
        chain = sch_integer(),
        draw = sch_integer(),
        param = sch_factor(levels = c("mu", "sigma")),
        value = sch_numeric()
    )
    fmt <- format(x)
    # All columns should be present in the flat list
    expect_true("chain" %in% names(fmt))
    expect_true("draw" %in% names(fmt))
    expect_true("param" %in% names(fmt))
    expect_true("value" %in% names(fmt))
})
