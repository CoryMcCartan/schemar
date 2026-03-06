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
        sch_custom("numeric",
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "reserved"
    )
    expect_error(
        sch_custom("other",
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "reserved"
    )
})

test_that("sch_custom() errors when name is not a single string", {
    expect_error(
        sch_custom(c("a", "b"),
            check = function(x, type) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        )
    )
})

test_that("sch_custom() errors on wrong function signatures", {
    # check must have 2 args
    expect_error(
        sch_custom("mytype",
            check = function(x) TRUE,
            msg = function(type) "x",
            coerce = function(x, type) x
        ),
        "check"
    )
    # msg must have 1 arg
    expect_error(
        sch_custom("mytype",
            check = function(x, type) TRUE,
            msg = function(type, extra) "x",
            coerce = function(x, type) x
        ),
        "msg"
    )
    # coerce must have 2 args
    expect_error(
        sch_custom("mytype",
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
    x <- sch_custom("mytype",
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
