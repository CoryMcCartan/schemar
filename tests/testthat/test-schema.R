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

# sch_nest() -------------------------------------------------------------

test_that("sch_nest() returns sch_schema class with type schema_nest", {
    x <- sch_nest(
        param = sch_character(),
        value = sch_numeric(),
        .keys = "param"
    )
    expect_s3_class(x, "sch_schema")
    expect_s3_class(x, "sch_type")
    expect_equal(x$type, "schema_nest")
    expect_equal(x$keys, "param")
})

test_that("sch_nest() stores columns correctly", {
    x <- sch_nest(
        a = sch_integer(),
        b = sch_numeric(),
        .keys = "a"
    )
    expect_named(x$cols, c("a", "b"))
    expect_s3_class(x$cols$a, "sch_type")
    expect_s3_class(x$cols$b, "sch_type")
})

test_that("sch_nest() accepts .desc", {
    x <- sch_nest(a = sch_integer(), .keys = "a", .desc = "My group")
    expect_equal(attr(x, "desc"), "My group")
})

test_that("sch_nest() accepts multiple keys", {
    x <- sch_nest(
        a = sch_integer(),
        b = sch_character(),
        c = sch_numeric(),
        .keys = c("a", "b")
    )
    expect_equal(x$keys, c("a", "b"))
})

test_that("sch_nest() errors when .keys is not character", {
    expect_error(
        sch_nest(a = sch_integer(), .keys = 1),
        "character"
    )
})

test_that("sch_nest() errors when .keys references non-existent columns", {
    expect_error(
        sch_nest(a = sch_integer(), .keys = "b"),
        "column"
    )
})

test_that("sch_nest() allows empty .keys", {
    expect_no_error(sch_nest(a = sch_integer(), .keys = character(0)))
})

test_that("sch_nest() errors on non-sch_type columns", {
    expect_error(
        sch_nest(a = 1, .keys = "a"),
        "column type constructor"
    )
})

test_that("sch_nest() errors on unnamed columns", {
    expect_error(
        sch_nest(sch_integer(), .keys = character(0)),
        "named"
    )
})

test_that("sch_nest() errors when sch_others() is included", {
    expect_error(
        sch_nest(a = sch_integer(), sch_others(), .keys = "a"),
        "sch_others"
    )
})

test_that("sch_nest() errors on duplicate column names", {
    expect_error(
        sch_nest(a = sch_integer(), a = sch_numeric(), .keys = "a")
    )
})

# sch_nest() inside sch_schema() -----------------------------------------

test_that("named sch_nest() works in sch_schema()", {
    x <- sch_schema(
        id = sch_integer(),
        draws = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    expect_s3_class(x, "sch_schema")
    expect_equal(x$cols$draws$type, "schema_nest")
})

test_that("unnamed sch_nest() works in sch_schema() (flat)", {
    x <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    # unnamed nest should be allowed
    expect_s3_class(x, "sch_schema")
    expect_length(x$cols, 2)
    expect_equal(x$cols[[2]]$type, "schema_nest")
})

test_that("unnamed sch_nest() and sch_others() coexist in sch_schema()", {
    x <- sch_schema(
        id = sch_integer(),
        sch_nest(a = sch_numeric(), .keys = "a"),
        sch_others()
    )
    expect_length(x$cols, 3)
})

# Print/format with sch_nest() ------------------------------------------

test_that("format.sch_schema() attaches 'levels' attribute", {
    x <- sch_schema(
        age = sch_integer(),
        sch_nest(
            subject = sch_character(),
            score = sch_numeric(),
            .keys = "subject",
            .desc = "exam scores"
        )
    )
    fmt_l <- format_schema_cols(x$cols, ansi = FALSE, depth = 0L)
    fmt <- fmt_l$out
    nms <- fmt_l$nms
    lvls <- fmt_l$levels
    expect_type(lvls, "integer")
    expect_length(lvls, length(fmt))
    # age is at level 0, header at level 0, subject/score at level 1
    expect_equal(lvls[nms == "age"], 0L)
    expect_equal(lvls[grep("flat", fmt)], 0L)
    expect_equal(lvls[nms == "subject"], 1L)
    expect_equal(lvls[nms == "score"], 1L)
})


test_that("format.sch_schema() with unnamed sch_nest() shows (flat) header", {
    x <- sch_schema(
        age = sch_integer(),
        sch_nest(
            subject = sch_character(),
            score = sch_numeric(),
            .keys = "subject",
            .desc = "exam scores"
        )
    )
    fmt <- format(x)
    # Should contain the flat header
    header_line <- fmt[grep("flat", fmt)]
    expect_length(header_line, 1)
    expect_match(header_line, "exam scores")
    expect_match(header_line, "\\(flat\\)")
})

test_that("format.sch_schema() with named sch_nest() shows (nested) header", {
    x <- sch_schema(
        age = sch_integer(),
        scores = sch_nest(
            subject = sch_character(),
            score = sch_numeric(),
            .keys = "subject",
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

test_that("print.sch_schema() with sch_nest() runs without error", {
    x <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param",
            .desc = "parameters"
        )
    )
    expect_output(print(x), "schema")
})

test_that("print.sch_schema() with named sch_nest() runs without error", {
    x <- sch_schema(
        id = sch_integer(),
        draws = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param",
            .desc = "MCMC draws"
        )
    )
    expect_output(print(x), "schema")
})

# Nested nesting -------------------------------------------------------

test_that("sch_nest() allows unnamed nested sch_nest()", {
    x <- sch_nest(
        group = sch_character(),
        sch_nest(
            item = sch_numeric(),
            .keys = "item",
            .desc = "items"
        ),
        .keys = "group",
        .desc = "groups"
    )
    expect_s3_class(x, "sch_schema")
    expect_equal(x$type, "schema_nest")
    expect_length(x$cols, 2)
    expect_equal(x$cols[[2]]$type, "schema_nest")
})

test_that("sch_schema() with nested sch_nest() works", {
    x <- sch_schema(
        id = sch_integer(),
        sch_nest(
            group = sch_character(),
            sch_nest(
                item = sch_numeric(),
                value = sch_numeric(),
                .keys = "item",
                .desc = "items"
            ),
            .keys = "group",
            .desc = "groups"
        )
    )
    expect_s3_class(x, "sch_schema")
    expect_length(x$cols, 2)
})

test_that("sch_nest() .keys can only reference direct columns (not nested)", {
    expect_error(
        sch_nest(
            group = sch_character(),
            sch_nest(
                item = sch_numeric(),
                .keys = "item",
                .desc = "items"
            ),
            .keys = c("group", "item"),
            .desc = "invalid"
        ),
        "not found"
    )
})

test_that("print.sch_schema() with multiple levels of nesting works", {
    x <- sch_schema(
        id = sch_integer(),
        sch_nest(
            group = sch_character(),
            sch_nest(
                item = sch_numeric(),
                value = sch_numeric(),
                .keys = "item",
                .desc = "items"
            ),
            .keys = "group",
            .desc = "groups"
        )
    )
    expect_output(print(x), "schema")
    # Check that indentation increases
    output <- capture.output(print(x))
    # Find the item line and ensure it's more indented than group
    item_line <- grep("item", output)
    group_line <- grep("group", output)
    if (length(item_line) > 0 && length(group_line) > 0) {
        item_indent <- nchar(output[item_line[1]]) - nchar(trimws(output[item_line[1]]))
        group_indent <- nchar(output[group_line[1]]) - nchar(trimws(output[group_line[1]]))
        expect_true(item_indent > group_indent)
    }
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
    expect_false(attr(sch_custom(name = "test", check = function(x, type) TRUE, msg = function(type) "test", coerce = function(x, type) x), "distinct"))
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

test_that("sch_nest() supports distinct argument", {
    x <- sch_nest(
        id = sch_integer(),
        value = sch_numeric(),
        distinct = FALSE
    )
    expect_false(attr(x, "distinct"))

    y <- sch_nest(
        id = sch_integer(),
        value = sch_numeric(),
        distinct = TRUE
    )
    expect_true(attr(y, "distinct"))
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
