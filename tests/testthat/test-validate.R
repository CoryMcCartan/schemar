# sch_validate() ---------------------------------------------------------

test_that("sch_validate() returns data invisibly on valid input", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character()
    )
    df <- data.frame(x = 1:3, y = c("a", "b", "c"))
    result <- sch_validate(schema, df)
    expect_identical(result, df)
})

test_that("sch_validate() passes valid data with all column types", {
    schema <- sch_schema(
        a = sch_numeric(),
        b = sch_integer(),
        c = sch_logical(),
        d = sch_character(),
        e = sch_factor(levels = c("x", "y")),
        f = sch_date(),
        g = sch_datetime()
    )
    df <- data.frame(
        a = c(1.1, 2.2),
        b = c(1L, 2L),
        c = c(TRUE, FALSE),
        d = c("hello", "world"),
        e = factor(c("x", "y"), levels = c("x", "y")),
        f = as.Date(c("2020-01-01", "2020-01-02")),
        g = as.POSIXct(c("2020-01-01 12:00", "2020-01-02 12:00"))
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_validate() errors on non-schema input", {
    expect_error(sch_validate(list(), data.frame()), "sch_schema")
})

test_that("sch_validate() errors on non-data-frame input", {
    schema <- sch_schema(x = sch_numeric())
    expect_error(sch_validate(schema, list(x = 1)), "data frame")
})

# Required columns -------------------------------------------------------

test_that("missing required column raises error", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character()
    )
    df <- data.frame(x = 1:3)
    expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df), "Required.*missing")
})

test_that("missing optional column is allowed", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character(required = FALSE)
    )
    df <- data.frame(x = 1:3)
    expect_no_error(sch_validate(schema, df))
})

# Missing values ---------------------------------------------------------

test_that("NAs in column with missing=FALSE raises error", {
    schema <- sch_schema(
        x = sch_numeric(missing = FALSE)
    )
    df <- data.frame(x = c(1, NA, 3))
    expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df), "missing values")
})

test_that("NAs in column with missing=TRUE are allowed", {
    schema <- sch_schema(
        x = sch_numeric(missing = TRUE)
    )
    df <- data.frame(x = c(1, NA, 3))
    expect_no_error(sch_validate(schema, df))
})

# Type checks ------------------------------------------------------------

test_that("wrong type raises error", {
    schema <- sch_schema(x = sch_integer())
    df <- data.frame(x = c(1.5, 2.5))
    expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df), "wrong type")
})

test_that("numeric bounds violation raises error", {
    schema <- sch_schema(x = sch_numeric(bounds = c(0, 10)))
    df <- data.frame(x = c(5, 15))
    expect_error(sch_validate(schema, df), "wrong type")
})

test_that("factor column passes type check", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b")))
    df <- data.frame(x = factor(c("a", "b"), levels = c("a", "b")))
    expect_no_error(sch_validate(schema, df))
})

test_that("character for non-strict factor passes", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = FALSE))
    df <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
    expect_no_error(sch_validate(schema, df))
})

test_that("character for strict factor fails", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b"), strict = TRUE))
    df <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
    expect_error(sch_validate(schema, df), "wrong type")
})

test_that("date type check works", {
    schema <- sch_schema(x = sch_date())
    df <- data.frame(x = as.Date(c("2020-01-01", "2020-06-15")))
    expect_no_error(sch_validate(schema, df))

    df_bad <- data.frame(x = c("2020-01-01", "2020-06-15"))
    expect_error(sch_validate(schema, df_bad), "wrong type")
})

test_that("inherits type check works", {
    schema <- sch_schema(x = sch_inherits(class = "Date"))
    df <- data.frame(x = as.Date(c("2020-01-01", "2020-06-15")))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_any() accepts any type", {
    schema <- sch_schema(x = sch_any())
    # Test with numeric
    df_numeric <- data.frame(x = c(1.5, 2.5))
    expect_no_error(sch_validate(schema, df_numeric))
    # Test with character
    df_char <- data.frame(x = c("a", "b"))
    expect_no_error(sch_validate(schema, df_char))
    # Test with logical
    df_logical <- data.frame(x = c(TRUE, FALSE))
    expect_no_error(sch_validate(schema, df_logical))
    # Test with factor
    df_factor <- data.frame(x = factor(c("a", "b")))
    expect_no_error(sch_validate(schema, df_factor))
})

test_that("sch_any() with mixed types passes", {
    schema <- sch_schema(
        a = sch_numeric(),
        b = sch_any(),
        c = sch_any()
    )
    df <- data.frame(
        a = c(1.1, 2.2),
        b = c("x", "y"),
        c = c(100, 200)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_any() respects missing=FALSE", {
    schema <- sch_schema(x = sch_any(missing = FALSE))
    df <- data.frame(x = c(1, NA))
    expect_error(sch_validate(schema, df), "missing values")
})

test_that("sch_any() respects missing=TRUE", {
    schema <- sch_schema(x = sch_any(missing = TRUE))
    df <- data.frame(x = c(1, NA))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_any() respects required=FALSE", {
    schema <- sch_schema(x = sch_any(required = FALSE))
    df <- data.frame()
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_any() with distinct=TRUE detects duplicates", {
    schema <- sch_schema(x = sch_any(distinct = TRUE))
    df <- data.frame(x = c(1, 1, 2))
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("sch_any() with distinct=TRUE allows unique values", {
    schema <- sch_schema(x = sch_any(distinct = TRUE))
    df <- data.frame(x = c(1, 2, 3))
    expect_no_error(sch_validate(schema, df))
})

# Extra columns ----------------------------------------------------------

test_that("extra columns without sch_others() raises error", {
    schema <- sch_schema(x = sch_numeric())
    df <- data.frame(x = 1, y = 2)
    expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df), "Unexpected")
})

test_that("extra columns with sch_others() are allowed", {
    schema <- sch_schema(x = sch_numeric(), sch_others())
    df <- data.frame(x = 1, y = 2, z = 3)
    expect_no_error(sch_validate(schema, df))
})

# Distinct ---------------------------------------------------------------

test_that("duplicate values in distinct column raises error", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE))
    df <- data.frame(x = c(1L, 2L, 1L))
    expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("unique values in distinct column pass", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE))
    df <- data.frame(x = c(1L, 2L, 3L))
    expect_no_error(sch_validate(schema, df))
})

test_that("omitting 'distinct' from check skips distinct checks", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE))
    df <- data.frame(x = c(1L, 1L, 1L))
    expect_no_error(sch_validate(schema, df, check = c("names", "types", "relationships")))
})

# Multiple issues --------------------------------------------------------

test_that("multiple issues are collected and reported", {
    schema <- sch_schema(
        x = sch_numeric(missing = FALSE),
        y = sch_integer()
    )
    df <- data.frame(x = c(1, NA), y = c(1.5, 2.5))
    err <- expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    # Should mention both issues
    expect_match(conditionMessage(err), "missing values")
    expect_match(conditionMessage(err), "wrong type")
})

test_that("issues list is attached to the error condition", {
    schema <- sch_schema(x = sch_numeric(), y = sch_integer())
    df <- data.frame(x = "not_numeric", y = 1.5)
    err <- expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_true(!is.null(err$issues))
    expect_true(length(err$issues) >= 2)
})

# Named nest validation --------------------------------------------------

test_that("named nest validates nested data frames", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric()
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(param = c("a", "b"), value = c(1.1, 2.2)),
        data.frame(param = c("a", "b"), value = c(3.3, 4.4))
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("named nest detects non-data-frame element", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character()
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(param = "a"),
        "not_a_df"
    )
    expect_error(sch_validate(schema, df), "not a data frame")
})

test_that("named nest detects when column is not a list", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character()
        )
    )
    df <- data.frame(id = 1L, details = "not_a_list")
    expect_error(sch_validate(schema, df), "should be a list")
})

test_that("named nest detects wrong type in inner column", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            value = sch_integer()
        )
    )
    df <- data.frame(id = 1L)
    df$details <- list(data.frame(value = c(1.5, 2.5)))
    expect_error(sch_validate(schema, df), "wrong type")
})

test_that("missing required named nest column raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character()
        )
    )
    df <- data.frame(id = 1L)
    expect_error(sch_validate(schema, df), "Required.*missing")
})

test_that("required named nest column must be present", {
    # sch_nest() does not have a `required` parameter in its constructor,
    # so named nests are always required by default. This test verifies that
    # a missing required named nest raises an error.
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character()
        )
    )
    df <- data.frame(id = 1L)
    expect_error(sch_validate(schema, df), "Required.*missing")
})

# Empty data frame -------------------------------------------------------

test_that("empty data frame with all required columns passes", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character()
    )
    df <- data.frame(x = numeric(0), y = character(0))
    expect_no_error(sch_validate(schema, df))
})

test_that("empty data frame missing required column fails", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character()
    )
    df <- data.frame(x = numeric(0))
    expect_error(sch_validate(schema, df), "missing")
})

# Error class and message quality ----------------------------------------

test_that("error has class sch_validation_error", {
    schema <- sch_schema(x = sch_numeric())
    df <- data.frame(x = "bad")
    err <- expect_error(
        sch_validate(schema, df),
        class = "sch_validation_error"
    )
    expect_s3_class(err, "sch_validation_error")
})

test_that("error message mentions issue count", {
    schema <- sch_schema(
        x = sch_numeric(missing = FALSE),
        y = sch_integer()
    )
    df <- data.frame(x = c(1, NA), y = c(1.5, 2.5))
    expect_error(sch_validate(schema, df), "2 issues")
})

# Regression tests: check="names" detects missing required columns ----

test_that('check="names" detects missing required regular column', {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character()
    )
    df <- data.frame(x = 1)
    expect_error(
        sch_validate(schema, df, check = "names"),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df, check = "names"), "Required.*missing")
})

test_that('check="names" does not error on absent optional column', {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character(required = FALSE)
    )
    df <- data.frame(x = 1)
    expect_no_error(sch_validate(schema, df, check = "names"))
})

test_that('check="names" detects missing required named nest column', {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(param = sch_character())
    )
    df <- data.frame(id = 1L)
    expect_error(
        sch_validate(schema, df, check = "names"),
        class = "sch_validation_error"
    )
    expect_error(sch_validate(schema, df, check = "names"), "Required.*missing")
})

# Regression tests: custom type checks with NAs ----

test_that("NA-unaware custom check with NA input raises sch_validation_error", {
    even <- sch_custom(
        name = "even",
        check = function(x, type) is.integer(x) && all(x %% 2 == 0),
        msg = function(type) "vector of even integers",
        coerce = function(x, type) (as.integer(x) %/% 2L) * 2L
    )
    schema <- sch_schema(x = even)
    df <- data.frame(x = c(2L, NA_integer_))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("custom check that explicitly returns NA raises sch_validation_error", {
    always_na <- sch_custom(
        name = "always_na",
        check = function(x, type) NA,
        msg = function(type) "a type that always returns NA",
        coerce = function(x, type) x
    )
    schema <- sch_schema(x = always_na)
    df <- data.frame(x = 1)
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("valid custom type with no NAs passes without error", {
    even <- sch_custom(
        name = "even2",
        check = function(x, type) is.integer(x) && all(x %% 2L == 0L),
        msg = function(type) "vector of even integers",
        coerce = function(x, type) (as.integer(x) %/% 2L) * 2L
    )
    schema <- sch_schema(x = even)
    df <- data.frame(x = c(2L, 4L, 6L))
    expect_no_error(sch_validate(schema, df))
})

# Regression tests: distinct=TRUE with multiple NAs ----

test_that("distinct=TRUE, missing=TRUE allows two NAs", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE, missing = TRUE))
    df <- data.frame(x = c(1L, 2L, NA_integer_, NA_integer_))
    expect_no_error(sch_validate(schema, df))
})

test_that("distinct=TRUE, missing=FALSE raises 'missing values' for single NA", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE, missing = FALSE))
    df <- data.frame(x = c(1L, 2L, NA_integer_))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing values")
})

test_that("distinct=TRUE, missing=TRUE allows one NA among distinct values", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE, missing = TRUE))
    df <- data.frame(x = c(1L, 2L, 3L, NA_integer_))
    expect_no_error(sch_validate(schema, df))
})

test_that("distinct=TRUE, missing=TRUE detects duplicate non-NA values", {
    schema <- sch_schema(x = sch_integer(distinct = TRUE, missing = TRUE))
    df <- data.frame(x = c(1L, 1L, NA_integer_, NA_integer_))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_error(sch_validate(schema, df), "duplicate")
})

# Regression tests: named nest completeness and inner distinct ----

test_that("named nest: inner distinct=TRUE all satisfied", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(distinct = TRUE),
            value = sch_numeric()
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(param = c("a", "b"), value = c(1.1, 2.2)),
        data.frame(param = c("a", "b"), value = c(3.3, 4.4))
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("named nest: inner distinct=TRUE violation raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(distinct = TRUE),
            value = sch_numeric()
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(param = c("a", "a"), value = c(1.1, 2.2)),
        data.frame(param = c("a", "b"), value = c(3.3, 4.4))
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("named nest with single element passes", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric()
        )
    )
    df <- data.frame(id = 1L)
    df$details <- list(data.frame(param = c("a", "b"), value = c(1.1, 2.2)))
    expect_no_error(sch_validate(schema, df))
})

test_that("named nest: inner missing=FALSE rejects NA", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            value = sch_numeric(missing = FALSE)
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(value = c(1.0, 2.0)),
        data.frame(value = c(3.0, NA_real_))
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_error(sch_validate(schema, df), "missing values")
})

test_that("named nest: extra inner column without sch_others raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character()
        )
    )
    df <- data.frame(id = 1L)
    df$details <- list(data.frame(param = "a", extra_col = 99))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_error(sch_validate(schema, df), "Unexpected")
})

test_that("named nest: zero-row elements pass", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric()
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(param = character(0), value = numeric(0)),
        data.frame(param = character(0), value = numeric(0))
    )
    expect_no_error(sch_validate(schema, df))
})

# Multi-level (deep) nesting validation ----------------------------------

test_that("two-level nest: valid data passes", {
    schema <- sch_schema(
        id = sch_integer(),
        level1 = sch_nest(
            name = sch_character(),
            level2 = sch_nest(value = sch_numeric())
        )
    )
    df <- data.frame(id = 1L)
    df$level1 <- list(
        data.frame(name = "x") |>
            (\(d) {
                d$level2 <- list(data.frame(value = 1.5))
                d
            })()
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("two-level nest: type violation in inner nest raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        level1 = sch_nest(
            name = sch_character(),
            level2 = sch_nest(value = sch_numeric())
        )
    )
    df <- data.frame(id = 1L)
    df$level1 <- list(
        data.frame(name = "x") |>
            (\(d) {
                d$level2 <- list(data.frame(value = "not_numeric"))
                d
            })()
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("two-level nest: missing column in inner nest raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        level1 = sch_nest(
            name = sch_character(),
            level2 = sch_nest(value = sch_numeric())
        )
    )
    df <- data.frame(id = 1L)
    # level2 is missing the `value` column
    df$level1 <- list(
        data.frame(name = "x") |>
            (\(d) {
                d$level2 <- list(data.frame(other = 1.5))
                d
            })()
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("two-level nest: distinct violation in inner nest raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        level1 = sch_nest(
            name = sch_character(),
            level2 = sch_nest(value = sch_numeric(distinct = TRUE))
        )
    )
    df <- data.frame(id = 1L)
    df$level1 <- list(
        data.frame(name = "x") |>
            (\(d) {
                d$level2 <- list(data.frame(value = c(1.5, 1.5)))
                d
            })()
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

# sch_multiple() validation ----------------------------------------------

test_that("sch_multiple: valid data with sch_groups passes", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric(bounds = c(0, 1)))
    )
    df <- data.frame(id = 1:3, trt_a = c(0.2, 0.5, 0.8), trt_b = c(0.8, 0.5, 0.2))
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: missing sch_groups attribute raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    df <- data.frame(id = 1L, trt_a = 0.5)
    # No sch_groups attribute
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "sch_groups")
})

test_that("sch_multiple: missing group name in sch_groups raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    df <- data.frame(id = 1L, trt_a = 0.5)
    attr(df, "sch_groups") <- list(other_group = "trt_a")
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "trt")
})

test_that("sch_multiple: empty group with required=TRUE raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric(), required = TRUE)
    )
    df <- data.frame(id = 1L)
    attr(df, "sch_groups") <- list(trt = character(0))
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("sch_multiple: empty group with required=FALSE passes", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric(), required = FALSE)
    )
    df <- data.frame(id = 1L)
    attr(df, "sch_groups") <- list(trt = character(0))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: group column missing from data raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    df <- data.frame(id = 1L)
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing")
})

test_that("sch_multiple: wrong type in group column raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    df <- data.frame(id = 1:2, trt_a = c("x", "y"), trt_b = c(0.5, 0.6))
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "wrong type")
})

test_that("sch_multiple: NA in group column with inner missing=FALSE raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric(missing = FALSE))
    )
    df <- data.frame(id = 1:2, trt_a = c(0.5, NA), trt_b = c(0.5, 0.5))
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing values")
})

test_that("sch_multiple: cross-column check passes when valid", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(
            name = "trt",
            type = sch_numeric(bounds = c(0, 1)),
            check = function(x, type) {
                row_sums <- Reduce("+", x)
                all(abs(row_sums - 1) < 1e-9)
            },
            msg = function(type) "set of values summing to 1",
            coerce = function(x, type) x
        )
    )
    df <- data.frame(id = 1:2, trt_a = c(0.3, 0.6), trt_b = c(0.7, 0.4))
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: cross-column check failure raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(
            name = "trt",
            type = sch_numeric(bounds = c(0, 1)),
            check = function(x, type) {
                row_sums <- Reduce("+", x)
                all(abs(row_sums - 1) < 1e-9)
            },
            msg = function(type) "set of values summing to 1",
            coerce = function(x, type) x
        )
    )
    df <- data.frame(id = 1:2, trt_a = c(0.3, 0.6), trt_b = c(0.3, 0.4))
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "trt|cross")
})

test_that("sch_multiple: group columns not flagged as extra", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    df <- data.frame(id = 1L, trt_a = 0.5, trt_b = 0.5)
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: multiple groups in one schema all validated", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric(bounds = c(0, 1))),
        sch_multiple(name = "lab", type = sch_character())
    )
    df <- data.frame(
        id = 1:2,
        trt_a = c(0.4, 0.6),
        trt_b = c(0.6, 0.4),
        lab_x = c("a", "b"),
        lab_y = c("c", "d")
    )
    attr(df, "sch_groups") <- list(trt = c("trt_a", "trt_b"), lab = c("lab_x", "lab_y"))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: check='names' detects missing sch_groups", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "trt", type = sch_numeric())
    )
    df <- data.frame(id = 1L)
    expect_error(
        sch_validate(schema, df, check = "names"),
        class = "sch_validation_error"
    )
})

# .relationships validation: uniqueness ----------------------------------

test_that("relationships: fully crossed data passes uniqueness check", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L),
        b = c("x", "y", "x", "y"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: duplicate primary key raises error", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L),
        b = c("x", "x", "x", "y"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

# .relationships validation: crossing ------------------------------------

test_that("relationships: incomplete crossing raises error", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    # a has {1,2}, b has {"x","y"}, but missing (2,"y")
    df <- data.frame(
        a = c(1L, 1L, 2L),
        b = c("x", "y", "x"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_error(sch_validate(schema, df), "combinations|crossing|incomplete")
})

test_that("relationships: three-way crossing complete data passes", {
    schema <- sch_schema(
        .relationships = ~ a * b * c,
        a = sch_integer(),
        b = sch_character(),
        c = sch_factor(levels = c("p", "q")),
        value = sch_numeric()
    )
    df <- expand.grid(
        a = 1:2,
        b = c("x", "y"),
        c = factor(c("p", "q"), levels = c("p", "q")),
        stringsAsFactors = FALSE
    )
    df$value <- seq_len(nrow(df))
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: three-way crossing missing combo raises error", {
    schema <- sch_schema(
        .relationships = ~ a * b * c,
        a = sch_integer(),
        b = sch_character(),
        c = sch_factor(levels = c("p", "q")),
        value = sch_numeric()
    )
    df <- expand.grid(
        a = 1:2,
        b = c("x", "y"),
        c = factor(c("p", "q"), levels = c("p", "q")),
        stringsAsFactors = FALSE
    )
    df$value <- seq_len(nrow(df))
    # Remove last row
    df <- df[-nrow(df), ]
    expect_error(sch_validate(schema, df), "combinations|crossing|incomplete")
})

test_that("relationships: intermediate-level crossing is enforced", {
    schema <- sch_schema(
        .relationships = ~ a / (b * c) / (d * e),
        a = sch_integer(),
        b = sch_integer(),
        c = sch_integer(),
        d = sch_integer(),
        e = sch_integer(),
    )
    df <- expand.grid(a = 1:2, b = 1:4, c = 1:4) |>
        subset(b %% 2 == a %% 2 & c %% 2 == a %% 2)
    df <- rbind(cbind(df, d = 1L, e = 1L), cbind(df, d = 1L, e = 2L))
    df <- df[do.call(order, df), ]

    expect_no_error(sch_validate(schema, df))
    # missing b=1, c=1 for a=1
    expect_error(sch_validate(schema, df[-1, ]), "combinations|crossing|incomplete")
    expect_error(sch_validate(schema, df[-1:-2, ]), "combinations|crossing|incomplete")
})

# .relationships validation: nesting ------------------------------------

test_that("relationships: nesting with different inner values per group passes", {
    schema <- sch_schema(
        .relationships = ~ a / b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    # a=1 has b={"x","y"}, a=2 has b={"z"} — nesting allows different b per a
    df <- data.frame(
        a = c(1L, 1L, 2L),
        b = c("x", "y", "z"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: nesting still enforces (a,b) uniqueness", {
    schema <- sch_schema(
        .relationships = ~ a / b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    # Duplicate (1, "x")
    df <- data.frame(
        a = c(1L, 1L, 2L),
        b = c("x", "x", "z"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

# .relationships validation: compound keys --------------------------------

test_that("relationships: compound key with crossing passes", {
    schema <- sch_schema(
        .relationships = ~ (a + b) * c,
        a = sch_integer(),
        b = sch_character(),
        c = sch_factor(levels = c("p", "q")),
        value = sch_numeric()
    )
    # (a,b) combos: (1,"x"), (2,"y"); c: {"p","q"}
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L),
        b = c("x", "x", "y", "y"),
        c = factor(c("p", "q", "p", "q"), levels = c("p", "q")),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: compound key missing crossing combo raises error", {
    schema <- sch_schema(
        .relationships = ~ (a + b) * c,
        a = sch_integer(),
        b = sch_character(),
        c = sch_factor(levels = c("p", "q")),
        value = sch_numeric()
    )
    # Only (1,"x","p") and (2,"y","q") — missing combos
    df <- data.frame(
        a = c(1L, 2L),
        b = c("x", "y"),
        c = factor(c("p", "q"), levels = c("p", "q")),
        value = c(1.0, 2.0)
    )
    expect_error(sch_validate(schema, df), "combinations|crossing|incomplete")
})

# .relationships validation: hierarchical --------------------------------

test_that("relationships: hierarchical a / (b * c) validates completeness within groups", {
    schema <- sch_schema(
        .relationships = ~ a / (b * c),
        a = sch_integer(),
        b = sch_character(),
        c = sch_factor(levels = c("p", "q")),
        value = sch_numeric()
    )
    # a=1: b x c fully crossed; a=2: b x c fully crossed (can differ)
    df <- data.frame(
        a = c(1L, 1L, 1L, 1L, 2L, 2L),
        b = c("x", "x", "y", "y", "z", "z"),
        c = factor(c("p", "q", "p", "q", "p", "q"), levels = c("p", "q")),
        value = 1:6
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: hierarchical a / (b * c) detects incomplete crossing within group", {
    schema <- sch_schema(
        .relationships = ~ a / (b * c),
        a = sch_integer(),
        b = sch_character(),
        c = sch_factor(levels = c("p", "q")),
        value = sch_numeric()
    )
    # a=1: b={"x","y"}, c={"p","q"}, but missing (y,q)
    df <- data.frame(
        a = c(1L, 1L, 1L, 2L, 2L),
        b = c("x", "x", "y", "z", "z"),
        c = factor(c("p", "q", "p", "p", "q"), levels = c("p", "q")),
        value = 1:5
    )
    expect_error(sch_validate(schema, df), "combinations|crossing|incomplete")
})

# .relationships validation: complex election-style ----------------------

test_that("relationships: complex nested/crossed formula validates", {
    schema <- sch_schema(
        .relationships = ~ (state + contest) / (party * time),
        state = sch_character(),
        contest = sch_character(),
        party = sch_character(),
        time = sch_integer(),
        value = sch_numeric()
    )
    # 2 races: (CA, Gov), (TX, Sen); party: {D, R}; time: {1, 2}
    df <- expand.grid(
        state = c("CA", "TX"),
        contest = c("Gov", "Sen"),
        party = c("D", "R"),
        time = 1:2,
        stringsAsFactors = FALSE
    )
    # Filter to actual race combos
    df <- df[
        (df$state == "CA" & df$contest == "Gov") |
            (df$state == "TX" & df$contest == "Sen"),
    ]
    df$value <- seq_len(nrow(df))
    expect_no_error(sch_validate(schema, df))
})

# .relationships validation: crossing soundness --------------------------
#
# The crossing check computes: actual = nrow(unique(all cols)),
# expected = prod(nrow(unique(child_cols)) for each child).
# These are equal if and only if the data IS the full Cartesian product,
# by a pigeonhole argument: each child-key can appear with at most
# prod(other children unique counts) combinations. So if actual ==
# expected, every combination must be present.

test_that("relationships: 3-way cross with right marginals but latin-square combos raises error", {
    # a={1,2}, b={x,y}, c={p,q}: unique per column = 2 each, expected 2*2*2=8
    # Latin-square arrangement has only 4 unique (a,b,c) triples -- caught
    schema <- sch_schema(
        .relationships = ~ a * b * c,
        a = sch_integer(),
        b = sch_character(),
        c = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L),
        b = c("x", "y", "x", "y"),
        c = c("p", "q", "q", "p"), # Latin square: each row unique, but 4 != 8
        value = 1:4
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("relationships: compound child aliased to single partner raises error", {
    # (a+b) has 3 unique pairs; c has 3 unique values: expected 9
    # But each (a,b) pair only ever appears with ONE c value -> only 3 unique tuples
    schema <- sch_schema(
        .relationships = ~ (a + b) * c,
        a = sch_integer(),
        b = sch_character(),
        c = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        a = c(1L, 2L, 3L),
        b = c("x", "y", "z"),
        c = c("p", "q", "r"),
        value = 1:3
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

test_that("relationships: crossing complete within each group passes", {
    schema <- sch_schema(
        .relationships = ~ g / ((a + b) * c),
        g = sch_integer(),
        a = sch_integer(),
        b = sch_character(),
        c = sch_character(),
        value = sch_numeric()
    )
    # Within each g: (a,b) pairs {(1,"x"),(2,"y")} fully crossed with c {p,q}
    df <- data.frame(
        g = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
        a = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
        b = c("x", "x", "y", "y", "x", "x", "y", "y"),
        c = c("p", "q", "p", "q", "p", "q", "p", "q"),
        value = 1:8
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: crossing incomplete within a group raises error", {
    # Same schema as above; within g=1: (a,b) pairs aliased to c values (not fully crossed)
    schema <- sch_schema(
        .relationships = ~ g / ((a + b) * c),
        g = sch_integer(),
        a = sch_integer(),
        b = sch_character(),
        c = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        g = c(1L, 1L, 2L, 2L),
        a = c(1L, 2L, 1L, 2L),
        b = c("x", "y", "x", "y"),
        c = c("p", "q", "p", "q"), # each (a,b) paired with only one c per group
        value = 1:4
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

# .relationships: new semantics (bottom-up within-group uniqueness) ------
#
# Intended semantics:
#   - `+` (compound): the tuple of columns must be unique within the current
#     group context (globally at the top level; per-group when nested inside /).
#   - `/` (nesting): the inner term is evaluated within each group defined by
#     the outer term.  The inner term itself must be unique within each group.
#   - `*` (crossing): complete Cartesian product within current group context
#     (unchanged).
#
# Key implication: "nesting" allows the same inner value to appear in multiple
# outer groups — that is NOT a violation.  A violation is a duplicate inner
# value *within the same* outer group.
#
# Note: once implemented, the following existing tests will need updating:
#   - "relationships: duplicate primary key raises error" (line ~849): a
#     duplicate row with ~ a * b currently raises "duplicate" (global key
#     check).  Under new semantics there is no global key check; the error
#     becomes "incomplete_crossing" instead.  The regex match on "duplicate"
#     should be relaxed or the description updated.
#   - test-schema.R "sch_schema() warns when .relationships root is + (compound
#     at top level)": the warning should be removed now that top-level + is
#     meaningful.

# compound (+) at top level ----

test_that("relationships: ~ a + b enforces (a,b) combination uniqueness", {
    schema <- sch_schema(
        .relationships = ~ a + b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    # (1,"x"), (1,"y"), (2,"x") — all (a,b) pairs distinct
    df <- data.frame(
        a = c(1L, 1L, 2L),
        b = c("x", "y", "x"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: ~ a + b catches duplicate (a,b) pair", {
    schema <- sch_schema(
        .relationships = ~ a + b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    # (1,"x") appears twice
    df <- data.frame(
        a = c(1L, 1L, 2L),
        b = c("x", "x", "y"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

# nesting (/) bottom-level: simple case ----

test_that("relationships: ~ a / b allows same b value across different a groups", {
    # This is the core nesting semantic: b="x" in both a=1 and a=2 is fine —
    # each a group has unique b values internally.
    schema <- sch_schema(
        .relationships = ~ a / b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L),
        b = c("x", "y", "x", "z"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: ~ a / b catches duplicate b within an a group", {
    schema <- sch_schema(
        .relationships = ~ a / b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    # b="x" appears twice within a=1
    df <- data.frame(
        a = c(1L, 1L, 2L),
        b = c("x", "x", "z"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

# nesting (/) bottom-level: compound inner ----

test_that("relationships: ~ foo / (bar + baz) allows same (bar,baz) across different foo groups", {
    schema <- sch_schema(
        .relationships = ~ foo / (bar + baz),
        foo = sch_integer(),
        bar = sch_character(),
        baz = sch_character(),
        value = sch_numeric()
    )
    # (bar="a", baz="x") appears in both foo=1 and foo=2 — allowed
    df <- data.frame(
        foo = c(1L, 1L, 2L, 2L),
        bar = c("a", "a", "a", "b"),
        baz = c("x", "y", "x", "x"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: ~ foo / (bar + baz) catches duplicate (bar,baz) within a foo group", {
    schema <- sch_schema(
        .relationships = ~ foo / (bar + baz),
        foo = sch_integer(),
        bar = sch_character(),
        baz = sch_character(),
        value = sch_numeric()
    )
    # (bar="a", baz="x") appears twice within foo=1
    df <- data.frame(
        foo = c(1L, 1L, 2L),
        bar = c("a", "a", "b"),
        baz = c("x", "x", "y"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

# nesting (/) bottom-level: compound outer ----

test_that("relationships: ~ (foo + bar) / baz allows same baz across different (foo,bar) groups", {
    schema <- sch_schema(
        .relationships = ~ (foo + bar) / baz,
        foo = sch_integer(),
        bar = sch_character(),
        baz = sch_character(),
        value = sch_numeric()
    )
    # baz="p" appears in (foo=1,bar="x") and (foo=1,bar="y") — allowed
    df <- data.frame(
        foo = c(1L, 1L, 2L),
        bar = c("x", "y", "x"),
        baz = c("p", "p", "q"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: ~ (foo + bar) / baz catches duplicate baz within a (foo,bar) group", {
    schema <- sch_schema(
        .relationships = ~ (foo + bar) / baz,
        foo = sch_integer(),
        bar = sch_character(),
        baz = sch_character(),
        value = sch_numeric()
    )
    # baz="p" appears twice within (foo=1, bar="x")
    df <- data.frame(
        foo = c(1L, 1L, 2L),
        bar = c("x", "x", "y"),
        baz = c("p", "p", "q"),
        value = c(1.0, 2.0, 3.0)
    )
    expect_error(sch_validate(schema, df), class = "sch_validation_error")
})

# .relationships validation: edge cases ----------------------------------

test_that("relationships: empty data frame with formula passes", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(a = integer(0), b = character(0), value = numeric(0))
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: single-row data passes all checks", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(a = 1L, b = "x", value = 1.0)
    expect_no_error(sch_validate(schema, df))
})

test_that("relationships: skipped when formula column missing from data", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(a = 1L, value = 1.0)
    # Names check catches it; relationships should not crash
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing")
})

test_that("relationships: check='relationships' can be run independently", {
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character(),
        value = sch_numeric()
    )
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L),
        b = c("x", "y", "x", "y"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_no_error(sch_validate(schema, df, check = "relationships"))
})

test_that("relationships: no formula means check='relationships' is a no-op", {
    schema <- sch_schema(
        a = sch_integer(),
        b = sch_character()
    )
    df <- data.frame(a = 1L, b = "x")
    expect_no_error(sch_validate(schema, df, check = "relationships"))
})

test_that("relationships: NAs in formula columns are treated as distinct values", {
    # NA is treated as a distinct value by vctrs::vec_unique, so a column
    # with NA can cause incomplete-crossing errors even if non-NA values are complete.
    schema <- sch_schema(
        .relationships = ~ a * b,
        a = sch_integer(),
        b = sch_character()
    )
    # Complete crossing for non-NA combos, but NA in a inflates unique count
    df <- data.frame(
        a = c(1L, 1L, 2L, 2L, NA_integer_),
        b = c("x", "y", "x", "y", "x")
    )
    # NA in `a` means 3 unique values of a (1, 2, NA) but only 5 rows,
    # not the 3*2=6 expected for a full crossing — raises an error.
    expect_error(sch_validate(schema, df), "incomplete|combinations|crossing")
})

# sch_multiple with distinct=TRUE -------------------------------------------

test_that("sch_multiple: inner distinct=TRUE detects duplicates in group column", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(distinct = TRUE))
    )
    # vals_a has duplicate 0.5
    df <- data.frame(id = 1:2, vals_a = c(0.5, 0.5), vals_b = c(0.7, 0.8))
    attr(df, "sch_groups") <- list(vals = c("vals_a", "vals_b"))
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "duplicate")
})

test_that("sch_multiple: inner distinct=TRUE passes with unique values in group columns", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(distinct = TRUE))
    )
    df <- data.frame(id = 1:3, vals_a = c(0.5, 0.7, 0.9), vals_b = c(0.3, 0.4, 0.6))
    attr(df, "sch_groups") <- list(vals = c("vals_a", "vals_b"))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: inner distinct=TRUE with NAs passes (NAs excluded from check)", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(distinct = TRUE, missing = TRUE))
    )
    df <- data.frame(id = 1:2, vals_a = c(0.5, NA), vals_b = c(0.7, NA))
    attr(df, "sch_groups") <- list(vals = c("vals_a", "vals_b"))
    expect_no_error(sch_validate(schema, df))
})

test_that("sch_multiple: inner distinct=TRUE skips check if sch_groups attribute missing", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(distinct = TRUE))
    )
    df <- data.frame(id = 1L, vals_a = 0.5, vals_b = 0.5)
    # No sch_groups attribute — the distinct check within the group should be skipped
    # (but other validations like missing sch_groups will catch the error)
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "sch_groups")
})

test_that("sch_multiple: inner distinct=TRUE skips check if group name not in sch_groups", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(distinct = TRUE))
    )
    df <- data.frame(id = 1L, vals_a = 0.5)
    attr(df, "sch_groups") <- list(other_group = "vals_a")
    # Group "vals" is not in sch_groups, so distinct check is skipped
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "vals")
})

test_that("sch_multiple: inner distinct=TRUE skips missing group columns", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(distinct = TRUE))
    )
    df <- data.frame(id = 1L, vals_a = 0.5)
    attr(df, "sch_groups") <- list(vals = c("vals_a", "vals_b"))
    # vals_b is in the group but missing from data — should skip and raise error for missing column
    err <- expect_error(sch_validate(schema, df), class = "sch_validation_error")
    expect_match(conditionMessage(err), "missing")
})

# .relationships with nested columns ------------------------------------

test_that("relationships: named sch_nest() column accepted as part of compound key", {
    schema <- sch_schema(
        .relationships = ~ (grade + teacher) / table_group,
        grade = sch_factor(strict = FALSE, levels = c("Kindergarten", "1st", "2nd")),
        teacher = sch_nest(
            first = sch_character(),
            last = sch_character()
        ),
        table_group = sch_integer()
    )

    smith_k <- data.frame(first = "Jane", last = "Smith")
    jones_1 <- data.frame(first = "Bob", last = "Jones")

    df <- data.frame(
        grade = factor(
            c("Kindergarten", "1st"),
            levels = c("Kindergarten", "1st", "2nd")
        ),
        table_group = c(1L, 2L)
    )
    df$teacher <- list(smith_k, jones_1)

    expect_no_error(sch_validate(schema, df))
})
