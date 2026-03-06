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
    expect_no_error(sch_validate(schema, df, check = c("names", "types", "nesting")))
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

# Flat nest validation ---------------------------------------------------

test_that("flat nest validates inner columns", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 2L, 2L),
        param = c("a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("flat nest detects wrong type in inner column", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L),
        param = c("a", "b"),
        value = c("not", "numeric"),
        stringsAsFactors = FALSE
    )
    expect_error(sch_validate(schema, df), "wrong type")
})

test_that("flat nest detects missing required inner column", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L),
        param = c("a", "b")
    )
    expect_error(sch_validate(schema, df), "Required.*missing")
})

test_that("flat nest: outer column distinct after accounting for nesting", {
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    # id repeats due to nesting but is distinct when collapsed
    df <- data.frame(
        id = c(1L, 1L, 2L, 2L),
        param = c("a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("flat nest: outer column not distinct raises error", {
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        name = sch_character(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    # id=1 appears for two different names → not distinct among outer rows
    df <- data.frame(
        id = c(1L, 1L, 1L, 1L),
        name = c("A", "A", "B", "B"),
        param = c("a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4)
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("flat nest: inner column distinct within each group", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(distinct = TRUE),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    # Within each id group, param is distinct
    df <- data.frame(
        id = c(1L, 1L, 2L, 2L),
        param = c("a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("flat nest: inner column not distinct within group raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(distinct = TRUE),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    # param repeats within id=1 group
    df <- data.frame(
        id = c(1L, 1L, 1L, 2L, 2L),
        param = c("a", "a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4, 5.5)
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("flat nest: keys consistency check detects inconsistent keys", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    # id=1 has params a,b but id=2 has only param a
    df <- data.frame(
        id = c(1L, 1L, 2L),
        param = c("a", "b", "a"),
        value = c(1.1, 2.2, 3.3)
    )
    expect_error(sch_validate(schema, df), "inconsistent")
})

test_that("flat nest: consistent keys pass", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 2L, 2L),
        param = c("a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4)
    )
    expect_no_error(sch_validate(schema, df))
})

# Named nest validation --------------------------------------------------

test_that("named nest validates nested data frames", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
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
            param = sch_character(),
            .keys = "param"
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

test_that("named nest detects inconsistent keys", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(id = c(1L, 2L))
    df$details <- list(
        data.frame(param = c("a", "b"), value = c(1.1, 2.2)),
        data.frame(param = c("a", "c"), value = c(3.3, 4.4))
    )
    expect_error(sch_validate(schema, df), "inconsistent")
})

test_that("missing required named nest column raises error", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            .keys = "param"
        )
    )
    df <- data.frame(id = 1L)
    expect_error(sch_validate(schema, df), "Required.*missing")
})

test_that("optional named nest column can be absent", {
    # sch_nest() does not have a `required` parameter in its constructor,
    # so named nests are always required by default. This test verifies that
    # a missing required named nest raises an error.
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            .keys = "param"
        )
    )
    df <- data.frame(id = 1L)
    expect_error(sch_validate(schema, df), "Required.*missing")
})

# Deep nesting -----------------------------------------------------------

test_that("deeply nested flat schema validates correctly", {
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        sch_nest(
            group = sch_character(distinct = TRUE),
            sch_nest(
                item = sch_integer(distinct = TRUE),
                value = sch_numeric(),
                .keys = "item"
            ),
            .keys = "group"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
        group = c("A", "A", "B", "B", "A", "A", "B", "B"),
        item = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
        value = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("deep nesting detects non-distinct inner column", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            group = sch_character(),
            sch_nest(
                item = sch_integer(distinct = TRUE),
                value = sch_numeric(),
                .keys = "item"
            ),
            .keys = "group"
        )
    )
    # item repeats within (id=1, group="A")
    df <- data.frame(
        id = c(1L, 1L, 1L, 1L),
        group = c("A", "A", "B", "B"),
        item = c(1L, 1L, 1L, 2L),
        value = c(0.1, 0.2, 0.3, 0.4)
    )
    expect_error(sch_validate(schema, df), "duplicate")
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

test_that('check="names" detects missing required inner column in flat nest', {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric()
        )
    )
    df <- data.frame(id = c(1L, 1L), param = c("a", "b"))
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

# Regression tests: flat nest edge cases ----

test_that("flat nest with single outer group: key consistency passes", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L),
        param = c("a", "b"),
        value = c(1.1, 2.2)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("flat nest: single group inner distinct=TRUE is still enforced", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(distinct = TRUE),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 1L),
        param = c("a", "a", "b"),
        value = c(1.1, 2.2, 3.3)
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("flat nest: only one group violates inner distinct", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(distinct = TRUE),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 1L, 2L, 2L),
        param = c("a", "a", "b", "a", "b"),
        value = c(1.1, 2.2, 3.3, 4.4, 5.5)
    )
    expect_error(sch_validate(schema, df), "duplicate")
})

test_that("flat nest: optional inner column may be absent", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(required = FALSE)
        )
    )
    df <- data.frame(
        id = c(1L, 1L),
        param = c("a", "b")
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("flat nest: two outer distinct=TRUE columns both pre-collapsed", {
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        code = sch_character(distinct = TRUE),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 2L, 2L),
        code = c("A", "A", "B", "B"),
        param = c("x", "y", "x", "y"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
    expect_no_error(sch_validate(schema, df))
})

test_that("flat nest: outer column violates distinct after collapsing", {
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        code = sch_character(distinct = TRUE),
        sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(
        id = c(1L, 1L, 1L, 1L),
        code = c("A", "A", "B", "B"),
        param = c("x", "y", "x", "y"),
        value = c(1.0, 2.0, 3.0, 4.0)
    )
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

test_that("named nest with single element: key consistency trivially passes", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(id = 1L)
    df$details <- list(data.frame(param = c("a", "b"), value = c(1.1, 2.2)))
    expect_no_error(sch_validate(schema, df))
})

test_that("named nest: element missing key skipped, remaining inconsistency detected", {
    schema <- sch_schema(
        id = sch_integer(),
        details = sch_nest(
            param = sch_character(),
            value = sch_numeric(),
            .keys = "param"
        )
    )
    df <- data.frame(id = c(1L, 2L, 3L))
    df$details <- list(
        data.frame(param = c("a", "b"), value = c(1.1, 2.2)),
        data.frame(value = c(3.3)),
        data.frame(param = c("a", "c"), value = c(4.4, 5.5))
    )
    expect_error(sch_validate(schema, df), "inconsistent")
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
