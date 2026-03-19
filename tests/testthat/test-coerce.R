# sch_coerce() -----------------------------------------------------------

test_that("sch_coerce() returns data invisibly when types already match", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character()
    )
    df <- data.frame(x = 1.5, y = "hello")
    result <- sch_coerce(schema, df)
    expect_identical(result, df)
})

test_that("sch_coerce() errors on non-schema input", {
    expect_error(sch_coerce(list(), data.frame()), "sch_schema")
})

test_that("sch_coerce() errors on non-data-frame input", {
    schema <- sch_schema(x = sch_numeric())
    expect_error(sch_coerce(schema, list(x = 1)), "data frame")
})

# Per-type coercion ------------------------------------------------------

test_that("sch_coerce() coerces character to numeric", {
    schema <- sch_schema(x = sch_numeric())
    df <- data.frame(x = c("1.1", "2.2", "3.3"), stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_equal(result$x, c(1.1, 2.2, 3.3))
    expect_true(is.numeric(result$x))
})

test_that("sch_coerce() coerces character to integer", {
    schema <- sch_schema(x = sch_integer())
    df <- data.frame(x = c("1", "2", "3"), stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_equal(result$x, c(1L, 2L, 3L))
    expect_true(is.integer(result$x))
})

test_that("sch_coerce() coerces integer to logical", {
    schema <- sch_schema(x = sch_logical())
    df <- data.frame(x = c(0L, 1L, 1L))
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_equal(result$x, c(FALSE, TRUE, TRUE))
    expect_true(is.logical(result$x))
})

test_that("sch_coerce() coerces integer to character", {
    schema <- sch_schema(x = sch_character())
    df <- data.frame(x = 1:3)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_equal(result$x, c("1", "2", "3"))
    expect_true(is.character(result$x))
})

test_that("sch_coerce() coerces character to factor (no levels)", {
    schema <- sch_schema(x = sch_factor())
    df <- data.frame(x = c("a", "b", "a"), stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(is.factor(result$x))
    expect_setequal(levels(result$x), c("a", "b"))
})

test_that("sch_coerce() coerces character to factor with specified levels", {
    schema <- sch_schema(x = sch_factor(levels = c("a", "b", "c")))
    df <- data.frame(x = c("a", "b", "a"), stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(is.factor(result$x))
    expect_equal(levels(result$x), c("a", "b", "c"))
})

test_that("sch_coerce() coerces character to Date", {
    schema <- sch_schema(x = sch_date())
    df <- data.frame(x = c("2020-01-01", "2021-06-15"), stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(inherits(result$x, "Date"))
    expect_equal(result$x, as.Date(c("2020-01-01", "2021-06-15")))
})

test_that("sch_coerce() coerces character to POSIXct", {
    schema <- sch_schema(x = sch_datetime())
    df <- data.frame(x = c("2020-01-01 12:00:00"), stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(inherits(result$x, "POSIXct"))
})

test_that("sch_coerce() leaves sch_any() columns unchanged", {
    schema <- sch_schema(x = sch_any())
    df <- data.frame(x = c(1L, 2L, 3L))
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_identical(result$x, df$x)
})

# Structural cases -------------------------------------------------------

test_that("sch_coerce() skips absent optional columns", {
    schema <- sch_schema(
        x = sch_numeric(),
        y = sch_character(required = FALSE)
    )
    df <- data.frame(x = 1:3)
    expect_no_error(sch_coerce(schema, df, validate = FALSE))
})

test_that("sch_coerce() leaves sch_others() columns untouched", {
    schema <- sch_schema(x = sch_numeric(), sch_others())
    df <- data.frame(x = "1.5", extra = "hello", stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(is.numeric(result$x))
    expect_equal(result$extra, "hello")
})

test_that("sch_coerce() recursively coerces nested schema columns", {
    schema <- sch_schema(
        group = sch_character(),
        info = sch_nest(
            x = sch_numeric(),
            n = sch_integer()
        )
    )
    df <- data.frame(group = c("A", "B"), stringsAsFactors = FALSE)
    df$info <- list(
        data.frame(x = "1.5", n = "3", stringsAsFactors = FALSE),
        data.frame(x = "2.7", n = "1", stringsAsFactors = FALSE)
    )
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(is.numeric(result$info[[1]]$x))
    expect_true(is.integer(result$info[[1]]$n))
    expect_true(is.numeric(result$info[[2]]$x))
    expect_true(is.integer(result$info[[2]]$n))
})

test_that("sch_coerce() coerces sch_multiple() group columns", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric())
    )
    df <- data.frame(id = 1L, v1 = "1.1", v2 = "2.2", stringsAsFactors = FALSE)
    attr(df, "sch_groups") <- list(vals = c("v1", "v2"))
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_true(is.numeric(result$v1))
    expect_true(is.numeric(result$v2))
    expect_equal(result$v1, 1.1)
    expect_equal(result$v2, 2.2)
})

test_that("sch_coerce() skips sch_multiple() with missing sch_groups attribute", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_multiple(name = "vals", type = sch_numeric(), required = FALSE)
    )
    df <- data.frame(id = 1L)
    expect_no_error(sch_coerce(schema, df, validate = FALSE))
})

# validate flag ----------------------------------------------------------

test_that("sch_coerce() with validate=TRUE runs sch_validate after coercion", {
    schema <- sch_schema(x = sch_numeric(bounds = c(0, 1)))
    df <- data.frame(x = "5.0", stringsAsFactors = FALSE)
    # Coercion succeeds (character -> numeric), but value is out of bounds
    expect_error(sch_coerce(schema, df, validate = TRUE), class = "sch_validation_error")
})

test_that("sch_coerce() with validate=FALSE skips post-coercion validation", {
    schema <- sch_schema(x = sch_numeric(bounds = c(0, 1)))
    df <- data.frame(x = "5.0", stringsAsFactors = FALSE)
    result <- sch_coerce(schema, df, validate = FALSE)
    expect_equal(result$x, 5.0)
})

test_that("sch_coerce() with validate=TRUE passes when coercion fixes the type", {
    schema <- sch_schema(x = sch_integer())
    df <- data.frame(x = c("1", "2", "3"), stringsAsFactors = FALSE)
    expect_no_error(sch_coerce(schema, df, validate = TRUE))
})

# Coercion errors --------------------------------------------------------

test_that("sch_coerce() raises sch_coercion_error when coercion fails", {
    fail_type <- sch_custom(
        name = "fail",
        check = function(x, type) is.character(x),
        msg = function(type) "character",
        coerce = function(x, type) stop("deliberate coercion failure")
    )
    schema <- sch_schema(x = fail_type)
    df <- data.frame(x = 1:3)
    expect_error(sch_coerce(schema, df), class = "sch_coercion_error")
})

test_that("sch_coercion_error collects all failing columns before throwing", {
    fail_type <- sch_custom(
        name = "fail",
        check = function(x, type) is.character(x),
        msg = function(type) "character",
        coerce = function(x, type) stop("deliberate coercion failure")
    )
    schema <- sch_schema(
        x = fail_type,
        y = fail_type
    )
    df <- data.frame(x = 1:2, y = 3:4)
    err <- expect_error(sch_coerce(schema, df), class = "sch_coercion_error")
    expect_length(err$issues, 2)
})
