skip_if_not_installed("dplyr")
library(dplyr)

# Shared fixtures --------------------------------------------------------

make_df <- function() {
    schema <- sch_schema(
        .desc = "test",
        .relationships = ~ chain * draw,
        chain = sch_integer(),
        draw = sch_integer(),
        value = sch_numeric(missing = FALSE)
    )
    d <- data.frame(
        chain = c(1L, 1L, 2L, 2L),
        draw = c(1L, 2L, 1L, 2L),
        value = c(1.1, 2.2, 3.3, 4.4)
    )
    new_sch_df(d, schema, class = "mcmc_df")
}

make_simple_df <- function() {
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        name = sch_character(missing = FALSE)
    )
    d <- data.frame(id = 1:3L, name = c("a", "b", "c"))
    new_sch_df(d, schema)
}


# dplyr_row_slice ---------------------------------------------------------

test_that("filter() preserves class and schema", {
    df <- make_simple_df()
    out <- filter(df, id > 1L)
    expect_s3_class(out, "sch_df")
    expect_identical(attr(out, "sch_schema"), attr(df, "sch_schema"))
})

test_that("slice() preserves class and schema", {
    df <- make_simple_df()
    out <- slice(df, 1:2)
    expect_s3_class(out, "sch_df")
})

test_that("arrange() preserves class and schema", {
    df <- make_simple_df()
    out <- arrange(df, desc(id))
    expect_s3_class(out, "sch_df")
})

test_that("dplyr_row_slice does not check relationships by default", {
    # Remove a row to break the chain * draw crossing
    df <- make_df()
    expect_no_error(filter(df, !(chain == 1L & draw == 1L)))
})

test_that("dplyr_row_slice checks relationships when flag is set", {
    df <- make_df()
    expect_error(
        dplyr::dplyr_row_slice(df, c(2L, 3L, 4L), .check_relationships = TRUE),
        class = "sch_validation_error"
    )
})

test_that("dplyr_row_slice passes relationships check when satisfied", {
    df <- make_df()
    # All four rows satisfy chain * draw crossing
    expect_no_error(
        dplyr::dplyr_row_slice(df, 1:4, .check_relationships = TRUE)
    )
})


# dplyr_col_modify --------------------------------------------------------

test_that("mutate() adding a valid column succeeds when schema has sch_others()", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_others()
    )
    df <- new_sch_df(data.frame(id = 1:3L), schema)
    expect_no_error(mutate(df, new_col = id * 2L))
})

test_that("mutate() changing a column to the correct type succeeds", {
    df <- make_simple_df()
    expect_no_error(mutate(df, name = toupper(name)))
})

test_that("mutate() changing a column to wrong type errors", {
    df <- make_simple_df()
    expect_error(
        mutate(df, id = as.character(id)),
        class = "sch_validation_error"
    )
})

test_that("mutate() setting required column to NULL (deleting it) errors", {
    df <- make_simple_df()
    expect_error(
        mutate(df, id = NULL),
        class = "sch_validation_error"
    )
})

test_that("mutate() introducing NA into missing=FALSE column errors", {
    df <- make_simple_df()
    expect_error(
        mutate(df, name = if_else(id == 1L, NA_character_, name)),
        class = "sch_validation_error"
    )
})

test_that("mutate() creating duplicates in distinct=TRUE column errors", {
    df <- make_simple_df()
    expect_error(
        mutate(df, id = 1L),
        class = "sch_validation_error"
    )
})

test_that("mutate() adding extra column to strict schema errors", {
    df <- make_simple_df()
    # schema has no sch_others(), so extra columns are not allowed
    expect_error(
        mutate(df, extra = TRUE),
        class = "sch_validation_error"
    )
})


# dplyr_reconstruct -------------------------------------------------------

test_that("inner_join() with schema-preserving result keeps class", {
    df <- make_simple_df()
    lookup <- tibble(id = 1:3L, score = c(10, 20, 30))
    schema <- sch_schema(
        id = sch_integer(distinct = TRUE),
        name = sch_character(missing = FALSE),
        sch_others()
    )
    df2 <- new_sch_df(data.frame(id = 1:3L, name = c("a", "b", "c")), schema)
    expect_no_error({
        out <- inner_join(df2, lookup, by = "id")
        expect_s3_class(out, "sch_df")
    })
})

test_that("left_join() that introduces NA into missing=FALSE column errors", {
    schema <- sch_schema(
        id = sch_integer(),
        name = sch_character(missing = FALSE)
    )
    df <- new_sch_df(data.frame(id = 1:3L, name = c("a", "b", "c")), schema)
    # right-side has no match for id=3, so left_join introduces NA in `name`
    right <- data.frame(id = 1:2L, name = c("x", "y"))
    expect_error(
        left_join(df, right, by = "id", suffix = c("", ".y")),
        class = "sch_validation_error"
    )
})


# [.sch_df ----------------------------------------------------------------

test_that("selecting all columns preserves class and passes validation", {
    df <- make_simple_df()
    out <- select(df, everything())
    expect_s3_class(out, "sch_df")
})

test_that("relocate() preserves all columns and class", {
    df <- make_simple_df()
    out <- relocate(df, name)
    expect_s3_class(out, "sch_df")
    expect_identical(sort(names(out)), sort(names(df)))
})

test_that("select() removing a required column errors", {
    df <- make_simple_df()
    expect_error(
        select(df, name),
        class = "sch_validation_error"
    )
})

test_that("1-d [ preserves schema on full column selection", {
    df <- make_simple_df()
    out <- df[c("id", "name")]
    expect_s3_class(out, "sch_df")
})

test_that("2-d [ (row subsetting) does not trigger names validation", {
    df <- make_simple_df()
    # Row subsetting should not invoke sch_validate at all
    expect_no_error(df[1:2, ])
    expect_s3_class(df[1:2, ], "sch_df")
})


# names<- / rename() / rename_with() -------------------------------------

test_that("rename() of a schema column errors", {
    df <- make_simple_df()
    expect_error(rename(df, new_id = id), class = "sch_rename_error")
})

test_that("rename() error message mentions the column name", {
    df <- make_simple_df()
    expect_error(rename(df, new_id = id), regexp = "id")
})

test_that("rename() of a non-schema column in a permissive schema succeeds", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_others()
    )
    df <- new_sch_df(data.frame(id = 1:3L, aux = letters[1:3]), schema)
    expect_no_error(rename(df, helper = aux))
})

test_that("rename_with() applied to schema columns errors", {
    df <- make_simple_df()
    expect_error(
        rename_with(df, toupper),
        class = "sch_rename_error"
    )
})

test_that("rename_with() applied only to non-schema columns succeeds", {
    schema <- sch_schema(
        id = sch_integer(),
        sch_others()
    )
    df <- new_sch_df(data.frame(id = 1:3L, aux = letters[1:3]), schema)
    expect_no_error(rename_with(df, toupper, .cols = "aux"))
})

test_that("rename_with() applied to schema column subset errors", {
    df <- make_simple_df()
    expect_error(
        rename_with(df, toupper, .cols = "name"),
        class = "sch_rename_error"
    )
})
