#' dplyr integration for sch_df
#'
#' These methods hook into dplyr's three extension generics
#' ([dplyr::dplyr_row_slice()], [dplyr::dplyr_col_modify()],
#' [dplyr::dplyr_reconstruct()]) plus the base-R [names()] replacement and
#' 1-d `[` to enforce schema constraints across dplyr operations with near-zero
#' overhead.
#'
#' Each method calls `NextMethod()` first (performing the actual dplyr
#' operation) and then re-validates the result against the schema, running only
#' the checks that can plausibly be violated by that type of operation:
#'
#' | Operation | Checks run |
#' |---|---|
#' | `dplyr_row_slice` | none by default; `"relationships"` behind a flag |
#' | `dplyr_col_modify` | `"names"`, `"types"`, `"distinct"` |
#' | `dplyr_reconstruct` | `"names"`, `"types"` |
#' | `[` (1-d column subsetting) | `"names"` |
#' | `names<-` | errors if any *schema* column name is changed |
#'
#' @section Row slicing (`arrange`, `filter`, `slice`, semi/anti joins):
#' Row operations cannot introduce new name or type violations, so no
#' validation is run by default. Relationship constraints (crossing
#' completeness, primary-key uniqueness) *can* be broken by removing rows.
#' Pass `.check_relationships = TRUE` in `...` to opt in to that check.
#'
#' @section Column modification (`mutate`):
#' A column modification can delete required columns (via `NULL` assignment),
#' assign the wrong type, or introduce duplicate values into a `distinct = TRUE`
#' column, so all three cheap checks are run.
#'
#' @section Reconstruction (joins):
#' `dplyr_reconstruct()` is called after joins. Only names and types are
#' checked: distinct and relationship checks are omitted because joins can
#' intentionally produce non-distinct rows or incomplete crossings.
#'
#' @section Column subsetting (`select`, `relocate`):
#' A 1-d `[` call selects or reorders columns. Reordering is always safe; but
#' selecting a column subset could drop required columns, so a names check is
#' run.
#'
#' @section Renaming (`rename`, `rename_with`, `select` with rename):
#' Renaming a column that belongs to the schema (either directly or as a member
#' of a [sch_multiple()] group) is never valid without also updating the schema
#' definition, so an error is raised immediately.
#'
#' @name dplyr-sch_df
NULL


# dplyr generics ----------------------------------------------------------

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.sch_df <- function(data, i, ..., .check_relationships = FALSE) {
    out <- NextMethod()
    if (.check_relationships) {
        sch_validate(
            attr(data, "sch_schema"),
            out,
            check = "relationships",
            call = rlang::caller_env()
        )
    }
    out
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.sch_df <- function(data, cols) {
    out <- NextMethod()
    sch_validate(
        attr(data, "sch_schema"),
        out,
        check = c("names", "types", "distinct"),
        call = rlang::caller_env()
    )
    out
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.sch_df <- function(data, template) {
    out <- NextMethod()
    sch_validate(
        attr(template, "sch_schema"),
        out,
        check = c("names", "types"),
        call = rlang::caller_env()
    )
    out
}


# Base-R generics ---------------------------------------------------------

#' @export
`[.sch_df` <- function(x, i, j, ..., drop = FALSE) {
    out <- NextMethod()
    # Only validate on 1-d (column) indexing. nargs() == 2 means x[i] with no j.
    if (nargs() == 2L && inherits(out, "sch_df")) {
        sch_validate(
            attr(x, "sch_schema"),
            out,
            check = "names",
            call = rlang::caller_env()
        )
    }
    out
}

#' @export
`names<-.sch_df` <- function(x, value) {
    old_names <- names(x)
    schema <- attr(x, "sch_schema")
    groups <- attr(x, "sch_groups")

    # Columns that map directly to data frame columns (exclude sch_others() and
    # sch_multiple() group-name keys, which are not actual column names).
    schema_owned <- names(schema$cols)[
        !vapply(
            schema$cols,
            function(col) col$type %in% c("other", "schema_multiple"),
            FALSE
        )
    ]
    # sch_multiple() member columns live in the sch_groups attribute.
    group_owned <- if (!is.null(groups)) unlist(groups, use.names = FALSE) else character(0)
    owned_cols <- union(schema_owned, group_owned)

    changed <- which(old_names != value)
    renamed_owned <- intersect(old_names[changed], owned_cols)

    if (length(renamed_owned) > 0L) {
        cli::cli_abort(
            "Cannot rename schema column{?s} {.field {renamed_owned}} in a {.cls sch_df}.",
            class = "sch_rename_error",
            call = rlang::caller_env()
        )
    }

    NextMethod()
}
