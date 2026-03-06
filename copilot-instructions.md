# Copilot Instructions for schemar

## Build & Test Commands

**Full test suite:**

``` r
devtools::test()
```

**Single test file or context:**

``` r
devtools::test(filter = "schema")     # test-schema.R
devtools::test(filter = "validate")   # test-validate.R
```

**Generate documentation from roxygen:**

``` r
devtools::document()
```

**Full package check (runs R CMD CHECK equivalent):**

``` r
devtools::check()
```

**Code formatting:** Uses the [air](https://github.com/posit-dev/air)
formatter configured in `air.toml` (100 char line width, 4-space
indent). Run after all edits with `air format .` from the main
directory.

## Package Architecture

**schemar** is a developer-focused package for creating structured data
frame subtypes. It has three core layers:

### 1. Schema Definition (R/schema.R)

- **`sch_schema(...)`** – Top-level function that defines a structured
  data frame schema. Takes named column specifications (type
  constructors) and an optional description.
- **Type Constructors** –
  [`sch_numeric()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_integer()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_character()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_factor()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_date()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_datetime()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_logical()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_inherits()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_list_of()`](http://corymccartan.com/schemar/reference/sch_schema.md),
  [`sch_custom()`](http://corymccartan.com/schemar/reference/sch_schema.md).
- **Nesting** –
  [`sch_nest()`](http://corymccartan.com/schemar/reference/sch_schema.md)
  defines nested column groups (stored flat or as list-columns).
  Supports `.keys` for key-value validation.
- **Other columns** –
  [`sch_others()`](http://corymccartan.com/schemar/reference/sch_schema.md)
  permits additional unlisted columns.
- **Type registry** – `type_fns` list maps type names to
  check/msg/coerce functions. Custom types are added the same way.

### 2. Validation (R/validate.R)

- **`sch_validate(schema, data, check=c("names","types","distinct","nesting"), ...)`**
  – Core validation function.
- **Check phases** (independent; each can be toggled):
  - `"names"` → `validate_names()` – missing required columns, extra
    columns, missing inner columns in nests
  - `"types"` → `validate_types_missing()` – type mismatches, NAs when
    `missing=FALSE`
  - `"distinct"` → `validate_distinct()` – duplicate values in
    `distinct=TRUE` columns (NAs excluded per semantics)
  - `"nesting"` → `validate_nests()` – nested schema structure, inner
    validation, key consistency
- **Error handling** – Collects *all* issues before raising a single
  `sch_validation_error` with attached `$issues` list.

### 3. Display & Format (R/schema.R + R/validate.R)

- **`print.sch_schema()`** – Pretty-prints schema with indentation for
  nesting levels, type descriptions, and constraint flags.
- **`format.sch_schema()`** – Returns named character vector for
  internal consumption.

## Key Conventions

### Type Definition Pattern

Every type in `type_fns` has this structure:

``` r
type_name = list(
    check = function(x, type) { ... },      # Returns TRUE/FALSE; must handle NA gracefully
    msg = function(type) { ... },           # Returns human-readable type description
    coerce = function(x, type) { ... }      # Attempts coercion (may fail)
)
```

**Important:** Check functions must wrap non-logical results with
[`isTRUE()`](https://rdrr.io/r/base/Logic.html) in validation to avoid
crashes on NA returns.

### Attributes vs. List Elements

- Type objects (S3 class `sch_type`) store:
  - **List elements** – metadata like `type`, `levels`, `bounds`,
    `closed` (accessed via `$`)
  - **Attributes** – constraints like `required`, `missing`, `distinct`,
    `desc` (accessed via [`attr()`](https://rdrr.io/r/base/attr.html))
- **Schema objects** (S3 class `sch_schema`) additionally have:
  - **List elements** – `cols` (column definitions), `keys` (for nests)

### Nested Validation Semantics

- **Flat nest** (unnamed
  [`sch_nest()`](http://corymccartan.com/schemar/reference/sch_schema.md))
  – columns are stored side-by-side in the data frame; outer columns
  collapse to unique rows when checking distinctness.
- **Named nest** – columns stored as a list-column of data frames; each
  element is validated independently.
- **Keys** (`.keys = c("col1", "col2")`) – The unique *combinations* of
  these columns must be identical across all groups defined by outer
  columns (flat nests) or across all list elements (named nests).
- **Group-by logic** – Inner `distinct=TRUE` columns are checked per
  outer group (when outer distinct/group columns exist).

### NA and NULL Semantics

- **Regular columns** – NAs are explicitly allowed/forbidden per
  `missing` attribute.
- **List columns** – `NULL` list elements are treated as “missing”
  (analogous to NA).
- **Distinct checking** – NAs/NULLs are **excluded** from uniqueness
  checks when `missing=TRUE` (no “duplicate NA” error).
- **Custom checks** – Must handle NAs gracefully; if a check returns
  `NA`, it’s treated as a type failure, not a crash.

### Column Name Requirements

- All columns must be named except
  [`sch_others()`](http://corymccartan.com/schemar/reference/sch_schema.md)
  (no name) and unnamed
  [`sch_nest()`](http://corymccartan.com/schemar/reference/sch_schema.md)
  (flat nesting).
- Nested nests (nests inside nests) are only allowed as unnamed nests
  inside a parent nest.

## Important Implementation Notes

1.  **sch_validate() modularization** – Each check phase can run
    independently (useful for performance; missing-column detection now
    happens in `validate_names()`, not `validate_types_missing()`).
2.  **Recursive nesting** – `validate_names()`,
    `validate_types_missing()`, `validate_distinct()`, and
    `validate_nests()` all accept `path` (column path for error
    reporting) and call each other recursively for nested columns.
3.  **vctrs integration** – Uses
    [`vctrs::vec_unique_count()`](https://vctrs.r-lib.org/reference/vec_unique.html),
    `vec_group_loc()`, `vec_slice()` for vectorized operations; handles
    NA semantics correctly.
4.  **Error collection** – All validators return a list of issues
    (possibly empty); issues are collected, then printed as a bullet
    list with
    [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
