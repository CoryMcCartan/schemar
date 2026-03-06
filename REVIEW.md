# Code Review: schemar

All 217 existing tests pass. The issues below focus on places where
runtime behavior diverges from documented intent or from what a
reasonable user would expect.

------------------------------------------------------------------------

## Bugs

### 1. `sch_factor()` never validates factor levels (strict mode)

**Severity: High**

When `strict = TRUE` (the default), the type check is simply
`is.factor(x)`. Factor levels are never compared to the schema. A factor
with completely wrong levels, or extra levels, silently passes
validation.

``` r
schema <- sch_schema(x = sch_factor(levels = c("a", "b")))

# Wrong levels — passes:
sch_validate(schema, data.frame(x = factor(c("x","y"), levels = c("x","y"))))

# Extra levels — passes:
sch_validate(schema, data.frame(x = factor(c("a","b"), levels = c("a","b","EXTRA"))))
```

The check function (schema.R ≈ line 584) should verify that the factor’s
levels match (or are a subset of) the schema’s levels.

Ironically, the `strict = FALSE` path *does* validate values against
levels (for character vectors), making the non-strict path stricter than
strict mode in this regard.

**Files:** `R/schema.R` (type_fns$factor$check)

------------------------------------------------------------------------

### 2. Non-strict factor rejects NAs even when `missing = TRUE`

**Severity: High**

The non-strict factor check uses `all(x %in% type$levels)`. In R,
`NA %in% c("a","b")` returns `FALSE`, so any NA causes the type check to
fail — before the separate missing-value check ever runs.

``` r
schema <- sch_schema(x = sch_factor(levels = c("a","b"), strict = FALSE))
df <- data.frame(x = c("a", NA), stringsAsFactors = FALSE)
sch_validate(schema, df)
# Error: wrong type — but missing=TRUE should allow NAs
```

The fix is to add `na.rm` logic: e.g.
`all(x %in% type$levels | is.na(x))` or
`all(x[!is.na(x)] %in% type$levels)`.

**Files:** `R/schema.R` (type_fns$factor$check)

------------------------------------------------------------------------

### 3. `check = "names"` does not detect missing required columns

**Severity: High**

The documentation for the `check` parameter states:

> `"names"`: check for missing required columns and unexpected extra
> columns.

But `validate_names()` only checks for *extra* columns. Missing required
column detection is handled inside `validate_types_missing()`, which
corresponds to `check = "types"`. So `check = "names"` silently ignores
absent required columns, contradicting the documented contract.

``` r
schema <- sch_schema(x = sch_numeric(), y = sch_character())
sch_validate(schema, data.frame(x = 1), check = "names")
# Passes silently — y is required but missing
```

Either move the missing-column check into `validate_names()`, or update
the documentation. A user who runs `check = "names"` to do a quick
structural check will get a false sense of safety.

**Files:** `R/validate.R` (validate_names, validate_types_missing),
documentation in `R/validate.R` roxygen

------------------------------------------------------------------------

### 4. Custom type checks crash on NAs instead of producing a validation error

**Severity: Medium**

When a custom check function doesn’t handle NAs (which is common — the
package’s own doc example doesn’t), the
`if (!type_fns[[tt$type]]$check(x, tt))` call in
`validate_types_missing()` can produce an NA instead of TRUE/FALSE,
which causes R to throw a bare `simpleError` (“missing value where
TRUE/FALSE needed”) rather than a structured `sch_validation_error`.

``` r
even <- sch_custom(
  name = "even",
  check = function(x, type) is.integer(x) && all(x %% 2 == 0),
  msg = function(type) "vector of even integers",
  coerce = function(x, type) (as.integer(x) %/% 2) * 2
)
sch_validate(sch_schema(x = even), data.frame(x = c(2L, NA)))
# Error: missing value where TRUE/FALSE needed  (unstructured crash)
```

The validate loop should wrap the check call with
[`isTRUE()`](https://rdrr.io/r/base/Logic.html) (which it already uses
elsewhere) or `tryCatch`, so a misbehaving check produces a clean
validation issue rather than an unhandled crash. The doc example should
also be fixed to use `na.rm = TRUE`.

**Files:** `R/validate.R` (validate_types_missing, ≈ line 164),
`R/schema.R` (doc example)

------------------------------------------------------------------------

## Documentation / Behavior Mismatches

### 5. `sch_inherits` and `sch_list_of` descriptions are swapped

The roxygen `@describeIn` lines read:

- `sch_inherits`: *“A list-column whose elements satisfy
  `inherits(_, class)`.”*
- `sch_list_of`: *“A vector satisfying `inherits(_, class)`.”*

The actual check functions do the opposite:

- `sch_inherits` checks `inherits(x, type$class)` — i.e. the **whole
  column**.
- `sch_list_of` checks `is.list(x) && all(vapply(x, inherits, …))` —
  i.e. **each element**.

The descriptions should be swapped to match the implementation.

**Files:** `R/schema.R` (roxygen for sch_inherits ≈ line 437,
sch_list_of ≈ line 452)

------------------------------------------------------------------------

### 6. `sch_nest(distinct=)` is accepted but never validated

[`sch_nest()`](http://corymccartan.com/schemar/reference/sch_schema.md)
accepts a `distinct` argument and stores it as an attribute, but
`validate_distinct()` skips all nest-typed columns (line 201:
`is_nest`). The parameter is dead code. `validate_distinct()` should
actually carry out the validation for nest-typed columns.

**Files:** `R/schema.R` (sch_nest), `R/validate.R` (validate_distinct)

------------------------------------------------------------------------

## Edge Cases / Robustness

### 8. `distinct = TRUE` with multiple NAs fails unexpectedly

[`vctrs::vec_unique_count()`](https://vctrs.r-lib.org/reference/vec_unique.html)
counts all NAs as a single unique value. A column like
`c(1L, 2L, NA, NA)` has unique count 3 but size 4, so the distinct check
fails — even though `missing = TRUE` permits NAs. Users expecting
SQL-style `NULL ≠ NULL` uniqueness semantics will be surprised.

``` r
schema <- sch_schema(x = sch_integer(distinct = TRUE, missing = TRUE))
sch_validate(schema, data.frame(x = c(1L, 2L, NA, NA)))
# Error: duplicate values
```

Should filter NAs before the uniqueness check.

**Files:** `R/validate.R` (check_col_distinct)

------------------------------------------------------------------------

### 9. `sch_list_of` with NULL elements fails even when `missing = TRUE`

For list-columns, the natural “NA” is `NULL`. But `inherits(NULL, cls)`
returns `FALSE`, so a list containing NULLs always fails the type check.
The missing-value check (`anyNA`) also won’t catch NULLs in a plain
list.

``` r
schema <- sch_schema(x = sch_list_of(class = "data.frame", missing = TRUE))
df <- data.frame(dummy = 1:2)
df$x <- list(data.frame(a = 1), NULL)
sch_validate(schema, df)  # Error: wrong type
```

**Files:** `R/schema.R` (type_fns$list_{o}f$check)

------------------------------------------------------------------------

## Test Coverage Gaps

The following scenarios have no test coverage and relate to bugs above:

| Scenario                                                | Related Bug |
|---------------------------------------------------------|-------------|
| Factor with wrong levels passes strict validation       | \#1         |
| Factor with extra levels passes strict validation       | \#1         |
| Non-strict factor with NAs and `missing = TRUE`         | \#2         |
| `check = "names"` missing required column detection     | \#3         |
| Custom type check that returns NA or errors on NA input | \#4         |
| `distinct = TRUE` column with multiple NAs              | \#8         |
| `sch_list_of` column with NULL elements                 | \#9         |
| `sch_nest(distinct = TRUE)` is a no-op                  | \#6         |
| Optional named nest (not currently possible)            | \#7         |
