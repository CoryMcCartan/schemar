# Define a structured data type

Defines the structure of a single 'observation' for a structured data
frame. Each column has type restrictions and may be required or
optional. Schemas support nesting relationships.

## Usage

``` r
sch_schema(..., .desc = NULL)

sch_others()

sch_nest(..., .keys = character(0), .desc = NULL, distinct = FALSE)

sch_numeric(
  desc = NULL,
  bounds = c(-Inf, Inf),
  closed = c(TRUE, TRUE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_integer(
  desc = NULL,
  bounds = c(-Inf, Inf),
  closed = c(TRUE, TRUE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_logical(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE)

sch_character(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE)

sch_factor(
  desc = NULL,
  levels,
  strict = TRUE,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_date(
  desc = NULL,
  bounds = c(as.Date(-Inf), as.Date(Inf)),
  closed = c(FALSE, FALSE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_datetime(
  desc = NULL,
  bounds = c(as.POSIXct(-Inf), as.POSIXct(Inf)),
  closed = c(FALSE, FALSE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_inherits(
  desc = NULL,
  class,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_list_of(
  desc = NULL,
  class,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_custom(
  name,
  desc = NULL,
  check,
  msg,
  coerce,
  ...,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)
```

## Arguments

- ...:

  Column specifications, in the form of `col_name = col_type` pairs,
  where `col_type` is a call to a column type constructor listed here,
  such as `sch_numeric()`. Every type must be a kind of vector, i.e.,
  [`vctrs::obj_is_vector()`](https://vctrs.r-lib.org/reference/vector-checks.html)
  must return `TRUE`.

  All columns must be named, except for `sch_others()`, as described
  below, and unnamed `sch_nest()`, which describes a set of columns
  which are logically nested within the outer columns but are stored
  flat in the actual data frame. A named `sch_nest()` describes columns
  stored as a nested data frame.

  The special function `sch_others()` indicates the preferred location
  of other columns not explicitly mentioned in the schema. If no
  `sch_others()` appears, then other columns are not allowed. Trailing
  commas are permitted.

- .keys:

  A character vector selecting one or more column names from `...` that
  serve as the key columns for the nested group.

- distinct:

  If `TRUE`, the column must contain no duplicate values (after
  accounting for nesting structure). If `FALSE` (the default),
  duplicates are allowed.

- desc, .desc:

  A description of the column for consumers of the schema. The type
  contraints will be described separately and do not need to be included
  in the description. For example for "age", the descriptoin might be
  "Age of the patient in years", not "Non-negative integer representing
  the age of the patient in years".

- bounds, :

  Length-two vector `c(min, max)` specifying the allowed range of
  values.

- closed:

  Length-two logical vector specifying whether the bounds are closed
  (inclusive) or open (exclusive).

- missing:

  If `TRUE`, the column may be contain missing values. Otherwise, any
  missing values result in an error.

- required:

  If `TRUE`, the column must be present. If `FALSE`, the column is
  optional.

- levels:

  A character vector of factor levels.

- strict:

  If `TRUE`, only factors with the specified levels are accepted. If
  `FALSE`, character vectors with the specified levels are also
  accepted.

- class:

  A character vector of class names.

- name:

  A name for the custom type.

- check:

  A two-argument function that checks whether an object satisfies the
  type. The first argument is the object to check, and the second is the
  full type specification.

- msg:

  A one-argument function that generates a descriptive message about the
  type when passed the type object itself. Should not end with a period.

- coerce:

  A two-argument function that attempts to coerce an object to the type.
  The first argument is the object to coerce, and the second is the full
  type specification.

## Value

An object of class `sch_schema`,

## Functions

- `sch_others()`: A placeholder for other non-required columns in a
  schema.

- `sch_nest()`: A set of columns that are logically nested within the
  outer schema. If given a name in the outer `sch_schema()`, the columns
  are stored as a nested data frame. If unnamed, the columns are stored
  flat (adjacent to the outer columns). The unique combinations of rows
  defined by `.keys` must repeat identically across groups defined by
  the outer columns.

- `sch_numeric()`: A numeric vector that is optionally constrained to be
  within a certain range.

- `sch_integer()`: An integer vector that is optionally constrained to
  be within a certain range.

- `sch_logical()`: A logical vector.

- `sch_character()`: A character vector.

- `sch_factor()`: A factor with specified levels.

- `sch_date()`: A Date vector that is optionally constrained to be
  within a certain range.

- `sch_datetime()`: A POSIXct vector that is optionally constrained to
  be within a certain range.

- `sch_inherits()`: A list-column whose elements satisfy
  `inherits(_, class)`.

- `sch_list_of()`: A vector satisfying `inherits(_, class)`.

- `sch_custom()`: A custom type defined by user-provided check, error,
  and coercion functions. Additional Additional named values to be
  stored along with the type specification may be passed via `...` and
  will be available to the check, error, and coercion functions.

## Examples

``` r
sch_schema(
    .desc = "Student data",
    age = sch_integer("Age in years", bounds = c(0, 130)),
    birthday = sch_date("Date of birth", required = FALSE),
    height = sch_numeric(
        "Height in inches",
        bounds = c(0, 108),
        closed = c(FALSE, TRUE)
    ),
    teacher = sch_factor(levels = c("Jones", "Smith", "Hernandez")),
    enrolled = sch_logical(missing = FALSE),
    sch_others()
)
#> Student data
#> A schema with 4 required columns:
#>      age  Age in years: An integer vector with values in [0, 130].
#> birthday  Date of birth: A date vector. [Optional]
#>   height  Height in inches: A numeric vector with values in (0, 108].
#>  teacher  A factor; one of Jones, Smith, or Hernandez.
#> enrolled  A logical vector. No NAs allowed.
#>      ...  Other columns

sch_schema(
    .desc = "MCMC draws",
    draw = sch_integer(
        "Draw number",
        bounds = c(1, Inf),
        closed = c(TRUE, FALSE),
        distinct = TRUE
    ),
    sch_nest(
        param = sch_factor("Parameter name", levels = c("mu", "sigma", "log_lik")),
        value = sch_numeric("Parameter value"),
        .keys = "param"
    )
)
#> MCMC draws
#> A schema with 2 required columns:
#> draw  Draw number: An integer vector with values in [1, Inf). [Distinct]
#>       (flat):
#>       param  Parameter name: A factor; one of mu, sigma, or log_lik.
#>       value  Parameter value: A numeric vector.

sch_custom(
   name = "even",
   check = function(x, type) is.integer(x) && all(x %% 2 == 0),
   msg = function(type) "vector of even integers",
   coerce = function(x, type) (as.integer(x) %/% 2) * 2
)
#> A vector of even integers
```
