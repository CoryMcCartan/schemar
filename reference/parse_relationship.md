# Parse a relationship formula into a tree

Parse a relationship formula into a tree

## Usage

``` r
parse_relationship(formula)
```

## Arguments

- formula:

  A one-sided formula (e.g., `~ a * b / c`).

## Value

A list representing the parsed tree with `$type` being one of `"var"`,
`"cross"`, `"nest"`, or `"compound"`.
