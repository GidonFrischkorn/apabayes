# Read one value out of a tidy table

`apa_value()` addresses a row exactly as
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
does and returns the value in one of its columns: the number itself, not
the string. It is the function for a value that goes into arithmetic,
into a comparison, or into a sentence whose author formats it themselves
— the cases a hand-rolled lookup helper is otherwise written for, and
those helpers index by position and report the wrong row the day the
table changes.

## Usage

``` r
apa_value(x, ...)

# S3 method for class 'apabayes_tidy'
apa_value(
  x,
  term = NULL,
  rhs = NULL,
  op = NULL,
  group = NULL,
  ...,
  column = NULL
)

# Default S3 method
apa_value(
  x,
  term = NULL,
  rhs = NULL,
  op = NULL,
  group = NULL,
  ...,
  column = NULL
)
```

## Arguments

- x:

  An
  [apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  table, or an object
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  accepts.

- ...:

  Tidy method: must be empty. Default method: passed to
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md).

- term:

  The row: a term, a label, a hypothesis string, a model, the left-hand
  side of a structural-equation path, or one variable of a correlation.
  `NULL` for all rows.

- rhs:

  The right-hand side of a structural-equation path, or the other
  variable of a correlation.

- op:

  The operator of a structural-equation path as lavaan writes it:
  `"=~"`, `"~~"`, `"~"`, `"~1"` or `":="`. `NULL` matches any; a
  correlation's is `"~~"`.

- group:

  A value of the `group` column to restrict the search to.

- column:

  The column to read, as a single string; any column of the table,
  contract or not. `NULL` reads the column the table's type is about —
  the value its contract requires beside the columns that name a row:
  `estimate` on a parameters, hypotheses, contrasts or correlations
  table, `elpd_diff` on a `loo` table, `bf` on a `bf_models` or
  `bf_inclusion` one. A `sem_fit` table carries several indices and a
  `diagnostics` table three statistics, so neither has a default and
  `column` must name one.

## Value

The selected values of `column`, unnamed, in the table's own row order.

## Addressing a row

`term`, `rhs`, `op` and `group` are
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)'s,
with
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)'s
meaning and
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)'s
refusals: a term, a label, a term with the brms class prefix removed, a
hypothesis string, a model, the two sides of a structural-equation path,
a correlation's pair in either order. No match, or more than one, is an
error that lists the candidates. `term = NULL` selects every row and
returns the whole column.

## What comes back

The column as it is stored: a double from `estimate`, `ci_low` or `pd`,
a character from `term` or `ci_method`, a logical from `std`. Nothing is
rounded, formatted or marked up, and the value carries no names. In
prose, format it — `apa_num(apa_value(t, "wt"))` — or use
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md),
which writes the whole string. In arithmetic, use it as it is.

A column that is `NA` on every addressed row is an error rather than a
missing value: a value queried for a manuscript is one the table is
expected to hold, and `NA` reaching the prose is the defect this refusal
exists to stop. A column missing on some rows and present on others
comes back as it is.

## See also

[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
for the formatted string,
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
for the tables.

## Examples

``` r
t <- apabayes_tidy(
  data.frame(
    term = c("b_Intercept", "b_wt"), label = c("(Intercept)", "wt"),
    estimate = c(37.3, -5.34), ci_low = c(31.2, -6.85),
    ci_high = c(43.1, -3.82), pd = c(1, 0.9995),
    component = "conditional"
  ),
  type = "parameters", centrality = "median", ci_method = "eti",
  ci_level = 0.95
)
apa_value(t, "wt")
#> [1] -5.34
apa_value(t, "wt", column = "ci_low")
#> [1] -6.85
apa_value(t, column = "term")
#> [1] "b_Intercept" "b_wt"       
# the number behind the string
apa_num(apa_value(t, "wt") * 2)
#> [1] "−10.68"
fit <- lavaan::cfa("visual =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
apa_value(fit, "visual", "x2", op = "=~", standardize = TRUE)
#> [1] 0.4788703
```
