# Inline results

Every
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
method returns an `apabayes_results` object: the strings for the running
text next to the numbers they were made from. It has the four elements
papaja's `apa_results` has, and inherits that class, so
[`papaja::apa_table()`](https://rdrr.io/pkg/papaja/man/apa_table.html)
accepts it.

## Usage

``` r
is_apa_results(x)

# S3 method for class 'apabayes_results'
print(x, ...)

# S3 method for class 'apabayes_results'
format(x, ...)

# S3 method for class 'apabayes_results'
as.character(x, ...)

# S3 method for class 'apabayes_results'
knit_print(x, ..., inline = FALSE)
```

## Arguments

- x:

  An object.

- ...:

  Ignored, or passed on by knitr.

- inline:

  Set by knitr: `TRUE` for inline code, `FALSE` for a chunk.

## Value

`is_apa_results()` returns a logical scalar;
[`format()`](https://rdrr.io/r/base/format.html) and
[`as.character()`](https://rdrr.io/r/base/character.html) return
`full_result`; [`print()`](https://rdrr.io/r/base/print.html) returns
`x` invisibly.

## Elements

- `estimate`:

  character; the estimate with its interval, one per reported row
  (`*b* = 0.31, 95% CrI [0.12, 0.50]`), or one for the whole table when
  the string describes all of it, as
  [`apa_convergence()`](https://www.gfrischkorn.org/apabayes/reference/apa_convergence.md)'s
  does. `NA` for a table kind that has no estimate.

- `statistic`:

  character; the statistics that follow it (`*pd* > .999`), or `NA` when
  the row carries none.

- `full_result`:

  character; the two joined with a comma, which is what prints.

- `table`:

  the reported rows, as an
  [apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  table with its attributes.

- `markup`:

  the markup target the strings were written for.

[`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
on an apabayes object returns the three string elements as named lists
instead, one element per row (see
[apabayes-papaja](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md));
[`print()`](https://rdrr.io/r/base/print.html) then writes each string
after its name.

## In a document

Inline code such as `` `r apa_inline(fit, "wt")` `` prints `full_result`
through a `knit_print` method; no `$full_result` is needed.
[`print()`](https://rdrr.io/r/base/print.html),
[`format()`](https://rdrr.io/r/base/format.html) and
[`as.character()`](https://rdrr.io/r/base/character.html) return the
same string at the console.

## See also

[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md).

## Examples

``` r
t <- apabayes_tidy(
  data.frame(
    term = "b_wt", estimate = -5.34, ci_low = -6.85,
    ci_high = -3.82, pd = 0.9995, component = "conditional"
  ),
  type = "parameters", centrality = "median", ci_method = "eti",
  ci_level = 0.95
)
r <- apa_inline(t, "b_wt")
r
#> *b* = −5.34, 95% CrI [−6.85, −3.82], *pd* > .999
is_apa_results(r)
#> [1] TRUE
r$estimate
#> [1] "*b* = −5.34, 95% CrI [−6.85, −3.82]"
r$statistic
#> [1] "*pd* > .999"
```
