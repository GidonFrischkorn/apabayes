# Format convergence diagnostics

`*R̂* = 1.00, bulk ESS = 1,240, tail ESS = 980` for one parameter, or for
the extremes over a fit. Parts that are `NULL` are omitted; the order is
always R-hat, bulk ESS, tail ESS.

## Usage

``` r
apa_rhat_ess(
  rhat = NULL,
  ess_bulk = NULL,
  ess_tail = NULL,
  digits = 2,
  markup = NULL
)
```

## Arguments

- rhat, ess_bulk, ess_tail:

  Numeric vectors or `NULL`. Vectors must share one length, or have
  length 1. At least one must be given.

- digits:

  Decimals for R-hat (effective sample sizes print as integers with a
  thousands separator).

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

## Value

A character vector of the common length; an `NA` in any given part gives
`NA_character_` for that element.

## Examples

``` r
apa_rhat_ess(1.003, 1240, 980)
#> [1] "*R̂* = 1.00, bulk ESS = 1,240, tail ESS = 980"
apa_rhat_ess(1.003, digits = 3, markup = "plain")
#> [1] "Rhat = 1.003"
apa_rhat_ess(ess_bulk = c(1240, 400))
#> [1] "bulk ESS = 1,240" "bulk ESS = 400"  
```
