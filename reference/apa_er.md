# Format evidence ratios

Evidence ratios from
[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
(`Evid.Ratio`): a Savage–Dickey density ratio for a point hypothesis,
posterior odds for a directional one. Printed as `≥ 10,000` from 10,000
up (an infinite ratio means no posterior draw contradicted the
hypothesis), rounded to an integer with a thousands separator from
1,000, to one decimal from 10, and to `digits` decimals below.

## Usage

``` r
apa_er(x, digits = 2, markup = NULL, symbol = FALSE)
```

## Arguments

- x:

  Numeric vector of evidence ratios, 0 or more; `NA` and `Inf` allowed.

- digits:

  Decimals below 10.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

- symbol:

  `TRUE` prepends `ER = ` (or `ER ≥ ` at the bound).

## Value

A character vector of `length(x)`; `NA` in gives `NA_character_` out.

## Details

Small ratios print `0.00`; apabayes keeps the ratio as brms reports it
and, for point hypotheses, reports `1 / Evid.Ratio` through
[`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
as the Bayes factor against equality.

## Examples

``` r
apa_er(c(0.5, 5.3412, 23.456, 2345.6, 12345, Inf, NA))
#> [1] "0.50"     "5.34"     "23.5"     "2,346"    "≥ 10,000" "≥ 10,000" NA        
apa_er(12345, symbol = TRUE)
#> [1] "ER ≥ 10,000"
```
