# Format numbers in APA style

Fixed decimals, an optional leading zero, a thousands separator and the
minus sign of the markup target. Every other formatter in apabayes
builds on this one.

## Usage

``` r
apa_num(x, digits = 2, leading_zero = TRUE, big_mark = TRUE, markup = NULL)
```

## Arguments

- x:

  Numeric vector. `NA`, `NaN`, `Inf` and `-Inf` are allowed.

- digits:

  Single whole number of decimals to print (fixed, never significant
  digits).

- leading_zero:

  `FALSE` drops the zero before the decimal point (`.47`, `-.47`), the
  APA rule for statistics that cannot exceed 1 in absolute value
  (correlations, standardized paths, proportions). Values of 1 or more
  keep their digits.

- big_mark:

  `TRUE` separates groups of three digits with a comma (`1,240`), the
  APA rule for numbers of 1,000 or more.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

## Value

A character vector of `length(x)` without names; `NA` in gives
`NA_character_` out.

## Details

Rounding is C `printf` rounding through
[`formatC()`](https://rdrr.io/r/base/formatc.html), the same as
[`papaja::apa_num()`](https://rdrr.io/pkg/papaja/man/apa_num.html). A
result that would read `-0.00` is printed `0.00`.

## papaja

papaja also exports a function called `apa_num()`. Whichever of the two
packages is attached last masks the other's. With both attached, call
`apabayes::apa_num()`. papaja's version prints a hyphen where this one
prints the minus sign.

## See also

[`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
[`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
[`apa_prob()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
[`apa_ci()`](https://www.gfrischkorn.org/apabayes/reference/apa_ci.md),
[`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md),
[`apa_er()`](https://www.gfrischkorn.org/apabayes/reference/apa_er.md),
[`apa_rhat_ess()`](https://www.gfrischkorn.org/apabayes/reference/apa_rhat_ess.md).

## Examples

``` r
apa_num(c(0.4712, -0.4712, 1234.5678, NA))
#> [1] "0.47"     "−0.47"    "1,234.57" NA        
apa_num(0.4712, leading_zero = FALSE)
#> [1] ".47"
apa_num(1240, digits = 0)
#> [1] "1,240"
apa_num(c(Inf, -Inf), markup = "latex")
#> [1] "$\\infty$"  "$-\\infty$"
```
