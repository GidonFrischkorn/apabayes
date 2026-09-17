# Format p values, probabilities of direction and proportions

Three decimals without a leading zero, floored at `< .001` and capped at
`> .999` (with `digits = 3`), so that a value which cannot be
distinguished from 0 or 1 at that precision is never printed as `.000`
or `1.000`. Exact 0 and exact 1 fall under the floor and the cap: a
probability of direction of 1 from a finite number of draws is reported
`> .999`.

## Usage

``` r
apa_p(x, digits = 3, markup = NULL, symbol = FALSE)

apa_pd(x, digits = 3, markup = NULL, symbol = FALSE, operator = FALSE)

apa_prob(x, digits = NULL, percent = FALSE, markup = NULL)
```

## Arguments

- x:

  Numeric vector in \[0, 1\]; `NA` allowed.

- digits:

  Single whole number of decimals, at least 1. For `apa_prob()` the
  default is 3 for proportions and 1 for percentages.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

- symbol:

  `TRUE` prepends the statistic symbol in the markup of the target:
  `*p* = .023`, `*pd* > .999`.

- operator:

  `apa_pd()` only: `TRUE` prepends `"= "` unless the value already opens
  with its own relation from the floor or the cap (`.956` becomes
  `= .956`; `> .999` is unchanged), so the result reads inside a
  sentence that already names the statistic (`*pd* {x}`) without the
  caller having to re-derive which branch fired. Ignored when `symbol`
  is `TRUE`, which already includes it.

- percent:

  `apa_prob()` only: print `12.3%` instead of `.123`. The floor and cap
  apply on the percentage scale (`< 0.1%`, `> 99.9%`).

## Value

A character vector of `length(x)`; `NA` in gives `NA_character_` out.

## Details

The probability of direction (pd) is the share of the posterior on the
side of the median's sign. It says how certain the sign of an effect is
and carries no information in favour of a null value; a pd of `.500`
means the sign is undetermined, not that the effect is absent (Makowski
et al., 2019). Report an interval or a ROPE share next to it when
evidence for a null is the question.

## apa7 and papaja

apa7 and papaja also export a function called `apa_p()`. Whichever
package is attached last masks the others. With more than one attached,
call `apabayes::apa_p()`. The versions do not print the same:
[`apa7::apa_p()`](https://wjschne.github.io/apa7/reference/apa_p.html)
gives `.01` where this one gives `.012`.

## References

Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D. (2019).
Indices of effect existence and significance in the Bayesian framework.
*Frontiers in Psychology, 10*, 2767.
[doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

## See also

[`apa_num()`](https://www.gfrischkorn.org/apabayes/reference/apa_num.md)
for the shared rounding,
[`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
for evidence in favour of a hypothesis.

## Examples

``` r
apa_p(c(0.0234, 0.0004, 0.9996, NA))
#> [1] ".023"   "< .001" "> .999" NA      
apa_p(0.0234, symbol = TRUE)
#> [1] "*p* = .023"
apa_pd(c(0.9874, 1), symbol = TRUE)
#> [1] "*pd* = .987" "*pd* > .999"
apa_prob(0.1234)
#> [1] ".123"
apa_prob(0.1234, percent = TRUE)
#> [1] "12.3%"
```
