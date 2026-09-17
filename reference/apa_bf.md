# Format Bayes factors

Prints a Bayes factor as a number, in the regime that keeps it readable,
with the subscript that matches the direction of the number. No verbal
category is attached; see
[`apa_bf_label()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf_label.md)
for the opt-in helper and the caveat.

## Usage

``` r
apa_bf(
  x,
  direction = c("10", "01"),
  style = c("auto", "sci", "plain"),
  digits = 2,
  big_mark = TRUE,
  markup = NULL,
  symbol = FALSE
)
```

## Arguments

- x:

  Numeric vector of Bayes factors **as BF10**, evidence for H1 over H0,
  the scale that `bayestestR`,
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
  and `BayesFactor` return. Values of 0 or more; `NA` and `Inf` allowed.

- direction:

  `"10"` prints BF10 as given; `"01"` prints BF01 = 1 / BF10 and the
  subscript `01`.

- style:

  Regime selection; see Details.

- digits:

  Decimals in the two-decimal regime and in the mantissa under
  `style = "sci"`.

- big_mark:

  `TRUE` separates groups of three digits with a comma (`1,240`), the
  APA rule for numbers of 1,000 or more.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

- symbol:

  `TRUE` prepends `*BF*~10~ = ` (or `*BF*~01~ = `).

## Value

A character vector of `length(x)`; `NA` in gives `NA_character_` out.

## Details

With `style = "auto"`, the value after `direction` is applied prints as

- the infinity symbol when infinite, `0` when zero;

- a mantissa with `digits` decimals times a power of ten when 10,000 or
  more, or below `10^-digits` (`1.23 × 10^5^`, `4.00 × 10^−4^` at the
  default `digits = 2`), so that a small BF10 never prints as `0.00`;

- one decimal from 10 up to 10,000 (`20.9`);

- `digits` decimals otherwise (`5.34`).

`style = "sci"` prints every finite positive value as a mantissa with
`digits` decimals times a power of ten (`2.09 × 10^1^`).
`style = "plain"` never uses scientific notation: one decimal from 10
up, `digits` decimals below.

The reporting guidelines this package follows treat the Bayes factor as
a continuous measure of relative evidence and ask for the number with an
unambiguous direction (Tendeiro et al., 2024; van Doorn et al., 2021).
Report the prior, the estimation method and the posterior estimate next
to it, and do not read a value near 1 as evidence of absence.

## References

Tendeiro, J. N., Kiers, H. A. L., Hoekstra, R., Wong, T. K., & Morey, R.
D. (2024). Diagnosing the misuse of the Bayes factor in applied
research. *Advances in Methods and Practices in Psychological Science,
7*(1).
[doi:10.1177/25152459231213371](https://doi.org/10.1177/25152459231213371)

van Doorn, J., van den Bergh, D., Böhm, U., Dablander, F., Derks, K.,
Draws, T., Etz, A., Evans, N. J., Gronau, Q. F., Haaf, J. M., Hinne, M.,
Kucharský, Š., Ly, A., Marsman, M., Matzke, D., Gupta, A. R. K. N.,
Sarafoglou, A., Stefan, A., Voelkel, J. G., & Wagenmakers, E.-J. (2021).
The JASP guidelines for conducting and reporting a Bayesian analysis.
*Psychonomic Bulletin & Review, 28*(3), 813–826.
[doi:10.3758/s13423-020-01798-5](https://doi.org/10.3758/s13423-020-01798-5)

## See also

[`apa_er()`](https://www.gfrischkorn.org/apabayes/reference/apa_er.md)
for evidence ratios from
[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html),
[`apa_bf_label()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf_label.md)
for verbal categories.

## Examples

``` r
apa_bf(c(0.05, 5.3412, 20.86, 123456, Inf, NA))
#> [1] "0.05"         "5.34"         "20.9"         "1.23 × 10^5^" "∞"           
#> [6] NA            
apa_bf(20.86, direction = "01", symbol = TRUE)
#> [1] "*BF*~01~ = 0.05"
apa_bf(20.86, style = "sci")
#> [1] "2.09 × 10^1^"
apa_bf(123456, markup = "latex")
#> [1] "1.23 $\\times$ 10^5^"
```
