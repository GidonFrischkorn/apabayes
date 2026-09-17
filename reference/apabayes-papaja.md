# Results for papaja's `apa_print()`

When papaja is installed,
[`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
works on the objects apabayes reports: a brms fit, an rstanarm fit, a
lavaan or blavaan fit, a brms hypothesis test, a LOO comparison from
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
a Bayes-factor model comparison from
[`bayestestR::bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.html),
inclusion Bayes factors from
[`bayestestR::bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.html),
a table of contrasts or marginal means from
[`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
or
[`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html),
a table of Bayesian correlations from
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html),
and a stored
[apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
table. Each method calls
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
and returns its result in the shape papaja users address, so
`apa_print(fit)$full_result$wt` works as it does on an `lm`.

## Usage

``` r
# S3 method for class 'apabayes_tidy'
apa_print(x, term = NULL, ..., in_paren = FALSE)

# S3 method for class 'brmsfit'
apa_print(x, term = NULL, ..., in_paren = FALSE)

# S3 method for class 'stanreg'
apa_print(x, term = NULL, ..., in_paren = FALSE)

# S3 method for class 'lavaan'
apa_print(x, term = NULL, ..., in_paren = FALSE)

# S3 method for class 'blavaan'
apa_print(x, term = NULL, ..., in_paren = FALSE)

# S3 method for class 'brmshypothesis'
apa_print(x, term = NULL, ..., in_paren = FALSE)

apa_print.compare.loo(x, term = NULL, ..., in_paren = FALSE)

apa_print.bayesfactor_models(x, term = NULL, ..., in_paren = FALSE)

apa_print.bayesfactor_inclusion(x, term = NULL, ..., in_paren = FALSE)

apa_print.estimate_contrasts(x, term = NULL, ..., in_paren = FALSE)

apa_print.estimate_means(x, term = NULL, ..., in_paren = FALSE)

apa_print.easycorrelation(x, term = NULL, ..., in_paren = FALSE)
```

## Arguments

- x:

  A `brmsfit`, `stanreg`, `lavaan`, `blavaan`, `brmshypothesis`,
  `compare.loo`, `bayesfactor_models`, `bayesfactor_inclusion`,
  `estimate_contrasts`, `estimate_means` or `easycorrelation` object, or
  an
  [apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  table.

- term:

  The row to report, as in
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md);
  `NULL` reports every row as named lists.

- ...:

  Passed to
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md):
  `rhs`, `op`, `group` and the formatting options, and on a fitted model
  the arguments of its
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  route.

- in_paren:

  `TRUE` writes brackets for the parentheses in the strings (`χ²[24]`,
  `log[*BF*~10~]`), for a result quoted inside parentheses, as papaja's
  argument of that name does.

## Value

An
[apa_results](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
object.

## Shape

Without `term`, `estimate`, `statistic` and `full_result` are named
lists with one string per row. The names follow papaja's rule:
parentheses, backticks and commas are removed and every other character
that is not a letter, digit or underscore becomes `_`, so `(Intercept)`
is `Intercept` and `visual =~ x1` is `visual_x1`. A parameter is named
by its label, a hypothesis by its string, a fit table and a model
comparison by its model, a correlation by its pair (`mpg~~wt` is
`mpg_wt`). A name that would repeat an earlier one gets `_2`, `_3`, …,
skipping any name another row has; a row with no usable name is `row`
and its position. With `term`, the three elements are plain strings and
the result is exactly `apa_inline(x, term, ...)`.

The strings use apabayes' markup (see
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)'s
`markup`), not the LaTeX math papaja writes, and print with a
`knit_print` method, so inline code needs no `$full_result` for a single
result.

papaja owns `apa_print()` methods for `emmGrid` and `BFBayesFactor`
objects, and apabayes never replaces them; the Bayes-factor table
apabayes reads from a `BFBayesFactor` object is reported with
`apa_print(apa_tidy(bf))`. papaja's `emmGrid` method reports a
frequentist grid; on a grid from a Bayesian fit it prints the estimate
alone. Report such a grid through its tidy table,
`apa_print(apa_tidy(grid))`, which names the rows as papaja does
(`$full_result$cyl_f4_cyl_f6`).

## See also

[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md),
which these methods call.

## Examples

``` r
model <- "visual =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6"
fit <- lavaan::cfa(model, lavaan::HolzingerSwineford1939)
r <- papaja::apa_print(fit, standardize = TRUE)
r$full_result$visual_x2
#> [1] ".43, 95% CI [.31, .55], *p* < .001"
papaja::apa_print(apa_tidy_sem_fit(fit), in_paren = TRUE)
#> row1: χ²[8] = 24.36, *p* = .002, CFI = .975, TLI = .953, RMSEA = .082, 90% CI [.046, .121], SRMR = .047
```
