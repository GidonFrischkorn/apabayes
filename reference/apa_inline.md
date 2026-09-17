# Report a result inline, in APA style

`apa_inline()` turns one row of a tidy table (or a fitted model) into
the string a Results section quotes: the estimate with its interval,
followed by the statistics the row carries. A row is addressed by name,
never by position.

## Usage

``` r
apa_inline(x, ...)

# S3 method for class 'apabayes_tidy'
apa_inline(
  x,
  term = NULL,
  rhs = NULL,
  op = NULL,
  group = NULL,
  ...,
  symbol = NULL,
  stats = NULL,
  interval = TRUE,
  ci_label = "auto",
  digits = NULL,
  digits_prob = 3,
  leading_zero = "auto",
  bf = c("auto", "sci", "plain"),
  bf_direction = c("10", "01"),
  markup = NULL
)

# Default S3 method
apa_inline(
  x,
  term = NULL,
  rhs = NULL,
  op = NULL,
  group = NULL,
  ...,
  symbol = NULL,
  stats = NULL,
  interval = TRUE,
  ci_label = "auto",
  digits = NULL,
  digits_prob = 3,
  leading_zero = "auto",
  bf = c("auto", "sci", "plain"),
  bf_direction = c("10", "01"),
  markup = NULL
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

- symbol:

  `NULL` for the default per row (`b` for a regression coefficient, `r`
  for a correlation, none elsewhere), a string printed in italics before
  the estimate (`"b"`, `"β"`, `"r"`), or `FALSE` for none.

- stats:

  `NULL` for the default, or a character vector naming a subset of what
  the row can print: `"pd"`, `"rope"`, `"bf"`, `"p"` for a parameters
  row (the default prints every one the row carries); `"bf"`, `"er"`,
  `"post_prob"` for a hypotheses row (the default prints the Bayes
  factor alone); the index names above for a sem_fit row; `"elpd_diff"`,
  `"elpd"`, `"p_loo"`, `"looic"`, `"weight"` for a loo row (default
  `"elpd_diff"`); `"bf"`, `"error"`, `"log_bf"`, `"post_prob"` for a
  bf_models row (default `"bf"`); `"bf"`, `"p_prior"`, `"p_posterior"`
  for a bf_inclusion row (default `"bf"`); `"pd"`, `"rope"` for a
  contrasts row (the default prints both when the row carries them);
  `"pd"`, `"rope"`, `"bf"`, `"n"` for a correlations row (default
  `c("pd", "bf")`).
  [`character()`](https://rdrr.io/r/base/character.html) prints the
  estimate alone.

- interval:

  `FALSE` drops the interval.

- ci_label:

  `"auto"` labels the interval from the row's `ci_method`; a string
  overrides it; `NULL` keeps the brackets and drops the label *and the
  comma that introduced it*, so the estimate reads
  `−5.39 [−6.95, −3.78]`. A row whose `ci_level` is `NA` prints the
  brackets without a label whatever `ci_label` says, because a label
  without its level would claim more than the table records — but it
  keeps the comma, because that is the table's silence and not something
  the caller asked for.

- digits:

  Decimals for estimates and interval bounds; `NULL` is 2, and on a
  sem_fit row 3 for the indices and 2 for χ².

- digits_prob:

  Decimals for pd, p and the ROPE share.

- leading_zero:

  `"auto"` drops the leading zero on standardized rows, correlations and
  fit indices and keeps it elsewhere; `TRUE` or `FALSE` force one rule.

- bf, bf_direction:

  Passed to
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  as `style` and `direction`.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

## Value

An
[apa_results](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
object with one string per reported row.

## Addressing a row

`term` matches, in this order, the `term` column, the `label` column,
and a term with the brms class prefix removed (`"wt"` finds `b_wt`). For
a hypotheses table it matches `hypothesis`. A structural-equation path
is addressed by its two sides:
`apa_inline(x, "visual", "x1", op = "=~")`; the order of the sides does
not matter for a covariance (`~~`) and does for every other operator.
`op` alone finds an intercept (`op = "~1"`). `group` restricts the
search first, for a multi-group fit or a grouped hypothesis.
`term = NULL` reports every row. No match, or more than one, is an error
that lists the candidates.

## What is printed

A `parameters` row prints the estimate, its interval labelled by the
row's own `ci_method` (`CrI` for an equal-tailed credible interval,
`HDI`, `CI` for a Wald or bootstrap interval), then whichever of the
probability of direction, the ROPE share, the Bayes factor and the p
value the row carries. A population-level regression coefficient
(`component` `"conditional"`, not a random-effect term) is prefixed
`*b*`; other rows carry no symbol unless `symbol` gives one. A
standardized row (`std`) drops the leading zero. A `hypotheses` row
prints the estimate, the interval at that row's level and the Bayes
factor; `stats` can add the evidence ratio and the posterior
probability. A `diagnostics` row prints R-hat and both effective sample
sizes.

A `sem_fit` row from
[`apa_tidy_sem_fit()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
prints the indices it carries, in a fixed order: for a lavaan fit
`χ²(df) = …, *p* …, CFI, TLI, RMSEA with its confidence interval, SRMR`;
for a blavaan fit `PPP, BRMSEA and BΓ̂` with their credible intervals,
labelled by the table's `ci_method`. The indices print with three
decimals and no leading zero and χ² with two, unless `digits` sets both;
`stats` selects among `"chisq"` (with its *p*), `"cfi"`, `"tli"`,
`"rmsea"`, `"srmr"`, `"ppp"`, `"brmsea"` and `"bgammahat"`. The default
method extracts with
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md),
which is the parameter table, so fit indices are reported from
`apa_tidy_sem_fit(fit)`.

A `loo` row from a
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
table prints the difference in expected log predictive density to the
best model with its standard error, `ΔELPD = −0.97, *SE* = 0.35`, on
every row, the best model's `0.00` included; `stats` adds `"elpd"` (the
model's own ELPD and SE), `"p_loo"`, `"looic"` and `"weight"` (`*w*`,
when the table was extracted with `weights =`; which kind of weight it
is lives in the table's `weight_method` attribute, not in the string). A
`bf_models` row prints its Bayes factor against the table's denominator
model, `*BF*~10~ = 6.38` (the denominator itself prints `1.00`); `stats`
adds `"log_bf"` and `"post_prob"`, the posterior model probability under
equal prior odds, `*P*(M | D)`, and `"error"`, the numerical error of a
BayesFactor Bayes factor as a percentage after the Bayes factor it
qualifies, `*BF*~10~ = 4.5 × 10^6^ ± 1.3%` (`± 0%` for an exact one,
nothing where none is recorded); `"error"` needs `"bf"`. A Bayes factor
too large or too small to exponentiate (a log Bayes factor beyond about
±709) prints as its log instead, never as `∞` or `0`. Such a row is
addressed by `model`, and a Bayes-factor row also by the `name` it was
passed as. LOO and Bayes-factor comparisons answer different questions
and are never merged into one string.

A `bf_inclusion` row from
[`bayestestR::bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.html)
prints its inclusion Bayes factor, `*BF*~incl~ = 1.9 × 10^4^`, or under
`bf_direction = "01"` the exclusion Bayes factor `*BF*~excl~`; `stats`
adds the prior and posterior inclusion probabilities, `*P*(incl) = .50`
and `*P*(incl | D) = .55`. A term in every model has no inclusion Bayes
factor, and a posterior inclusion probability rounded to 1 or 0 an
infinite one; printing the Bayes factor of such a row is an error that
says which. A row is addressed by its `term`.

A `contrasts` row from an emmeans grid prints like a parameters row: the
estimate, its interval labelled from the row's `ci_method`
(`95% HDI [1.38, 7.01]` on the emmGrid route's default, `CrI` under
`ci = "eti"`), then `*pd*` and the ROPE share when the table carries
one. No symbol is printed unless `symbol` gives one: a contrast has no
coefficient rule to apply. A row is addressed by its `contrast` string
(`"cyl_f4 - cyl_f6"`, or emmeans's label of a marginal mean,
`"cyl_f4"`); a contrast computed within `by` groups repeats its string
across them and is addressed with `group`.

A `correlations` row prints
`*r* = −.82, 95% HDI [−.92, −.66], *pd* > .999, *BF*~10~ = 1.3 × 10^7^`:
the correlation without its leading zero, then pd and the Bayes factor;
`stats` adds the ROPE share and `"n"`, `*n* = 32`. A row is addressed by
its pair, `apa_inline(x, "mpg", "wt")`, in either order, or by its term
`"mpg~~wt"`; a single variable finds the row it appears in when there is
one. A correlation computed within groups is addressed with `group`.

Every number goes through the format layer
([`apa_num()`](https://www.gfrischkorn.org/apabayes/reference/apa_num.md),
[`apa_ci()`](https://www.gfrischkorn.org/apabayes/reference/apa_ci.md),
[`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
[`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md),
[`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)),
and nothing printed judges the result.

## The default method

On a fitted model or any other object
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
accepts, the default method calls `apa_tidy(x, ...)` and reports the
result, so `...` takes that route's arguments: `standardize = TRUE` on a
lavaan or blavaan fit, `effects = "all"` on a brms fit, `ci = "hdi"` on
any posterior, `ci = "eti"` or `rope =` on an emmeans grid. Storing the
tidy table and reporting from it is the same thing in two steps, and
lets a document be knitted without the packages that made the fit.

## See also

[apa_results](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
for the object,
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
for the tables,
[`apa_value()`](https://www.gfrischkorn.org/apabayes/reference/apa_value.md)
for the number behind the string.

## Examples

``` r
t <- apabayes_tidy(
  data.frame(
    term = c("b_Intercept", "b_wt", "sigma"),
    label = c("(Intercept)", "wt", "sigma"),
    estimate = c(37.3, -5.34, 2.71), ci_low = c(31.2, -6.85, 2.09),
    ci_high = c(43.1, -3.82, 3.60), pd = c(1, 0.9995, 1),
    component = c("conditional", "conditional", "sigma")
  ),
  type = "parameters", centrality = "median", ci_method = "eti",
  ci_level = 0.95
)
apa_inline(t, "wt")
#> *b* = −5.34, 95% CrI [−6.85, −3.82], *pd* > .999
apa_inline(t, "b_wt", symbol = FALSE, interval = FALSE)
#> −5.34, *pd* > .999
apa_inline(t, "sigma", markup = "plain")
#> 2.71, 95% CrI [2.09, 3.60], pd > .999
apa_inline(t)
#> *b* = 37.30, 95% CrI [31.20, 43.10], *pd* > .999
#> *b* = −5.34, 95% CrI [−6.85, −3.82], *pd* > .999
#> 2.71, 95% CrI [2.09, 3.60], *pd* > .999
fit <- lavaan::cfa("visual =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
apa_inline(fit, "visual", "x2", op = "=~", standardize = TRUE)
#> .48, 95% CI [.36, .60], *p* < .001
```
