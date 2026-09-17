# Fit indices of a structural equation model

`apa_tidy_sem_fit()` returns one row of fit indices for a fitted SEM.
For a `lavaan` fit those are the model chi-square with its degrees of
freedom and p value, CFI, TLI, RMSEA with its confidence interval, and
SRMR, from
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
For a `blavaan` fit they are the posterior predictive p value, BRMSEA
and BGammaHat with their credible intervals, from
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
and
[`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html).
The two are complementary: a fit of one kind carries none of the other's
indices, and those columns are `NA`. apabayes computes no index of its
own, and none of its output judges a fit.

## Usage

``` r
# S3 method for class 'blavaan'
apa_tidy_sem_fit(
  x,
  model = NA_character_,
  centrality = c("median", "mean"),
  pD = c("loo", "waic", "dic"),
  rescale = c("devM", "ppmc", "mcmc"),
  fit_ci_level = 0.9,
  ...
)

apa_tidy_sem_fit(x, ...)

# S3 method for class 'lavaan'
apa_tidy_sem_fit(
  x,
  model = NA_character_,
  test = c("standard", "scaled", "robust"),
  rmsea_level = 0.9,
  ...
)
```

## Arguments

- x:

  A fitted model.

- model:

  A single string naming the model in the `model` column, or `NA` (the
  default).

- centrality:

  The posterior summary of BRMSEA and BGammaHat: `"median"` (the
  default) or `"mean"`, blavaan's `EAP` column. The interval is the same
  highest-density interval either way.

- pD:

  Which effective-number-of-parameters estimator
  [`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html)
  rescales the posterior chi-square with: `"loo"` (its own default),
  `"waic"` or `"dic"`. Spelled as blavaan spells it, rather than
  lower-cased, so that it is not read as the `pd` of the parameters
  contract, which is the probability of direction. Recorded in the `pD`
  attribute.

- rescale:

  How the posterior chi-square is rescaled: `"devM"` (the default),
  `"ppmc"` or `"mcmc"`. Recorded in the `rescale` attribute.

- fit_ci_level:

  Mass of the fit indices' credible interval, separate from the
  `ci_level` of the parameter table; blavaan's own default is `0.90`.

- ...:

  Passed to the method.

- test:

  Which chi-square family the indices come from: `"standard"` (every fit
  has it), `"scaled"` (the `.scaled` variants of a fit with a scaled
  test statistic, e.g. `estimator = "MLR"`; SRMR has none), or
  `"robust"` (the scaled chi-square with lavaan's `.robust` CFI, TLI and
  RMSEA). A variant the fit does not carry is an error.

- rmsea_level:

  Confidence level of the RMSEA interval; lavaan's default is `0.90`.

## Value

An
[apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
tibble of type `"sem_fit"` with one row and columns `model`, `chisq`,
`df`, `p`, `cfi`, `tli`, `rmsea`, `rmsea_low`, `rmsea_high`,
`rmsea_level`, `srmr`, `ppp`, `brmsea`, `brmsea_low`, `brmsea_high`,
`bgammahat`, `bgammahat_low` and `bgammahat_high`; the ones the fit does
not carry are `NA`. Attributes `estimator` and `n` record the estimator
and the sample size, plus `test` on a lavaan fit (the chi-square family
reported) and `pD` and `rescale` on a blavaan one (how the posterior
chi-square was rescaled).

## Methods (by class)

- `apa_tidy_sem_fit(blavaan)`: A `blavaan` fit. The posterior predictive
  p value comes from
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
  and BRMSEA and BGammaHat from
  [`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html),
  summarised as blavaan summarises them: the posterior median with a
  highest-density interval, which is what
  [`summary()`](https://rdrr.io/r/base/summary.html) of that object
  prints. A blavaan fit carries no chi-square, CFI, TLI, RMSEA or SRMR
  at all, so every column of the lavaan row is `NA` here, and `test` and
  `rmsea_level` are absent from this method rather than accepted and
  ignored. BCFI, BTLI and BNFI need a baseline model and are not
  reported yet. A fit made with `test = "none"` has neither a PPP nor
  fit indices and is refused.

- `apa_tidy_sem_fit(lavaan)`: A `lavaan` fit. `test = "scaled"` or
  `"robust"` needs a fit with a scaled test statistic.

## See also

[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
for the parameter table.

## Examples

``` r
fit <- lavaan::cfa(
  "visual =~ x1 + x2 + x3
   speed  =~ x7 + x8 + x9",
  data = lavaan::HolzingerSwineford1939
)
apa_tidy(fit, component = "loading", standardize = TRUE)
#> # apabayes tidy table: parameters (6 rows)
#> # 95% CI (Wald); source: lavaan
#> # A tibble: 6 × 18
#>   term     label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr>    <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 visual=… visu…    0.667  0.549   0.784 wald          0.95    NA       NA    NA
#> 2 visual=… visu…    0.456  0.337   0.575 wald          0.95    NA       NA    NA
#> 3 visual=… visu…    0.678  0.561   0.796 wald          0.95    NA       NA    NA
#> 4 speed=~… spee…    0.572  0.467   0.676 wald          0.95    NA       NA    NA
#> 5 speed=~… spee…    0.741  0.640   0.841 wald          0.95    NA       NA    NA
#> 6 speed=~… spee…    0.649  0.548   0.751 wald          0.95    NA       NA    NA
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>
apa_tidy_sem_fit(fit, model = "Two factors")
#> # apabayes tidy table: sem_fit (1 row)
#> # source: lavaan
#> # A tibble: 1 × 18
#>   model   chisq    df       p   cfi   tli rmsea rmsea_low rmsea_high rmsea_level
#>   <chr>   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>       <dbl>
#> 1 Two fa…  47.4     8 1.28e-7 0.879 0.774 0.128    0.0942      0.164         0.9
#> # ℹ 8 more variables: srmr <dbl>, ppp <dbl>, brmsea <dbl>, brmsea_low <dbl>,
#> #   brmsea_high <dbl>, bgammahat <dbl>, bgammahat_low <dbl>,
#> #   bgammahat_high <dbl>
```
