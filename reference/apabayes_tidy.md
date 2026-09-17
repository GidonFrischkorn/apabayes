# The apabayes tidy contract

`apabayes_tidy()` builds the object the extract layer hands to the
format, inline and table layers: a tibble whose column names and types
are fixed by the kind of table, carrying the reporting metadata as
attributes. Every
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
method returns one, and every apabayes function that reports numbers
takes one.

## Usage

``` r
apabayes_tidy(
  x,
  ...,
  type = c("parameters", "diagnostics", "hypotheses", "loo", "bf_models", "bf_inclusion",
    "sem_fit", "contrasts", "correlations"),
  centrality = c("median", "mean"),
  ci_method = "eti",
  ci_level = 0.95,
  source_class = NA_character_,
  package_versions = character()
)

validate_apabayes_tidy(x)

is_apabayes_tidy(x)

# S3 method for class 'apabayes_tidy'
print(x, n = NULL, width = NULL, ...)
```

## Arguments

- x:

  A data frame with at least the required columns of `type`.

- ...:

  Further attributes, all named.

- type:

  Which column contract applies: one of `"parameters"`, `"diagnostics"`,
  `"hypotheses"`, `"loo"`, `"bf_models"`, `"bf_inclusion"`, `"sem_fit"`,
  `"contrasts"`, `"correlations"`.

- centrality:

  `"median"` or `"mean"`; what `estimate` holds. `NA` when the estimate
  is not a posterior summary (a lavaan maximum-likelihood estimate).

- ci_method:

  `"eti"`, `"hdi"`, `"hpd"`, `"spi"`, `"bci"`, `"wald"`, `"boot"` or
  `NA`; seeds the `ci_method` column when the caller supplies none. The
  column describes intervals apabayes did not necessarily compute, so it
  knows more methods than any route offers through its own `ci`
  argument.

- ci_level:

  A number in (0, 1\] or `NA`; seeds the `ci_level` column the same way.

- source_class:

  Character; [`class()`](https://rdrr.io/r/base/class.html) of the
  object the numbers were extracted from.

- package_versions:

  Named character vector of the packages that produced the numbers.

- n, width:

  Passed to the tibble print method.

## Value

A tibble of class `apabayes_tidy` with the contract columns first, in
contract order, then any extra columns. `validate_apabayes_tidy()`
returns its input invisibly or aborts; `is_apabayes_tidy()` returns
`TRUE` or `FALSE`.

## Column contracts

`type` selects the contract. Columns the caller does not supply are
filled with the typed `NA`, so a consumer can select a column by name
without testing whether it exists.

`"parameters"` — one row per model parameter. Required: `term`,
`estimate`.

- `term`:

  character; the parameter as its source names it (`b_wt`, `visual=~x1`,
  `mu`).

- `label`:

  character; the display label. Defaults to `term`.

- `estimate`:

  double; the posterior median or mean, per the `centrality` attribute.

- `ci_low`, `ci_high`:

  double; interval bounds.

- `ci_method`:

  character; `"eti"`, `"hdi"`, `"hpd"`, `"spi"` (shortest probability
  interval) or `"bci"` (bias-corrected and accelerated) for a credible
  interval, `"wald"` or `"boot"` (percentile bootstrap) for a
  frequentist confidence interval (lavaan).

- `ci_level`:

  double; the interval mass, e.g. `0.95`.

- `pd`:

  double; probability of direction.

- `rope_pct`:

  double; percentage of the posterior inside the ROPE, `NA` unless a
  ROPE was requested.

- `rhat`, `ess_bulk`, `ess_tail`:

  double; convergence diagnostics.

- `bf`:

  double; a Bayes factor for the parameter, `NA` unless one was
  requested.

- `component`, `group`, `effects`:

  character; the structure a model puts a parameter in.

- `std`:

  logical; whether the row is a standardized estimate.

- `p`:

  double; a frequentist p value (lavaan only).

The other contracts are `"diagnostics"` (`term`, `rhat`, `ess_bulk`,
`ess_tail`), `"hypotheses"`, `"loo"`, `"bf_models"` (`model`, `bf`,
`log_bf`, `denominator`, `method`, `post_prob`, and `error`, the
proportional numerical error of the Bayes factor where BayesFactor
records one), `"bf_inclusion"` (`term`, `p_prior`, `p_posterior`, `bf`,
`log_bf`: inclusion Bayes factors), `"sem_fit"`, `"contrasts"`
(`contrast`, `group` for the `by` variable's value, `estimate`, the
interval columns, `pd`, `rope_pct`) and `"correlations"` (`term` as
`var1~~var2`, `var1`, `var2`, `group`, `estimate`, the interval columns,
`pd`, `rope_pct`, `bf`, and `n`, the pairwise number of observations);
the print method lists the columns of each, and every extract method
documents the ones it fills. Columns beyond the contract are kept, after
the contract columns.

## Metadata

`type`, `centrality`, `ci_method`, `ci_level`, `source_class` and
`package_versions` are attributes of the table, plus anything passed
through `...` (`rope_range`, `rope_ci`, `bf_method`, `note`).
`ci_method` and `ci_level` are *both* attributes and columns: a report
can mix intervals of different kinds in one table, so each row says what
it is, while the attribute says what was asked for.

## Validity

A tibble subclass survives `[` and the dplyr verbs, and nothing
re-validates on the way, so a table you have edited can carry the class
without satisfying the contract. Every apabayes function that consumes a
tidy table therefore validates it on entry; `validate_apabayes_tidy()`
is that check.

The contracts check column names and types, not content, with one
exception: a `"sem_fit"` table must carry at least one non-missing fit
index (`chisq`, `cfi`, `tli`, `rmsea`, `srmr`, `ppp`, `brmsea`,
`bgammahat`; the interval bounds and `df` qualify an index and are not
one). `"sem_fit"` is the one contract whose required column — `model` —
is generic enough to belong to another type's table, and a model
comparison accepted as a table of fit indices reports every index as
`NA` rather than refusing. The check runs in the constructor, where the
caller chooses the type, and not in `validate_apabayes_tidy()`, so that
a subset of a valid table stays valid; a zero-row table is exempt, as it
is for every other type.

## Examples

``` r
apabayes_tidy(data.frame(term = "b_wt", estimate = -5.34))
#> # apabayes tidy table: parameters (1 row)
#> # median, 95% CrI (equal-tailed)
#> # A tibble: 1 × 18
#>   term  label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 b_wt  b_wt     -5.34     NA      NA eti           0.95    NA       NA    NA
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>
apabayes_tidy(
  data.frame(
    term = "b_wt", estimate = -5.34, ci_low = -6.85,
    ci_high = -3.82, pd = 1
  ),
  ci_method = "hdi", source_class = "stanfit"
)
#> # apabayes tidy table: parameters (1 row)
#> # median, 95% HDI; source: stanfit
#> # A tibble: 1 × 18
#>   term  label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 b_wt  b_wt     -5.34  -6.85   -3.82 hdi           0.95     1       NA    NA
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>
```
