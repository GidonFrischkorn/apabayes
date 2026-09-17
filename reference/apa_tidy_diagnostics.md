# Convergence diagnostics for every sampled quantity

`apa_tidy_diagnostics()` returns R-hat and bulk and tail ESS for every
variable of a fitted model's posterior, including the group-level
deviations that a parameter table does not print. It is the table
[`apa_convergence()`](https://www.gfrischkorn.org/apabayes/reference/apa_convergence.md)
reports from: a convergence statement has to cover what was sampled, not
what a table shows.

## Usage

``` r
# S3 method for class 'blavaan'
apa_tidy_diagnostics(x, variables = NULL, ...)

apa_tidy_diagnostics(x, ...)

# Default S3 method
apa_tidy_diagnostics(x, variables = NULL, ...)

# S3 method for class 'runjags'
apa_tidy_diagnostics(x, ...)
```

## Arguments

- x:

  A fitted model, posterior draws, or anything
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  accepts.

- variables:

  Character vector of variables to report, in the order given, or `NULL`
  for every variable that is not *internal* — the same rule
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  uses on the draws route: names ending in `__`, `lprior`, and names
  starting with `prior_` are dropped by default and reported when named
  here.

- ...:

  Passed to the method.

## Value

An
[apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
tibble of type `"diagnostics"` with columns `term`, `rhat`, `ess_bulk`
and `ess_tail`. Its `divergences` attribute is the number of divergent
post-warmup transitions summed over chains, read from the sampler's own
record, for a `brmsfit`, `stanreg`, `stanfit`, `CmdStanMCMC` or
`blavaan` fit sampled with NUTS; it is `NA` for draws, `mcmc.list` and
runjags objects and for fits made by optimisation, variational inference
or another sampler, which have no such record.

## Details

Numbers come from
[`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html);
apabayes computes no diagnostic of its own.

## Methods (by class)

- `apa_tidy_diagnostics(blavaan)`: A `blavaan` fit, which
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  cannot read (measured). The chains are
  `blavaan::blavInspect(x, "mcmc")`, named as
  [`coef()`](https://rdrr.io/r/stats/coef.html) names the parameters
  (`visual=~x2`, `x1~~x1`), and the diagnostics are `posterior`'s, not
  the `rhat` and `neff` blavaan prints. A multi-group fit is reported:
  its names carry the group suffix (`visual=~x2.g2`).

- `apa_tidy_diagnostics(default)`: Anything
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  accepts, which includes `brmsfit`, `stanreg`, `stanfit`, `CmdStanFit`,
  `mcmc` and `mcmc.list`. One coercing method serves every supported
  object, as on the draws route.

- `apa_tidy_diagnostics(runjags)`: A `runjags` object keeps its chains
  in `$mcmc`; `posterior` has no method for the object itself.

## See also

[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
for parameter tables.

## Examples

``` r
z <- stats::qnorm(stats::ppoints(400))
z <- z[order(sin(seq_along(z)))]
draws <- posterior::as_draws_df(
  data.frame(mu = 2 + z, sigma = exp(0.3 * z))
)
apa_tidy_diagnostics(draws)
#> # apabayes tidy table: diagnostics (2 rows)
#> # source: draws_df
#> # A tibble: 2 × 4
#>   term   rhat ess_bulk ess_tail
#>   <chr> <dbl>    <dbl>    <dbl>
#> 1 mu    0.998     502.     248.
#> 2 sigma 0.998     502.     248.
```
