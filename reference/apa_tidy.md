# Extract a model into the apabayes tidy contract

`apa_tidy()` turns a fitted model, a set of posterior draws or a
comparison object into an
[apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
table: fixed column names, the numbers computed by easystats, no
formatting. It is the whole of the extract layer; everything apabayes
reports is built from its output.

## Usage

``` r
# S3 method for class 'BFBayesFactor'
apa_tidy(x, ...)

# S3 method for class 'BFBayesFactorList'
apa_tidy(x, ...)

# S3 method for class 'BFmcmc'
apa_tidy(x, ...)

# S3 method for class 'bayesfactor_inclusion'
apa_tidy(x, ...)

# S3 method for class 'blavaan'
apa_tidy(
  x,
  variables = NULL,
  labels = NULL,
  component = "all",
  standardize = FALSE,
  centrality = c("median", "mean"),
  ci = c("eti", "hdi"),
  ci_level = 0.95,
  rope = NULL,
  rope_ci = 0.95,
  diagnostics = TRUE,
  ...
)

# S3 method for class 'brmsfit'
apa_tidy(
  x,
  variables = NULL,
  labels = NULL,
  effects = c("fixed", "all", "random"),
  component = "all",
  centrality = c("median", "mean"),
  ci = c("eti", "hdi"),
  ci_level = 0.95,
  rope = NULL,
  rope_ci = 0.95,
  diagnostics = TRUE,
  ...
)

# S3 method for class 'brmshypothesis'
apa_tidy(x, directional = NULL, ...)

# S3 method for class 'compare.loo'
apa_tidy(x, weights = NULL, reference = NULL, ...)

# S3 method for class 'bayesfactor_models'
apa_tidy(x, ...)

# S3 method for class 'emmGrid'
apa_tidy(
  x,
  centrality = c("median", "mean"),
  ci = c("hdi", "eti"),
  ci_level = 0.95,
  rope = NULL,
  rope_ci = 0.95,
  ...
)

# S3 method for class 'emm_list'
apa_tidy(x, ...)

# S3 method for class 'easycorrelation'
apa_tidy(x, centrality = c("median", "mean"), ci = c("hdi", "eti"), ...)

apa_tidy(x, ...)

# Default S3 method
apa_tidy(x, ...)

# S3 method for class 'runjags'
apa_tidy(x, ...)

# S3 method for class 'draws'
apa_tidy(
  x,
  variables = NULL,
  labels = NULL,
  centrality = c("median", "mean"),
  ci = c("eti", "hdi"),
  ci_level = 0.95,
  rope = NULL,
  rope_ci = 0.95,
  diagnostics = TRUE,
  ...
)

# S3 method for class 'lavaan'
apa_tidy(
  x,
  variables = NULL,
  labels = NULL,
  component = "all",
  standardize = FALSE,
  ci_level = 0.95,
  ...
)

# S3 method for class 'estimate_contrasts'
apa_tidy(x, centrality = NULL, ci = NULL, ...)

# S3 method for class 'estimate_means'
apa_tidy(x, centrality = NULL, ci = NULL, ...)

# S3 method for class 'parameters_model'
apa_tidy(x, ...)

# S3 method for class 'describe_posterior'
apa_tidy(x, variables = NULL, labels = NULL, centrality = NULL, ...)

# S3 method for class 'stanreg'
apa_tidy(
  x,
  variables = NULL,
  labels = NULL,
  effects = c("fixed", "all", "random"),
  component = "all",
  centrality = c("median", "mean"),
  ci = c("eti", "hdi"),
  ci_level = 0.95,
  rope = NULL,
  rope_ci = 0.95,
  diagnostics = TRUE,
  ...
)
```

## Arguments

- x:

  The object to extract.

- ...:

  Passed to the method.

- variables:

  Character vector of draws variables to report, in the order given, or
  `NULL` for every variable that is not *internal*. A variable is
  internal when its name ends in `__` (the Stan convention for sampler
  quantities such as `lp__`), is exactly `lprior`, or begins with
  `prior_` (the brms log-prior and prior-draw variables, present
  whenever a fit was sampled with `sample_prior = "yes"`). The rule is a
  default, not a filter: naming a variable in `variables` reports it,
  and `variables = posterior::variables(x)` reports everything.

- labels:

  Named character vector of display labels, e.g. `c(b_wt = "Weight")`.
  Draws objects carry no formula, so `label` equals `term` for every
  variable you do not name.

- component:

  Which model component to report, passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
  `"all"` is the easystats default.

- standardize:

  `FALSE` for the unstandardized solution, `TRUE` for the completely
  standardized one (`"std.all"`), or one of `"std.all"`, `"std.lv"` and
  `"std.nox"` as in
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).
  The `std` column records which.

- centrality:

  `"median"` (the default of `parameters` for brms and blavaan) or
  `"mean"`. On the result-object method it defaults to `NULL`, meaning
  the centrality the table already holds: there is nothing left to
  choose, and a table computed with `centrality = "all"` must be told
  which of the two to report.

- ci:

  `"eti"`, the equal-tailed interval reported as CrI, or `"hdi"`, the
  highest-density interval.

- ci_level:

  The interval mass, a number strictly between 0 and 1.

- rope:

  `NULL`, or the two bounds of a region of practical equivalence. The
  ROPE percentage is opt-in; the bounds are kept in the `rope_range`
  attribute so a table note can state them.

- rope_ci:

  The share of the posterior the ROPE percentage is computed on, passed
  to bayestestR as `rope_ci`; `1` uses the whole posterior, the default
  `0.95` the central 95%.

- diagnostics:

  `FALSE` leaves `rhat`, `ess_bulk` and `ess_tail` as `NA` instead of
  computing them.

- effects:

  Which parameters to report, passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html):
  `"fixed"` (the easystats default, population-level parameters and the
  distributional ones such as `sigma`), `"all"` (adds the group-level
  standard deviations and correlations) or `"random"`. Asking for
  `"random"` from a model that has no random effects is an error,
  decided by
  [`insight::is_mixed_model()`](https://easystats.github.io/insight/reference/is_mixed_model.html)
  before easystats is called.

- directional:

  `NULL` to read from `x` which rows test a directional hypothesis, or a
  logical vector of length 1 or one per row saying so.
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
  records the operator only in the `Hypothesis` string, and replaces
  that string with the user's name when the hypothesis was named, so the
  derivation reads the interval instead: brms uses the quantiles at
  `alpha` and `1 - alpha` for a directional hypothesis and at
  `alpha / 2` and `1 - alpha / 2` for a point one. A row where neither
  rule fits, or both (a posterior with no spread), is an error rather
  than a missing Bayes factor.

- weights:

  `NULL`, or model weights to report in the `weight` column: the result
  of
  [`loo::loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html)
  (its kind, `"stacking"`, `"pseudo-BMA+"` or `"pseudo-BMA"`, becomes
  the `weight_method` attribute), or a numeric vector named by model.
  The weights are matched to rows by model name, never by position, and
  must name exactly the models of `x`. The comparison object carries no
  weights;
  [`performance::compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.html)'s
  `LOOIC_wt` is the stacking weight.

- reference:

  `NULL` for loo's own reference — the model in the first row, the one
  with the highest ELPD, against which
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  signs every difference — or the name of another model of `x` to take
  the differences from, so that a model behind the reference prints a
  positive `elpd_diff`. Naming loo's own reference changes nothing: the
  object's numbers are returned untouched.

  Under another reference `elpd_diff` becomes `elpd` minus the
  reference's `elpd` — a difference of two numbers loo reported,
  computed by apabayes, which is why it is named here — and the standard
  error of the difference survives only where loo measured it: `0` on
  the reference row, loo's own `se_diff` on the model that was loo's
  reference (the same pair, read the other way round), and `NA` on every
  other row, because a `compare.loo` object does not carry the pointwise
  ELPDs another pair would need. Run
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  on two models to get that standard error. `p_worse` and `diag_diff`
  qualify a difference against loo's reference and become `NA` for the
  same reason; every column that describes a model rather than a pair is
  unchanged, as is the row order.

## Value

An
[apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
tibble.

## Methods (by class)

- `apa_tidy(BFBayesFactor)`: A `BFBayesFactor` object from BayesFactor
  ([`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html),
  [`BayesFactor::lmBF()`](https://rdrr.io/pkg/BayesFactor/man/lmBF.html),
  [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html),
  [`BayesFactor::correlationBF()`](https://rdrr.io/pkg/BayesFactor/man/correlationBF.html),
  ...), as a `bf_models` table read from the object: the denominator
  model first, then every numerator in the object's order, each named by
  BayesFactor's short name (`model`, made unique as BayesFactor names
  its rows: `"wt"`, `"wt #1"`) and long name (`name`). `log_bf` is
  BayesFactor's own natural-log Bayes factor, `bf` its exponential,
  `error` its proportional numerical error (`NA` on the denominator row
  and where BayesFactor records none, 0 for the exact families), and
  `method` the prior family (`"JZS (BayesFactor)"`,
  `"Jeffreys-beta* (BayesFactor)"`, ...). `post_prob` is computed at
  equal prior odds, as on the `bayesfactor_models` route. A numerator
  identical to the denominator (`bf / bf[1]`) is not repeated. A
  `BFBayesFactorList` (`bf / bf`) holds several denominators and is
  refused; report one column of it, `x[, j]`. Parameter estimates come
  from the draws: `apa_tidy(BayesFactor::posterior(bf, iterations = ))`
  reads a `BFmcmc` object through the draws route, and takes that
  route's arguments.

- `apa_tidy(BFBayesFactorList)`: A `BFBayesFactorList` is refused: it
  holds Bayes factors against several denominators, and a table has one.

- `apa_tidy(BFmcmc)`: A `BFmcmc` object from
  [`BayesFactor::posterior()`](https://rdrr.io/pkg/BayesFactor/man/posterior-methods.html)
  is read by the draws route, with every argument of that route. An
  object with several numerators needs `index =` in
  [`BayesFactor::posterior()`](https://rdrr.io/pkg/BayesFactor/man/posterior-methods.html).

- `apa_tidy(bayesfactor_inclusion)`: The output of
  [`bayestestR::bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.html),
  as a `bf_inclusion` table: one row per `term`, with the prior and
  posterior inclusion probabilities (`p_prior`, `p_posterior`), the
  inclusion Bayes factor `bf` and its log `log_bf`. A term in every
  model has no inclusion Bayes factor (`NA`, where bayestestR has
  `NaN`); a posterior inclusion probability that rounds to 1 gives an
  infinite one, which is kept. The `averaging` attribute says whether
  the models were averaged over `"all"` or `"matched"` models,
  `prior_odds` whether the prior odds were `"equal"` or `"custom"` (the
  values in `prior_odds_values`). bayestestR does not record how the
  models' Bayes factors were computed, so `bf_method` is `NA`.

- `apa_tidy(blavaan)`: A `blavaan` fit from
  [`blavaan::bcfa()`](https://blavaan.org/reference/bcfa.html),
  [`blavaan::bsem()`](https://blavaan.org/reference/bsem.html) or
  [`blavaan::bgrowth()`](https://blavaan.org/reference/bgrowth.html).
  Estimates, the credible interval and pd come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  which reports the *free* parameters only — the fixed marker loading is
  absent, and `p` is `NA` throughout. `term` is blavaan's own parameter
  name (`visual=~x1`, `x1~~x1`) and `label` the same with spaces. Here
  `component` is `"all"`, `"latent"` or `"residual"`, blavaan's own
  vocabulary rather than lavaan's six names.

  R-hat and both ESS columns come from
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)
  over `blavaan::blavInspect(x, "mcmc")`, and blavaan's own are never
  computed. They therefore **differ from what `blavaan::summary()`
  prints**, which reports `blavInspect(x, "rhat")` and `"neff"`: one ESS
  where the contract has two, from an estimator that is neither the bulk
  nor the tail ESS the "greater than 400" rule of thumb is defined for.

  `standardize` reads
  [`blavaan::standardizedPosterior()`](https://blavaan.org/reference/standardizedPosterior.html)
  instead of easystats, which cannot standardize a blavaan fit at all.
  The standardized solution covers the whole parameter table, so its
  rows are a *superset* of the unstandardized ones: the fixed markers
  appear, with a real interval, and under `"std.all"` the latent
  variances are exactly 1. Those rows carry no component information, so
  `component` is `NA` there and cannot be combined with `standardize`;
  select rows with `variables` instead. A multi-group fit is refused.

- `apa_tidy(brmsfit)`: A `brmsfit`, and by inheritance a `bmmfit`.
  Estimates, interval, pd and the ROPE percentage come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  R-hat and both ESS columns from
  [`bayestestR::diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.html);
  apabayes computes no summary of its own. A bmm model parameter is a
  brms distributional parameter, so `component` carries it (`drift`,
  `kappa`, ...).

  One shape is out of reach upstream: a model whose response is a matrix
  with a `trials()` term (`family = multinomial()`, which is every bmm
  M3 fit) fails inside
  [`insight::get_data()`](https://easystats.github.io/insight/reference/get_data.html)
  on R 4.3 or newer, before any number is computed. The method then
  aborts naming `apa_tidy(brms::as_draws_df(fit))`, which reads the
  draws directly;
  [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
  and
  [`apa_convergence()`](https://www.gfrischkorn.org/apabayes/reference/apa_convergence.md)
  do not go through easystats and are unaffected.

- `apa_tidy(brmshypothesis)`: The output of
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html).
  Estimates, interval, evidence ratio and posterior probability are
  brms's own; apabayes adds `bf10` (`1 / evid_ratio` for a point
  hypothesis, the evidence ratio itself for a directional one) and
  `directional`. easystats has no method for this class, so no easystats
  function is called, and `package_versions` credits `brms` alone.

- `apa_tidy(compare.loo)`: The output of
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
  of either shape: the data frame loo 2.10.0 and later return, or the
  matrix of earlier versions. One row per model, best first, with loo's
  own numbers: the ELPD difference to the best model and its standard
  error, the model's ELPD, `p_loo` and LOOIC. No loo function is called;
  `loo` must be installed to record its version. apabayes never turns an
  ELPD difference into a Bayes factor or a weight into evidence
  (ARCHITECTURE decision 17).

- `apa_tidy(bayesfactor_models)`: The output of
  [`bayestestR::bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.html).
  `bf` is `exp(log_BF)`, kept next to `log_bf` because the exponential
  overflows above a log Bayes factor of about 709; `denominator` marks
  the row every Bayes factor is taken against; `method` is how they were
  computed (bridge sampling, the BIC approximation, BayesFactor's JZS).
  `post_prob` is the posterior probability of each model under **equal
  prior odds**, the assumption the `prior_odds` attribute records.
  `model` is the model as bayestestR names it (the formula's right-hand
  side for most classes) and the extra column `name` the argument it was
  passed as.

- `apa_tidy(emmGrid)`: An `emmGrid` from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  or
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  on a Bayesian fit: one row per grid row, the contrast string or, on a
  grid of marginal means, the row's label as emmeans writes it
  (`cyl_f4`, `cyl_f4 auto`), in the `contrast` column. The estimate,
  interval, pd and ROPE share come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
  `ci = "hdi"`, the default, is the highest-density interval
  `summary(emmGrid)` prints (emmeans calls it HPD; the numbers are
  identical) and is labelled HDI; `ci = "eti"` gives the equal-tailed
  interval. A `by` variable's values go into the `group` column and its
  name into the `by` attribute; every grid variable is kept as an extra
  column. The grid is reported as it is: subset it with `[` first to
  report fewer rows. A grid without posterior draws (from a frequentist
  fit) is refused, and so is an `emm_list`
  (`emmeans(fit, pairwise ~ f)`), which holds two tables of different
  kinds — pass one of its parts.

- `apa_tidy(emm_list)`: An `emm_list`, which
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  returns for a two-sided formula (`pairwise ~ f`), is refused: it holds
  a table of means and a table of contrasts, and one tidy table reports
  one kind. Pass `x$emmeans` or `x$contrasts`.

- `apa_tidy(easycorrelation)`: A table of Bayesian correlations from
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  or
  [`correlation::cor_test()`](https://easystats.github.io/correlation/reference/cor_test.html)
  with `bayesian = TRUE`, as a `correlations` table: one row per pair,
  named `var1~~var2`, with the posterior estimate of the correlation,
  its interval, pd, the ROPE share, the Bayes factor and the pairwise n,
  all read from the table. The table does not record which interval or
  which centrality it holds, so `ci` and `centrality` must name what it
  was computed with; they default to correlation's own defaults
  (`bayesian_ci_method = "hdi"`, the median) and apabayes cannot check
  them. A table computed with `centrality = "all"` has no `rho` column
  and is read from the column `centrality` names. correlation writes its
  `ci` argument into the table without applying it (the bounds are 95 %
  whatever `ci` was), so a table labelled with any other level is
  refused. The ROPE share is reported without bounds, which the table
  does not carry. Frequentist tables and tables with diagonal rows
  (`redundant = TRUE`) are refused.

- `apa_tidy(default)`: Anything
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  accepts — `stanfit`, `CmdStanFit`, `mcmc`, `mcmc.list`, a draws matrix
  or a data frame of draws — is converted and handed to the `draws`
  method. A data frame, tibble (grouped or not) or data.table counts as
  draws only when its columns are all numeric; a summary table (one with
  a class of its own, or with a label column) is refused rather than
  read as draws. The class of the object you passed is kept as the
  `source_class` attribute.

- `apa_tidy(runjags)`: A `runjags` object keeps its chains in `$mcmc`;
  `posterior` has no method for the object itself.

- `apa_tidy(draws)`: Posterior draws. Estimates, interval, pd and the
  ROPE percentage come from
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),
  R-hat and ESS from
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html);
  apabayes computes no summary of its own.

- `apa_tidy(lavaan)`: A `lavaan` fit from
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html),
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or
  [`lavaan::growth()`](https://rdrr.io/pkg/lavaan/man/growth.html).
  Estimates, standard errors, the confidence interval and the p value
  come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  which reads
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or, with `standardize`,
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).
  `term` is lavaan's own parameter name (`visual=~x1`, `x1~~x1`,
  `dem60~ind60`, `x1~1`; `.g2` appended in the second group of a
  multi-group fit) and `label` the same with spaces. Here `component` is
  `"all"` or one or more of `"loading"`, `"regression"`,
  `"correlation"`, `"variance"`, `"mean"` and `"defined"`. `centrality`
  is `NA` on the result — the estimate is a maximum-likelihood point
  estimate — and `ci_method` is `"wald"`, or `"boot"` (percentile
  bootstrap) for the unstandardized solution of a fit with
  `se = "bootstrap"`. A fixed parameter (the marker loading) has
  `p = NA`, as lavaan reports it. A `blavaan` fit is refused: this
  method reports maximum-likelihood fits.

- `apa_tidy(estimate_contrasts)`: A table of contrasts from
  [`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
  on a Bayesian fit, as a `contrasts` table. A row is named as
  modelbased names it, `Level1 - Level2` (`"6 - 4"`, or
  `"6, auto - 4, auto"` for two variables), or by its `Parameter` for a
  custom comparison; a `by` variable's values go into the `group`
  column. The numbers are read from the table, not recomputed, and so is
  the ROPE share with its bounds when the table has one. The table
  records its interval only in its call: with no `ci_method` there it is
  modelbased's default equal-tailed interval, otherwise the one named.
  When the call gives `ci_method` as a variable, or the table has lost
  its call, name the interval with `ci = "eti"` or `ci = "hdi"`; a `ci`
  that contradicts the call is refused. Tables computed with
  `backend = "emmeans"` are refused, since their estimate column does
  not say which centrality it holds: pass the emmeans grid to
  `apa_tidy()` instead.

- `apa_tidy(estimate_means)`: A table of marginal means from
  [`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html)
  on a Bayesian fit, as a `contrasts` table whose rows are named by the
  values of their `by` variables (`"4"`, `"4, auto"`). Everything else
  is read as for `estimate_contrasts` tables.

- `apa_tidy(parameters_model)`: A `parameters_model` table from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
  A table of a Bayesian model inherits `describe_posterior` and is
  handled by that method; a table that does not — a frequentist model,
  or one computed with several `ci` levels — is refused here rather than
  coerced to draws.

- `apa_tidy(describe_posterior)`: A `describe_posterior` table from
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),
  or from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  on a Bayesian model. The numbers are read from the table, so `ci`,
  `ci_level`, `rope` and `diagnostics` are not arguments: they were
  settled when the table was computed, and `ci_method` and the interval
  level are read from it. `centrality = NULL` reports whichever of the
  median and the mean the table holds, and asks you to name one when it
  holds both. `rhat` and `ess_tail` are read when present; `ess_bulk` is
  always `NA`, because no easystats table carries it — pass the fit for
  both ESS columns. `bf` is `exp(log_BF)` when the table was computed
  with `test = "bf"`. The `package_versions` attribute records the
  versions installed when `apa_tidy()` ran, not necessarily those that
  computed the table.

- `apa_tidy(stanreg)`: A `stanreg` object from rstanarm. Estimates,
  interval, pd and the ROPE percentage come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  with `priors = FALSE`; R-hat and both ESS columns from
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)
  over the fit's draws, which cover every reported parameter including
  `sigma`. apabayes computes no summary of its own.

## See also

[`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
for the contract.

## Examples

``` r
# Deterministic draws, so the example does not depend on RNG state:
# normal quantiles, reordered by a fixed rule so the chain is not
# monotone and the diagnostics are meaningful.
z <- stats::qnorm(stats::ppoints(400))
z <- z[order(sin(seq_along(z)))]
draws <- posterior::as_draws_df(
  data.frame(mu = 2 + z, sigma = exp(0.3 * z))
)
apa_tidy(draws)
#> # apabayes tidy table: parameters (2 rows)
#> # median, 95% CrI (equal-tailed); source: draws_df
#> # A tibble: 2 × 18
#>   term  label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 mu    mu        2    0.0599    3.94 eti           0.95 0.978       NA 0.998
#> 2 sigma sigma     1.00 0.559     1.79 eti           0.95 1           NA 0.998
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>
apa_tidy(draws, variables = "mu", ci = "hdi", ci_level = 0.9)
#> # apabayes tidy table: parameters (1 row)
#> # median, 90% HDI; source: draws_df
#> # A tibble: 1 × 18
#>   term  label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 mu    mu           2  0.343    3.63 hdi            0.9 0.978       NA 0.998
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>

# An easystats table you already computed is reported as it stands:
# the interval, its method and the ROPE are read off the object, not
# recomputed, so those are not arguments here.
apa_tidy(bayestestR::describe_posterior(draws))
#> # apabayes tidy table: parameters (2 rows)
#> # median, 95% CrI (equal-tailed); source: describe_posterior
#> # A tibble: 2 × 18
#>   term  label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 mu    mu        2    0.0599    3.94 eti           0.95 0.978  0.00263    NA
#> 2 sigma sigma     1.00 0.559     1.79 eti           0.95 1      0          NA
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>

# A lavaan fit: a maximum-likelihood estimate with a Wald interval and
# a p value, so `centrality` is NA and `ci_method` is "wald".
fit <- lavaan::cfa(
  "visual =~ x1 + x2 + x3",
  data = lavaan::HolzingerSwineford1939
)
apa_tidy(fit, component = "loading", standardize = TRUE)
#> # apabayes tidy table: parameters (3 rows)
#> # 95% CI (Wald); source: lavaan
#> # A tibble: 3 × 18
#>   term     label estimate ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr>    <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 visual=… visu…    0.621  0.489   0.753 wald          0.95    NA       NA    NA
#> 2 visual=… visu…    0.479  0.356   0.602 wald          0.95    NA       NA    NA
#> 3 visual=… visu…    0.710  0.570   0.850 wald          0.95    NA       NA    NA
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>

# `brms::hypothesis()` reads a plain data frame of draws as well as a
# fitted model, so the hypothesis route needs no Stan here. The first
# row is directional (a 90% interval, `bf10` the posterior odds), the
# second a point hypothesis whose evidence ratio needs prior draws
# this data frame does not carry.
q <- stats::qnorm(stats::ppoints(400))
draws <- data.frame(b_wt = -5 + q, b_am = 0.2 + 2 * q)
apa_tidy(brms::hypothesis(draws, c("b_wt < 0", "b_am = 0")))
#> # apabayes tidy table: hypotheses (2 rows)
#> # mean; source: brmshypothesis
#> # A tibble: 2 × 11
#>   hypothesis group estimate ci_low ci_high ci_method ci_level evid_ratio
#>   <chr>      <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl>      <dbl>
#> 1 (b_wt) < 0 NA        -5    -6.63   -3.37 eti           0.9         Inf
#> 2 (b_am) = 0 NA         0.2  -3.68    4.08 eti           0.95         NA
#> # ℹ 3 more variables: post_prob <dbl>, bf10 <dbl>, directional <lgl>

# An emmeans grid from a Bayesian fit carries the coefficient draws;
# `emmeans::qdrg()` builds one from a draws matrix directly, which is
# what a brms or rstanarm fit would supply. The interval is the HDI,
# emmeans's own HPD interval; `ci = "eti"` asks for the equal-tailed one.
q <- stats::qnorm(stats::ppoints(400))
coefs <- cbind(
  `(Intercept)` = 26.7 + q, cyl_f6 = -6.9 + 1.5 * q,
  cyl_f8 = -11.6 + 1.3 * q
)
cars <- transform(mtcars, cyl_f = factor(cyl))
grid <- emmeans::qdrg(~cyl_f, data = cars, mcmc = coefs)
means <- emmeans::emmeans(grid, ~cyl_f)
apa_tidy(means)
#> # apabayes tidy table: contrasts (3 rows)
#> # median, 95% HDI; source: emmGrid
#> # A tibble: 3 × 10
#>   contrast group estimate ci_low ci_high ci_method ci_level    pd rope_pct cyl_f
#>   <chr>    <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <chr>
#> 1 cyl_f4   NA        26.7   24.8    28.7 hdi           0.95     1       NA 4    
#> 2 cyl_f6   NA        19.8   15.0    24.8 hdi           0.95     1       NA 6    
#> 3 cyl_f8   NA        15.1   10.6    19.7 hdi           0.95     1       NA 8    
apa_tidy(emmeans::contrast(means, "pairwise"), rope = c(-1, 1))
#> # apabayes tidy table: contrasts (3 rows)
#> # median, 95% HDI; source: emmGrid
#> # A tibble: 3 × 9
#>   contrast       group estimate ci_low ci_high ci_method ci_level    pd rope_pct
#>   <chr>          <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl>
#> 1 cyl_f4 - cyl_… NA         6.9   3.93    9.81 hdi           0.95     1        0
#> 2 cyl_f4 - cyl_… NA        11.6   9.02   14.1  hdi           0.95     1        0
#> 3 cyl_f6 - cyl_… NA         4.7   4.30    5.09 hdi           0.95     1        0

# A modelbased table is reported as it stands. It records its interval
# only in its call: none named there is modelbased's equal-tailed
# default, and a variable there has to be named with `ci =`.
# \donttest{
cars <- transform(mtcars, cyl_f = factor(cyl))
fit <- rstanarm::stan_glm(
  mpg ~ wt + cyl_f,
  data = cars, chains = 2, iter = 1000, seed = 1, refresh = 0
)
apa_tidy(modelbased::estimate_contrasts(fit, contrast = "cyl_f"))
#> Error: Package `marginaleffects` required for this function to work.
#>   Please install it by running `install.packages("marginaleffects")`.
method <- "hdi"
means <- modelbased::estimate_means(fit, by = "cyl_f", ci_method = method)
#> Error: Package `marginaleffects` required for this function to work.
#>   Please install it by running `install.packages("marginaleffects")`.
apa_tidy(means, ci = "hdi")
#> # apabayes tidy table: contrasts (3 rows)
#> # median, 95% HDI; source: emmGrid
#> # A tibble: 3 × 10
#>   contrast group estimate ci_low ci_high ci_method ci_level    pd rope_pct cyl_f
#>   <chr>    <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <chr>
#> 1 cyl_f4   NA        26.7   24.8    28.7 hdi           0.95     1       NA 4    
#> 2 cyl_f6   NA        19.8   15.0    24.8 hdi           0.95     1       NA 6    
#> 3 cyl_f8   NA        15.1   10.6    19.7 hdi           0.95     1       NA 8    
# }

# Bayesian correlations. The table records neither its interval nor
# its centrality, so `ci` and `centrality` name them; the defaults are
# correlation's own.
r <- correlation::correlation(
  mtcars[, c("mpg", "wt", "hp")],
  bayesian = TRUE
)
apa_tidy(r)
#> # apabayes tidy table: correlations (3 rows)
#> # median, 95% HDI; source: easycorrelation
#> # A tibble: 3 × 17
#>   term    var1  var2  group estimate ci_low ci_high ci_method ci_level    pd
#>   <chr>   <chr> <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>
#> 1 mpg~~wt mpg   wt    NA      -0.812 -0.906  -0.661 hdi           0.95     1
#> 2 mpg~~hp mpg   hp    NA      -0.707 -0.852  -0.503 hdi           0.95     1
#> 3 wt~~hp  wt    hp    NA       0.588  0.369   0.779 hdi           0.95     1
#> # ℹ 7 more variables: rope_pct <dbl>, bf <dbl>, n <dbl>, method <chr>,
#> #   prior_distribution <chr>, prior_location <dbl>, prior_scale <dbl>
r_eti <- correlation::correlation(
  mtcars[, c("mpg", "wt")],
  bayesian = TRUE, bayesian_ci_method = "eti"
)
apa_tidy(r_eti, ci = "eti")
#> # apabayes tidy table: correlations (1 row)
#> # median, 95% CrI (equal-tailed); source: easycorrelation
#> # A tibble: 1 × 17
#>   term    var1  var2  group estimate ci_low ci_high ci_method ci_level    pd
#>   <chr>   <chr> <chr> <chr>    <dbl>  <dbl>   <dbl> <chr>        <dbl> <dbl>
#> 1 mpg~~wt mpg   wt    NA      -0.818 -0.905  -0.667 eti           0.95     1
#> # ℹ 7 more variables: rope_pct <dbl>, bf <dbl>, n <dbl>, method <chr>,
#> #   prior_distribution <chr>, prior_location <dbl>, prior_scale <dbl>

# A BayesFactor object is read as a table of model comparisons, with
# BayesFactor's own names, prior family and numerical error; its
# inclusion Bayes factors come from bayestestR; its estimates from the
# draws of `BayesFactor::posterior()`.
cars <- transform(mtcars, am_f = factor(am), vs_f = factor(vs))
bf <- BayesFactor::anovaBF(mpg ~ am_f * vs_f, data = cars, progress = FALSE)
apa_tidy(bf)
#> # apabayes tidy table: bf_models (5 rows)
#> # source: BFBayesFactor
#> # A tibble: 5 × 8
#>   model                    bf log_bf denominator method post_prob    error name 
#>   <chr>                 <dbl>  <dbl> <lgl>       <chr>      <dbl>    <dbl> <chr>
#> 1 Intercept only       1   e0   0    TRUE        JZS (…   3.01e-6 NA       Inte…
#> 2 am_f                 8.66e1   4.46 FALSE       JZS (…   2.61e-4  1.88e-8 mpg …
#> 3 vs_f                 5.29e2   6.27 FALSE       JZS (…   1.60e-3  1.46e-9 mpg …
#> 4 am_f + vs_f          2.04e5  12.2  FALSE       JZS (…   6.16e-1  1.11e-2 mpg …
#> 5 am_f + vs_f + am_f:… 1.27e5  11.8  FALSE       JZS (…   3.83e-1  3.62e-2 mpg …
apa_tidy(bayestestR::bayesfactor_inclusion(bf))
#> # apabayes tidy table: bf_inclusion (3 rows)
#> # source: bayestestRBF
#> # A tibble: 3 × 5
#>   term      p_prior p_posterior      bf log_bf
#>   <chr>       <dbl>       <dbl>   <dbl>  <dbl>
#> 1 am_f          0.6       0.998  416.    6.03 
#> 2 vs_f          0.6       1.000 2524.    7.83 
#> 3 am_f:vs_f     0.2       0.383    2.48  0.907
bf_t <- BayesFactor::ttestBF(formula = mpg ~ am_f, data = cars)
apa_tidy(BayesFactor::posterior(bf_t, iterations = 500, progress = FALSE))
#> # apabayes tidy table: parameters (5 rows)
#> # median, 95% CrI (equal-tailed); source: BFmcmc
#> # A tibble: 5 × 18
#>   term    label estimate  ci_low ci_high ci_method ci_level    pd rope_pct  rhat
#>   <chr>   <chr>    <dbl>   <dbl>   <dbl> <chr>        <dbl> <dbl>    <dbl> <dbl>
#> 1 mu      mu       20.7   19.0    22.4   eti           0.95     1       NA 1.01 
#> 2 beta (… beta…    -6.64 -10.6    -2.80  eti           0.95     1       NA 0.998
#> 3 sig2    sig2     24.9   15.3    43.8   eti           0.95     1       NA 1.00 
#> 4 delta   delta    -1.34  -2.11   -0.484 eti           0.95     1       NA 0.998
#> 5 g       g         1.66   0.174  51.0   eti           0.95     1       NA 0.999
#> # ℹ 8 more variables: ess_bulk <dbl>, ess_tail <dbl>, bf <dbl>,
#> #   component <chr>, group <chr>, effects <chr>, std <lgl>, p <dbl>
```
