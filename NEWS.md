# apabayes 0.0.0.9000

* feat: `apa_tidy()` reports a table from `modelbased::estimate_contrasts()`
  or `modelbased::estimate_means()` on a Bayesian fit as a `contrasts`
  table, the contract the emmeans route fills. Rows are named as
  modelbased names them (`6 - 4`, `6, auto - 4, auto`, a mean by its row
  values), a `by` variable's values fill `group`, and every number,
  the ROPE share and its bounds included, is read from the table. The
  table records its interval only in its call, so `ci = NULL` reads it
  there (modelbased's equal-tailed default when no `ci_method` was
  passed); when the call gives `ci_method` as a variable, `ci = "eti"`
  or `"hdi"` names it, and a `ci` that contradicts the call is refused.
  Tables from `backend = "emmeans"` (whose estimate column does not say
  which centrality it holds) and from frequentist models are refused.
  `papaja::apa_print()` works on both classes. `modelbased` joins
  Suggests.
* fix: `apa_tidy()` no longer reads any data frame as draws. A plain data
  frame, tibble or data.table of numeric columns still is; a data frame
  with a class of its own (a modelbased or easystats table, a
  `bayesfactor_models` object) or a column that is not numeric is
  refused with a message naming the class or the columns, where
  `posterior::as_draws_df()` had silently turned it into a nonsense
  parameters table.
* feat: `apa_tidy()` reports an emmeans grid from a Bayesian fit — the
  contrasts of `emmeans::contrast()` or `pairs()`, or the marginal means
  of `emmeans::emmeans()` — as a `contrasts` table: the contrast string
  (on a means grid, the row label as emmeans writes it, `cyl_f4`,
  `cyl_f4 auto`), the posterior median or mean, the interval, pd and on
  request the ROPE share, all from `parameters::model_parameters()`.
  `ci = "hdi"`, the default, is the highest-density interval
  `summary(emmGrid)` prints (emmeans's HPD), reproduced bit for bit and
  labelled `HDI`, the same name as on every other route; `ci = "eti"`
  gives the equal-tailed one. A `by` variable's values fill
  the contract's new `group` column and its name the `by` attribute;
  the contract also gained `rope_pct`. Every grid variable is kept as an
  extra column. A grid without posterior draws (a frequentist fit) and
  an `emm_list` (`emmeans(fit, pairwise ~ f)`, two tables of different
  kinds) are refused with a message. `emmeans` joins Suggests.
* feat: `apa_inline()` reports a `contrasts` row like a parameters row,
  `4.28, 95% HDI [1.38, 7.01], *pd* = .998`, with the ROPE share when
  the table carries one and no symbol unless `symbol` gives one. A row
  is addressed by its contrast string, and by `group` when the contrast
  repeats across `by` groups. papaja's own `apa_print.emmGrid()` is left
  alone; a Bayesian grid is reported through `apa_print(apa_tidy(grid))`.

* feat: `apa_tidy()` reports a LOO model comparison from
  `loo::loo_compare()` as a `loo` table: per model the ELPD difference to
  the best model and its SE, the model's ELPD and SE, `p_loo`, LOOIC and
  loo's own `p_worse` and diagnostic flags. Both shapes are read: the data
  frame loo 2.10.0 and later return and the matrix of earlier versions,
  which keeps the models in its row names. The comparison carries no
  weights, so `weights =` takes a `loo::loo_model_weights()` result (its
  kind, stacking, pseudo-BMA+ or pseudo-BMA, goes into the
  `weight_method` attribute) or a numeric vector named by model, matched
  by name. `loo` joins Suggests.
* feat: `apa_tidy()` reports a Bayes-factor model comparison from
  `bayestestR::bayesfactor_models()` as a `bf_models` table: `bf`, the
  natural-log `log_bf` next to it, the denominator row, the method
  (bridge sampling, the BIC approximation, BayesFactor's JZS) and
  `post_prob`, the posterior model probability under equal prior odds,
  computed by log-sum-exp so that it stays finite where `exp()`
  overflows. `model` is the model as bayestestR names it; the new column
  `name` is the argument it was passed as. An object whose denominator
  index no longer points at its denominator row (bayestestR keeps the
  index through `[`) is refused. Before this method such an object fell
  through to the draws route and came back as a meaningless parameters
  table.
* feat: `apa_inline()` reports both tables. A `loo` row prints `ΔELPD =
  −0.97, *SE* = 0.35` (the best model's `0.00` included), with `elpd`,
  `p_loo`, `looic` and `weight` on request; a `bf_models` row prints
  `*BF*~10~ = 6.38` against the denominator model, with `log_bf` and
  `post_prob` (`*P*(M | D)`) on request. A Bayes factor beyond what
  `exp()` can represent prints as its log, never as `∞` or `0`. Rows are
  addressed by `model`, and a Bayes-factor row also by `name`.
* feat: `papaja::apa_print()` works on `compare.loo` and
  `bayesfactor_models` objects, named by model.

* feat: `papaja::apa_print()` works on `brmsfit`, `stanreg`, `lavaan`,
  `blavaan` and `brmshypothesis` objects and on stored `apabayes_tidy`
  tables (Milestone 3, third slice). The methods are registered when
  papaja is installed, which stays optional, and call `apa_inline()`.
  Without a term the result has papaja's shape: `estimate`, `statistic`
  and `full_result` are named lists, one string per row, named by
  papaja's own rule (`apa_print(fit)$full_result$wt`,
  `$visual_x2`); with a term they are the strings `apa_inline()`
  returns. `in_paren = TRUE` writes brackets for parentheses. papaja's
  own methods for `BFBayesFactor` and `emmGrid` are left alone.
* feat: `apa_convergence()`, the convergence sentence (Milestone 3,
  second slice). From a fit or a stored diagnostics table it states
  R-hat, both effective sample sizes and the divergent transitions over
  every sampled quantity: `*R̂* ≤ 1.004, bulk ESS ≥ 1,240, tail ESS ≥
  980, no divergent transitions`. The extremes are stated as bounds and
  rounded away from the data (the largest R-hat up, the smallest ESS
  down), so that `≤` and `≥` hold for the unrounded numbers; the m3
  manuscript's `sprintf("%.3f")` printed `≤ 1.003` for 1.0034. When a
  value reaches a threshold (`rhat = 1.01`, `ess = 400`) the part says
  how many and gives the extreme (`2 of 13 *R̂* ≥ 1.01, maximum 1.018`).
  No word judges the fit; a `passed` flag and a `summary` data frame are
  on the object for code.
* feat: `apa_tidy_diagnostics()` records the number of divergent
  post-warmup transitions in a `divergences` attribute, read from the
  sampler's own record: `rstan::get_sampler_params()` on the stanfit
  inside a `brmsfit` (both backends), `stanreg` or `blavaan` fit, and the
  object's own `sampler_diagnostics()` on a `CmdStanMCMC`. It is `NA`,
  not 0, for draws, `mcmc.list` and runjags objects and for fits made by
  optimisation, variational inference or a sampler without that record.
  `rstan` joins Suggests.
* feat: `apa_tidy_diagnostics()` gains a `blavaan` method.
  `posterior::as_draws_df()` cannot read a blavaan fit, so the chains
  are `blavInspect(x, "mcmc")`, under the `coef()` names. Measured
  before it was written: the sampler object behind a blavaan fit is a
  stanfit on the default target but a `CmdStanMCMC` on `target =
  "cmdstan"`, and both are counted.
* feat: `apa_inline()` reports a `sem_fit` table. A lavaan row prints
  `χ²(24) = 85.31, *p* < .001, CFI = .931, TLI = .896, RMSEA = .092, 90%
  CI [.071, .114], SRMR = .065`, a blavaan row `PPP = .030, BRMSEA =
  .094, 90% HDI [.071, .119], BΓ̂ = .983, 90% HDI [.973, .990]`. The
  indices print with three decimals and χ² with two unless `digits` sets
  both, and `stats` selects among them. Under `interval = FALSE, digits =
  2, markup = "latex"` a blavaan row reproduces the miniQ manuscript's
  `fmt_bfit()` character for character.
* feat: `apa_tidy_sem_fit()` on a blavaan fit gains `centrality =
  c("median", "mean")`. The mean is blavaan's `EAP` summary, which is
  what the miniQ manuscript reports; without it its numbers could not be
  reproduced.
* fix: a diagnostics table no longer claims a median in its print
  header; its `centrality` attribute is `NA`.
* internal: an `apabayes_results` object may carry one string for the
  whole table instead of one per row, which the convergence sentence
  needs.

* feat: `apa_inline()`, the inline layer (Milestone 3, first slice).
  One row of a tidy table becomes the string a Results section quotes:
  `*b* = −5.34, 95% CrI [−6.85, −3.82], *pd* > .999` for a regression
  coefficient, `.45, 95% CrI [.33, .57], *pd* > .999` for a standardized
  path, `−5.36, 90% CrI [−6.69, −4.02], *BF*~10~ = ∞` for a
  `brms::hypothesis()` row, and R-hat with both effective sample sizes
  for a diagnostics row. The row is addressed by name only: a term, a
  label, a hypothesis string, or a structural-equation path by its two
  sides and operator (`apa_inline(x, "visual", "textual", op = "~~")`,
  order-insensitive for a covariance and for nothing else), with
  `group =` for a multi-group fit. No match or several matches is an
  error listing the candidates. Every number goes through the format
  layer; the interval label follows the row's own `ci_method`, so a table
  of HDIs says `HDI` and a lavaan table says `CI`. The default method
  runs `apa_tidy(x, ...)` first, so every class that has an extract
  route can be reported from the fit as well as from a stored table.
  Tables of type `sem_fit`, `loo`, `bf_models` and `contrasts` are
  refused by name until their slices land.
* feat: the `apabayes_results` object, the inline layer's return value:
  `estimate`, `statistic`, `full_result` and `table`, papaja's shape,
  inheriting papaja's `apa_results` class so that `papaja::apa_table()`
  accepts it. Measured before it was written: knitr's inline hook
  ignores `as.character`, `format` and `print` methods and prints every
  element of a list, so bare `` `r apa_inline(fit, "wt")` `` works
  through a `knit_print` method registered on knitr, which stays in
  Suggests. (papaja's own object needs `$full_result` inline for the
  same reason.)
* internal: `sem_term_parts()` in `R/extract-shared.R` splits a
  lavaan-style parameter name into its sides and operator, for the
  blavaan labels and the inline addressing alike; `blavaan_labels()` now
  uses it. No behaviour change.
* chore: the maintainer address is `gfrischkorn@icloud.com`, and the
  design record, the specs and the fixture builders are kept outside the
  tracked tree.
* feat: `apa_tidy()` gains a `brmshypothesis` method, for the output of
  `brms::hypothesis()`. It is the first route that calls no easystats
  function: easystats has no method for the class, so the numbers are
  brms's own `$hypothesis` table. No brms function is called either —
  the route reads list elements — though brms must be installed, because
  the table names the version that produced the numbers.
  `bf10` follows the reporting decision already recorded: the
  Savage–Dickey ratio of a point hypothesis is inverted to be a Bayes
  factor against equality, and the posterior odds of a directional one
  are kept as they are.
* feat: the `"hypotheses"` contract gains a `group` column.
  `brms::hypothesis(scope = "coef")` returns one row per hypothesis and
  group level, and without the column six rows of a three-level factor
  cannot be told apart.
* feat: `apa_tidy()` on a `brmshypothesis` reports the interval level
  **per row**. `brms::hypothesis()` takes the quantiles at `alpha/2` and
  `1 - alpha/2` for a point hypothesis but at `alpha` and `1 - alpha`
  for a directional one, so one object holds intervals of two masses —
  95% and 90% at the default `alpha`. The `ci_level` attribute states a
  level only when every row agrees; the column always says what each row
  is. An `alpha` of 0.5 or more, which `brms::hypothesis()` accepts and
  which turns a directional interval inside out, is refused by name.
* feat: neither the kind of a hypothesis nor what `Estimate` holds is
  stored by brms, and both are read off the object's own draws rather
  than assumed. A *named* hypothesis loses its operator (the
  `Hypothesis` string becomes the name, and a name may itself contain
  `<`), so `directional` is derived from which quantile pair the
  interval used; `directional =` overrides it, and a row that cannot be
  read is an error rather than a silent missing Bayes factor. Likewise
  `robust = TRUE` leaves no marker, so `centrality` is decided by
  comparing the estimate with the mean and the median of its draws.
* feat: the `ci_method` column and attribute accept `"spi"` and
  `"bci"`. The column is descriptive — it says what an interval is, and
  the result-object route reports tables apabayes did not compute, where
  the user chose the method. No route *offers* them: every `ci`
  argument is still `"eti"` or `"hdi"`, exactly as `"wald"` and
  `"boot"` are describable without being offered.
* feat: `apa_tidy()` and `apa_tidy_sem_fit()` gain a `blavaan` method,
  the Bayesian mirror of the lavaan route: a posterior median with a
  credible interval, pd and convergence diagnostics where lavaan has a
  maximum-likelihood estimate with a Wald interval and a p value, and
  `p` is `NA` throughout. The two fit-index rows are complementary —
  lavaan fills χ², CFI, TLI, RMSEA and SRMR, blavaan fills the posterior
  predictive p value, BRMSEA and BGammaHat, and each leaves the other's
  columns `NA`, because a blavaan fit carries none of them.
  `apa_tidy_sem_fit()` therefore drops `test` and `rmsea_level` on this
  method rather than accepting and ignoring them, and gains `pD`,
  `rescale` and `fit_ci_level` for `blavaan::blavFitIndices()`. `pD` is
  spelled as blavaan spells it, so that it is not read as the contract's
  `pd`, the probability of direction. The interval on that row is the
  highest-density one, because that is what reproduces the numbers
  `blavaan` itself publishes. A `blavaan` fit now
  dispatches to its own method; the lavaan guard stays for the method
  called by name.
* feat: `standardize` on a `blavaan` fit reads
  `blavaan::standardizedPosterior()`, not easystats — measured,
  `parameters::model_parameters(x, standardize = )` aborts for *every*
  value of the argument, `FALSE` included. The standardized solution
  covers the whole parameter table, so its rows are a **superset** of
  the unstandardized ones: the fixed marker loadings appear with a real
  interval, and under `"std.all"` the latent variances are exactly 1.
  Those rows carry full R-hat and ESS, because row *i* of that matrix
  was measured to be the standardization of draw *i* of the chains. They
  carry no component information, so `component` is `NA` there and
  cannot be combined with `standardize`; select rows with `variables`.
* feat: R-hat and both ESS columns of a `blavaan` fit come from
  `posterior::summarise_draws()` over the fit's own chains, and
  blavaan's are never computed. They **differ from what
  `blavaan::summary()` prints**, which reports `blavInspect(x, "rhat")`
  and `"neff"` — one ESS where the contract has two, from an estimator
  that is neither the bulk nor the tail ESS the "greater than 400" rule
  of thumb is defined for. The help page says so.
* feat: a multi-group `blavaan` fit is refused with a message naming the
  cause. `model_parameters()` aborts on one, and every fallback names
  the parameters differently, so reporting any of them would silently
  change what `term` means between fits.
* refactor: `apply_label_overrides()` shares the "`labels =` wins on the
  terms it names" step across all three label helpers, and
  `check_sem_component()` takes its vocabulary as an argument, since
  blavaan's component names (`latent`, `residual`) are not lavaan's six.
  `apa_tidy.lavaan()` and `apa_tidy_sem_fit.lavaan()` split their row
  assembly and their guards into helpers, bringing both back under the
  50-line guideline.
* feat: `apa_tidy()` gains a `lavaan` method, the extract layer's first
  frequentist route, and `apa_tidy_sem_fit()` arrives with it as a
  generic for the fit-index row (χ² with its df and p, CFI, TLI, RMSEA
  with its interval, SRMR; `test = "scaled"` or `"robust"` picks
  lavaan's variants, and a fit without a scaled test statistic is
  refused rather than given one). Estimates, standard errors, the
  interval and the p value come from `parameters::model_parameters()`
  and the fit indices from `lavaan::fitMeasures()`; apabayes computes no
  number of its own and nothing it prints judges a fit. `term` is
  lavaan's own parameter name (`visual=~x1`, with the `.g2` suffix from
  the second group of a multi-group fit) and `label` the same with
  spaces, qualified by the group label where there is one. `component`
  selects one or more of `"loading"`, `"regression"`, `"correlation"`,
  `"variance"`, `"mean"` and `"defined"`, validated before easystats
  sees it because a name it does not know silently yields zero rows.
  `standardize` takes `TRUE` (`"std.all"`) or a lavaan type string, and
  the `std` column records which solution the row is.
* fix: a fixed parameter of a lavaan fit — a marker loading, or a latent
  variance under `standardize` — reports `p = NA` as lavaan does.
  `parameters` writes `p = 0` there (measured: `p[is.na(p)] <- 0` in its
  source, on exactly the rows whose test statistic is `NA`), and a p
  value of 0 for a parameter that was never tested reads as
  significance. This is the one easystats number the route overrides,
  and it restores the upstream value rather than computing one.
* feat: the tidy contract gains `centrality = NA`, for a point estimate
  that is no posterior summary, and `ci_method` gains `"wald"` and
  `"boot"`. A lavaan fit gets a Wald interval, except that the
  unstandardized solution of a fit with `se = "bootstrap"` gets the
  percentile bootstrap interval and says so; the standardized solution
  of that same fit is Wald again, because lavaan takes it through the
  delta method.
* fix: `labels =` is applied by which terms it names rather than by
  which substitutions differ from the term. On a route whose derived
  label is not the term itself — `visual =~ x1` against `visual=~x1` —
  asking for the term as the label is a real request, and the old test
  read it as no request at all.
* feat: `apa_tidy()` reports an easystats **result object** — the table
  `bayestestR::describe_posterior()` or `parameters::model_parameters()`
  already computed — and not only a fitted model. Because every Bayesian
  model easystats can summarise returns a table inheriting
  `describe_posterior`, this one route covers model classes apabayes has
  no fit method for. Nothing is recomputed: the estimates, interval,
  `pd`, ROPE percentage and diagnostics are read off the table, and the
  reporting settings with them, so the method takes no `ci`, `ci_level`,
  `rope` or `diagnostics` argument. `centrality = NULL` reports whichever
  of the median and the mean the table holds and asks you to name one
  when it holds both. `ess_bulk` is always `NA` — no easystats table
  carries it; pass the fit for both ESS columns. A `parameters_model`
  table that is *not* a posterior summary (a frequentist model, or one
  computed with several `ci` levels) is refused rather than coerced to
  draws, which is what the coercing default would otherwise do to it.
* feat: `apa_tidy()` gains a `stanreg` method for rstanarm fits, the
  third route of the extract layer, with the brmsfit method's signature.
  Estimates, interval, pd and the optional ROPE come from
  `parameters::model_parameters()`, always called with `priors = FALSE`
  (the prior merge appends junk `NA` rows under `effects = "random"`)
  and `component = "all"` (the bare easystats default omits `sigma` on
  this class). R-hat and both ESS columns come from
  `posterior::summarise_draws()` over the fit's draws rather than from
  `bayestestR::diagnostic_posterior()`, which never covers `sigma` on a
  stanreg and disagrees numerically with `posterior` on the terms it
  does cover; every reported parameter therefore has a diagnostic.
  `apa_tidy_diagnostics()` already handled stanreg through its default
  method, so there is no `stanreg` method for it.
* internal: the brmsfit route's label derivation, optional-column
  reading and `variables =` resolution move to `R/extract-shared.R` as
  `parameters_labels()`, `optional_column()` and
  `resolve_parameters_variables()`, ahead of the stanreg route that uses
  them unchanged. No behaviour change.
* fix: `apa_tidy(fit, effects = "random")` on a model without random
  effects now says so ("`effects` is "random", but `x` has no random
  effects.") instead of surfacing easystats' `'by' must specify a
  uniquely valid column`. The check is `insight::is_mixed_model()` on
  the model formula, run before easystats is called; `insight` moves
  from Suggests to Imports for it, and `rstanarm` joins Suggests.
* internal: the shape every parameters route repeats — check the
  reporting arguments, call easystats once, match rows by name, assemble
  the contract columns — moves into `R/extract-shared.R`, and the
  `apabayes_tidy()` constructor splits out its input checks and its
  column assembly. No behaviour change: the same 585 tests pass either
  side of the refactor.
* feat: `apa_tidy()` gains a `brmsfit` method, the second route of the
  extract layer. Estimates, interval, pd and the optional ROPE come from
  `parameters::model_parameters()`; because that call returns no
  `ESS_bulk` column, R-hat and both ESS columns come from a second call
  to `bayestestR::diagnostic_posterior()`, joined by parameter name
  rather than by position (its row order differs and its default omits
  `sigma`). The `effects` and `group` columns are read defensively: they
  exist only for a model with random effects, and `""` becomes `NA`.
  Display labels come from the `pretty_names` attribute, disambiguated by
  group where two parameters share a label, so the inline layer's
  term-or-label addressing stays unique.
* feat: `apa_tidy_diagnostics()`, a generic for convergence diagnostics
  alone, with a coercing `default` method for anything
  `posterior::as_draws_df()` accepts and a `runjags` method. Its tables
  carry no interval, so `ci_method` and `ci_level` are `NA` and the print
  header does not claim an interval that is not there.
* feat: extract layer, tidy contract and draws route (Milestone 2).
  `apabayes_tidy()` builds the object the extract layer hands to the
  format, inline and table layers: a tibble whose columns are fixed by
  `type` (`"parameters"`, `"diagnostics"`, `"hypotheses"`, `"loo"`,
  `"bf_models"`, `"sem_fit"`, `"contrasts"`), with the reporting metadata
  as attributes. `validate_apabayes_tidy()` is the check every consumer
  runs on entry, `is_apabayes_tidy()` the predicate, and
  `print.apabayes_tidy()` writes what the numbers are above the tibble.
* feat: `apa_tidy()`, the extract generic, with methods for `draws`
  objects, a coercing `default` method for anything
  `posterior::as_draws_df()` accepts (`stanfit`, `CmdStanFit`, `mcmc`,
  `mcmc.list`, matrices, data frames) and one for `runjags`. Estimates,
  interval, pd and the ROPE percentage come from
  `bayestestR::describe_posterior()`, R-hat and ESS from
  `posterior::summarise_draws()`. Sampler variables (`lp__`, `lprior`,
  `prior_*`) are dropped by default and reported when named in
  `variables`.
* fix: `apabayes_tidy()` and `apa_tidy()` returned their table invisibly,
  so a bare call at the console printed nothing. Both now return visibly
  and reach `print.apabayes_tidy()`.
* feat: format layer (Milestone 1). `apa_num()`, `apa_p()`, `apa_pd()`,
  `apa_prob()`, `apa_ci()`, `apa_bf()`, `apa_er()` and `apa_rhat_ess()`
  turn numbers into APA-style text for four markup targets (`md`,
  `latex`, `typst`, `plain`), chosen from the `markup` argument,
  `options(apabayes.markup = )` or the knitr output format. `apa_bf()`
  prints Bayes factors as numbers only, with `direction = "01"` for
  BF01; `apa_bf_label()` is the opt-in helper that returns a verbal
  category from a named scheme (`"jeffreys"`, `"raftery"`), with the
  caveat on its help page.
* Package skeleton.
