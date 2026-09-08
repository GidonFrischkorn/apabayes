# apabayes 0.0.0.9000

* feat: `apa_tidy()` and `apa_tidy_sem_fit()` gain a `blavaan` method,
  the Bayesian mirror of the lavaan route: a posterior median with a
  credible interval, pd and convergence diagnostics where lavaan has a
  maximum-likelihood estimate with a Wald interval and a p value, and
  `p` is `NA` throughout. The two fit-index rows are complementary —
  lavaan fills χ², CFI, TLI, RMSEA and SRMR, blavaan fills the posterior
  predictive p value, BRMSEA and BGammaHat, and each leaves the other's
  columns `NA`, because a blavaan fit carries none of them.
  `apa_tidy_sem_fit()` therefore drops `test` and `rmsea_level` on this
  method rather than accepting and ignoring them, and gains `pd`,
  `rescale` and `fit_ci_level` for `blavaan::blavFitIndices()`. The
  interval on that row is the highest-density one, because that is what
  reproduces the numbers `blavaan` itself publishes. A `blavaan` fit now
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
* Package skeleton and design record (`ARCHITECTURE.md`).
