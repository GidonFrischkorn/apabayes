# apabayes 0.0.0.9000

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
