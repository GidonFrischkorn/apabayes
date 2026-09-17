# Changelog

## apabayes 0.1.0

- fix: every formatter rounds by apabayes’s own rule instead of the C
  library’s, so the same value prints the same string on every platform.
  The number is rounded as it is written, with ties going away from zero
  (`apa_num(0.005)` is `0.01`, `apa_num(2.675)` is `2.68`), and
  [`formatC()`](https://rdrr.io/r/base/formatc.html) only lays the
  rounded number out. Measured: the first CI run printed `.00` on
  Windows where macOS and Linux printed `.01`, because Windows rounds
  the 15-significant-digit decimal with ties to even while the others
  round the stored double. Values whose double sits just below the
  written midpoint now round up on macOS and Linux as well (`0.145`
  prints `0.15`, was `0.14`), and an exact binary tie goes away from
  zero rather than to even (`0.125` prints `0.13`, was `0.12`). A value
  that rounds to zero still prints without a sign (`.00`, never `-.00`),
  unchanged on every platform and now asserted directly rather than
  through a seed helper that keeps the sign. The regime boundaries of
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  and
  [`apa_er()`](https://www.gfrischkorn.org/apabayes/reference/apa_er.md)
  use the same rounding, so a value is put in the regime it is printed
  in.

- fix: the `modelbased` example guards on `marginaleffects` and
  `collapse` as well, the live test guards on the call itself, and
  `marginaleffects (>= 0.29.0)` joins `Suggests`. `estimate_contrasts()`
  does its work through marginaleffects, which is a Suggests of
  modelbased rather than a dependency, and marginaleffects does this
  path’s through collapse, so an installed modelbased was not enough: on
  a machine without either, the example run of `R CMD check` ERRORed.

- internal: a machine that cannot build a model now reports the live-fit
  tests as *not run* rather than as failing. `test_brms_fit()`,
  `test_bmm_fit()` and `test_blavaan_fit()` skip with the condition
  message when the fit itself errors, because apabayes fits nothing and
  a failure inside `brm()` or `bcfa()` says something about the
  environment and not about the package — `skip_on_cran()` and
  `skip_if_not_installed()` cannot see a missing C++ toolchain. The
  runjags test takes the JAGS binary from the PATH instead of a
  hardcoded Homebrew path, and the blavaan cmdstan-target test guards
  its fit the same way.

- internal: `apa_tidy_sem_fit(fit_ci_level =)` is tested by the property
  it promises — that the bounds are the HDI at the level asked for,
  computed from the same indices — rather than by a strict inequality
  between two bounds that the draws can make equal, which is how it
  failed on Windows.

- feat:
  [`apa_value()`](https://www.gfrischkorn.org/apabayes/reference/apa_value.md)
  reads one value out of a tidy table. It addresses a row exactly as
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  does — by term, by label, by the two sides of a structural-equation
  path, by a correlation’s pair in either order — and returns the number
  rather than the string, for a value that goes into arithmetic, into a
  comparison, or into wording the author formats themselves. `column`
  names the column and defaults to the value the table’s type is about
  (`estimate`, `elpd_diff` on a `loo` table, `bf` on a Bayes-factor
  one); a column that is `NA` on every addressed row is an error,
  because `NA` reaching a manuscript is the defect the refusal exists to
  stop. It replaces the hand-rolled lookup helper that indexes by
  position and reports the wrong row the day the table changes: 29 of
  the 43 sites that stayed hand-rolled in a real manuscript conversion
  were that request.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  on a
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  result takes `reference =`, the model the ELPD differences are taken
  from. loo signs every difference against the model with the highest
  ELPD, so a sentence reporting how far a baseline trails had to flip
  the sign by hand; under a new reference the model loo referenced
  prints a positive `ΔELPD`. Naming loo’s own reference changes nothing
  — measured, re-deriving the differences moves them by up to 1.2e-14,
  and loo’s numbers are the ones to report. The standard errors that
  survive are the ones loo measured: `0` on the reference row, loo’s own
  `se_diff` on the model that was loo’s reference (the same pair, read
  the other way round), `NA` elsewhere, because a `compare.loo` object
  does not carry the pointwise ELPDs another pair would need. `p_worse`
  and `diag_diff` qualify a difference against loo’s reference and
  become `NA` for the same reason.

- feat: `apa_inline(ci_label = NULL)` drops the separator with the
  label, so an estimate and its interval print as `−5.39 [−6.95, −3.78]`
  — the shape a sentence listing several estimates needs, and the one
  [`?apa_inline`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  already described. A row whose `ci_level` is `NA` still prints the
  brackets after a comma: that is the table’s silence about its own
  interval, not something the caller asked for.

- fix: `apabayes_tidy(type = "sem_fit")` requires at least one fit
  index. The `"sem_fit"` contract asked only for a `model` column, which
  a LOO comparison also has, so a model-comparison table was accepted as
  a table of fit indices with every index `NA`, and
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  then reported `NA` into a manuscript instead of refusing. The check is
  the constructor’s, where the caller chooses the label; a subset of a
  valid table stays valid.

- docs: the package’s own claims are checked against the package. The
  `Description` field and the README no longer name `performance` as a
  source of numbers — it is called nowhere in `R/` and is in neither
  `Imports` nor `Suggests` — and no longer say every number comes from
  easystats, which six deliberate non-easystats sources contradict
  ([`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html),
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
  [`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html),
  [`blavaan::standardizedPosterior()`](https://blavaan.org/reference/standardizedPosterior.html),
  [`rstan::get_sampler_params()`](https://mc-stan.org/rstan/reference/stanfit-class.html),
  and brms’s own hypothesis table). The README’s worked example is now
  an **evaluated** chunk reading a stored object, so its output is the
  package’s rather than hand-written: the block had printed an ASCII
  hyphen where apabayes emits U+2212, and the same unevaluated block had
  carried a call to a function that never existed. The README also
  gained the
  [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  stored-summary path, which the acceptance test called its single most
  useful discovery, a sentence saying that a plain numeric data frame is
  read as draws, and the apaquarto 6.0.0 precondition for Typst.

- internal: `table_parameters()` builds its `*pd*` and `% in ROPE`
  columns through `share_columns()`, the helper the contrasts and
  correlations tables already used, so a ROPE-formatting change cannot
  reach one table and miss the most-used one. No output changes. The
  short-draws guard in the draws route carries a comment naming the
  upstream string it matches and the test that fails loudly if
  bayestestR rewords it.

- fix: a brms fit on which
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  fails upstream now re-raises naming `apa_tidy(brms::as_draws_df(fit))`
  as the route that reads the draws directly and does not hit the
  failure, the same shape as the blavaan re-raise below. The trigger is
  any brms model with a matrix response and a `trials()` term
  (`family = multinomial()`, and so every bmm M3 fit): insight 1.5.4’s
  `get_data()` compares a two-name response with `&&`, an error since R
  4.3.0, and every easystats entry point goes through it. Measured on a
  real bmm M3 fit, a fresh 40-row `brm()` without bmm, and a real bmm
  diffusion (CSWald) fit, which is unaffected and reports `component` as
  the bmm model parameter with no bmm code. bmm fits are now in the test
  suite (bmm in Suggests).

- fix:
  [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
  and
  [`apa_convergence()`](https://www.gfrischkorn.org/apabayes/reference/apa_convergence.md)
  count divergent transitions on a brms fit sampled through cmdstanr.
  brms stores such a fit as a `stanfit` whose method is named `"sample"`
  (cmdstan’s word) where rstan writes `"sampling"`, and the reader took
  the difference for “no sampler record” and returned `NA`. bmm samples
  through cmdstanr whenever it is installed, so every bmm fit reported
  no record.

- fix:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  refuses a data frame or matrix too short to be a real posterior
  instead of reporting a confident but meaningless estimate.
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
  already warns “the posterior is too short, returning NAs” in this
  case; that warning is now promoted to an error, because a 2-draw
  posterior cannot produce a reportable interval and the caller was
  getting an estimate anyway (the mean of the “draws”). Found converting
  a real manuscript, where a stored 2 x 6 matrix of per-model
  convergence summaries (max R-hat, min ESS) was silently read as
  posterior draws.

- fix:
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  honours `digits` in `style = "auto"`’s scientific regime; it always
  printed a one-decimal mantissa there regardless of `digits`.
  `apa_bf(4.975034e14, digits = 2)` now returns `4.98 × 10^14^`, not
  `5.0 × 10^14^`. This makes
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  a true drop-in for a caller’s own two-decimal Bayes-factor formatter
  with no `style =` needed.

- feat: `apa_pd(operator = TRUE)` prepends `"= "` unless the value
  already opens with its own relation from the floor or the cap (`.956`
  becomes `= .956`; `> .999` is unchanged), so the result reads inside a
  sentence that already names the statistic (`*pd* {x}`) without the
  caller re-deriving which branch fired.

- fix: a blavaan fit on which
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  fails upstream now re-raises naming `standardize = TRUE` as the path
  that reads the posterior directly and does not hit the failure, rather
  than passing the upstream message through unnamed.

- docs:
  [`?apa_table`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  states its scope explicitly — one row per parameter, no pivot, no
  second estimate column — and the vignette “Reporting brms models in
  apaquarto” gained a worked example carrying
  [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  output through
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  into a cross-tabulated design table, plus a section naming five gaps
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  and
  [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  do not yet cover. All measured converting a real manuscript
  (miniQmetrics) rather than invented.

- feat: apabayes ships an apaquarto example and a vignette, and the test
  suite gained a gate that renders a document. `inst/apaquarto-example/`
  holds a minimal apaquarto manuscript with a README naming the two
  commands that render it, and the vignette “Reporting brms models in
  apaquarto” walks through
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md),
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md),
  [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  and
  [`apa_note()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md).
  Both build from stored posterior summaries in `inst/extdata`, so
  neither needs a compiler or a fit. `test-render.R` renders every
  contract type to all four apaquarto formats and reads the output back;
  it is opt-in behind `APABAYES_RENDER_TEST` and skips, with its reason,
  wherever the toolchain is absent. The reason it exists: the
  word-joiner defect below was in committed code at 100 % coverage with
  `R CMD check` clean, because nothing this project ran rendered a
  document.

- feat: Typst joins the promised formats. `apaquarto-typst` compiles
  from apaquarto 6.0.0, where it previously did not compile at all, so
  all four apaquarto formats are now rendered and read by the render
  test. On apaquarto 5.x Typst still fails, for reasons that are
  apaquarto’s and not apabayes’s.

- fix: the `sem_fit` table header for Bayesian gamma-hat is `BGammaHat`,
  not `BΓ̂`. The rendering font does not compose a combining circumflex
  over Greek capital Gamma in a table cell: it substitutes a spacing
  glyph that swallows the following space, in both PDF engines, so the
  header printed as `BΓˆ[90% HDI]`. The characters were correct, which
  is why reading the extracted text did not show it.
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  is unchanged and still says `BΓ̂`, which composes correctly in running
  text. The table note follows the header, so it now reads
  `BGammaHat = Bayesian gamma-hat`.

- fix: every bracket a table prints opens with a word joiner (U+2060),
  so an apaquarto PDF renders its intervals. apa7 writes each run of a
  cell as `\fontspec{Times New Roman} <text>`, and fontspec reads
  `\fontspec{font}[options]`, so a run beginning with `[` had the
  interval taken for a font option list: silently where the brackets
  balanced, and as `! Argument of \fontspec has an extra }.` where a
  split left them unbalanced. Either way the interval was lost, in every
  table that has one. The joiner is invisible, sits on the bracket
  rather than the cell so that a level or label prefix cannot strand it,
  and changes nothing else: the bounds keep their `,`, the columns keep
  their widths. Notes and
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  keep plain brackets, being prose rather than cells.

- feat:
  [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  reports the last three contract types, so every table
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  produces can be tabulated. A `sem_fit` table has one row per model:
  `Model` when any row names one, then `*χ*^2^`, `*df*` and `*p*` where
  the fit records them, and one column per fit index, each index whose
  interval the table records carrying that interval in the cell
  (`RMSEA [90% CI]`, `.092 [.071, .114]`) as a comparison table carries
  its standard error. The default shows the indices the table has a
  value for, so a lavaan fit gives the frequentist set and a blavaan fit
  PPP, BRMSEA and BΓ̂ with no argument, and the two stacked give both. A
  `contrasts` table has `Contrast`, `Group` when the contrasts were
  computed within one, the estimate and its interval, `*pd*` and
  `% in ROPE`. A `correlations` table names each pair in `Variable 1`
  and `Variable 2` rather than repeating lavaan’s `~~`, then `*r*`, its
  interval, `*pd*`, `*BF*~10~` and the pairwise `*n*`, which can differ
  between rows of one table.

- fix: a frequentist interval column is headed `95% CI (Wald)` or
  `95% CI (bootstrap)` rather than `95% CI`, and an interval whose
  method the table does not record is headed `95% Interval`. apa7 reads
  any header ending in `<digits>% CI` as a confidence-interval column of
  its own and re-formats it: a row with no bounds had its empty cell
  rewritten to `NA`.
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  still writes `95% CI` in running text, which apa7 never sees. A
  `ci_label` that would produce such a header is refused, naming it.

- feat:
  [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  also reports hypotheses, model comparisons, Bayes factor tables and
  inclusion Bayes factors. A `hypotheses` table has `Hypothesis`,
  `Group` when some row has one, the estimate and its interval as on a
  parameters table, and `*BF*~10~`, with `ER` and `*P*(H)` on request. A
  `loo` table has `Model`, `ΔELPD (*SE*)`, `ELPD (*SE*)` — the standard
  error travels inside the cell — `*p*~loo~` and `*w*`, with `LOOIC` on
  request, and the note names the model the differences are taken from.
  A `bf_models` table has `Model`, `*BF*~10~` against the denominator
  model (which prints `1.00`) and `Error (%)`, with `log(*BF*~10~)` and
  `*P*(M | D)` on request. A `bf_inclusion` table has `Term`,
  `*P*(incl)`, `*P*(incl | D)` and `*BF*~incl~`, or `*BF*~excl~` under
  `bf_direction = "01"`. A default `stats` is the reporting set, not
  every transform of the same evidence. Where
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  refuses a Bayes factor it cannot print as a number, a table keeps the
  row: an overflowed or underflowed one leaves its cell empty and brings
  in a log column, a missing or infinite inclusion Bayes factor leaves
  its cell empty, and the note names the terms and says why. Every
  header is the string the inline layer prints, so a table reads like
  the sentence beside it.

- fix:
  [`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
  [`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  and
  [`apa_prob()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  accept a probability that misses `[0, 1]` by floating-point rounding
  error — bayestestR returns a posterior inclusion probability of
  `1 + 2.2e-16` — instead of refusing it. A value genuinely outside the
  range is still an error.

- feat:
  [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  turns a parameters or diagnostics table, or any object
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  accepts, into the tibble
  [`apa7::apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.html)
  renders as it is: every value formatted to text and decimal-aligned,
  every header its APA markdown (`Predictor` or `Path`, `*Mdn*`,
  `95% CrI`, `*pd*`, `% in ROPE`, `*BF*~10~`, `*p*`, `*R̂*`, `ESS~bulk~`,
  `ESS~tail~`), and the options of
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  (`stats`, `interval`, `ci_label`, `digits`, `digits_prob`,
  `leading_zero`, `bf`, `bf_direction`). R-hat and ESS are opt-in on a
  parameters table; `group_rows = TRUE` adds a `Component` column for
  `apa_flextable(row_title_column = )`. No header is a column name apa7
  would re-format, so the numeric `CI_low`/`CI_high` path of apa7 0.1.3,
  which aborts, is never reached.
  [`apa_note()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  returns the table note — the abbreviations the table shows, the
  interval’s kind, the ROPE range, a count of divergent transitions,
  never a verdict — for the apaquarto chunk option
  `#| apa-note: !expr apa_note(tab)`, with the table made in an earlier
  chunk. Fit-index, contrast and correlation tables are refused by name
  for now.
  [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  shares its name with
  [`papaja::apa_table()`](https://rdrr.io/pkg/papaja/man/apa_table.html):
  whichever package is attached last masks the other.

- docs:
  [`?apa_p`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  and
  [`?apa_num`](https://www.gfrischkorn.org/apabayes/reference/apa_num.md)
  state that apa7 and papaja export functions of the same names and that
  the package attached last masks the others.
  [`apa7::apa_p()`](https://wjschne.github.io/apa7/reference/apa_p.html)
  prints two decimals where
  [`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  prints three;
  [`papaja::apa_num()`](https://rdrr.io/pkg/papaja/man/apa_num.html)
  prints a hyphen for the minus sign.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports a `BayesFactor` object (`ttestBF()`, `anovaBF()`,
  `regressionBF()`, `correlationBF()`, `contingencyTableBF()`, …) as a
  `bf_models` table read from the object itself: the denominator first,
  then each numerator, labelled as BayesFactor prints them
  (`"Intercept only"`, `"wt + hp"`, `"Alt., r=0.707"`), with the formula
  or full description in `name`, the method BayesFactor used
  (`"JZS (BayesFactor)"`, `"Jeffreys-beta* (BayesFactor)"`) and its
  proportional error.
  [`bayestestR::bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.html)
  on the same object keeps the numbers but calls every method JZS,
  relabels contingency models wrongly and drops the error. A numerator
  that is the denominator model is listed once. The `bf_models` contract
  gains an `error` column, and `apa_inline(stats = c("bf", "error"))`
  prints `*BF*~10~ = 4.5 × 10^6^ ± 1.3%`.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  on
  [`BayesFactor::posterior()`](https://rdrr.io/pkg/BayesFactor/man/posterior-methods.html)
  output reports the posterior through the draws route, without
  posterior’s warning about the S4 class. A `BFBayesFactorList` is
  refused (report a column, `x[, j]`), and so is a
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  table of a BayesFactor object, which summarises only the first
  numerator and can misalign its Bayes factors.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports inclusion Bayes factors from
  [`bayestestR::bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.html)
  as a new `bf_inclusion` table (`term`, `p_prior`, `p_posterior`, `bf`,
  `log_bf`; attributes for matched-model averaging and custom prior
  odds). The object does not record how the underlying Bayes factors
  were computed.
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  prints `*BF*~incl~ = 1.9 × 10^4^`, `*BF*~excl~` under
  `bf_direction = "01"`, and the prior and posterior inclusion
  probabilities on request; a row with no finite inclusion Bayes factor
  (a term in every model, or a probability rounded to 1) is refused
  rather than printed as `∞`.
  [`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
  works on the class.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports Bayesian correlations from
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  or
  [`correlation::cor_test()`](https://easystats.github.io/correlation/reference/cor_test.html)
  (with `bayesian = TRUE`) as a new `correlations` table: one row per
  pair, named `mpg~~wt`, with `var1`, `var2`, the posterior estimate,
  its interval, pd, the ROPE share, the Bayes factor and the pairwise
  `n`, all read from the table, plus the method and prior columns. The
  table records neither its interval nor its centrality, so `ci`
  (default `"hdi"`) and `centrality` (default `"median"`) name them,
  matching correlation’s own defaults; apabayes cannot check them.
  correlation records its `ci` argument without applying it (the bounds
  are always 95 %), so a table labelled with any other level is refused.
  Frequentist tables and tables with diagonal rows (`redundant = TRUE`)
  are refused.
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  prints a row as
  `*r* = −.82, 95% HDI [−.92, −.66], *pd* > .999, *BF*~10~ = 1.3 × 10^7^`,
  without the leading zero; `stats` adds `"rope"` and `"n"`. A row is
  addressed by its pair in either order (`apa_inline(x, "wt", "mpg")`),
  by its term, or by one variable when only one row has it.
  [`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
  works on the class. `correlation` and `BayesFactor` join Suggests.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports a table from
  [`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
  or
  [`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html)
  on a Bayesian fit as a `contrasts` table, the contract the emmeans
  route fills. Rows are named as modelbased names them (`6 - 4`,
  `6, auto - 4, auto`, a mean by its row values), a `by` variable’s
  values fill `group`, and every number, the ROPE share and its bounds
  included, is read from the table. The table records its interval only
  in its call, so `ci = NULL` reads it there (modelbased’s equal-tailed
  default when no `ci_method` was passed); when the call gives
  `ci_method` as a variable, `ci = "eti"` or `"hdi"` names it, and a
  `ci` that contradicts the call is refused. Tables from
  `backend = "emmeans"` (whose estimate column does not say which
  centrality it holds) and from frequentist models are refused.
  [`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
  works on both classes. `modelbased` joins Suggests.

- fix:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  no longer reads any data frame as draws. A plain data frame, tibble or
  data.table of numeric columns still is; a data frame with a class of
  its own (a modelbased or easystats table, a `bayesfactor_models`
  object) or a column that is not numeric is refused with a message
  naming the class or the columns, where
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  had silently turned it into a nonsense parameters table.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports an emmeans grid from a Bayesian fit — the contrasts of
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  or [`pairs()`](https://rdrr.io/r/graphics/pairs.html), or the marginal
  means of
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  — as a `contrasts` table: the contrast string (on a means grid, the
  row label as emmeans writes it, `cyl_f4`, `cyl_f4 auto`), the
  posterior median or mean, the interval, pd and on request the ROPE
  share, all from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
  `ci = "hdi"`, the default, is the highest-density interval
  `summary(emmGrid)` prints (emmeans’s HPD), reproduced bit for bit and
  labelled `HDI`, the same name as on every other route; `ci = "eti"`
  gives the equal-tailed one. A `by` variable’s values fill the
  contract’s new `group` column and its name the `by` attribute; the
  contract also gained `rope_pct`. Every grid variable is kept as an
  extra column. A grid without posterior draws (a frequentist fit) and
  an `emm_list` (`emmeans(fit, pairwise ~ f)`, two tables of different
  kinds) are refused with a message. `emmeans` joins Suggests.

- feat:
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  reports a `contrasts` row like a parameters row,
  `4.28, 95% HDI [1.38, 7.01], *pd* = .998`, with the ROPE share when
  the table carries one and no symbol unless `symbol` gives one. A row
  is addressed by its contrast string, and by `group` when the contrast
  repeats across `by` groups. papaja’s own `apa_print.emmGrid()` is left
  alone; a Bayesian grid is reported through
  `apa_print(apa_tidy(grid))`.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports a LOO model comparison from
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  as a `loo` table: per model the ELPD difference to the best model and
  its SE, the model’s ELPD and SE, `p_loo`, LOOIC and loo’s own
  `p_worse` and diagnostic flags. Both shapes are read: the data frame
  loo 2.10.0 and later return and the matrix of earlier versions, which
  keeps the models in its row names. The comparison carries no weights,
  so `weights =` takes a
  [`loo::loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html)
  result (its kind, stacking, pseudo-BMA+ or pseudo-BMA, goes into the
  `weight_method` attribute) or a numeric vector named by model, matched
  by name. `loo` joins Suggests.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports a Bayes-factor model comparison from
  [`bayestestR::bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.html)
  as a `bf_models` table: `bf`, the natural-log `log_bf` next to it, the
  denominator row, the method (bridge sampling, the BIC approximation,
  BayesFactor’s JZS) and `post_prob`, the posterior model probability
  under equal prior odds, computed by log-sum-exp so that it stays
  finite where [`exp()`](https://rdrr.io/r/base/Log.html) overflows.
  `model` is the model as bayestestR names it; the new column `name` is
  the argument it was passed as. An object whose denominator index no
  longer points at its denominator row (bayestestR keeps the index
  through `[`) is refused. Before this method such an object fell
  through to the draws route and came back as a meaningless parameters
  table.

- feat:
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  reports both tables. A `loo` row prints `ΔELPD = −0.97, *SE* = 0.35`
  (the best model’s `0.00` included), with `elpd`, `p_loo`, `looic` and
  `weight` on request; a `bf_models` row prints `*BF*~10~ = 6.38`
  against the denominator model, with `log_bf` and `post_prob`
  (`*P*(M | D)`) on request. A Bayes factor beyond what
  [`exp()`](https://rdrr.io/r/base/Log.html) can represent prints as its
  log, never as `∞` or `0`. Rows are addressed by `model`, and a
  Bayes-factor row also by `name`.

- feat:
  [`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
  works on `compare.loo` and `bayesfactor_models` objects, named by
  model.

- feat:
  [`papaja::apa_print()`](https://rdrr.io/pkg/papaja/man/apa_print.html)
  works on `brmsfit`, `stanreg`, `lavaan`, `blavaan` and
  `brmshypothesis` objects and on stored `apabayes_tidy` tables
  (Milestone 3, third slice). The methods are registered when papaja is
  installed, which stays optional, and call
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md).
  Without a term the result has papaja’s shape: `estimate`, `statistic`
  and `full_result` are named lists, one string per row, named by
  papaja’s own rule (`apa_print(fit)$full_result$wt`, `$visual_x2`);
  with a term they are the strings
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  returns. `in_paren = TRUE` writes brackets for parentheses. papaja’s
  own methods for `BFBayesFactor` and `emmGrid` are left alone.

- feat:
  [`apa_convergence()`](https://www.gfrischkorn.org/apabayes/reference/apa_convergence.md),
  the convergence sentence (Milestone 3, second slice). From a fit or a
  stored diagnostics table it states R-hat, both effective sample sizes
  and the divergent transitions over every sampled quantity:
  `*R̂* ≤ 1.004, bulk ESS ≥ 1,240, tail ESS ≥ 980, no divergent transitions`.
  The extremes are stated as bounds and rounded away from the data (the
  largest R-hat up, the smallest ESS down), so that `≤` and `≥` hold for
  the unrounded numbers; the m3 manuscript’s `sprintf("%.3f")` printed
  `≤ 1.003` for 1.0034. When a value reaches a threshold (`rhat = 1.01`,
  `ess = 400`) the part says how many and gives the extreme
  (`2 of 13 *R̂* ≥ 1.01, maximum 1.018`). No word judges the fit; a
  `passed` flag and a `summary` data frame are on the object for code.

- feat:
  [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
  records the number of divergent post-warmup transitions in a
  `divergences` attribute, read from the sampler’s own record:
  [`rstan::get_sampler_params()`](https://mc-stan.org/rstan/reference/stanfit-class.html)
  on the stanfit inside a `brmsfit` (both backends), `stanreg` or
  `blavaan` fit, and the object’s own `sampler_diagnostics()` on a
  `CmdStanMCMC`. It is `NA`, not 0, for draws, `mcmc.list` and runjags
  objects and for fits made by optimisation, variational inference or a
  sampler without that record. `rstan` joins Suggests.

- feat:
  [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
  gains a `blavaan` method.
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  cannot read a blavaan fit, so the chains are `blavInspect(x, "mcmc")`,
  under the [`coef()`](https://rdrr.io/r/stats/coef.html) names.
  Measured before it was written: the sampler object behind a blavaan
  fit is a stanfit on the default target but a `CmdStanMCMC` on
  `target = "cmdstan"`, and both are counted.

- feat:
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  reports a `sem_fit` table. A lavaan row prints
  `χ²(24) = 85.31, *p* < .001, CFI = .931, TLI = .896, RMSEA = .092, 90% CI [.071, .114], SRMR = .065`,
  a blavaan row
  `PPP = .030, BRMSEA = .094, 90% HDI [.071, .119], BΓ̂ = .983, 90% HDI [.973, .990]`.
  The indices print with three decimals and χ² with two unless `digits`
  sets both, and `stats` selects among them. Under
  `interval = FALSE, digits = 2, markup = "latex"` a blavaan row
  reproduces the miniQ manuscript’s `fmt_bfit()` character for
  character.

- feat:
  [`apa_tidy_sem_fit()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
  on a blavaan fit gains `centrality = c("median", "mean")`. The mean is
  blavaan’s `EAP` summary, which is what the miniQ manuscript reports;
  without it its numbers could not be reproduced.

- fix: a diagnostics table no longer claims a median in its print
  header; its `centrality` attribute is `NA`.

- internal: an `apabayes_results` object may carry one string for the
  whole table instead of one per row, which the convergence sentence
  needs.

- feat:
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md),
  the inline layer (Milestone 3, first slice). One row of a tidy table
  becomes the string a Results section quotes:
  `*b* = −5.34, 95% CrI [−6.85, −3.82], *pd* > .999` for a regression
  coefficient, `.45, 95% CrI [.33, .57], *pd* > .999` for a standardized
  path, `−5.36, 90% CrI [−6.69, −4.02], *BF*~10~ = ∞` for a
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
  row, and R-hat with both effective sample sizes for a diagnostics row.
  The row is addressed by name only: a term, a label, a hypothesis
  string, or a structural-equation path by its two sides and operator
  (`apa_inline(x, "visual", "textual", op = "~~")`, order-insensitive
  for a covariance and for nothing else), with `group =` for a
  multi-group fit. No match or several matches is an error listing the
  candidates. Every number goes through the format layer; the interval
  label follows the row’s own `ci_method`, so a table of HDIs says `HDI`
  and a lavaan table says `CI`. The default method runs
  `apa_tidy(x, ...)` first, so every class that has an extract route can
  be reported from the fit as well as from a stored table. Tables of
  type `sem_fit`, `loo`, `bf_models` and `contrasts` are refused by name
  until their slices land.

- feat: the `apabayes_results` object, the inline layer’s return value:
  `estimate`, `statistic`, `full_result` and `table`, papaja’s shape,
  inheriting papaja’s `apa_results` class so that
  [`papaja::apa_table()`](https://rdrr.io/pkg/papaja/man/apa_table.html)
  accepts it. Measured before it was written: knitr’s inline hook
  ignores `as.character`, `format` and `print` methods and prints every
  element of a list, so bare `` `r apa_inline(fit, "wt")` `` works
  through a `knit_print` method registered on knitr, which stays in
  Suggests. (papaja’s own object needs `$full_result` inline for the
  same reason.)

- internal: `sem_term_parts()` in `R/extract-shared.R` splits a
  lavaan-style parameter name into its sides and operator, for the
  blavaan labels and the inline addressing alike; `blavaan_labels()` now
  uses it. No behaviour change.

- chore: the maintainer address is `gfrischkorn@icloud.com`, and the
  design record, the specs and the fixture builders are kept outside the
  tracked tree.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  gains a `brmshypothesis` method, for the output of
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html).
  It is the first route that calls no easystats function: easystats has
  no method for the class, so the numbers are brms’s own `$hypothesis`
  table. No brms function is called either — the route reads list
  elements — though brms must be installed, because the table names the
  version that produced the numbers. `bf10` follows the reporting
  decision already recorded: the Savage–Dickey ratio of a point
  hypothesis is inverted to be a Bayes factor against equality, and the
  posterior odds of a directional one are kept as they are.

- feat: the `"hypotheses"` contract gains a `group` column.
  `brms::hypothesis(scope = "coef")` returns one row per hypothesis and
  group level, and without the column six rows of a three-level factor
  cannot be told apart.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  on a `brmshypothesis` reports the interval level **per row**.
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
  takes the quantiles at `alpha/2` and `1 - alpha/2` for a point
  hypothesis but at `alpha` and `1 - alpha` for a directional one, so
  one object holds intervals of two masses — 95% and 90% at the default
  `alpha`. The `ci_level` attribute states a level only when every row
  agrees; the column always says what each row is. An `alpha` of 0.5 or
  more, which
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
  accepts and which turns a directional interval inside out, is refused
  by name.

- feat: neither the kind of a hypothesis nor what `Estimate` holds is
  stored by brms, and both are read off the object’s own draws rather
  than assumed. A *named* hypothesis loses its operator (the
  `Hypothesis` string becomes the name, and a name may itself contain
  `<`), so `directional` is derived from which quantile pair the
  interval used; `directional =` overrides it, and a row that cannot be
  read is an error rather than a silent missing Bayes factor. Likewise
  `robust = TRUE` leaves no marker, so `centrality` is decided by
  comparing the estimate with the mean and the median of its draws.

- feat: the `ci_method` column and attribute accept `"spi"` and `"bci"`.
  The column is descriptive — it says what an interval is, and the
  result-object route reports tables apabayes did not compute, where the
  user chose the method. No route *offers* them: every `ci` argument is
  still `"eti"` or `"hdi"`, exactly as `"wald"` and `"boot"` are
  describable without being offered.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  and
  [`apa_tidy_sem_fit()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
  gain a `blavaan` method, the Bayesian mirror of the lavaan route: a
  posterior median with a credible interval, pd and convergence
  diagnostics where lavaan has a maximum-likelihood estimate with a Wald
  interval and a p value, and `p` is `NA` throughout. The two fit-index
  rows are complementary — lavaan fills χ², CFI, TLI, RMSEA and SRMR,
  blavaan fills the posterior predictive p value, BRMSEA and BGammaHat,
  and each leaves the other’s columns `NA`, because a blavaan fit
  carries none of them.
  [`apa_tidy_sem_fit()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
  therefore drops `test` and `rmsea_level` on this method rather than
  accepting and ignoring them, and gains `pD`, `rescale` and
  `fit_ci_level` for
  [`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html).
  `pD` is spelled as blavaan spells it, so that it is not read as the
  contract’s `pd`, the probability of direction. The interval on that
  row is the highest-density one, because that is what reproduces the
  numbers `blavaan` itself publishes. A `blavaan` fit now dispatches to
  its own method; the lavaan guard stays for the method called by name.

- feat: `standardize` on a `blavaan` fit reads
  [`blavaan::standardizedPosterior()`](https://blavaan.org/reference/standardizedPosterior.html),
  not easystats — measured,
  `parameters::model_parameters(x, standardize = )` aborts for *every*
  value of the argument, `FALSE` included. The standardized solution
  covers the whole parameter table, so its rows are a **superset** of
  the unstandardized ones: the fixed marker loadings appear with a real
  interval, and under `"std.all"` the latent variances are exactly 1.
  Those rows carry full R-hat and ESS, because row *i* of that matrix
  was measured to be the standardization of draw *i* of the chains. They
  carry no component information, so `component` is `NA` there and
  cannot be combined with `standardize`; select rows with `variables`.

- feat: R-hat and both ESS columns of a `blavaan` fit come from
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)
  over the fit’s own chains, and blavaan’s are never computed. They
  **differ from what `blavaan::summary()` prints**, which reports
  `blavInspect(x, "rhat")` and `"neff"` — one ESS where the contract has
  two, from an estimator that is neither the bulk nor the tail ESS the
  “greater than 400” rule of thumb is defined for. The help page says
  so.

- feat: a multi-group `blavaan` fit is refused with a message naming the
  cause. `model_parameters()` aborts on one, and every fallback names
  the parameters differently, so reporting any of them would silently
  change what `term` means between fits.

- refactor: `apply_label_overrides()` shares the “`labels =` wins on the
  terms it names” step across all three label helpers, and
  `check_sem_component()` takes its vocabulary as an argument, since
  blavaan’s component names (`latent`, `residual`) are not lavaan’s six.
  [`apa_tidy.lavaan()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  and
  [`apa_tidy_sem_fit.lavaan()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
  split their row assembly and their guards into helpers, bringing both
  back under the 50-line guideline.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  gains a `lavaan` method, the extract layer’s first frequentist route,
  and
  [`apa_tidy_sem_fit()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
  arrives with it as a generic for the fit-index row (χ² with its df and
  p, CFI, TLI, RMSEA with its interval, SRMR; `test = "scaled"` or
  `"robust"` picks lavaan’s variants, and a fit without a scaled test
  statistic is refused rather than given one). Estimates, standard
  errors, the interval and the p value come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  and the fit indices from
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html);
  apabayes computes no number of its own and nothing it prints judges a
  fit. `term` is lavaan’s own parameter name (`visual=~x1`, with the
  `.g2` suffix from the second group of a multi-group fit) and `label`
  the same with spaces, qualified by the group label where there is one.
  `component` selects one or more of `"loading"`, `"regression"`,
  `"correlation"`, `"variance"`, `"mean"` and `"defined"`, validated
  before easystats sees it because a name it does not know silently
  yields zero rows. `standardize` takes `TRUE` (`"std.all"`) or a lavaan
  type string, and the `std` column records which solution the row is.

- fix: a fixed parameter of a lavaan fit — a marker loading, or a latent
  variance under `standardize` — reports `p = NA` as lavaan does.
  `parameters` writes `p = 0` there (measured: `p[is.na(p)] <- 0` in its
  source, on exactly the rows whose test statistic is `NA`), and a p
  value of 0 for a parameter that was never tested reads as
  significance. This is the one easystats number the route overrides,
  and it restores the upstream value rather than computing one.

- feat: the tidy contract gains `centrality = NA`, for a point estimate
  that is no posterior summary, and `ci_method` gains `"wald"` and
  `"boot"`. A lavaan fit gets a Wald interval, except that the
  unstandardized solution of a fit with `se = "bootstrap"` gets the
  percentile bootstrap interval and says so; the standardized solution
  of that same fit is Wald again, because lavaan takes it through the
  delta method.

- fix: `labels =` is applied by which terms it names rather than by
  which substitutions differ from the term. On a route whose derived
  label is not the term itself — `visual =~ x1` against `visual=~x1` —
  asking for the term as the label is a real request, and the old test
  read it as no request at all.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  reports an easystats **result object** — the table
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
  or
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  already computed — and not only a fitted model. Because every Bayesian
  model easystats can summarise returns a table inheriting
  `describe_posterior`, this one route covers model classes apabayes has
  no fit method for. Nothing is recomputed: the estimates, interval,
  `pd`, ROPE percentage and diagnostics are read off the table, and the
  reporting settings with them, so the method takes no `ci`, `ci_level`,
  `rope` or `diagnostics` argument. `centrality = NULL` reports
  whichever of the median and the mean the table holds and asks you to
  name one when it holds both. `ess_bulk` is always `NA` — no easystats
  table carries it; pass the fit for both ESS columns. A
  `parameters_model` table that is *not* a posterior summary (a
  frequentist model, or one computed with several `ci` levels) is
  refused rather than coerced to draws, which is what the coercing
  default would otherwise do to it.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  gains a `stanreg` method for rstanarm fits, the third route of the
  extract layer, with the brmsfit method’s signature. Estimates,
  interval, pd and the optional ROPE come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  always called with `priors = FALSE` (the prior merge appends junk `NA`
  rows under `effects = "random"`) and `component = "all"` (the bare
  easystats default omits `sigma` on this class). R-hat and both ESS
  columns come from
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)
  over the fit’s draws rather than from
  [`bayestestR::diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.html),
  which never covers `sigma` on a stanreg and disagrees numerically with
  `posterior` on the terms it does cover; every reported parameter
  therefore has a diagnostic.
  [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
  already handled stanreg through its default method, so there is no
  `stanreg` method for it.

- internal: the brmsfit route’s label derivation, optional-column
  reading and `variables =` resolution move to `R/extract-shared.R` as
  `parameters_labels()`, `optional_column()` and
  `resolve_parameters_variables()`, ahead of the stanreg route that uses
  them unchanged. No behaviour change.

- fix: `apa_tidy(fit, effects = "random")` on a model without random
  effects now says so (“`effects` is”random”, but `x` has no random
  effects.”) instead of surfacing easystats’
  `'by' must specify a uniquely valid column`. The check is
  [`insight::is_mixed_model()`](https://easystats.github.io/insight/reference/is_mixed_model.html)
  on the model formula, run before easystats is called; `insight` moves
  from Suggests to Imports for it, and `rstanarm` joins Suggests.

- internal: the shape every parameters route repeats — check the
  reporting arguments, call easystats once, match rows by name, assemble
  the contract columns — moves into `R/extract-shared.R`, and the
  [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  constructor splits out its input checks and its column assembly. No
  behaviour change: the same 585 tests pass either side of the refactor.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  gains a `brmsfit` method, the second route of the extract layer.
  Estimates, interval, pd and the optional ROPE come from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html);
  because that call returns no `ESS_bulk` column, R-hat and both ESS
  columns come from a second call to
  [`bayestestR::diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.html),
  joined by parameter name rather than by position (its row order
  differs and its default omits `sigma`). The `effects` and `group`
  columns are read defensively: they exist only for a model with random
  effects, and `""` becomes `NA`. Display labels come from the
  `pretty_names` attribute, disambiguated by group where two parameters
  share a label, so the inline layer’s term-or-label addressing stays
  unique.

- feat:
  [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md),
  a generic for convergence diagnostics alone, with a coercing `default`
  method for anything
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  accepts and a `runjags` method. Its tables carry no interval, so
  `ci_method` and `ci_level` are `NA` and the print header does not
  claim an interval that is not there.

- feat: extract layer, tidy contract and draws route (Milestone 2).
  [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  builds the object the extract layer hands to the format, inline and
  table layers: a tibble whose columns are fixed by `type`
  (`"parameters"`, `"diagnostics"`, `"hypotheses"`, `"loo"`,
  `"bf_models"`, `"sem_fit"`, `"contrasts"`), with the reporting
  metadata as attributes.
  [`validate_apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  is the check every consumer runs on entry,
  [`is_apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  the predicate, and
  [`print.apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  writes what the numbers are above the tibble.

- feat:
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md),
  the extract generic, with methods for `draws` objects, a coercing
  `default` method for anything
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
  accepts (`stanfit`, `CmdStanFit`, `mcmc`, `mcmc.list`, matrices, data
  frames) and one for `runjags`. Estimates, interval, pd and the ROPE
  percentage come from
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),
  R-hat and ESS from
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html).
  Sampler variables (`lp__`, `lprior`, `prior_*`) are dropped by default
  and reported when named in `variables`.

- fix:
  [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  and
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  returned their table invisibly, so a bare call at the console printed
  nothing. Both now return visibly and reach
  [`print.apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md).

- feat: format layer (Milestone 1).
  [`apa_num()`](https://www.gfrischkorn.org/apabayes/reference/apa_num.md),
  [`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
  [`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
  [`apa_prob()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
  [`apa_ci()`](https://www.gfrischkorn.org/apabayes/reference/apa_ci.md),
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md),
  [`apa_er()`](https://www.gfrischkorn.org/apabayes/reference/apa_er.md)
  and
  [`apa_rhat_ess()`](https://www.gfrischkorn.org/apabayes/reference/apa_rhat_ess.md)
  turn numbers into APA-style text for four markup targets (`md`,
  `latex`, `typst`, `plain`), chosen from the `markup` argument,
  `options(apabayes.markup = )` or the knitr output format.
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  prints Bayes factors as numbers only, with `direction = "01"` for
  BF01;
  [`apa_bf_label()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf_label.md)
  is the opt-in helper that returns a verbal category from a named
  scheme (`"jeffreys"`, `"raftery"`), with the caveat on its help page.

- Package skeleton.
