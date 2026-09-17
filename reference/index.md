# Package index

## Extract

Turn a fitted model, posterior draws or a comparison into the tidy
contract.

- [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  : Extract a model into the apabayes tidy contract
- [`apa_tidy_sem_fit()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_sem_fit.md)
  : Fit indices of a structural equation model
- [`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
  : Convergence diagnostics for every sampled quantity
- [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  [`validate_apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  [`is_apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  [`print(`*`<apabayes_tidy>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  : The apabayes tidy contract

## Inline

One estimate, addressed by name, as an APA string.

- [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  : Report a result inline, in APA style

- [`apa_value()`](https://www.gfrischkorn.org/apabayes/reference/apa_value.md)
  : Read one value out of a tidy table

- [`is_apa_results()`](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
  [`print(`*`<apabayes_results>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
  [`format(`*`<apabayes_results>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
  [`as.character(`*`<apabayes_results>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
  [`knit_print(`*`<apabayes_results>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apa_results.md)
  : Inline results

- [`apa_print(`*`<apabayes_tidy>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print(`*`<brmsfit>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print(`*`<stanreg>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print(`*`<lavaan>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print(`*`<blavaan>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print(`*`<brmshypothesis>`*`)`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print.compare.loo()`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print.bayesfactor_models()`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print.bayesfactor_inclusion()`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print.estimate_contrasts()`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print.estimate_means()`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  [`apa_print.easycorrelation()`](https://www.gfrischkorn.org/apabayes/reference/apabayes-papaja.md)
  :

  Results for papaja's `apa_print()`

## Tables

A tidy table formatted for apa7::apa_flextable(), and its note.

- [`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  [`apa_note()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
  : Report a result as an APA table

## Number formatting

The format layer every inline string and table cell is built from.

- [`apa_num()`](https://www.gfrischkorn.org/apabayes/reference/apa_num.md)
  : Format numbers in APA style
- [`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  [`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  [`apa_prob()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)
  : Format p values, probabilities of direction and proportions
- [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  : Format Bayes factors
- [`apa_bf_label()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf_label.md)
  : Verbal category for a Bayes factor (opt-in)
- [`apa_er()`](https://www.gfrischkorn.org/apabayes/reference/apa_er.md)
  : Format evidence ratios
- [`apa_ci()`](https://www.gfrischkorn.org/apabayes/reference/apa_ci.md)
  : Format an interval
- [`apa_rhat_ess()`](https://www.gfrischkorn.org/apabayes/reference/apa_rhat_ess.md)
  : Format convergence diagnostics
- [`apa_convergence()`](https://www.gfrischkorn.org/apabayes/reference/apa_convergence.md)
  : Report convergence diagnostics in one sentence
