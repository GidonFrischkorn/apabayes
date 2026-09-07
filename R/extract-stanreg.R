# The stanreg route (ARCHITECTURE.md decision 18, the stanreg row). The
# brmsfit route with a different easystats call, specified against the
# object's own measured shape (dev/specs/spec-apa_tidy_stanreg.md), which
# differs from brmsfit in three ways that matter here: the bare
# `model_parameters()` default omits `sigma` (so `component = "all"` is
# always passed), `priors = TRUE` corrupts the row set under
# `effects = "random"` (so `priors = FALSE` is always passed), and
# `diagnostic_posterior()` never covers `sigma` (so the diagnostics come
# from `posterior::summarise_draws()` over the fit's own draws).

#' @describeIn apa_tidy A `stanreg` object from rstanarm. Estimates,
#'   interval, pd and the ROPE percentage come from
#'   [parameters::model_parameters()] with `priors = FALSE`; R-hat and
#'   both ESS columns from [posterior::summarise_draws()] over the fit's
#'   draws, which cover every reported parameter including `sigma`.
#'   apabayes computes no summary of its own.
#' @export
apa_tidy.stanreg <- function(x,
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
                             ...) {
  # Guarded for the same reason as `brms`: a stanreg restored with
  # `readRDS()` keeps its class without the package. `posterior` is a
  # hard dependency of rstanarm, so the second guard only ever speaks
  # when the first would have; it names what this route needs it for.
  rlang::check_installed("rstanarm", reason = "to read stanreg objects.")
  rlang::check_installed("posterior", reason = "to read stanreg draws.")
  effects <- rlang::arg_match(effects)
  check_random_effects(x, effects)
  centrality <- rlang::arg_match(centrality)
  ci <- rlang::arg_match(ci)
  rope <- check_route_args(ci_level, diagnostics, rope, rope_ci)

  # `component` is not a formal of `model_parameters.stanreg()`; it
  # travels through `...` and is honoured (measured). `priors = FALSE`:
  # the contract has no prior columns, and the prior merge is what adds
  # the junk NA rows under `effects = "random"`.
  mp <- call_model_parameters(
    x, centrality, ci, ci_level, rope, rope_ci,
    effects = effects, component = component, priors = FALSE
  )
  terms <- resolve_parameters_variables(variables, mp$Parameter)
  out <- parameters_rows(mp, terms, labels, centrality, ci, ci_level, rope)
  if (diagnostics) {
    out[c("rhat", "ess_bulk", "ess_tail")] <- stanreg_diagnostics(x, terms)
  }

  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = class(x),
    packages = c("rstanarm", "parameters", "posterior", "apabayes"),
    rope = rope, rope_ci = rope_ci
  )
  rlang::exec(apabayes_tidy, out, !!!extra)
}

# R-hat and both ESS columns from the fit's own draws. `as_draws_df()`
# keeps the chains and names the variables exactly as `model_parameters()`
# names its `Parameter` (measured on both fits), so the draws route's
# `draws_diagnostics()` serves unchanged and every reported term,
# `sigma` and the `b[...]` deviations included, gets a row.
stanreg_diagnostics <- function(x, terms) {
  draws <- posterior::subset_draws(posterior::as_draws_df(x), variable = terms)
  draws_diagnostics(draws, terms)
}
