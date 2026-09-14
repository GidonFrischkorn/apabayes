# The brmsfit route (ARCHITECTURE.md decisions 2, 7 and 18). Unlike the
# draws route, easystats reads a brmsfit directly, so `parameters` owns
# the estimates and `bayestestR` the diagnostics; apabayes selects,
# renames to the contract and arranges. Shapes measured 2026-09-07 and
# recorded in dev/specs/spec-apa_tidy_brmsfit.md; the two that bite are
# that `model_parameters()` returns no `ESS_bulk` column and that
# `diagnostic_posterior()` returns its rows in a different order.

# ---- the parameters method ---------------------------------------------

#' @describeIn apa_tidy A `brmsfit`, and by inheritance a `bmmfit`.
#'   Estimates, interval, pd and the ROPE percentage come from
#'   [parameters::model_parameters()], R-hat and both ESS columns from
#'   [bayestestR::diagnostic_posterior()]; apabayes computes no summary
#'   of its own.
#'
#' @param effects Which parameters to report, passed to
#'   [parameters::model_parameters()]: `"fixed"` (the easystats default,
#'   population-level parameters and the distributional ones such as
#'   `sigma`), `"all"` (adds the group-level standard deviations and
#'   correlations) or `"random"`. Asking for `"random"` from a model that
#'   has no random effects is an error, decided by
#'   [insight::is_mixed_model()] before easystats is called.
#' @param component Which model component to report, passed to
#'   [parameters::model_parameters()]. `"all"` is the easystats default.
#' @export
apa_tidy.brmsfit <- function(x,
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
  # No `check_installed("parameters")`: it is in Imports (decision 13), so
  # it is always installed. `brms` is in Suggests and is guarded, because
  # holding a `brmsfit` does not imply having `brms`: a fit read back with
  # `readRDS()` on another machine keeps its class attribute, dispatches
  # here, and would otherwise fail deep inside `model_parameters()`.
  rlang::check_installed("brms", reason = "to read brmsfit objects.")
  effects <- rlang::arg_match(effects)
  check_random_effects(x, effects)
  centrality <- rlang::arg_match(centrality)
  ci <- rlang::arg_match(ci)
  rope <- check_route_args(ci_level, diagnostics, rope, rope_ci)

  mp <- call_model_parameters(
    x, centrality, ci, ci_level, rope, rope_ci,
    effects = effects, component = component
  )
  terms <- resolve_parameters_variables(variables, mp$Parameter)
  out <- parameters_rows(mp, terms, labels, centrality, ci, ci_level)
  if (diagnostics) {
    out[c("rhat", "ess_bulk", "ess_tail")] <- brms_diagnostics(x, terms)
  }

  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = class(x),
    packages = c("brms", "parameters", "bayestestR", "apabayes"),
    rope = rope, rope_ci = rope_ci
  )
  rlang::exec(apabayes_tidy, out, !!!extra)
}

# R-hat and both ESS columns. `model_parameters()` returns `ESS_tail`
# only, so this is a second call; `diagnostic_posterior()`'s default omits
# `sigma`, so it is called with effects = "all", component = "all"; and
# its row order differs from `model_parameters()`, so rows are matched by
# `Parameter` and never by position.
brms_diagnostics <- function(x, terms) {
  dp <- as.data.frame(bayestestR::diagnostic_posterior(
    x,
    effects = "all", component = "all"
  ))
  row <- match(terms, dp$Parameter)
  list(
    rhat = dp$Rhat[row],
    ess_bulk = dp$ESS_bulk[row],
    ess_tail = dp$ESS_tail[row]
  )
}

# ---- the diagnostics table ---------------------------------------------

#' Convergence diagnostics for every sampled quantity
#'
#' `apa_tidy_diagnostics()` returns R-hat and bulk and tail ESS for every
#' variable of a fitted model's posterior, including the group-level
#' deviations that a parameter table does not print. It is the table
#' [apa_convergence()] reports from: a convergence statement has to cover
#' what was sampled, not what a table shows.
#'
#' Numbers come from [posterior::summarise_draws()]; apabayes computes no
#' diagnostic of its own.
#'
#' @param x A fitted model, posterior draws, or anything
#'   [posterior::as_draws_df()] accepts.
#' @param ... Passed to the method.
#' @param variables Character vector of variables to report, in the order
#'   given, or `NULL` for every variable that is not *internal* — the same
#'   rule [apa_tidy()] uses on the draws route: names ending in `__`,
#'   `lprior`, and names starting with `prior_` are dropped by default and
#'   reported when named here.
#'
#' @return An [apabayes_tidy] tibble of type `"diagnostics"` with columns
#'   `term`, `rhat`, `ess_bulk` and `ess_tail`. Its `divergences`
#'   attribute is the number of divergent post-warmup transitions summed
#'   over chains, read from the sampler's own record, for a `brmsfit`,
#'   `stanreg`, `stanfit`, `CmdStanMCMC` or `blavaan` fit sampled with
#'   NUTS; it is `NA` for draws, `mcmc.list` and runjags objects and for
#'   fits made by optimisation, variational inference or another sampler,
#'   which have no such record.
#' @seealso [apa_tidy()] for parameter tables.
#' @examplesIf rlang::is_installed("posterior")
#' z <- stats::qnorm(stats::ppoints(400))
#' z <- z[order(sin(seq_along(z)))]
#' draws <- posterior::as_draws_df(
#'   data.frame(mu = 2 + z, sigma = exp(0.3 * z))
#' )
#' apa_tidy_diagnostics(draws)
#' @export
apa_tidy_diagnostics <- function(x, ...) {
  UseMethod("apa_tidy_diagnostics")
}

#' @describeIn apa_tidy_diagnostics Anything
#'   [posterior::as_draws_df()] accepts, which includes `brmsfit`,
#'   `stanreg`, `stanfit`, `CmdStanFit`, `mcmc` and `mcmc.list`. One
#'   coercing method serves every supported object, as on the draws route.
#' @export
apa_tidy_diagnostics.default <- function(x, variables = NULL, ...) {
  rlang::check_installed(
    "posterior",
    reason = paste0("to read draws from ", class(x)[1], " objects.")
  )
  draws <- tryCatch(
    posterior::as_draws_df(x),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg x} must be an object {.pkg posterior} can convert to
         draws, not {.cls {class(x)}}.",
        parent = cnd
      )
    }
  )
  diagnostics_table(
    draws, variables,
    source_class = class(x),
    packages = c("posterior", "apabayes"),
    divergences = sampler_divergences(x)
  )
}

# The diagnostics table of a set of draws, shared by the coercing method
# and the blavaan method. A diagnostics table carries no estimate and no
# interval, so the centrality and interval attributes are NA rather than
# the constructor's defaults: the print header must not claim a median or
# a 95% CrI that is not there.
diagnostics_table <- function(draws, variables, source_class, packages,
                              divergences) {
  terms <- resolve_draws_variables(variables, posterior::variables(draws))
  summarised <- as.data.frame(posterior::summarise_draws(
    posterior::subset_draws(draws, variable = terms),
    "rhat", "ess_bulk", "ess_tail"
  ))
  row <- match(terms, summarised$variable)

  out <- data.frame(
    term = terms,
    rhat = summarised$rhat[row],
    ess_bulk = summarised$ess_bulk[row],
    ess_tail = summarised$ess_tail[row],
    stringsAsFactors = FALSE
  )
  apabayes_tidy(
    out,
    type = "diagnostics",
    centrality = NA_character_,
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = as.character(source_class),
    package_versions = package_versions_of(packages),
    divergences = divergences
  )
}

# ---- divergent transitions ---------------------------------------------

# The number of divergent post-warmup transitions, summed over chains, or
# NA when `x` carries no NUTS sampler record. Measured
# (local/probes/probe_convergence*.R): a brmsfit keeps a stanfit in
# `$fit` under both backends, a stanreg in `$stanfit`, and a blavaan fit
# its sampler object under `blavInspect(x, "mcobj")` — a stanfit on the
# default target but a CmdStanMCMC on `target = "cmdstan"`. Draws,
# `mcmc.list`, runjags and JAGS objects have no divergences to count.
sampler_divergences <- function(x) {
  if (inherits(x, "brmsfit")) {
    return(sampler_divergences(x$fit))
  }
  if (inherits(x, "stanreg")) {
    return(sampler_divergences(x$stanfit))
  }
  if (inherits(x, "blavaan")) {
    return(sampler_divergences(blavaan::blavInspect(x, "mcobj")))
  }
  if (inherits(x, "stanfit")) {
    return(stanfit_divergences(x))
  }
  if (inherits(x, "CmdStanMCMC")) {
    return(cmdstan_divergences(x))
  }
  NA_integer_
}

# Measured: a stanfit holds NUTS draws exactly when `@mode` is 0 (2 after
# optimizing, `chains = 0` or a failed run), its method is "sampling"
# (meanfield says "variational", where `get_sampler_params()` aborts),
# and every chain records `divergent__` (Fixed_param and static HMC do
# not). Anything else has no count, which is not a count of zero.
stanfit_divergences <- function(x) {
  sampled <- isTRUE(x@mode == 0) &&
    identical(x@stan_args[[1]]$method, "sampling")
  if (!sampled) {
    return(NA_integer_)
  }
  rlang::check_installed("rstan", reason = "to count divergent transitions.")
  params <- rstan::get_sampler_params(x, inc_warmup = FALSE)
  has_column <- vapply(
    params, function(m) "divergent__" %in% colnames(m), logical(1)
  )
  if (!all(has_column)) {
    return(NA_integer_)
  }
  as.integer(sum(vapply(
    params, function(m) sum(m[, "divergent__"]), numeric(1)
  )))
}

# A CmdStanMCMC is an R6 object, so its own methods are called and no
# cmdstanr function is (cmdstanr is not on CRAN). Only HMC records
# `divergent__`; the fixed-parameter sampler does not.
cmdstan_divergences <- function(x) {
  if (!identical(x$metadata()$algorithm, "hmc")) {
    return(NA_integer_)
  }
  diagnostics <- x$sampler_diagnostics(inc_warmup = FALSE, format = "draws_df")
  if (!"divergent__" %in% posterior::variables(diagnostics)) {
    return(NA_integer_)
  }
  as.integer(sum(diagnostics$divergent__))
}

#' @describeIn apa_tidy_diagnostics A `runjags` object keeps its chains in
#'   `$mcmc`; `posterior` has no method for the object itself.
#' @export
apa_tidy_diagnostics.runjags <- function(x, ...) {
  with_source_class(apa_tidy_diagnostics(x$mcmc, ...), class(x))
}
