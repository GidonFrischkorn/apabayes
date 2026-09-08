# The blavaan route (ARCHITECTURE.md decisions 2, 18, 19 and 24): Bayesian
# SEM, and the mirror image of the lavaan route. There the estimate is a
# maximum-likelihood point estimate with a Wald interval and a p value;
# here it is a posterior median with a credible interval, pd and
# convergence diagnostics, and `p` is NA. The two `sem_fit` rows are
# complementary: lavaan fills chi-square, CFI, TLI, RMSEA and SRMR and
# leaves the blavaan columns NA; blavaan fills PPP, BRMSEA and BGammaHat
# and leaves every lavaan column NA.
#
# Shapes measured 2026-09-07 and recorded in
# dev/specs/spec-apa_tidy_blavaan.md. Three of them shape the code and
# each looks surprising without the measurement behind it:
#
#  * `model_parameters(x, standardize = )` aborts for *every* value, FALSE
#    included, so `standardize` cannot be passed through and switches the
#    data source instead;
#  * blavaan's own `Rhat`/`ESS` are `blavInspect()`'s "rhat"/"neff", a
#    third estimator that is neither `ess_bulk` nor `ess_tail`, so the
#    diagnostics come from `posterior::summarise_draws()` and blavaan's
#    are never computed;
#  * a multi-group fit aborts inside `model_parameters()` and every
#    fallback names the parameters differently, so it is refused.

# ---- the parameters method ---------------------------------------------

#' @describeIn apa_tidy A `blavaan` fit from [blavaan::bcfa()],
#'   [blavaan::bsem()] or [blavaan::bgrowth()]. Estimates, the credible
#'   interval and pd come from [parameters::model_parameters()], which
#'   reports the *free* parameters only — the fixed marker loading is
#'   absent, and `p` is `NA` throughout. `term` is blavaan's own
#'   parameter name (`visual=~x1`, `x1~~x1`) and `label` the same with
#'   spaces. Here `component` is `"all"`, `"latent"` or `"residual"`,
#'   blavaan's own vocabulary rather than lavaan's six names.
#'
#'   R-hat and both ESS columns come from
#'   [posterior::summarise_draws()] over `blavaan::blavInspect(x,
#'   "mcmc")`, and blavaan's own are never computed. They therefore
#'   **differ from what `blavaan::summary()` prints**, which reports
#'   `blavInspect(x, "rhat")` and `"neff"`: one ESS where the contract
#'   has two, from an estimator that is neither the bulk nor the tail ESS
#'   the "greater than 400" rule of thumb is defined for.
#'
#'   `standardize` reads [blavaan::standardizedPosterior()] instead of
#'   easystats, which cannot standardize a blavaan fit at all. The
#'   standardized solution covers the whole parameter table, so its rows
#'   are a *superset* of the unstandardized ones: the fixed markers
#'   appear, with a real interval, and under `"std.all"` the latent
#'   variances are exactly 1. Those rows carry no component information,
#'   so `component` is `NA` there and cannot be combined with
#'   `standardize`; select rows with `variables` instead. A multi-group
#'   fit is refused.
#' @export
apa_tidy.blavaan <- function(x,
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
                             ...) {
  # Guarded like brms and lavaan: a fit restored with `readRDS()`
  # dispatches here on its class attribute without the packages this
  # route reads it with.
  rlang::check_installed("blavaan", reason = "to read blavaan objects.")
  rlang::check_installed("lavaan", reason = "to read blavaan objects.")
  rlang::check_installed("posterior", reason = "to read blavaan draws.")
  component <- check_sem_component(component, blavaan_components())
  standardize <- check_standardize(standardize)
  centrality <- rlang::arg_match(centrality)
  ci <- rlang::arg_match(ci)
  rope <- check_route_args(ci_level, diagnostics, rope, rope_ci)
  check_blavaan_component_use(component, standardize)
  check_blavaan_single_group(x)
  check_lavaan_converged(x)

  source <- blavaan_posterior(
    x, standardize, component, centrality, ci, ci_level, rope, rope_ci
  )
  terms <- resolve_parameters_variables(variables, source$terms)
  out <- blavaan_rows(
    source$tbl, terms, labels, centrality, ci, ci_level, !isFALSE(standardize)
  )
  if (diagnostics) {
    out[c("rhat", "ess_bulk", "ess_tail")] <- draws_diagnostics(
      source$draws, terms
    )
  }

  extra <- blavaan_attributes(
    x, standardize, centrality, ci, ci_level, rope, rope_ci
  )
  rlang::exec(apabayes_tidy, out, !!!extra)
}

# The table's metadata. `standardized` and `estimator` are the lavaan
# route's two extras; the package the numbers came from depends on the
# path, so that a table note credits `parameters` or `bayestestR` for
# what it actually produced and never for what it did not.
blavaan_attributes <- function(x, standardize, centrality, ci, ci_level,
                               rope, rope_ci) {
  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = as.character(class(x)),
    packages = c(
      "blavaan", if (isFALSE(standardize)) "parameters" else "bayestestR",
      "posterior", "apabayes"
    ),
    rope = rope, rope_ci = rope_ci
  )
  extra$standardized <- standardize
  extra$estimator <- lavaan::lavInspect(x, "options")$estimator
  extra
}

# ---- the two posterior sources -------------------------------------------

# Which posterior the reported numbers come from, with the draws the
# diagnostics come from alongside. Unstandardized that is easystats over
# the fit; standardized it is `bayestestR::describe_posterior()` over
# `standardizedPosterior()`, because `model_parameters(standardize = )`
# aborts on a blavaan fit for every value of the argument (measured,
# including `FALSE`). `diagnostic = NULL` keeps blavaan from computing
# an R-hat and an ESS this route does not report.
blavaan_posterior <- function(x, standardize, component, centrality, ci,
                              ci_level, rope, rope_ci) {
  draws <- posterior::as_draws_df(blavaan::blavInspect(x, "mcmc"))
  if (isFALSE(standardize)) {
    tbl <- call_model_parameters(
      x, centrality, ci, ci_level, rope, rope_ci,
      component = component, diagnostic = NULL
    )
    return(list(tbl = tbl, draws = draws, terms = tbl$Parameter))
  }
  draws <- blavaan_standardized_draws(x, standardize, draws)
  tbl <- describe_draws(draws, centrality, ci, ci_level, rope, rope_ci)
  list(tbl = tbl, draws = draws, terms = posterior::variables(draws))
}

# `standardizedPosterior()` returns a plain matrix with no chain
# information. Row *i* of it was measured to be the standardization of
# draw *i* of `blavInspect(x, "mcmc")` — the `std.all` loading computed by
# hand from the unstandardized draws reproduces the matrix column
# elementwise to 1.1e-16 — so `.chain` and `.iteration` carry over
# unchanged and the standardized rows get R-hat and ESS like any other.
blavaan_standardized_draws <- function(x, type, draws) {
  standardized <- as.data.frame(
    blavaan::standardizedPosterior(x, type = type)
  )
  standardized$.chain <- draws$.chain
  standardized$.iteration <- draws$.iteration
  posterior::as_draws_df(standardized)
}

# The contract rows of either path. Both tables carry `Parameter`,
# `CI_low`, `CI_high` and `pd`, and rows are matched by name; the
# standardized one has no `Component`, which is exactly the NA the
# contract wants there.
blavaan_rows <- function(tbl, terms, labels, centrality, ci, ci_level, std) {
  row <- match(terms, tbl$Parameter)
  data.frame(
    term = terms,
    label = blavaan_labels(terms, labels),
    estimate = tbl[[route_estimate_column(centrality)]][row],
    ci_low = tbl$CI_low[row],
    ci_high = tbl$CI_high[row],
    ci_method = ci,
    ci_level = ci_level,
    pd = optional_numeric(tbl, "pd", row),
    rope_pct = optional_numeric(tbl, "ROPE_Percentage", row),
    component = optional_column(tbl, "Component", row),
    std = std,
    stringsAsFactors = FALSE
  )
}

# `lhs op rhs` with single spaces, as on the lavaan route (`visual =~ x1`;
# an intercept has an empty right-hand side, so `x1 ~1`). blavaan's
# `pretty_names` is the identity (measured), so the shared
# `parameters_labels()` would add nothing, and the parameter name is
# parsed here instead. The operators are lavaan's, longest first so that
# `=~` and `~~` are not read as `~`.
blavaan_labels <- function(terms, labels) {
  out <- trimws(sub(
    "^(.*?)(=~|~~|:=|~\\*~|~1|~)(.*)$", "\\1 \\2 \\3", terms
  ))
  apply_label_overrides(out, labels, terms)
}

# ---- guards --------------------------------------------------------------

# The component names `model_parameters.blavaan` accepts, which are not
# lavaan's: its filter is `tolower(Component) %in% component`, and its
# `Component` values are `latent` and `residual`.
blavaan_components <- function() c("all", "latent", "residual")

# The standardized posterior is a bare matrix of draws over the parameter
# table with no component information, and deriving one from the operator
# was measured on CFA fits only. Refused rather than silently ignored.
check_blavaan_component_use <- function(component, standardize,
                                        call = rlang::caller_env()) {
  if (!identical(component, "all") && !isFALSE(standardize)) {
    cli::cli_abort(
      c(
        "{.arg component} cannot be used with {.arg standardize}: the
         standardized posterior carries no component information.",
        i = "Select standardized rows with {.arg variables} instead."
      ),
      call = call
    )
  }
  invisible(component)
}

# Measured on a two-group fit: `model_parameters()` itself aborts with
# "arguments imply differing number of rows", and no fallback rescues it
# — `describe_posterior()` names its rows `visual=~x2 (group 1)` and
# carries no `Component`, `standardizedPosterior()` uses a third naming
# again, and neither is `names(coef(x))`, which is what `term` means on
# every other fit. Refused, rather than silently changing what `term` is.
check_blavaan_single_group <- function(x, call = rlang::caller_env()) {
  if (lavaan::lavInspect(x, "ngroups") > 1) {
    cli::cli_abort(
      c(
        "{.arg x} is a multi-group fit, which {.pkg parameters} cannot
         summarise.",
        i = "Report the groups from single-group fits, one per group."
      ),
      call = call
    )
  }
  invisible(x)
}

# ---- the fit-index row ---------------------------------------------------

#' @describeIn apa_tidy_sem_fit A `blavaan` fit. The posterior predictive
#'   p value comes from [lavaan::fitMeasures()] and BRMSEA and BGammaHat
#'   from [blavaan::blavFitIndices()], summarised as blavaan summarises
#'   them: the posterior median with a highest-density interval, which is
#'   what `summary()` of that object prints. A blavaan fit carries no
#'   chi-square, CFI, TLI, RMSEA or SRMR at all, so every column of the
#'   lavaan row is `NA` here, and `test` and `rmsea_level` are absent
#'   from this method rather than accepted and ignored. BCFI, BTLI and
#'   BNFI need a baseline model and are not reported yet. A fit made with
#'   `test = "none"` has neither a PPP nor fit indices and is refused.
#'
#' @param pd Which effective-number-of-parameters estimator
#'   [blavaan::blavFitIndices()] rescales the posterior chi-square with:
#'   `"loo"` (its own default), `"waic"` or `"dic"`. Recorded in the `pD`
#'   attribute.
#' @param rescale How the posterior chi-square is rescaled: `"devM"` (the
#'   default), `"ppmc"` or `"mcmc"`. Recorded in the `rescale` attribute.
#' @param fit_ci_level Mass of the fit indices' credible interval,
#'   separate from the `ci_level` of the parameter table; blavaan's own
#'   default is `0.90`.
#' @export
apa_tidy_sem_fit.blavaan <- function(x,
                                     model = NA_character_,
                                     pd = c("loo", "waic", "dic"),
                                     rescale = c("devM", "ppmc", "mcmc"),
                                     fit_ci_level = 0.90,
                                     ...) {
  rlang::check_installed("blavaan", reason = "to read blavaan objects.")
  rlang::check_installed("lavaan", reason = "to read blavaan objects.")
  model <- check_sem_model(model)
  pd <- rlang::arg_match(pd)
  rescale <- rlang::arg_match(rescale)
  check_ci_level(
    fit_ci_level,
    allow_na = FALSE, strict = TRUE, arg = "fit_ci_level"
  )
  check_blavaan_single_group(x)
  check_lavaan_converged(x)
  options <- lavaan::lavInspect(x, "options")
  if (identical(options$test, "none")) {
    cli::cli_abort(
      "{.arg x} was fitted with {.code test = \"none\"}; it carries no
       posterior predictive p value and no fit indices."
    )
  }

  indices <- blavaan_fit_indices(x, pd, rescale, fit_ci_level)
  out <- data.frame(
    model = model,
    ppp = unname(lavaan::fitMeasures(x, "ppp")),
    indices,
    stringsAsFactors = FALSE
  )
  apabayes_tidy(
    out,
    type = "sem_fit",
    centrality = "median",
    ci_method = "hdi",
    ci_level = fit_ci_level,
    source_class = as.character(class(x)),
    package_versions = package_versions_of(c("blavaan", "apabayes")),
    estimator = options$estimator,
    n = lavaan::lavInspect(x, "ntotal"),
    pD = pd,
    rescale = rescale
  )
}

# BRMSEA and BGammaHat, summarised by blavaan's own `summary()` method
# rather than recomputed: its bounds are the highest-density interval
# (measured, and equal to `describe_posterior(ci_method = "hdi")` to
# 1e-6, while the equal-tailed interval differs), so this is the one
# summary that reproduces the numbers blavaan itself publishes.
# `adjBGammaHat` and `BMc` are computed too but have no contract column.
blavaan_fit_indices <- function(x, pd, rescale, fit_ci_level) {
  fi <- blavaan::blavFitIndices(x, pD = pd, rescale = rescale)
  summarised <- summary(
    fi,
    central.tendency = "median", prob = fit_ci_level
  )
  list(
    brmsea = summarised["BRMSEA", "Median"],
    brmsea_low = summarised["BRMSEA", "lower"],
    brmsea_high = summarised["BRMSEA", "upper"],
    bgammahat = summarised["BGammaHat", "Median"],
    bgammahat_low = summarised["BGammaHat", "lower"],
    bgammahat_high = summarised["BGammaHat", "upper"]
  )
}
