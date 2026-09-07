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

  mp <- brms_model_parameters(
    x, effects, component, centrality, ci, ci_level, rope, rope_ci
  )
  terms <- resolve_brms_variables(variables, mp$Parameter)
  row <- match(terms, mp$Parameter)

  group <- brms_optional_column(mp, "Group", row)
  # `Group` is "" on fixed rows (easystats' printing convenience); the
  # contract's `group` says "no grouping" with NA.
  group[!is.na(group) & !nzchar(group)] <- NA_character_

  out <- data.frame(
    term = terms,
    label = brms_labels(mp, terms, group, labels),
    estimate = mp[[route_estimate_column(centrality)]][row],
    ci_low = mp$CI_low[row],
    ci_high = mp$CI_high[row],
    ci_method = ci,
    ci_level = ci_level,
    pd = mp$pd[row],
    rope_pct = if (is.null(rope)) NA_real_ else mp$ROPE_Percentage[row],
    component = brms_optional_column(mp, "Component", row),
    group = group,
    effects = brms_optional_column(mp, "Effects", row),
    stringsAsFactors = FALSE
  )
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

# The one `model_parameters()` call, with the arguments the method was
# given. `rope_ci` and `rope_range` are passed only when a ROPE was asked
# for, so the default call is exactly the easystats default call.
brms_model_parameters <- function(x, effects, component, centrality, ci,
                                  ci_level, rope, rope_ci) {
  args <- list(
    x,
    centrality = centrality,
    ci = ci_level,
    ci_method = ci,
    test = route_test_arg(rope),
    effects = effects,
    component = component
  )
  if (!is.null(rope)) {
    args$rope_range <- rope
    args$rope_ci <- rope_ci
  }
  as.data.frame(do.call(parameters::model_parameters, args))
}

# `Effects` and `Group` exist only when the model has random effects and
# `effects = "all"` (measured); `Component` is always there. A column that
# is absent is the typed NA, not an error.
brms_optional_column <- function(mp, name, row) {
  if (!name %in% names(mp)) {
    return(rep(NA_character_, length(row)))
  }
  as.character(mp[[name]])[row]
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

# The display label. `pretty_names` is a character vector named by
# `Parameter`, so it is looked up by name; its values are not unique
# (`b_Intercept` and `sd_cyl_f__Intercept` are both "(Intercept)"), and
# two rows with the same label would break the term-or-label addressing
# of the inline layer. A duplicated label is therefore qualified with its
# group, and falls back to the term if that still does not separate it.
brms_labels <- function(mp, terms, group, labels) {
  pretty <- attr(mp, "pretty_names")
  out <- if (is.character(pretty) && !is.null(names(pretty))) {
    unname(pretty[terms])
  } else {
    rep(NA_character_, length(terms))
  }
  out[is.na(out)] <- terms[is.na(out)]

  repeated <- out %in% out[duplicated(out)]
  qualify <- repeated & !is.na(group)
  out[qualify] <- sprintf("%s (%s)", out[qualify], group[qualify])
  repeated <- out %in% out[duplicated(out)]
  out[repeated] <- terms[repeated]

  if (is.null(labels)) {
    return(out)
  }
  named <- resolve_draws_labels(labels, terms)
  out[named != terms] <- named[named != terms]
  out
}

resolve_brms_variables <- function(variables, available,
                                   call = rlang::caller_env()) {
  if (length(available) == 0) {
    cli::cli_abort("{.arg x} has no parameters to report.", call = call)
  }
  if (is.null(variables)) {
    return(available)
  }
  if (!is.character(variables)) {
    cli::cli_abort(
      "{.arg variables} must be a character vector or NULL, not
       {.cls {class(variables)}}.",
      call = call
    )
  }
  # Checked before `unknown`: an empty selection has no offending term to
  # name, and folding it into the branch below reported `NA` as the term
  # that was not found.
  if (length(variables) == 0) {
    cli::cli_abort(
      "{.arg variables} selects no parameter to report.",
      call = call
    )
  }
  unknown <- setdiff(variables, available)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg variables} must name reported parameters;
         {.val {unknown[1]}} is not one.",
        i = "Available: {.val {available}}."
      ),
      call = call
    )
  }
  variables
}

# ---- the diagnostics table ---------------------------------------------

#' Convergence diagnostics for every sampled quantity
#'
#' `apa_tidy_diagnostics()` returns R-hat and bulk and tail ESS for every
#' variable of a fitted model's posterior, including the group-level
#' deviations that a parameter table does not print. It is the table
#' `apa_convergence()` reports from: a convergence statement has to cover
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
#'   `term`, `rhat`, `ess_bulk` and `ess_tail`.
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
#'   `stanfit`, `CmdStanFit`, `mcmc` and `mcmc.list`. One coercing method
#'   serves every supported object, as on the draws route.
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
  # A diagnostics table carries no estimate and no interval, so the
  # interval attributes are NA rather than the constructor's defaults:
  # the print header must not claim a 95% CrI that is not there.
  apabayes_tidy(
    out,
    type = "diagnostics",
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = class(x),
    package_versions = package_versions_of(c("posterior", "apabayes"))
  )
}

#' @describeIn apa_tidy_diagnostics A `runjags` object keeps its chains in
#'   `$mcmc`; `posterior` has no method for the object itself.
#' @export
apa_tidy_diagnostics.runjags <- function(x, ...) {
  with_source_class(apa_tidy_diagnostics(x$mcmc, ...), class(x))
}
