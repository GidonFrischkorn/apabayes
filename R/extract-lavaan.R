# The lavaan route (ARCHITECTURE.md decisions 2, 6, 18 and 24), the first
# frequentist one. `parameters::model_parameters()` owns the parameter
# table and `lavaan::fitMeasures()` the fit indices; apabayes selects,
# names and arranges. Shapes measured 2026-09-07 and recorded in
# dev/specs/spec-apa_tidy_lavaan.md. Four of them bite: a blavaan object
# dispatches here unless refused; `parameters` reports p = 0 for a fixed
# parameter where lavaan reports NA; a bootstrap fit gets a percentile
# interval where every other fit gets a Wald one; and a non-converged fit
# or one with `se = "none"` fails upstream with a message that names
# neither.

# ---- the parameters method ---------------------------------------------

#' @describeIn apa_tidy A `lavaan` fit from [lavaan::cfa()],
#'   [lavaan::sem()] or [lavaan::growth()]. Estimates, standard errors,
#'   the confidence interval and the p value come from
#'   [parameters::model_parameters()], which reads
#'   [lavaan::parameterEstimates()] or, with `standardize`,
#'   [lavaan::standardizedSolution()]. `term` is lavaan's own parameter
#'   name (`visual=~x1`, `x1~~x1`, `dem60~ind60`, `x1~1`; `.g2` appended
#'   in the second group of a multi-group fit) and `label` the same with
#'   spaces. Here `component` is `"all"` or one or more of `"loading"`,
#'   `"regression"`, `"correlation"`, `"variance"`, `"mean"` and
#'   `"defined"`. `centrality` is `NA` on the result — the estimate is a
#'   maximum-likelihood point estimate — and `ci_method` is `"wald"`, or
#'   `"boot"` (percentile bootstrap) for the unstandardized solution of
#'   a fit with `se = "bootstrap"`. A fixed parameter (the marker
#'   loading) has `p = NA`, as lavaan reports it. A `blavaan` fit is
#'   refused: this method reports maximum-likelihood fits.
#'
#' @param standardize `FALSE` for the unstandardized solution, `TRUE` for
#'   the completely standardized one (`"std.all"`), or one of
#'   `"std.all"`, `"std.lv"` and `"std.nox"` as in
#'   [lavaan::standardizedSolution()]. The `std` column records which.
#' @export
apa_tidy.lavaan <- function(x,
                            variables = NULL,
                            labels = NULL,
                            component = "all",
                            standardize = FALSE,
                            ci_level = 0.95,
                            ...) {
  # Guarded like brms: a fit restored with `readRDS()` dispatches here on
  # its class attribute, and `lavInspect()` is needed before anything is
  # read off the object.
  rlang::check_installed("lavaan", reason = "to read lavaan objects.")
  check_not_blavaan(x)
  component <- check_sem_component(component)
  standardize <- check_standardize(standardize)
  check_ci_level(ci_level, allow_na = FALSE, strict = TRUE)
  check_lavaan_converged(x)
  options <- check_lavaan_se(x)

  mp <- as.data.frame(parameters::model_parameters(
    x,
    ci = ci_level, standardize = standardize, component = component
  ))
  all_terms <- lavaan_terms(mp)
  terms <- resolve_parameters_variables(variables, all_terms)
  row <- match(terms, all_terms)
  group <- lavaan_groups(mp, x)[row]
  standardized <- !isFALSE(standardize)
  ci <- if (identical(options$se, "bootstrap") && !standardized) {
    "boot"
  } else {
    "wald"
  }

  out <- lavaan_rows(mp, terms, labels, group, row, ci, ci_level, standardized)
  apabayes_tidy(
    out,
    centrality = NA_character_,
    ci_method = ci,
    ci_level = ci_level,
    source_class = as.character(class(x)),
    package_versions = package_versions_of(
      c("lavaan", "parameters", "apabayes")
    ),
    standardized = standardize,
    estimator = options$estimator,
    se = options$se
  )
}

# ---- reading the parameter table ----------------------------------------

# The contract rows of the lavaan route, matched to the requested terms by
# `row`. The Bayesian columns are left to the constructor's typed NA:
# there is no posterior here.
lavaan_rows <- function(mp, terms, labels, group, row, ci, ci_level, std) {
  data.frame(
    term = terms,
    label = lavaan_labels(mp, terms, group, labels, row),
    estimate = mp$Coefficient[row],
    ci_low = mp$CI_low[row],
    ci_high = mp$CI_high[row],
    ci_method = ci,
    ci_level = ci_level,
    component = as.character(mp$Component)[row],
    group = group,
    std = std,
    p = lavaan_p(mp, row),
    stringsAsFactors = FALSE
  )
}

# lavaan's own parameter naming, `lhs op rhs` without spaces: measured to
# equal `lav_partable_labels()` for every unlabelled row and `coef()`'s
# names for every free one. A multi-group fit repeats the same names per
# group, and lavaan disambiguates them with `.g<k>` from the second group
# on; so does this.
lavaan_terms <- function(mp) {
  terms <- paste0(mp$To, mp$Operator, mp$From)
  if ("Group" %in% names(mp)) {
    g <- as.integer(mp$Group)
    later <- g > 1
    terms[later] <- paste0(terms[later], ".g", g[later])
  }
  terms
}

# The group label of each row on a multi-group fit (`Group` is the
# integer index; the labels come from the fit), NA otherwise.
lavaan_groups <- function(mp, x) {
  if (!"Group" %in% names(mp)) {
    return(rep(NA_character_, nrow(mp)))
  }
  labels <- as.character(lavaan::lavInspect(x, "group.label"))
  labels[as.integer(mp$Group)]
}

# `lhs op rhs` with single spaces (`visual =~ x1`; an intercept has an
# empty `From`, so `x1 ~1`), qualified with the group label where there
# is one, so that labels stay unique across groups as they do across
# terms. `labels =` then overrides by term, as on every route.
lavaan_labels <- function(mp, terms, group, labels, row) {
  out <- trimws(paste(mp$To, mp$Operator, mp$From))[row]
  grouped <- !is.na(group)
  out[grouped] <- sprintf("%s (%s)", out[grouped], group[grouped])
  apply_label_overrides(out, labels, terms)
}

# `parameters` writes 0 where lavaan reports no p value — a fixed
# parameter has no test statistic (measured: `p[is.na(p)] <- 0` in its
# source, on exactly the rows with `z` NA). A p value of 0 for an
# untested parameter would be read as significance, so lavaan's NA is
# restored. This is the one easystats number the route overrides, and it
# restores the upstream value rather than computing one.
lavaan_p <- function(mp, row) {
  p <- as.double(mp$p)[row]
  p[is.na(mp$z[row])] <- NA_real_
  p
}

# ---- guards --------------------------------------------------------------

# A blavaan object is an S4 subclass of lavaan, and S3 dispatch on the S4
# class chain reaches this method from it (measured). Reported here it
# would carry a Wald interval and a p value on a posterior. Since the
# blavaan route landed, `apa_tidy()` dispatches a blavaan fit to its own
# method and never reaches this; it still guards the lavaan method called
# by name, which is the only way left to ask for the wrong one.
check_not_blavaan <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "blavaan")) {
    cli::cli_abort(
      c(
        "{.arg x} is a {.cls blavaan} fit; this method reports
         {.cls lavaan} fits estimated by maximum likelihood.",
        i = "Use {.fn apa_tidy} or {.fn apa_tidy_sem_fit} without naming
             the method, which dispatches to the blavaan route."
      ),
      call = call
    )
  }
  invisible(x)
}

# Measured: `model_parameters()` aborts inside `data.frame()` on a fit
# with `se = "none"`, with a message that names neither the fit nor the
# option. The fit's options are returned, since the caller reads the
# estimator and the bootstrap flag off them anyway.
check_lavaan_se <- function(x, call = rlang::caller_env()) {
  options <- lavaan::lavInspect(x, "options")
  if (identical(options$se, "none")) {
    cli::cli_abort(
      "{.arg x} was fitted with {.code se = \"none\"}; there is no
       standard error, interval or p value to report.",
      call = call
    )
  }
  options
}

# Measured: `model_parameters()` returns a table with NA standard errors
# from a non-converged fit, and `fitMeasures()` aborts with a message
# that does not name the fit.
check_lavaan_converged <- function(x, call = rlang::caller_env()) {
  if (!isTRUE(lavaan::lavInspect(x, "converged"))) {
    cli::cli_abort(
      "{.arg x} did not converge; there is nothing to report.",
      call = call
    )
  }
  invisible(x)
}

# The easystats component names, lower case. A name easystats does not
# know — including the capitalised `Component` values it prints — is
# never a no-op upstream: on lavaan it silently yields zero rows, on
# blavaan easystats then aborts with "replacement has 1 row, data has 0
# rows" (both measured). So the argument is checked before easystats sees
# it. `"all"` wins over anything named with it.
#
# `values` differs per SEM route: `model_parameters.blavaan` reports
# `latent` and `residual` and knows none of lavaan's six, so the two
# vocabularies are not shared even though this check is.
check_sem_component <- function(component,
                                values = lavaan_components(),
                                call = rlang::caller_env()) {
  component <- rlang::arg_match(
    component, values,
    multiple = TRUE, error_call = call
  )
  if ("all" %in% component) "all" else component
}

lavaan_components <- function() {
  c(
    "all", "loading", "regression", "correlation", "variance", "mean",
    "defined"
  )
}

# `standardize` as `model_parameters()` takes it, with `TRUE` mapped to
# the type it means so that the `standardized` attribute names one.
check_standardize <- function(standardize, call = rlang::caller_env()) {
  if (isFALSE(standardize)) {
    return(FALSE)
  }
  if (isTRUE(standardize)) {
    return("std.all")
  }
  types <- c("std.all", "std.lv", "std.nox")
  ok <- is.character(standardize) && length(standardize) == 1 &&
    !is.na(standardize) && standardize %in% types
  if (!ok) {
    cli::cli_abort(
      "{.arg standardize} must be TRUE, FALSE, or one of {.val {types}}.",
      call = call
    )
  }
  standardize
}

# ---- the fit-index row ---------------------------------------------------

#' Fit indices of a structural equation model
#'
#' `apa_tidy_sem_fit()` returns one row of fit indices for a fitted SEM.
#' For a `lavaan` fit those are the model chi-square with its degrees of
#' freedom and p value, CFI, TLI, RMSEA with its confidence interval, and
#' SRMR, from [lavaan::fitMeasures()]. For a `blavaan` fit they are the
#' posterior predictive p value, BRMSEA and BGammaHat with their credible
#' intervals, from [lavaan::fitMeasures()] and
#' [blavaan::blavFitIndices()]. The two are complementary: a fit of one
#' kind carries none of the other's indices, and those columns are `NA`.
#' apabayes computes no index of its own, and none of its output judges a
#' fit (ARCHITECTURE.md decision 24).
#'
#' @param x A fitted model.
#' @param ... Passed to the method.
#' @param model A single string naming the model in the `model` column,
#'   or `NA` (the default).
#' @param test Which chi-square family the indices come from:
#'   `"standard"` (every fit has it), `"scaled"` (the `.scaled` variants
#'   of a fit with a scaled test statistic, e.g. `estimator = "MLR"`;
#'   SRMR has none), or `"robust"` (the scaled chi-square with lavaan's
#'   `.robust` CFI, TLI and RMSEA). A variant the fit does not carry is
#'   an error.
#' @param rmsea_level Confidence level of the RMSEA interval; lavaan's
#'   default is `0.90`.
#'
#' @return An [apabayes_tidy] tibble of type `"sem_fit"` with one row and
#'   columns `model`, `chisq`, `df`, `p`, `cfi`, `tli`, `rmsea`,
#'   `rmsea_low`, `rmsea_high`, `rmsea_level`, `srmr`, `ppp`, `brmsea`,
#'   `brmsea_low`, `brmsea_high`, `bgammahat`, `bgammahat_low` and
#'   `bgammahat_high`; the ones the fit does not carry are `NA`.
#'   Attributes `estimator` and `n` record the estimator and the sample
#'   size, plus `test` on a lavaan fit (the chi-square family reported)
#'   and `pD` and `rescale` on a blavaan one (how the posterior
#'   chi-square was rescaled).
#' @seealso [apa_tidy()] for the parameter table.
#' @examplesIf rlang::is_installed("lavaan")
#' fit <- lavaan::cfa(
#'   "visual =~ x1 + x2 + x3
#'    speed  =~ x7 + x8 + x9",
#'   data = lavaan::HolzingerSwineford1939
#' )
#' apa_tidy(fit, component = "loading", standardize = TRUE)
#' apa_tidy_sem_fit(fit, model = "Two factors")
#' @export
apa_tidy_sem_fit <- function(x, ...) {
  UseMethod("apa_tidy_sem_fit")
}

#' @describeIn apa_tidy_sem_fit A `lavaan` fit. `test = "scaled"` or
#'   `"robust"` needs a fit with a scaled test statistic.
#' @export
apa_tidy_sem_fit.lavaan <- function(x,
                                    model = NA_character_,
                                    test = c("standard", "scaled", "robust"),
                                    rmsea_level = 0.90,
                                    ...) {
  rlang::check_installed("lavaan", reason = "to read lavaan objects.")
  check_not_blavaan(x)
  model <- check_sem_model(model)
  test <- rlang::arg_match(test)
  check_ci_level(
    rmsea_level,
    allow_na = FALSE, strict = TRUE, arg = "rmsea_level"
  )
  check_lavaan_converged(x)
  options <- lavaan::lavInspect(x, "options")
  check_lavaan_test(x, test, options)

  values <- lavaan_fit_values(x, test, rmsea_level)
  out <- data.frame(model = model, values, stringsAsFactors = FALSE)
  apabayes_tidy(
    out,
    type = "sem_fit",
    centrality = NA_character_,
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = as.character(class(x)),
    package_versions = package_versions_of(c("lavaan", "apabayes")),
    estimator = options$estimator,
    test = test,
    n = lavaan::lavInspect(x, "ntotal")
  )
}

# A scaled or robust variant asked of a fit that has no scaled test
# statistic. Left to lavaan this is a warning and a dropped measure, so
# the argument is refused against the fit's own options instead.
check_lavaan_test <- function(x, test, options, call = rlang::caller_env()) {
  if (test != "standard" && all(options$test == "standard")) {
    cli::cli_abort(
      c(
        "{.arg x} carries no scaled test statistic, so {.arg test} cannot
         be {.val {test}}.",
        i = "Refit with {.code estimator = \"MLR\"} or a {.arg test} that
             provides one."
      ),
      call = call
    )
  }
  invisible(x)
}

# The contract's fit indices, read off `fitMeasures()` by name. All
# measures at once rather than the named ones: asking `fitMeasures()` for
# a name it does not know warns and drops it, and the check here wants to
# name what is missing without that warning first.
lavaan_fit_values <- function(x, test, rmsea_level,
                              call = rlang::caller_env()) {
  measures <- sem_fit_measures(test)
  fm <- unclass(lavaan::fitMeasures(
    x,
    fm.args = list(rmsea.ci.level = rmsea_level)
  ))
  missing <- setdiff(measures, names(fm))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg x} reports no {.field {missing[1]}}.",
      call = call
    )
  }
  values <- as.list(unname(fm[measures]))
  names(values) <- names(measures)
  values
}

# The `fitMeasures()` names behind each contract column, per chi-square
# family. lavaan pairs the robust CFI, TLI and RMSEA with the *scaled*
# chi-square (there is no `chisq.robust`), and SRMR has no variant.
sem_fit_measures <- function(test) {
  chi <- if (test == "standard") "" else ".scaled"
  index <- switch(test,
    standard = "",
    scaled = ".scaled",
    robust = ".robust"
  )
  c(
    chisq = paste0("chisq", chi),
    df = paste0("df", chi),
    p = paste0("pvalue", chi),
    cfi = paste0("cfi", index),
    tli = paste0("tli", index),
    rmsea = paste0("rmsea", index),
    rmsea_low = paste0("rmsea.ci.lower", index),
    rmsea_high = paste0("rmsea.ci.upper", index),
    rmsea_level = "rmsea.ci.level",
    srmr = "srmr"
  )
}

check_sem_model <- function(model, call = rlang::caller_env()) {
  if (length(model) == 1 && is.na(model)) {
    return(NA_character_)
  }
  if (!(is.character(model) && length(model) == 1)) {
    cli::cli_abort(
      "{.arg model} must be a single string or NA.",
      call = call
    )
  }
  model
}
