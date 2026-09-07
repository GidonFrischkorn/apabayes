# The result-object route (ARCHITECTURE.md decision 21). The input is not
# a model but the table easystats already computed from one:
# `bayestestR::describe_posterior()` or `parameters::model_parameters()`
# output, which on every Bayesian model class inherits `describe_posterior`
# (measured, dev/specs/spec-apa_tidy_result.md). Nothing is computed here;
# the numbers and the reporting settings are read off the object.
#
# Two methods because of the class order: `parameters_model` precedes
# `describe_posterior` in the class vector of a `model_parameters()`
# table, so a `parameters_model` method is what gets dispatched, and it is
# also the right place to refuse the frequentist and multi-level tables
# that would otherwise be swallowed by the coercing default (measured:
# `posterior::as_draws_df()` accepts a summary table as draws).

#' @describeIn apa_tidy A `parameters_model` table from
#'   [parameters::model_parameters()]. A table of a Bayesian model
#'   inherits `describe_posterior` and is handled by that method; a
#'   table that does not — a frequentist model, or one computed with
#'   several `ci` levels — is refused here rather than coerced to draws.
#' @export
apa_tidy.parameters_model <- function(x, ...) {
  if (!inherits(x, "describe_posterior")) {
    refuse_parameters_model(x)
  }
  NextMethod()
}

#' @describeIn apa_tidy A `describe_posterior` table from
#'   [bayestestR::describe_posterior()], or from
#'   [parameters::model_parameters()] on a Bayesian model. The numbers
#'   are read from the table, so `ci`, `ci_level`, `rope` and
#'   `diagnostics` are not arguments: they were settled when the table
#'   was computed, and `ci_method` and the interval level are read from
#'   it. `centrality = NULL` reports whichever of the median and the mean
#'   the table holds, and asks you to name one when it holds both.
#'   `rhat` and `ess_tail` are read when present; `ess_bulk` is always
#'   `NA`, because no easystats table carries it — pass the fit for both
#'   ESS columns. `bf` is `exp(log_BF)` when the table was computed with
#'   `test = "bf"`. The `package_versions` attribute records the
#'   versions installed when `apa_tidy()` ran, not necessarily those that
#'   computed the table.
#' @export
apa_tidy.describe_posterior <- function(x,
                                        variables = NULL,
                                        labels = NULL,
                                        centrality = NULL,
                                        ...) {
  mp <- as.data.frame(x)
  centrality <- result_centrality(mp, centrality)
  ci <- result_ci_method(x)
  ci_level <- result_ci_level(mp, x)
  terms <- resolve_parameters_variables(variables, mp$Parameter)

  out <- parameters_rows(mp, terms, labels, centrality, ci, ci_level)
  row <- match(terms, mp$Parameter)
  out$rhat <- optional_numeric(mp, "Rhat", row)
  out$ess_tail <- optional_numeric(mp, "ESS_tail", row)

  rope <- result_rope_range(mp)
  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = class(x),
    packages = c(
      if (inherits(x, "parameters_model")) "parameters",
      "bayestestR", "apabayes"
    ),
    rope = rope, rope_ci = if (is.null(rope)) NULL else result_rope_ci(mp)
  )
  extra$model_class <- attr(x, "model_class", exact = TRUE)
  rlang::exec(apabayes_tidy, out, !!!extra)
}

# ---- reading the settings off the object --------------------------------

# Why a `parameters_model` table did not inherit `describe_posterior`,
# from the two attributes that can say (measured points 2 and 9).
refuse_parameters_model <- function(x, call = rlang::caller_env()) {
  levels <- attr(x, "ci", exact = TRUE)
  if (!isTRUE(attr(x, "is_bayesian", exact = TRUE))) {
    model_class <- attr(x, "model_class", exact = TRUE)
    cli::cli_abort(
      c(
        "{.arg x} summarises a model that is not Bayesian; apabayes
         reports posterior summaries.",
        if (!is.null(model_class)) {
          c(i = "Class of the model: {.cls {model_class}}.")
        }
      ),
      call = call
    )
  }
  if (length(levels) > 1) {
    abort_ci_levels(levels, call)
  }
  cli::cli_abort(
    "{.arg x} is not a posterior summary {.fn apa_tidy} can read.",
    call = call
  )
}

abort_ci_levels <- function(levels, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{.arg x} holds {length(levels)} interval levels ({.val {levels}});
       {.fn apa_tidy} reports one.",
      i = "Compute the table with a single {.arg ci}."
    ),
    call = call
  )
}

# The centrality is not an attribute of either producer; it is visible
# only as a `Median` and/or `Mean` column (measured point 4).
result_centrality <- function(mp, centrality, call = rlang::caller_env()) {
  present <- c(median = "Median", mean = "Mean")
  present <- present[present %in% names(mp)]
  if (is.null(centrality)) {
    if (length(present) == 1) {
      return(names(present))
    }
    if (length(present) == 2) {
      cli::cli_abort(
        "{.arg x} carries both a median and a mean; name
         {.arg centrality}.",
        call = call
      )
    }
    cli::cli_abort(
      c(
        "{.arg x} carries neither a {.field Median} nor a {.field Mean}
         column.",
        i = "The tidy contract reports one of the two; a MAP estimate is
             not a contract centrality."
      ),
      call = call
    )
  }
  centrality <- rlang::arg_match(
    centrality, c("median", "mean"),
    error_call = call
  )
  column <- route_estimate_column(centrality)
  if (!column %in% names(mp)) {
    cli::cli_abort(
      "{.arg x} has no {.field {column}} column.",
      call = call
    )
  }
  centrality
}

# `ci_method` is an attribute of both producers, in different case
# (measured point 5); `spi` and `bci` have no name in the contract, and
# this route reports credible intervals only, so the frequentist values
# the contract gained with the lavaan route are not accepted here either.
result_ci_method <- function(x, call = rlang::caller_env()) {
  method <- attr(x, "ci_method", exact = TRUE)
  if (!is.character(method) || length(method) != 1) {
    cli::cli_abort(
      "{.arg x} carries no {.field ci_method} attribute.",
      call = call
    )
  }
  method <- tolower(method)
  if (!method %in% c("eti", "hdi")) {
    cli::cli_abort(
      "{.arg x} reports a {.val {method}} interval; this route reports
       {.val eti} and {.val hdi}.",
      call = call
    )
  }
  method
}

# The level is the `CI` column where there is one and the `ci` attribute
# otherwise (measured point 6). More than one level means duplicated
# parameters (the long form), which the inline layer cannot address.
#
# `exact = TRUE` is load-bearing: a bare `describe_posterior()` object
# carries `ci_method` but no `ci`, and `attr()`'s default partial matching
# then returns `"eti"` as the interval level — measured, it produced a
# silent `ci_level` of `NA` instead of the refusal below.
result_ci_level <- function(mp, x, call = rlang::caller_env()) {
  levels <- if ("CI" %in% names(mp)) mp$CI else attr(x, "ci", exact = TRUE)
  levels <- unique(levels[!is.na(levels)])
  if (length(levels) == 0) {
    cli::cli_abort("{.arg x} carries no interval level.", call = call)
  }
  if (length(levels) > 1) {
    abort_ci_levels(levels, call)
  }
  as.double(levels)
}

# The ROPE bounds, when the object carries them for every row alike.
# `describe_posterior()` has them per row; `model_parameters()` has only
# the percentage (measured point 7), so a table from it has no
# `rope_range` attribute even when `rope_pct` is filled.
result_rope_range <- function(mp) {
  if (!all(c("ROPE_low", "ROPE_high") %in% names(mp))) {
    return(NULL)
  }
  low <- unique(mp$ROPE_low)
  high <- unique(mp$ROPE_high)
  if (length(low) != 1 || length(high) != 1) {
    return(NULL)
  }
  c(low, high)
}

result_rope_ci <- function(mp) {
  if (!"ROPE_CI" %in% names(mp)) {
    return(NULL)
  }
  rope_ci <- unique(mp$ROPE_CI)
  if (length(rope_ci) != 1) NULL else rope_ci
}
