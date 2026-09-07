# The draws route (ARCHITECTURE.md decision 18, Gidon's answer 3): the
# only working path for stanfit, CmdStanFit, mcmc, mcmc.list and runjags
# output. Measured 2026-09-06 (local/probes/): model_parameters() aborts
# on a bare stanfit and returns an empty data frame on a genuine JAGS
# mcmc.list, while posterior::as_draws_df() keeps variables and chains
# for both.

# ---- generic -----------------------------------------------------------

#' Extract a model into the apabayes tidy contract
#'
#' `apa_tidy()` turns a fitted model, a set of posterior draws or a
#' comparison object into an [apabayes_tidy] table: fixed column names,
#' the numbers computed by easystats, no formatting. It is the whole of
#' the extract layer; everything apabayes reports is built from its
#' output.
#'
#' @param x The object to extract.
#' @param ... Passed to the method.
#'
#' @return An [apabayes_tidy] tibble.
#' @seealso [apabayes_tidy()] for the contract.
#' @examplesIf rlang::is_installed("posterior")
#' # Deterministic draws, so the example does not depend on RNG state:
#' # normal quantiles, reordered by a fixed rule so the chain is not
#' # monotone and the diagnostics are meaningful.
#' z <- stats::qnorm(stats::ppoints(400))
#' z <- z[order(sin(seq_along(z)))]
#' draws <- posterior::as_draws_df(
#'   data.frame(mu = 2 + z, sigma = exp(0.3 * z))
#' )
#' apa_tidy(draws)
#' apa_tidy(draws, variables = "mu", ci = "hdi", ci_level = 0.9)
#'
#' # An easystats table you already computed is reported as it stands:
#' # the interval, its method and the ROPE are read off the object, not
#' # recomputed, so those are not arguments here.
#' apa_tidy(bayestestR::describe_posterior(draws))
#' @export
apa_tidy <- function(x, ...) {
  UseMethod("apa_tidy")
}

# ---- coercing methods --------------------------------------------------

#' @describeIn apa_tidy Anything [posterior::as_draws_df()] accepts —
#'   `stanfit`, `CmdStanFit`, `mcmc`, `mcmc.list`, a draws matrix or a
#'   data frame of draws — is converted and handed to the `draws`
#'   method. The class of the object you passed is kept as the
#'   `source_class` attribute.
#' @export
apa_tidy.default <- function(x, ...) {
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
  with_source_class(apa_tidy(draws, ...), class(x))
}

#' @describeIn apa_tidy A `runjags` object keeps its chains in `$mcmc`;
#'   `posterior` has no method for the object itself.
#' @export
apa_tidy.runjags <- function(x, ...) {
  with_source_class(apa_tidy(x$mcmc, ...), class(x))
}

with_source_class <- function(x, source_class) {
  # `class()` of an S4 object carries a `package` attribute (`"stanfit"`
  # with `package = "rstan"`); `source_class` is metadata a table note
  # prints, so it keeps the names and drops the dispatch machinery.
  attr(x, "source_class") <- as.character(source_class)
  x
}

# ---- the draws method --------------------------------------------------

#' @describeIn apa_tidy Posterior draws. Estimates, interval, pd and the
#'   ROPE percentage come from [bayestestR::describe_posterior()], R-hat
#'   and ESS from [posterior::summarise_draws()]; apabayes computes no
#'   summary of its own.
#'
#' @param variables Character vector of draws variables to report, in the
#'   order given, or `NULL` for every variable that is not *internal*. A
#'   variable is internal when its name ends in `__` (the Stan convention
#'   for sampler quantities such as `lp__`), is exactly `lprior`, or
#'   begins with `prior_` (the brms log-prior and prior-draw variables,
#'   present whenever a fit was sampled with `sample_prior = "yes"`).
#'   The rule is a default, not a filter: naming a variable in
#'   `variables` reports it, and `variables = posterior::variables(x)`
#'   reports everything.
#' @param labels Named character vector of display labels, e.g.
#'   `c(b_wt = "Weight")`. Draws objects carry no formula, so `label`
#'   equals `term` for every variable you do not name.
#' @param centrality `"median"` (the default of `parameters` for brms and
#'   blavaan) or `"mean"`. On the result-object method it defaults to
#'   `NULL`, meaning the centrality the table already holds: there is
#'   nothing left to choose, and a table computed with
#'   `centrality = "all"` must be told which of the two to report.
#' @param ci `"eti"`, the equal-tailed interval reported as CrI, or
#'   `"hdi"`, the highest-density interval.
#' @param ci_level The interval mass, a number strictly between 0 and 1.
#' @param rope `NULL`, or the two bounds of a region of practical
#'   equivalence. The ROPE percentage is opt-in; the bounds are kept in
#'   the `rope_range` attribute so a table note can state them.
#' @param rope_ci The share of the posterior the ROPE percentage is
#'   computed on, passed to bayestestR as `rope_ci`; `1` uses the whole
#'   posterior, the default `0.95` the central 95%.
#' @param diagnostics `FALSE` leaves `rhat`, `ess_bulk` and `ess_tail` as
#'   `NA` instead of computing them.
#' @export
apa_tidy.draws <- function(x,
                           variables = NULL,
                           labels = NULL,
                           centrality = c("median", "mean"),
                           ci = c("eti", "hdi"),
                           ci_level = 0.95,
                           rope = NULL,
                           rope_ci = 0.95,
                           diagnostics = TRUE,
                           ...) {
  rlang::check_installed("posterior", reason = "to read draws objects.")
  centrality <- rlang::arg_match(centrality)
  ci <- rlang::arg_match(ci)
  rope <- check_route_args(ci_level, diagnostics, rope, rope_ci)

  terms <- resolve_draws_variables(variables, posterior::variables(x))
  selected <- posterior::subset_draws(x, variable = terms)

  described <- describe_draws(
    selected, centrality, ci, ci_level, rope, rope_ci
  )
  # describe_posterior() keeps the variable order but permutes the row
  # names (measured), so rows are matched by name, never by position.
  row <- match(terms, described$Parameter)

  out <- data.frame(
    term = terms,
    label = resolve_draws_labels(labels, terms),
    estimate = described[[route_estimate_column(centrality)]][row],
    ci_low = described$CI_low[row],
    ci_high = described$CI_high[row],
    ci_method = ci,
    ci_level = ci_level,
    pd = described$pd[row],
    rope_pct = if (is.null(rope)) {
      NA_real_
    } else {
      described$ROPE_Percentage[row]
    },
    stringsAsFactors = FALSE
  )

  if (diagnostics) {
    out[c("rhat", "ess_bulk", "ess_tail")] <- draws_diagnostics(
      selected, terms
    )
  }

  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = class(x),
    packages = c("posterior", "bayestestR", "apabayes"),
    rope = rope, rope_ci = rope_ci
  )
  rlang::exec(apabayes_tidy, out, !!!extra)
}

# The one `describe_posterior()` call, with the arguments the method was
# given; the ROPE arguments are passed only when a ROPE was asked for.
describe_draws <- function(selected, centrality, ci, ci_level, rope,
                           rope_ci) {
  args <- list(
    selected,
    centrality = centrality,
    ci = ci_level,
    ci_method = ci,
    test = route_test_arg(rope)
  )
  if (!is.null(rope)) {
    args$rope_range <- rope
    args$rope_ci <- rope_ci
  }
  as.data.frame(do.call(bayestestR::describe_posterior, args))
}

# R-hat and both ESS columns, matched to the requested terms by name.
draws_diagnostics <- function(selected, terms) {
  summarised <- as.data.frame(posterior::summarise_draws(
    selected, "rhat", "ess_bulk", "ess_tail"
  ))
  row <- match(terms, summarised$variable)
  data.frame(
    rhat = summarised$rhat[row],
    ess_bulk = summarised$ess_bulk[row],
    ess_tail = summarised$ess_tail[row],
    stringsAsFactors = FALSE
  )
}

# ---- helpers -----------------------------------------------------------

is_internal_draws_variable <- function(x) {
  grepl("__$", x) | x == "lprior" | startsWith(x, "prior_")
}

resolve_draws_variables <- function(variables, available,
                                    call = rlang::caller_env()) {
  if (length(available) == 0) {
    cli::cli_abort("{.arg x} has no draws variables to report.", call = call)
  }
  if (is.null(variables)) {
    keep <- available[!is_internal_draws_variable(available)]
    if (length(keep) == 0) {
      cli::cli_abort(
        c(
          "{.arg x} has no draws variables to report.",
          i = "Every variable is internal; name one in {.arg variables}
               to report it."
        ),
        call = call
      )
    }
    return(keep)
  }
  if (!is.character(variables)) {
    cli::cli_abort(
      "{.arg variables} must be a character vector or NULL, not
       {.cls {class(variables)}}.",
      call = call
    )
  }
  if (length(variables) == 0) {
    cli::cli_abort("{.arg x} has no draws variables to report.", call = call)
  }
  unknown <- setdiff(variables, available)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg variables} must name draws variables; {.val {unknown[1]}}
         is not one.",
        i = "Available: {.val {available}}."
      ),
      call = call
    )
  }
  variables
}

resolve_draws_labels <- function(labels, terms, call = rlang::caller_env()) {
  if (is.null(labels)) {
    return(terms)
  }
  named <- is.character(labels) && !is.null(names(labels)) &&
    all(nzchar(names(labels))) && !anyNA(names(labels))
  if (!named) {
    cli::cli_abort(
      '{.arg labels} must be a named character vector, e.g.
       {.code c(b_wt = "Weight")}.',
      call = call
    )
  }
  hit <- intersect(names(labels), terms)
  if (length(hit) == 0) {
    cli::cli_abort(
      c(
        "{.arg labels} names no reported term; {.val {names(labels)[1]}}
         is not one of {.val {terms}}.",
        i = "Labels are matched against {.field term}."
      ),
      call = call
    )
  }
  out <- terms
  out[match(hit, terms)] <- unname(labels[hit])
  out
}

check_rope <- function(rope, call = rlang::caller_env()) {
  if (is.null(rope)) {
    return(NULL)
  }
  ok <- is.numeric(rope) && length(rope) == 2 && !anyNA(rope) &&
    rope[1] < rope[2]
  if (!ok) {
    cli::cli_abort(
      "{.arg rope} must be two numbers, the lower and upper bound.",
      call = call
    )
  }
  as.double(rope)
}

check_rope_ci <- function(rope_ci, call = rlang::caller_env()) {
  ok <- is.numeric(rope_ci) && length(rope_ci) == 1 && !is.na(rope_ci) &&
    rope_ci > 0 && rope_ci <= 1
  if (!ok) {
    cli::cli_abort(
      "{.arg rope_ci} must be a single number greater than 0 and at
       most 1.",
      call = call
    )
  }
  invisible(rope_ci)
}
