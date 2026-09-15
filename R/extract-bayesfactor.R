# The BayesFactor routes and inclusion Bayes factors (ARCHITECTURE.md
# decision 18, the `BFBayesFactor` row; answer 3 for the posteriors).
# Measured 2026-09-15 (local/specs/spec-apa_tidy_BFBayesFactor.md, probes
# `probe_bayesfactor*.R`). `bayestestR::bayesfactor_models()` on a
# BayesFactor object has the same log Bayes factors but names every family
# "JZS (BayesFactor)", relabels the non-JZS models wrongly, drops the
# numerical error and duplicates a denominator that is also a numerator,
# so the route reads the S4 object itself. Slots are read with `@`, which
# is base R; `inherits()` covers the S4 inheritance, so `methods` is not
# needed.

# ---- BFBayesFactor -------------------------------------------------------

#' @describeIn apa_tidy A `BFBayesFactor` object from BayesFactor
#'   ([BayesFactor::anovaBF()], [BayesFactor::lmBF()],
#'   [BayesFactor::ttestBF()], [BayesFactor::correlationBF()], ...), as a
#'   `bf_models` table read from the object: the denominator model first,
#'   then every numerator in the object's order, each named by
#'   BayesFactor's short name (`model`, made unique as BayesFactor names
#'   its rows: `"wt"`, `"wt #1"`) and long name (`name`). `log_bf`
#'   is BayesFactor's own natural-log Bayes factor, `bf` its exponential,
#'   `error` its proportional numerical error (`NA` on the denominator row
#'   and where BayesFactor records none, 0 for the exact families), and
#'   `method` the prior family (`"JZS (BayesFactor)"`,
#'   `"Jeffreys-beta* (BayesFactor)"`, ...). `post_prob` is computed at
#'   equal prior odds, as on the `bayesfactor_models` route. A numerator
#'   identical to the denominator (`bf / bf[1]`) is not repeated. A
#'   `BFBayesFactorList` (`bf / bf`) holds several denominators and is
#'   refused; report one column of it, `x[, j]`. Parameter estimates come
#'   from the draws: `apa_tidy(BayesFactor::posterior(bf, iterations = ))`
#'   reads a `BFmcmc` object through the draws route, and takes that
#'   route's arguments.
#' @export
apa_tidy.BFBayesFactor <- function(x, ...) {
  rlang::check_dots_empty()
  bfs <- check_bf_slot(x)
  numerators <- x@numerator
  # Measured session 25: `bf / bf[1]` keeps the denominator among the
  # numerators, and `identical()` is TRUE for exactly that one.
  keep <- !vapply(
    numerators, function(m) identical(m, x@denominator), logical(1)
  )
  numerators <- numerators[keep]
  log_bf <- c(0, as.double(bfs$bf[keep]))
  method <- paste0(x@denominator@type, " (BayesFactor)")
  out <- data.frame(
    # The slot's row names are the numerators' short names, made unique:
    # `c(bf[1], bf[1])` has two short names "wt" and rows "wt" and
    # "wt #1" (measured session 25), and a row is addressed by `model`.
    model = c(x@denominator@shortName, rownames(bfs)[keep]),
    bf = exp(log_bf),
    log_bf = log_bf,
    denominator = seq_along(log_bf) == 1L,
    method = method,
    post_prob = posterior_model_probs(log_bf),
    # The error is a logical NA on a correlation with a null interval
    # (measured), hence `as.double()`.
    error = c(NA_real_, as.double(bfs$error[keep])),
    name = c(
      x@denominator@longName,
      vapply(numerators, function(m) m@longName, character(1))
    ),
    stringsAsFactors = FALSE
  )
  packages <- c(installed_packages_of("BayesFactor"), "apabayes")
  apabayes_tidy(
    out,
    type = "bf_models",
    centrality = NA_character_,
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = as.character(class(x)),
    package_versions = package_versions_of(packages),
    bf_method = method,
    prior_odds = "equal",
    denominator_model = out$model[1]
  )
}

check_bf_slot <- function(x, call = rlang::caller_env()) {
  bfs <- x@bayesFactor
  if (!is.data.frame(bfs) || !is.numeric(bfs$bf)) {
    cli::cli_abort(
      "The {.field bayesFactor} slot of {.arg x} has no numeric
       {.field bf} column.",
      call = call
    )
  }
  if (nrow(bfs) == 0) {
    cli::cli_abort("{.arg x} has no Bayes factors to report.", call = call)
  }
  bfs
}

#' @describeIn apa_tidy A `BFBayesFactorList` is refused: it holds Bayes
#'   factors against several denominators, and a table has one.
#' @export
apa_tidy.BFBayesFactorList <- function(x, ...) {
  cli::cli_abort(
    c(
      "{.arg x} is a {.cls BFBayesFactorList}: Bayes factors against
       several denominators.",
      i = "Report one column of it, {.code x[, j]}, a table against one
           denominator."
    )
  )
}

#' @describeIn apa_tidy A `BFmcmc` object from [BayesFactor::posterior()]
#'   is read by the draws route, with every argument of that route. An
#'   object with several numerators needs `index =` in
#'   `BayesFactor::posterior()`.
#' @export
apa_tidy.BFmcmc <- function(x, ...) {
  rlang::check_installed("posterior", reason = "to read BFmcmc draws.")
  # Measured session 25: `posterior` reads the S4 object with the warning
  # "Setting class(x) to "matrix" sets attribute to NULL"; a plain matrix
  # of the same numbers and column names gives the same draws without it.
  m <- matrix(as.numeric(x), nrow(x), dimnames = list(NULL, colnames(x)))
  out <- apa_tidy(posterior::as_draws_matrix(m), ...)
  with_source_class(out, class(x))
}

# ---- bayesfactor_inclusion -----------------------------------------------

#' @describeIn apa_tidy The output of [bayestestR::bayesfactor_inclusion()],
#'   as a `bf_inclusion` table: one row per `term`, with the prior and
#'   posterior inclusion probabilities (`p_prior`, `p_posterior`), the
#'   inclusion Bayes factor `bf` and its log `log_bf`. A term in every
#'   model has no inclusion Bayes factor (`NA`, where bayestestR has
#'   `NaN`); a posterior inclusion probability that rounds to 1 gives an
#'   infinite one, which is kept. The `averaging` attribute says whether
#'   the models were averaged over `"all"` or `"matched"` models,
#'   `prior_odds` whether the prior odds were `"equal"` or `"custom"` (the
#'   values in `prior_odds_values`). bayestestR does not record how the
#'   models' Bayes factors were computed, so `bf_method` is `NA`.
#' @method apa_tidy bayesfactor_inclusion
#' @export
apa_tidy.bayesfactor_inclusion <- function(x, ...) {
  rlang::check_dots_empty()
  check_bayesfactor_inclusion(x)
  log_bf <- as.double(x$log_BF)
  log_bf[is.nan(log_bf)] <- NA_real_
  out <- data.frame(
    term = rownames(x),
    p_prior = as.double(x$p_prior),
    p_posterior = as.double(x$p_posterior),
    bf = exp(log_bf),
    log_bf = log_bf,
    stringsAsFactors = FALSE
  )
  prior_odds <- attr(x, "priorOdds", exact = TRUE)
  extra <- list(
    averaging = if (isTRUE(attr(x, "matched", exact = TRUE))) {
      "matched"
    } else {
      "all"
    },
    prior_odds = if (is.null(prior_odds)) "equal" else "custom",
    prior_odds_values = if (!is.null(prior_odds)) as.numeric(prior_odds),
    bf_method = NA_character_
  )
  rlang::exec(
    apabayes_tidy, out,
    type = "bf_inclusion",
    centrality = NA_character_,
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = class(x),
    package_versions = package_versions_of(c("bayestestR", "apabayes")),
    !!!Filter(Negate(is.null), extra)
  )
}

check_bayesfactor_inclusion <- function(x, call = rlang::caller_env()) {
  needed <- c("p_prior", "p_posterior", "log_BF")
  missing <- setdiff(needed, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg x} has no {.field {missing}} column{?s}.",
      call = call
    )
  }
  numeric <- vapply(needed, function(nm) is.numeric(x[[nm]]), logical(1))
  if (!all(numeric)) {
    cli::cli_abort(
      "Column{?s} {.field {needed[!numeric]}} of {.arg x} must be numeric.",
      call = call
    )
  }
  if (nrow(x) == 0) {
    cli::cli_abort("{.arg x} has no terms to report.", call = call)
  }
  invisible(x)
}
