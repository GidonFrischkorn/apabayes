# The inline layer (ARCHITECTURE.md decision 5): a tidy table in, the
# APA string for the running text out. The tidy method is the whole
# layer; the default method extracts first and then calls it, so every
# class `apa_tidy()` dispatches on is covered by one method. Row
# addressing lives in the address file, the per-type strings in the
# parts file.

#' Report a result inline, in APA style
#'
#' `apa_inline()` turns one row of a tidy table (or a fitted model) into
#' the string a Results section quotes: the estimate with its interval,
#' followed by the statistics the row carries. A row is addressed by
#' name, never by position.
#'
#' @section Addressing a row:
#' `term` matches, in this order, the `term` column, the `label` column,
#' and a term with the brms class prefix removed (`"wt"` finds `b_wt`).
#' For a hypotheses table it matches `hypothesis`. A structural-equation
#' path is addressed by its two sides: `apa_inline(x, "visual", "x1",
#' op = "=~")`; the order of the sides does not matter for a covariance
#' (`~~`) and does for every other operator. `op` alone finds an
#' intercept (`op = "~1"`). `group` restricts the search first, for a
#' multi-group fit or a grouped hypothesis. `term = NULL` reports every
#' row. No match, or more than one, is an error that lists the
#' candidates.
#'
#' @section What is printed:
#' A `parameters` row prints the estimate, its interval labelled by the
#' row's own `ci_method` (`CrI` for an equal-tailed credible interval,
#' `HDI`, `CI` for a Wald or bootstrap interval), then whichever of the
#' probability of direction, the ROPE share, the Bayes factor and the p
#' value the row carries. A population-level regression coefficient
#' (`component` `"conditional"`, not a random-effect term) is prefixed
#' `*b*`; other rows carry no symbol unless `symbol` gives one. A
#' standardized row (`std`) drops the leading zero. A `hypotheses` row
#' prints the estimate, the interval at that row's level and the Bayes
#' factor; `stats` can add the evidence ratio and the posterior
#' probability. A `diagnostics` row prints R-hat and both effective
#' sample sizes.
#'
#' A `sem_fit` row from [apa_tidy_sem_fit()] prints the indices it
#' carries, in a fixed order: for a lavaan fit `χ²(df) = …, *p* …, CFI,
#' TLI, RMSEA with its confidence interval, SRMR`; for a blavaan fit
#' `PPP, BRMSEA and BΓ̂` with their credible intervals, labelled by the
#' table's `ci_method`. The indices print with three decimals and no
#' leading zero and χ² with two, unless `digits` sets both; `stats`
#' selects among `"chisq"` (with its *p*), `"cfi"`, `"tli"`,
#' `"rmsea"`, `"srmr"`, `"ppp"`, `"brmsea"` and `"bgammahat"`. The
#' default method extracts with [apa_tidy()], which is the parameter
#' table, so fit indices are reported from `apa_tidy_sem_fit(fit)`.
#'
#' A `loo` row from a [loo::loo_compare()] table prints the difference
#' in expected log predictive density to the best model with its
#' standard error, `ΔELPD = −0.97, *SE* = 0.35`, on every row, the best
#' model's `0.00` included; `stats` adds `"elpd"` (the model's own ELPD
#' and SE), `"p_loo"`, `"looic"` and `"weight"` (`*w*`, when the table
#' was extracted with `weights =`; which kind of weight it is lives in
#' the table's `weight_method` attribute, not in the string). A
#' `bf_models` row prints its Bayes factor against the table's
#' denominator model, `*BF*~10~ = 6.38` (the denominator itself prints
#' `1.00`); `stats` adds `"log_bf"` and `"post_prob"`, the posterior
#' model probability under equal prior odds, `*P*(M | D)`, and
#' `"error"`, the numerical error of a BayesFactor Bayes factor as a
#' percentage after the Bayes factor it qualifies,
#' `*BF*~10~ = 4.5 × 10^6^ ± 1.3%` (`± 0%` for an exact one, nothing
#' where none is recorded); `"error"` needs `"bf"`. A Bayes
#' factor too large or too small to exponentiate (a log Bayes factor
#' beyond about ±709) prints as its log instead, never as `∞` or `0`.
#' Such a row is addressed by `model`, and a Bayes-factor row also by
#' the `name` it was passed as. LOO and Bayes-factor comparisons answer
#' different questions and are never merged into one string.
#'
#' A `bf_inclusion` row from [bayestestR::bayesfactor_inclusion()] prints
#' its inclusion Bayes factor, `*BF*~incl~ = 1.9 × 10^4^`, or under
#' `bf_direction = "01"` the exclusion Bayes factor `*BF*~excl~`; `stats`
#' adds the prior and posterior inclusion probabilities, `*P*(incl) =
#' .50` and `*P*(incl | D) = .55`. A term in every model has no inclusion
#' Bayes factor, and a posterior inclusion probability rounded to 1 or 0
#' an infinite one; printing the Bayes factor of such a row is an error
#' that says which. A row is addressed by its `term`.
#'
#' A `contrasts` row from an emmeans grid prints like a parameters row:
#' the estimate, its interval labelled from the row's `ci_method`
#' (`95% HDI [1.38, 7.01]` on the emmGrid route's default, `CrI` under
#' `ci = "eti"`), then `*pd*` and the ROPE share when the table carries
#' one. No symbol is printed unless `symbol` gives one: a contrast has
#' no coefficient rule to apply. A row is addressed by its `contrast`
#' string (`"cyl_f4 - cyl_f6"`, or emmeans's label of a marginal mean,
#' `"cyl_f4"`); a contrast computed within `by` groups repeats its
#' string across them and is addressed with `group`.
#'
#' A `correlations` row prints
#' `*r* = −.82, 95% HDI [−.92, −.66], *pd* > .999, *BF*~10~ = 1.3 × 10^7^`:
#' the correlation without its leading
#' zero, then pd and the Bayes factor; `stats` adds the ROPE share and
#' `"n"`, `*n* = 32`. A row is addressed by its pair, `apa_inline(x,
#' "mpg", "wt")`, in either order, or by its term `"mpg~~wt"`; a single
#' variable finds the row it appears in when there is one. A correlation
#' computed within groups is addressed with `group`.
#'
#' Every number goes through the format layer
#' ([apa_num()], [apa_ci()], [apa_pd()], [apa_bf()], [apa_p()]), and
#' nothing printed judges the result.
#'
#' @section The default method:
#' On a fitted model or any other object [apa_tidy()] accepts, the
#' default method calls `apa_tidy(x, ...)` and reports the result, so
#' `...` takes that route's arguments: `standardize = TRUE` on a lavaan
#' or blavaan fit, `effects = "all"` on a brms fit, `ci = "hdi"` on any
#' posterior, `ci = "eti"` or `rope =` on an emmeans grid. Storing the
#' tidy table and reporting from it is the same
#' thing in two steps, and lets a document be knitted without the
#' packages that made the fit.
#'
#' @param x An [apabayes_tidy] table, or an object [apa_tidy()] accepts.
#' @param term The row: a term, a label, a hypothesis string, a model,
#'   the left-hand side of a structural-equation path, or one variable of
#'   a correlation. `NULL` for all rows.
#' @param rhs The right-hand side of a structural-equation path, or the
#'   other variable of a correlation.
#' @param op The operator of a structural-equation path as lavaan writes
#'   it: `"=~"`, `"~~"`, `"~"`, `"~1"` or `":="`. `NULL` matches any; a
#'   correlation's is `"~~"`.
#' @param group A value of the `group` column to restrict the search to.
#' @param ... Tidy method: must be empty. Default method: passed to
#'   [apa_tidy()].
#' @param symbol `NULL` for the default per row (`b` for a regression
#'   coefficient, `r` for a correlation, none elsewhere), a string printed
#'   in italics before the estimate (`"b"`, `"β"`, `"r"`), or `FALSE`
#'   for none.
#' @param stats `NULL` for the default, or a character vector naming a
#'   subset of what the row can print: `"pd"`, `"rope"`, `"bf"`, `"p"`
#'   for a parameters row (the default prints every one the row
#'   carries); `"bf"`, `"er"`, `"post_prob"` for a hypotheses row (the
#'   default prints the Bayes factor alone); the index names above for a
#'   sem_fit row; `"elpd_diff"`, `"elpd"`, `"p_loo"`, `"looic"`,
#'   `"weight"` for a loo row (default `"elpd_diff"`); `"bf"`,
#'   `"error"`, `"log_bf"`, `"post_prob"` for a bf_models row (default
#'   `"bf"`); `"bf"`, `"p_prior"`, `"p_posterior"` for a bf_inclusion row
#'   (default `"bf"`);
#'   `"pd"`, `"rope"` for a contrasts row (the default prints both when
#'   the row carries them); `"pd"`, `"rope"`, `"bf"`, `"n"` for a
#'   correlations row (default `c("pd", "bf")`). `character()` prints the
#'   estimate alone.
#' @param interval `FALSE` drops the interval.
#' @param ci_label `"auto"` labels the interval from the row's
#'   `ci_method`; a string overrides it; `NULL` keeps the brackets and
#'   drops the label *and the comma that introduced it*, so the estimate
#'   reads `−5.39 [−6.95, −3.78]`. A row whose `ci_level` is `NA` prints
#'   the brackets without a label whatever `ci_label` says, because a
#'   label without its level would claim more than the table records —
#'   but it keeps the comma, because that is the table's silence and not
#'   something the caller asked for.
#' @param digits Decimals for estimates and interval bounds; `NULL` is 2,
#'   and on a sem_fit row 3 for the indices and 2 for χ².
#' @param digits_prob Decimals for pd, p and the ROPE share.
#' @param leading_zero `"auto"` drops the leading zero on standardized
#'   rows, correlations and fit indices and keeps it elsewhere; `TRUE` or
#'   `FALSE` force one rule.
#' @param bf,bf_direction Passed to [apa_bf()] as `style` and
#'   `direction`.
#' @inheritParams apa_num
#'
#' @return An [apa_results] object with one string per reported row.
#' @seealso [apa_results] for the object, [apa_tidy()] for the tables,
#'   [apa_value()] for the number behind the string.
#' @examples
#' t <- apabayes_tidy(
#'   data.frame(
#'     term = c("b_Intercept", "b_wt", "sigma"),
#'     label = c("(Intercept)", "wt", "sigma"),
#'     estimate = c(37.3, -5.34, 2.71), ci_low = c(31.2, -6.85, 2.09),
#'     ci_high = c(43.1, -3.82, 3.60), pd = c(1, 0.9995, 1),
#'     component = c("conditional", "conditional", "sigma")
#'   ),
#'   type = "parameters", centrality = "median", ci_method = "eti",
#'   ci_level = 0.95
#' )
#' apa_inline(t, "wt")
#' apa_inline(t, "b_wt", symbol = FALSE, interval = FALSE)
#' apa_inline(t, "sigma", markup = "plain")
#' apa_inline(t)
#' @examplesIf rlang::is_installed("lavaan")
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
#' apa_inline(fit, "visual", "x2", op = "=~", standardize = TRUE)
#' @export
apa_inline <- function(x, ...) {
  UseMethod("apa_inline")
}

#' @rdname apa_inline
#' @export
apa_inline.apabayes_tidy <- function(x, term = NULL, rhs = NULL, op = NULL,
                                     group = NULL, ..., symbol = NULL,
                                     stats = NULL, interval = TRUE,
                                     ci_label = "auto", digits = NULL,
                                     digits_prob = 3, leading_zero = "auto",
                                     bf = c("auto", "sci", "plain"),
                                     bf_direction = c("10", "01"),
                                     markup = NULL) {
  rlang::check_dots_empty()
  validate_apabayes_tidy(x)
  opts <- inline_options(
    attr(x, "type", exact = TRUE), symbol, stats, interval, ci_label,
    digits, digits_prob,
    leading_zero, bf, bf_direction, markup
  )
  rows <- select_inline_rows(x, term, rhs, op, group)
  table <- x[rows, ]
  parts <- inline_strings(table, opts)
  full <- join_parts(parts$estimate, parts$statistic)
  new_apa_results(parts$estimate, parts$statistic, full, table, opts$markup)
}

#' @rdname apa_inline
#' @export
apa_inline.default <- function(x, term = NULL, rhs = NULL, op = NULL,
                               group = NULL, ..., symbol = NULL,
                               stats = NULL, interval = TRUE,
                               ci_label = "auto", digits = NULL,
                               digits_prob = 3, leading_zero = "auto",
                               bf = c("auto", "sci", "plain"),
                               bf_direction = c("10", "01"),
                               markup = NULL) {
  tidy <- apa_tidy(x, ...)
  apa_inline(
    tidy,
    term = term, rhs = rhs, op = op, group = group, symbol = symbol,
    stats = stats, interval = interval, ci_label = ci_label, digits = digits,
    digits_prob = digits_prob, leading_zero = leading_zero, bf = bf,
    bf_direction = bf_direction, markup = markup
  )
}

# ---- options -------------------------------------------------------------

# The table kinds the inline layer prints: every contract, since the
# contrasts builder landed (session 22). A type outside the list aborts
# by name rather than print a partial string; the check stays because
# the contract table can grow before its builder does.
inline_types <- function() {
  c(
    "parameters", "hypotheses", "diagnostics", "sem_fit", "loo", "bf_models",
    "bf_inclusion", "contrasts", "correlations"
  )
}

# The statistics a type can print after the estimate, in print order.
inline_stats_vocabulary <- function(type) {
  switch(type,
    parameters = c("pd", "rope", "bf", "p"),
    hypotheses = c("bf", "er", "post_prob"),
    sem_fit = c(
      "chisq", "cfi", "tli", "rmsea", "srmr", "ppp", "brmsea", "bgammahat"
    ),
    loo = c("elpd_diff", "elpd", "p_loo", "looic", "weight"),
    bf_models = c("bf", "error", "log_bf", "post_prob"),
    bf_inclusion = c("bf", "p_prior", "p_posterior"),
    contrasts = c("pd", "rope"),
    correlations = c("pd", "rope", "bf", "n"),
    character()
  )
}

# What `stats = NULL` prints: one statistic where the rest are opt-in
# (the decisions of spec-apa_inline.md and spec-apa_inline-comparisons.md),
# every statistic elsewhere. A correlation prints pd and its Bayes factor;
# its ROPE was fixed by correlation, not chosen, and its n is not part of
# the usual string (spec-apa_tidy_correlation.md).
inline_default_stats <- function(type) {
  switch(type,
    hypotheses = "bf",
    loo = "elpd_diff",
    bf_models = "bf",
    bf_inclusion = "bf",
    correlations = c("pd", "bf"),
    inline_stats_vocabulary(type)
  )
}

# Validate every formatting option once and return them as a list.
inline_options <- function(type, symbol, stats, interval, ci_label, digits,
                           digits_prob, leading_zero, bf, bf_direction,
                           markup, call = rlang::caller_env()) {
  if (!type %in% inline_types()) {
    cli::cli_abort(
      "{.fn apa_inline} does not yet report a table of type {.val {type}}.",
      call = call
    )
  }
  check_inline_symbol(symbol, call)
  stats <- check_inline_stats(stats, type, call)
  check_inline_error_stat(stats, call)
  check_flag(interval, call = call)
  check_inline_ci_label(ci_label, call)
  if (!is.null(digits)) {
    check_digits(digits, call = call)
  }
  # A fit-index row resolves `NULL` per statistic (see inline_sem_fit()).
  if (type != "sem_fit") {
    digits <- digits %||% 2
  }
  check_digits(digits_prob, min = 1, call = call)
  check_leading_zero(leading_zero, call)
  bf <- rlang::arg_match(bf, c("auto", "sci", "plain"), error_call = call)
  bf_direction <- rlang::arg_match(
    bf_direction, c("10", "01"),
    error_call = call
  )
  list(
    type = type, symbol = symbol, stats = stats, interval = interval,
    ci_label = ci_label, digits = digits, digits_prob = digits_prob,
    leading_zero = leading_zero, bf = bf, bf_direction = bf_direction,
    markup = markup_target(markup)
  )
}

check_inline_symbol <- function(symbol, call = rlang::caller_env()) {
  ok <- is.null(symbol) || isFALSE(symbol) || rlang::is_string(symbol)
  if (!ok) {
    cli::cli_abort(
      "{.arg symbol} must be NULL, FALSE or a single string, not
       {.cls {class(symbol)}}.",
      call = call
    )
  }
  invisible(symbol)
}

# `NULL` means every statistic the type knows; a character vector must
# lie inside that vocabulary. The return value is always the resolved
# vector, in the type's print order.
check_inline_stats <- function(stats, type, call = rlang::caller_env()) {
  vocabulary <- inline_stats_vocabulary(type)
  if (is.null(stats)) {
    return(inline_default_stats(type))
  }
  if (!is.character(stats)) {
    cli::cli_abort(
      "{.arg stats} must be NULL or a character vector, not
       {.cls {class(stats)}}.",
      call = call
    )
  }
  unknown <- setdiff(stats, vocabulary)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg stats} names {.val {unknown}}, which a {.val {type}} table
       cannot print.",
      "i" = if (length(vocabulary) > 0) {
        "Available: {.val {vocabulary}}."
      } else {
        "A {.val {type}} table prints no statistic beyond its own."
      }
    ), call = call)
  }
  vocabulary[vocabulary %in% stats]
}

# The error of a Bayes factor is printed after it, never on its own
# (spec-apa_tidy_BFBayesFactor.md § 5).
check_inline_error_stat <- function(stats, call = rlang::caller_env()) {
  if ("error" %in% stats && !"bf" %in% stats) {
    cli::cli_abort(
      c(
        "{.arg stats} names {.val error} without {.val bf}.",
        i = "{.val error} qualifies the Bayes factor; ask for both."
      ),
      call = call
    )
  }
  invisible(stats)
}

check_inline_ci_label <- function(ci_label, call = rlang::caller_env()) {
  if (!(is.null(ci_label) || rlang::is_string(ci_label))) {
    cli::cli_abort(
      '{.arg ci_label} must be "auto", a single string or NULL.',
      call = call
    )
  }
  invisible(ci_label)
}

check_leading_zero <- function(leading_zero, call = rlang::caller_env()) {
  is_flag <- is.logical(leading_zero) && length(leading_zero) == 1 &&
    !is.na(leading_zero)
  if (!identical(leading_zero, "auto") && !is_flag) {
    cli::cli_abort(
      '{.arg leading_zero} must be "auto", TRUE or FALSE.',
      call = call
    )
  }
  invisible(leading_zero)
}
