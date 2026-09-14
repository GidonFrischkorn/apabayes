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
#' sample sizes. Every number goes through the format layer
#' ([apa_num()], [apa_ci()], [apa_pd()], [apa_bf()], [apa_p()]), and
#' nothing printed judges the result.
#'
#' @section The default method:
#' On a fitted model or any other object [apa_tidy()] accepts, the
#' default method calls `apa_tidy(x, ...)` and reports the result, so
#' `...` takes that route's arguments: `standardize = TRUE` on a lavaan
#' or blavaan fit, `effects = "all"` on a brms fit, `ci = "hdi"` on any
#' posterior. Storing the tidy table and reporting from it is the same
#' thing in two steps, and lets a document be knitted without the
#' packages that made the fit.
#'
#' @param x An [apabayes_tidy] table, or an object [apa_tidy()] accepts.
#' @param term The row: a term, a label, a hypothesis string, or the
#'   left-hand side of a structural-equation path. `NULL` for all rows.
#' @param rhs The right-hand side of a structural-equation path.
#' @param op The operator of a structural-equation path as lavaan writes
#'   it: `"=~"`, `"~~"`, `"~"`, `"~1"` or `":="`. `NULL` matches any.
#' @param group A value of the `group` column to restrict the search to.
#' @param ... Tidy method: must be empty. Default method: passed to
#'   [apa_tidy()].
#' @param symbol `NULL` for the default per row, a string printed in
#'   italics before the estimate (`"b"`, `"β"`, `"r"`), or `FALSE`
#'   for none.
#' @param stats `NULL` for the default, or a character vector naming a
#'   subset of what the row can print: `"pd"`, `"rope"`, `"bf"`, `"p"`
#'   for a parameters row (the default prints every one the row
#'   carries); `"bf"`, `"er"`, `"post_prob"` for a hypotheses row (the
#'   default prints the Bayes factor alone). `character()` prints the
#'   estimate alone.
#' @param interval `FALSE` drops the interval.
#' @param ci_label `"auto"` labels the interval from the row's
#'   `ci_method`; a string overrides it; `NULL` keeps the brackets and
#'   drops the label. A row whose `ci_level` is `NA` prints the brackets
#'   alone whatever `ci_label` says, because a label without its level
#'   would claim more than the table records.
#' @param digits Decimals for estimates and interval bounds; `NULL` is 2.
#' @param digits_prob Decimals for pd, p and the ROPE share.
#' @param leading_zero `"auto"` drops the leading zero on standardized
#'   rows and keeps it elsewhere; `TRUE` or `FALSE` force one rule.
#' @param bf,bf_direction Passed to [apa_bf()] as `style` and
#'   `direction`.
#' @inheritParams apa_num
#'
#' @return An [apa_results] object with one string per reported row.
#' @seealso [apa_results] for the object, [apa_tidy()] for the tables.
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

# The table kinds slice 1 of the inline layer prints. The others abort by
# name rather than print a partial string.
inline_types <- function() c("parameters", "hypotheses", "diagnostics")

# The statistics a type can print after the estimate, in print order.
inline_stats_vocabulary <- function(type) {
  switch(type,
    parameters = c("pd", "rope", "bf", "p"),
    hypotheses = c("bf", "er", "post_prob"),
    character()
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
  check_flag(interval, call = call)
  check_inline_ci_label(ci_label, call)
  digits <- digits %||% 2
  check_digits(digits, call = call)
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
    return(if (type == "hypotheses") "bf" else vocabulary)
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
