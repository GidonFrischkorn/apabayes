# The convergence sentence (ARCHITECTURE.md decision 7): one string over
# everything that was sampled, from a diagnostics table or from a fit.
# The sentence states the extremes as bounds that the data cannot
# contradict, and says what was exceeded when a threshold was; it never
# says whether the model converged. The spec is spec-apa_convergence.md
# in the design record.

#' Report convergence diagnostics in one sentence
#'
#' `apa_convergence()` summarises R-hat, the bulk and tail effective
#' sample sizes and the divergent transitions of every sampled quantity
#' into the sentence a Method or Results section quotes:
#' `*R̂* ≤ 1.004, bulk ESS ≥ 1,240, tail ESS ≥ 980, no divergent
#' transitions`.
#'
#' @section The sentence:
#' Each diagnostic is stated over its non-missing values. When none
#' reaches its threshold the extreme is given as a bound: the largest
#' R-hat rounded *up* to `digits` decimals and the smallest ESS rounded
#' *down* to an integer, so that `≤` and `≥` hold for the unrounded
#' numbers too. When some do, the part says how many and gives the
#' extreme: `2 of 13 *R̂* ≥ 1.01, maximum 1.018`, `9 of 13 bulk ESS ≤
#' 400, minimum 152`. An R-hat equal to `rhat`, or an ESS equal to `ess`,
#' counts as reaching it (Vehtari et al., 2021, recommend R-hat below
#' 1.01 and ESS above 400).
#'
#' Divergent transitions are stated as a count (`no divergent
#' transitions`, `3 divergent transitions`) when the table records them,
#' which [apa_tidy_diagnostics()] does for fits sampled with NUTS. Draws,
#' `mcmc.list` objects and tables stored without the record print no
#' divergence part, and the sentence then says nothing about divergences.
#'
#' No word judging the fit is printed. The `passed` element is `TRUE`
#' when no part reports a value at a threshold and no divergent transition
#' was counted; it is for code, not for the text.
#'
#' @param x A table of type `"diagnostics"` from [apa_tidy_diagnostics()],
#'   or any object [apa_tidy_diagnostics()] accepts.
#' @param ... Tidy method: must be empty. Default method: passed to
#'   [apa_tidy_diagnostics()] (`variables =`).
#' @param rhat The R-hat threshold, a single number of 1 or more.
#' @param ess The threshold for both effective sample sizes, a single
#'   positive number.
#' @param digits Decimals of R-hat.
#' @inheritParams apa_num
#'
#' @return An [apa_results] object whose `full_result` is the sentence,
#'   with `estimate` `NA`, `table` the whole diagnostics table, and two
#'   further elements: `passed` (logical) and `summary`, a one-row data
#'   frame with the number of variables, and per diagnostic the number of
#'   non-missing values, the extreme and the number at the threshold, the
#'   divergence count (`NA` when not recorded) and both thresholds.
#' @seealso [apa_tidy_diagnostics()] for the table, [apa_rhat_ess()] for
#'   the diagnostics of one parameter.
#' @examples
#' t <- apabayes_tidy(
#'   data.frame(
#'     term = c("b_Intercept", "b_wt", "sigma"),
#'     rhat = c(1.0012, 1.0036, 1.0008),
#'     ess_bulk = c(1240.6, 1810, 2203), ess_tail = c(980.2, 1422, 1733)
#'   ),
#'   type = "diagnostics", centrality = NA_character_,
#'   ci_method = NA_character_, ci_level = NA_real_, divergences = 0L
#' )
#' apa_convergence(t)
#' apa_convergence(t, ess = 1000, markup = "plain")
#' @examplesIf rlang::is_installed("posterior")
#' z <- stats::qnorm(stats::ppoints(400))
#' draws <- posterior::as_draws_df(data.frame(
#'   mu = 2 + z[order(sin(seq_along(z)))],
#'   sigma = exp(0.3 * z[order(cos(seq_along(z)))])
#' ))
#' apa_convergence(draws)
#' @export
apa_convergence <- function(x, ...) {
  UseMethod("apa_convergence")
}

#' @rdname apa_convergence
#' @export
apa_convergence.apabayes_tidy <- function(x, ..., rhat = 1.01, ess = 400,
                                          digits = 3, markup = NULL) {
  rlang::check_dots_empty()
  validate_apabayes_tidy(x)
  check_convergence_table(x)
  check_threshold(rhat, min = 1, strict = FALSE)
  check_threshold(ess, min = 0, strict = TRUE)
  check_digits(digits)
  target <- markup_target(markup)

  divergences <- attr(x, "divergences", exact = TRUE)
  if (is.null(divergences)) {
    divergences <- NA_integer_
  }
  rh <- summarise_diagnostic(x$rhat, rhat, above = TRUE)
  bulk <- summarise_diagnostic(x$ess_bulk, ess, above = FALSE)
  tail <- summarise_diagnostic(x$ess_tail, ess, above = FALSE)

  parts <- c(
    rhat_part(rh, rhat, digits, target),
    ess_part(bulk, "bulk ESS", ess, target),
    ess_part(tail, "tail ESS", ess, target),
    divergence_part(divergences, target)
  )
  sentence <- paste(parts, collapse = ", ")
  passed <- rh$flagged == 0L && bulk$flagged == 0L && tail$flagged == 0L &&
    (is.na(divergences) || divergences == 0L)

  summary <- data.frame(
    variables = nrow(x),
    rhat_n = rh$n, rhat_max = rh$extreme, rhat_flagged = rh$flagged,
    ess_bulk_n = bulk$n, ess_bulk_min = bulk$extreme,
    ess_bulk_flagged = bulk$flagged,
    ess_tail_n = tail$n, ess_tail_min = tail$extreme,
    ess_tail_flagged = tail$flagged,
    divergences = as.integer(divergences),
    rhat_threshold = rhat, ess_threshold = ess
  )
  new_apa_results(
    NA_character_, sentence, sentence, x, target,
    passed = passed, summary = summary
  )
}

#' @rdname apa_convergence
#' @export
apa_convergence.default <- function(x, ..., rhat = 1.01, ess = 400,
                                    digits = 3, markup = NULL) {
  apa_convergence(
    apa_tidy_diagnostics(x, ...),
    rhat = rhat, ess = ess, digits = digits, markup = markup
  )
}

# ---- checks --------------------------------------------------------------

# A parameters table has R-hat and ESS columns too, but only for the rows
# it prints; a convergence statement must cover what was sampled.
check_convergence_table <- function(x, call = rlang::caller_env()) {
  type <- attr(x, "type", exact = TRUE)
  if (type != "diagnostics") {
    cli::cli_abort(
      c(
        "{.fn apa_convergence} reports a {.val diagnostics} table, not a
         {.val {type}} table.",
        i = "Pass the fit, or {.code apa_tidy_diagnostics(fit)}, so that
             every sampled quantity is covered."
      ),
      call = call
    )
  }
  if (nrow(x) == 0) {
    cli::cli_abort("The diagnostics table has no variables.", call = call)
  }
  invisible(x)
}

check_threshold <- function(x, min, strict, arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  ok <- is.numeric(x) && length(x) == 1 && is.finite(x) &&
    (if (strict) x > min else x >= min)
  if (!ok) {
    # nolint next: object_usage_linter. Used in the cli string below.
    bound <- if (strict) paste("greater than", min) else paste(min, "or more")
    cli::cli_abort(
      "{.arg {arg}} must be a single finite number {bound}.",
      call = call
    )
  }
  invisible(x)
}

# ---- the parts -------------------------------------------------------------

# The non-missing count, the extreme in the direction of concern, and how
# many values reach the threshold (R-hat at or above, ESS at or below).
summarise_diagnostic <- function(values, threshold, above) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n == 0) {
    return(list(n = 0L, extreme = NA_real_, flagged = 0L))
  }
  flagged <- if (above) values >= threshold else values <= threshold
  list(
    n = n,
    extreme = if (above) max(values) else min(values),
    flagged = sum(flagged)
  )
}

rhat_part <- function(s, threshold, digits, target) {
  if (s$n == 0) {
    return(NULL)
  }
  name <- symbol("rhat", target)
  if (s$flagged == 0) {
    bound <- round_away(s$extreme, digits, up = TRUE)
    value <- format_num(bound, digits, TRUE, FALSE, target)
    return(stat_string(name, paste(symbol("leq", target), value)))
  }
  paste0(
    count_of(s, target), " ", name, " ", symbol("geq", target), " ",
    format_threshold(threshold, 2, 6, target),
    ", maximum ",
    format_num(s$extreme, digits, TRUE, FALSE, target)
  )
}

ess_part <- function(s, label, threshold, target) {
  if (s$n == 0) {
    return(NULL)
  }
  if (s$flagged == 0) {
    bound <- round_away(s$extreme, 0, up = FALSE)
    value <- format_num(bound, 0, TRUE, TRUE, target)
    return(paste(label, symbol("geq", target), value))
  }
  paste0(
    count_of(s, target), " ", label, " ", symbol("leq", target), " ",
    format_threshold(threshold, 0, 2, target), ", minimum ",
    format_num(s$extreme, 0, TRUE, TRUE, target)
  )
}

divergence_part <- function(divergences, target) {
  if (is.na(divergences)) {
    return(NULL)
  }
  if (divergences == 0) {
    return("no divergent transitions")
  }
  noun <- if (divergences == 1) "transition" else "transitions"
  paste(format_num(divergences, 0, TRUE, TRUE, target), "divergent", noun)
}

# `2 of 13`, with thousands separators.
count_of <- function(s, target) {
  paste(
    format_num(s$flagged, 0, TRUE, TRUE, target), "of",
    format_num(s$n, 0, TRUE, TRUE, target)
  )
}

# Round towards the side a bound may lie on. The small offset keeps a
# value already at the precision where it is: 1.004 is stored as
# 1.00400000000000000355, and a bare ceiling would print 1.005.
round_away <- function(x, digits, up) {
  scale <- 10^digits
  if (up) {
    ceiling(x * scale - 1e-8) / scale
  } else {
    floor(x * scale + 1e-8) / scale
  }
}

# A threshold printed at its own precision, between `min_digits` and
# `max_digits` decimals and independent of `digits`: 1.01 stays `1.01`
# at `digits = 3` and at `digits = 0`, 1.005 stays `1.005`, 400 stays
# `400`.
format_threshold <- function(x, min_digits, max_digits, target) {
  digits <- max_digits
  for (d in seq(min_digits, max_digits)) {
    if (abs(round(x, d) - x) < 1e-12) {
      digits <- d
      break
    }
  }
  format_num(x, digits, TRUE, TRUE, target)
}
