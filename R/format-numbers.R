# Numbers to text (ARCHITECTURE.md decisions 8, 10, 15). These functions
# are the only place in apabayes where a number becomes a string; the
# table formatters of Milestone 4 wrap them. All are vectorised and pass
# NA through as NA_character_.

# ---- validators -------------------------------------------------------

# A logical vector that is all NA (the bare `NA`) counts as numeric NA.
check_numeric <- function(x, arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (is.logical(x) && all(is.na(x))) {
    return(invisible(as.numeric(x)))
  }
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be numeric, not {.cls {class(x)}}.",
      call = call
    )
  }
  invisible(x)
}

check_digits <- function(digits, min = 0, call = rlang::caller_env()) {
  ok <- is.numeric(digits) && length(digits) == 1 && !is.na(digits) &&
    digits == round(digits) && digits >= min
  if (!ok) {
    cli::cli_abort(
      "{.arg digits} must be a single whole number of {min} or more.",
      call = call
    )
  }
  invisible(digits)
}

check_flag <- function(x, arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!rlang::is_bool(x)) {
    cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.", call = call)
  }
  invisible(x)
}

check_range01 <- function(x, arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  bad <- x[!is.na(x) & (x < 0 | x > 1)]
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.arg {arg}} must lie between 0 and 1; found {.val {bad[1]}}.",
      call = call
    )
  }
  invisible(x)
}

check_nonneg <- function(x, what, arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  bad <- x[!is.na(x) & x < 0]
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.arg {arg}} must be {what} of 0 or more; found {.val {bad[1]}}.",
      call = call
    )
  }
  invisible(x)
}

# The number a value becomes after printf rounding to `digits` decimals;
# regime boundaries compare this, not the raw value. Infinite values
# stay infinite.
as_printed <- function(x, digits) {
  out <- x
  finite <- is.finite(x)
  out[finite] <- as.numeric(formatC(x[finite], digits = digits, format = "f"))
  out
}

# ---- apa_num ----------------------------------------------------------

#' Format numbers in APA style
#'
#' Fixed decimals, an optional leading zero, a thousands separator and the
#' minus sign of the markup target. Every other formatter in apabayes
#' builds on this one.
#'
#' @param x Numeric vector. `NA`, `NaN`, `Inf` and `-Inf` are allowed.
#' @param digits Single whole number of decimals to print (fixed, never
#'   significant digits).
#' @param leading_zero `FALSE` drops the zero before the decimal point
#'   (`.47`, `-.47`), the APA rule for statistics that cannot exceed 1
#'   in absolute value (correlations, standardized paths, proportions).
#'   Values of 1 or more keep their digits.
#' @param big_mark `TRUE` separates groups of three digits with a comma
#'   (`1,240`), the APA rule for numbers of 1,000 or more.
#' @param markup `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`.
#'   `NULL` uses `getOption("apabayes.markup")`, then the knitr output
#'   format, then `"md"`. Decides the minus sign (U+2212 outside
#'   `"plain"`) and the infinity symbol.
#'
#' @details
#' Rounding is C `printf` rounding through [formatC()], the same as
#' `papaja::apa_num()`. A result that would read `-0.00` is printed
#' `0.00`.
#'
#' @section papaja:
#' papaja also exports a function called `apa_num()`. Whichever of the
#' two packages is attached last masks the other's. With both attached,
#' call `apabayes::apa_num()`. papaja's version prints a hyphen where
#' this one prints the minus sign.
#'
#' @return A character vector of `length(x)` without names; `NA` in
#'   gives `NA_character_` out.
#' @seealso [apa_p()], [apa_pd()], [apa_prob()], [apa_ci()], [apa_bf()],
#'   [apa_er()], [apa_rhat_ess()].
#' @examples
#' apa_num(c(0.4712, -0.4712, 1234.5678, NA))
#' apa_num(0.4712, leading_zero = FALSE)
#' apa_num(1240, digits = 0)
#' apa_num(c(Inf, -Inf), markup = "latex")
#' @export
apa_num <- function(x, digits = 2, leading_zero = TRUE, big_mark = TRUE,
                    markup = NULL) {
  x <- check_numeric(x)
  check_digits(digits)
  check_flag(leading_zero)
  check_flag(big_mark)
  target <- markup_target(markup)
  format_num(x, digits, leading_zero, big_mark, target)
}

# The worker: `target` is already resolved and the arguments are checked.
format_num <- function(x, digits, leading_zero, big_mark, target) {
  out <- rep(NA_character_, length(x))
  finite <- is.finite(x)
  if (any(finite)) {
    s <- formatC(x[finite],
      digits = digits, format = "f",
      big.mark = if (big_mark) "," else ""
    )
    s <- sub("^-(0(\\.0*)?)$", "\\1", s)
    if (!leading_zero) {
      s <- sub("^(-?)0\\.", "\\1.", s)
    }
    s <- sub("^-", symbol("minus", target), s)
    out[finite] <- s
  }
  out[is.infinite(x) & x > 0] <- symbol("infinity", target)
  out[is.infinite(x) & x < 0] <- symbol("neg_infinity", target)
  out
}

# ---- apa_p, apa_pd, apa_prob ------------------------------------------

#' Format p values, probabilities of direction and proportions
#'
#' Three decimals without a leading zero, floored at `< .001` and capped
#' at `> .999` (with `digits = 3`), so that a value which cannot be
#' distinguished from 0 or 1 at that precision is never printed as `.000`
#' or `1.000`. Exact 0 and exact 1 fall under the floor and the cap: a
#' probability of direction of 1 from a finite number of draws is
#' reported `> .999`.
#'
#' @param x Numeric vector in \[0, 1\]; `NA` allowed.
#' @param digits Single whole number of decimals, at least 1. For
#'   `apa_prob()` the default is 3 for proportions and 1 for percentages.
#' @param symbol `TRUE` prepends the statistic symbol in the markup of the
#'   target: `*p* = .023`, `*pd* > .999`.
#' @param percent `apa_prob()` only: print `12.3%` instead of `.123`. The
#'   floor and cap apply on the percentage scale (`< 0.1%`, `> 99.9%`).
#' @inheritParams apa_num
#'
#' @details
#' The probability of direction (pd) is the share of the posterior on
#' the side of the median's sign. It says how certain the sign of an
#' effect is and carries no information in favour of a null value; a pd
#' of `.500` means the sign is undetermined, not that the effect is
#' absent (Makowski et al., 2019). Report an interval or a ROPE share
#' next to it when evidence for a null is the question.
#'
#' @section apa7 and papaja:
#' apa7 and papaja also export a function called `apa_p()`. Whichever
#' package is attached last masks the others. With more than one
#' attached, call `apabayes::apa_p()`. The versions do not print the
#' same: `apa7::apa_p()` gives `.01` where this one gives `.012`.
#'
#' @return A character vector of `length(x)`; `NA` in gives
#'   `NA_character_` out.
#' @references
#' Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D.
#' (2019). Indices of effect existence and significance in the Bayesian
#' framework. *Frontiers in Psychology, 10*, 2767.
#' \doi{10.3389/fpsyg.2019.02767}
#' @seealso [apa_num()] for the shared rounding, [apa_bf()] for evidence
#'   in favour of a hypothesis.
#' @examples
#' apa_p(c(0.0234, 0.0004, 0.9996, NA))
#' apa_p(0.0234, symbol = TRUE)
#' apa_pd(c(0.9874, 1), symbol = TRUE)
#' apa_prob(0.1234)
#' apa_prob(0.1234, percent = TRUE)
#' @export
apa_p <- function(x, digits = 3, markup = NULL, symbol = FALSE) {
  x <- check_numeric(x)
  check_range01(x)
  check_digits(digits, min = 1)
  check_flag(symbol)
  target <- markup_target(markup)
  out <- format_bounded(x, digits)
  if (symbol) {
    out <- stat_string(markup("p", target, italic = TRUE), out)
  }
  out
}

#' @rdname apa_p
#' @export
apa_pd <- function(x, digits = 3, markup = NULL, symbol = FALSE) {
  x <- check_numeric(x)
  check_range01(x)
  check_digits(digits, min = 1)
  check_flag(symbol)
  target <- markup_target(markup)
  out <- format_bounded(x, digits)
  if (symbol) {
    out <- stat_string(markup("pd", target, italic = TRUE), out)
  }
  out
}

#' @rdname apa_p
#' @export
apa_prob <- function(x, digits = NULL, percent = FALSE, markup = NULL) {
  x <- check_numeric(x)
  check_range01(x)
  check_flag(percent)
  digits <- digits %||% if (percent) 1 else 3
  check_digits(digits, min = 1)
  markup_target(markup)
  format_bounded(x, digits, percent = percent)
}

# Floor, cap and leading-zero rule shared by the three probability
# formatters. Values are in [0, 1]; `eps` is one unit of the last digit.
format_bounded <- function(x, digits, percent = FALSE) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) {
    return(out)
  }
  eps <- 10^-digits
  if (percent) {
    scale <- 100
    fmt <- function(v) paste0(formatC(v, digits = digits, format = "f"), "%")
  } else {
    scale <- 1
    fmt <- function(v) {
      sub("^0\\.", ".", formatC(v, digits = digits, format = "f"))
    }
  }
  v <- x[ok] * scale
  below <- v < eps
  above <- v > scale - eps
  s <- fmt(v)
  s[below] <- paste0("< ", fmt(eps))
  s[above] <- paste0("> ", fmt(scale - eps))
  out[ok] <- s
  out
}

# ---- apa_ci -----------------------------------------------------------

#' Format an interval
#'
#' `95% CrI [0.20, 0.74]`, with the same digits and leading-zero rule as
#' the estimate it accompanies and a label that names the interval type.
#'
#' @param low,high Numeric vectors of the same length (one of them may
#'   have length 1); `NA` and infinite bounds allowed.
#' @param level Single number in (0, 1), printed as a percentage before
#'   the label: `0.95` gives `95%`, `0.9` gives `90%`.
#' @param label Interval name after the level: `"CrI"` (equal-tailed
#'   credible interval, the default), `"HDI"`, `"HPD"`, `"CI"`. `NULL`
#'   drops the prefix and returns the bracket only, for table cells whose
#'   header carries the level.
#' @inheritParams apa_num
#'
#' @details
#' The label is part of the report: a reader must be able to tell an
#' equal-tailed interval from a highest-density interval, because the two
#' differ for skewed posteriors. apabayes never relabels; the caller
#' says which interval was computed.
#'
#' @return A character vector; `NA` in either bound gives
#'   `NA_character_` for that element.
#' @examples
#' apa_ci(0.2, 0.74)
#' apa_ci(0.2, 0.74, leading_zero = FALSE)
#' apa_ci(0.2, 0.74, label = "HDI", level = 0.9)
#' apa_ci(-0.5, 0.74, label = NULL)
#' @export
apa_ci <- function(low, high, level = 0.95, label = "CrI", digits = 2,
                   leading_zero = TRUE, big_mark = TRUE, markup = NULL) {
  low <- check_numeric(low)
  high <- check_numeric(high)
  n <- max(length(low), length(high))
  if (!all(c(length(low), length(high)) %in% c(1L, n))) {
    cli::cli_abort("{.arg low} and {.arg high} must have the same length.")
  }
  level_ok <- is.numeric(level) && length(level) == 1 && !is.na(level) &&
    level > 0 && level < 1
  if (!level_ok) {
    cli::cli_abort("{.arg level} must be a single number between 0 and 1.")
  }
  if (!is.null(label) && !rlang::is_string(label)) {
    cli::cli_abort("{.arg label} must be a single string or NULL.")
  }
  check_digits(digits)
  check_flag(leading_zero)
  check_flag(big_mark)
  target <- markup_target(markup)
  if (n == 0) {
    return(character(0))
  }
  low <- rep_len(low, n)
  high <- rep_len(high, n)
  reversed <- !is.na(low) & !is.na(high) & high < low
  if (any(reversed)) {
    cli::cli_abort(c(
      "{.arg high} must not be below {.arg low}.",
      "x" = "Found [{low[reversed][1]}, {high[reversed][1]}]."
    ))
  }
  lo <- format_num(low, digits, leading_zero, big_mark, target)
  hi <- format_num(high, digits, leading_zero, big_mark, target)
  prefix <- if (is.null(label)) {
    ""
  } else {
    paste0(format_level(level), "% ", label, " ")
  }
  out <- paste0(prefix, "[", lo, ", ", hi, "]")
  out[is.na(lo) | is.na(hi)] <- NA_character_
  out
}

format_level <- function(level) {
  as.character(round(level * 100, 3))
}

# ---- apa_bf -----------------------------------------------------------

#' Format Bayes factors
#'
#' Prints a Bayes factor as a number, in the regime that keeps it
#' readable, with the subscript that matches the direction of the number.
#' No verbal category is attached; see [apa_bf_label()] for the opt-in
#' helper and the caveat.
#'
#' @param x Numeric vector of Bayes factors **as BF10**, evidence for H1
#'   over H0, the scale that `bayestestR`, `brms::hypothesis()` and
#'   `BayesFactor` return. Values of 0 or more; `NA` and `Inf` allowed.
#' @param direction `"10"` prints BF10 as given; `"01"` prints
#'   BF01 = 1 / BF10 and the subscript `01`.
#' @param style Regime selection; see Details.
#' @param digits Decimals in the two-decimal regime and in the mantissa
#'   under `style = "sci"`.
#' @param symbol `TRUE` prepends `*BF*~10~ = ` (or `*BF*~01~ = `).
#' @inheritParams apa_num
#'
#' @details
#' With `style = "auto"`, the value after `direction` is applied prints
#' as
#' * the infinity symbol when infinite, `0` when zero;
#' * a mantissa to one decimal times a power of ten when 10,000 or more,
#'   or below `10^-digits` (`1.2 × 10^5^`, `4.0 × 10^−4^`), so that a
#'   small BF10 never prints as `0.00`;
#' * one decimal from 10 up to 10,000 (`20.9`);
#' * `digits` decimals otherwise (`5.34`).
#'
#' `style = "sci"` prints every finite positive value as a mantissa with
#' `digits` decimals times a power of ten (`2.09 × 10^1^`).
#' `style = "plain"` never uses scientific notation: one decimal from 10
#' up, `digits` decimals below.
#'
#' The reporting guidelines this package follows treat the Bayes factor
#' as a continuous measure of relative evidence and ask for the number
#' with an unambiguous direction (Tendeiro et al., 2024; van Doorn et
#' al., 2021). Report the prior, the estimation method and the posterior
#' estimate next to it, and do not read a value near 1 as evidence of
#' absence.
#'
#' @return A character vector of `length(x)`; `NA` in gives
#'   `NA_character_` out.
#' @references
#' Tendeiro, J. N., Kiers, H. A. L., Hoekstra, R., Wong, T. K., & Morey,
#' R. D. (2024). Diagnosing the misuse of the Bayes factor in applied
#' research. *Advances in Methods and Practices in Psychological
#' Science, 7*(1). \doi{10.1177/25152459231213371}
#'
#' van Doorn, J., van den Bergh, D., Böhm, U., Dablander, F., Derks, K.,
#' Draws, T., Etz, A., Evans, N. J., Gronau, Q. F., Haaf, J. M., Hinne,
#' M., Kucharský, Š., Ly, A., Marsman, M., Matzke, D., Gupta, A. R. K.
#' N., Sarafoglou, A., Stefan, A., Voelkel, J. G., & Wagenmakers, E.-J.
#' (2021). The JASP guidelines for conducting and reporting a Bayesian
#' analysis. *Psychonomic Bulletin & Review, 28*(3), 813–826.
#' \doi{10.3758/s13423-020-01798-5}
#' @seealso [apa_er()] for evidence ratios from `brms::hypothesis()`,
#'   [apa_bf_label()] for verbal categories.
#' @examples
#' apa_bf(c(0.05, 5.3412, 20.86, 123456, Inf, NA))
#' apa_bf(20.86, direction = "01", symbol = TRUE)
#' apa_bf(20.86, style = "sci")
#' apa_bf(123456, markup = "latex")
#' @export
apa_bf <- function(x, direction = c("10", "01"),
                   style = c("auto", "sci", "plain"), digits = 2,
                   big_mark = TRUE, markup = NULL, symbol = FALSE) {
  x <- check_numeric(x)
  direction <- rlang::arg_match(direction)
  style <- rlang::arg_match(style)
  check_digits(digits)
  check_flag(big_mark)
  check_flag(symbol)
  check_nonneg(x, "Bayes factors")
  target <- markup_target(markup)
  v <- if (direction == "01") 1 / x else x
  out <- format_bf_value(v, style, digits, big_mark, target)
  if (symbol) {
    name <- markup("BF", target, italic = TRUE, subscript = direction)
    out <- stat_string(name, out)
  }
  out
}

format_bf_value <- function(v, style, digits, big_mark, target) {
  out <- rep(NA_character_, length(v))
  out[is.infinite(v)] <- symbol("infinity", target)
  out[!is.na(v) & v == 0] <- "0"
  pos <- is.finite(v) & v > 0
  if (!any(pos)) {
    return(out)
  }
  p <- v[pos]
  fixed <- function(values, d) format_num(values, d, TRUE, big_mark, target)
  res <- character(length(p))
  if (style == "sci") {
    res <- format_sci(p, digits, target)
  } else {
    # Upper boundaries are judged on the value as it would print, so a
    # value that rounds up to the next regime is printed in that regime
    # (9,999.99 never prints as "10,000.0").
    sci <- style == "auto" & (as_printed(p, 1) >= 1e4 | p < 10^-digits)
    one <- !sci & as_printed(p, digits) >= 10
    two <- !sci & !one
    res[sci] <- format_sci(p[sci], 1, target)
    res[one] <- fixed(p[one], 1)
    res[two] <- fixed(p[two], digits)
  }
  out[pos] <- res
  out
}

# Mantissa times a power of ten, composed from the symbol table. A
# mantissa that rounds to 10 carries into the exponent.
format_sci <- function(p, digits, target) {
  if (length(p) == 0) {
    return(character(0))
  }
  e <- floor(log10(p))
  m <- p / 10^e
  carry <- as.numeric(formatC(m, digits = digits, format = "f")) >= 10
  e[carry] <- e[carry] + 1
  m[carry] <- m[carry] / 10
  mantissa <- format_num(m, digits, TRUE, FALSE, target)
  exponent <- as.character(e)
  exponent[e < 0] <- paste0(symbol("minus", target), abs(e[e < 0]))
  paste0(
    mantissa, " ", symbol("times", target), " ",
    markup("10", target, superscript = exponent)
  )
}

# ---- apa_bf_label -----------------------------------------------------

#' Verbal category for a Bayes factor (opt-in)
#'
#' Returns the words a published labelling scheme assigns to a Bayes
#' factor, for authors whose venue asks for them. This is the only
#' function in apabayes that turns evidence into words. No other
#' function calls it, and nothing attaches its output to a number unless
#' the user does.
#'
#' @param x Numeric vector of Bayes factors as BF10; values of 0 or more,
#'   `NA` and `Inf` allowed.
#' @param scheme The scheme, chosen explicitly: `"jeffreys"` or
#'   `"raftery"`. There is no default; see Details.
#' @inheritParams apa_num
#'
#' @details
#' Verbal categories are interpretation aids, not part of the statistic.
#' The Bayes factor is a continuous measure of relative evidence, and the
#' guidelines this package follows recommend reporting the number itself
#' with its direction made explicit (Tendeiro et al., 2024; van Doorn et
#' al., 2021; Heck et al., 2023). Use a label only where a venue requires
#' one, name the scheme in the text, and never let the label replace the
#' number.
#'
#' The category is looked up on the evidence in favour of whichever
#' hypothesis the value supports (`x` for H1, `1 / x` for H0), and the
#' result names that hypothesis: `"moderate evidence for H~1~"`. A value
#' of exactly 1 gives `"no evidence for either hypothesis"`. Bounds are
#' inclusive on the upper side: a Bayes factor of 3 is the last value in
#' the lowest category, as `effectsize::interpret_bf()` reads them.
#'
#' Thresholds and words (BF in favour, upper bound inclusive):
#' * `"jeffreys"`: up to 3 anecdotal, 10 moderate, 30 strong, 100 very
#'   strong, above 100 extreme.
#' * `"raftery"`: up to 3 weak, 20 positive, 150 strong, above 150 very
#'   strong.
#'
#' Both tables are taken from `effectsize::interpret_bf()` (version
#' 1.0.3, rules `"jeffreys1961"` and `"raftery1995"`), whose help page
#' cites Jeffreys (1961) and Raftery (1995). The primary sources were not
#' consulted when this function was written; check the thresholds against
#' them before relying on the attribution in a manuscript. The scheme of
#' Lee and Wagenmakers (2013), which uses the same five words as the
#' `"jeffreys"` table above, is not offered until its table has been
#' verified against the book.
#'
#' @return A character vector of `length(x)`; `NA` in gives
#'   `NA_character_` out.
#' @references
#' Heck, D. W., Boehm, U., Böing-Messing, F., Bürkner, P.-C., Derks, K.,
#' Dienes, Z., Fu, Q., Gu, X., Karimova, D., Kiers, H. A. L., Klugkist,
#' I., Kuiper, R. M., Lee, M. D., Leenders, R., Leplaa, H. J., Linde, M.,
#' Ly, A., Meijerink-Bosman, M., Moerbeek, M., ... Hoijtink, H. (2023). A
#' review of applications of the Bayes factor in psychological research.
#' *Psychological Methods, 28*(3), 558–579. \doi{10.1037/met0000454}
#'
#' Jeffreys, H. (1961). *Theory of probability* (3rd ed.). Oxford
#' University Press.
#'
#' Raftery, A. E. (1995). Bayesian model selection in social research.
#' *Sociological Methodology, 25*, 111–163.
#'
#' Tendeiro, J. N., Kiers, H. A. L., Hoekstra, R., Wong, T. K., & Morey,
#' R. D. (2024). Diagnosing the misuse of the Bayes factor in applied
#' research. *Advances in Methods and Practices in Psychological
#' Science, 7*(1). \doi{10.1177/25152459231213371}
#'
#' van Doorn, J., et al. (2021). The JASP guidelines for conducting and
#' reporting a Bayesian analysis. *Psychonomic Bulletin & Review, 28*(3),
#' 813–826. \doi{10.3758/s13423-020-01798-5}
#' @seealso [apa_bf()], which prints the number.
#' @examples
#' apa_bf_label(c(0.2, 1, 5, 50, Inf), scheme = "jeffreys")
#' apa_bf_label(5, scheme = "raftery", markup = "plain")
#' @export
apa_bf_label <- function(x, scheme, markup = NULL) {
  if (missing(scheme)) {
    cli::cli_abort(c(
      "{.arg scheme} must be chosen explicitly.",
      "i" = "Schemes: {.val {names(bf_label_schemes)}}.",
      "i" = paste(
        "The Bayes factor is a continuous measure of evidence; verbal",
        "categories are interpretation aids, not part of the statistic.",
        "Report the number with {.fn apa_bf} and add a label only where a",
        "venue requires one."
      )
    ))
  }
  scheme <- rlang::arg_match0(scheme, names(bf_label_schemes))
  x <- check_numeric(x)
  check_nonneg(x, "Bayes factors")
  target <- markup_target(markup)
  rules <- bf_label_schemes[[scheme]]
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  for_h1 <- ok & x > 1
  for_h0 <- ok & x < 1
  favour <- x
  favour[for_h0] <- 1 / x[for_h0]
  bin <- findInterval(favour, rules$bounds, left.open = TRUE)
  category <- rules$words[bin + 1]
  out[for_h1] <- paste0(
    category[for_h1], " evidence for ",
    markup("H", target, subscript = "1")
  )
  out[for_h0] <- paste0(
    category[for_h0], " evidence for ",
    markup("H", target, subscript = "0")
  )
  out[ok & x == 1] <- "no evidence for either hypothesis"
  out
}

# Upper bounds (inclusive) and the words for each interval; one word
# more than bounds. Source: effectsize 1.0.3, interpret_bf(), read
# 2026-09-06.
bf_label_schemes <- list(
  jeffreys = list(
    bounds = c(3, 10, 30, 100),
    words = c("anecdotal", "moderate", "strong", "very strong", "extreme")
  ),
  raftery = list(
    bounds = c(3, 20, 150),
    words = c("weak", "positive", "strong", "very strong")
  )
)

# ---- apa_er -----------------------------------------------------------

#' Format evidence ratios
#'
#' Evidence ratios from `brms::hypothesis()` (`Evid.Ratio`): a
#' Savage–Dickey density ratio for a point hypothesis, posterior odds for
#' a directional one. Printed as `≥ 10,000` from 10,000 up (an infinite
#' ratio means no posterior draw contradicted the hypothesis), rounded to
#' an integer with a thousands separator from 1,000, to one decimal from
#' 10, and to `digits` decimals below.
#'
#' @param x Numeric vector of evidence ratios, 0 or more; `NA` and `Inf`
#'   allowed.
#' @param digits Decimals below 10.
#' @param symbol `TRUE` prepends `ER = ` (or `ER ≥ ` at the bound).
#' @inheritParams apa_num
#'
#' @details
#' Small ratios print `0.00`; apabayes keeps the ratio as brms reports
#' it and, for point hypotheses, reports `1 / Evid.Ratio` through
#' [apa_bf()] as the Bayes factor against equality.
#'
#' @return A character vector of `length(x)`; `NA` in gives
#'   `NA_character_` out.
#' @examples
#' apa_er(c(0.5, 5.3412, 23.456, 2345.6, 12345, Inf, NA))
#' apa_er(12345, symbol = TRUE)
#' @export
apa_er <- function(x, digits = 2, markup = NULL, symbol = FALSE) {
  x <- check_numeric(x)
  check_digits(digits)
  check_flag(symbol)
  check_nonneg(x, "evidence ratios")
  target <- markup_target(markup)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  # Regimes are judged on the value as it would print in the regime
  # below, so 9,999.6 reaches the bound and 999.96 the integer regime.
  bound <- ok & as_printed(x, 0) >= 1e4
  thousands <- ok & !bound & as_printed(x, 1) >= 1e3
  tens <- ok & !bound & !thousands & as_printed(x, digits) >= 10
  small <- ok & !bound & !thousands & !tens
  out[bound] <- paste(symbol("geq", target), "10,000")
  out[thousands] <- format_num(round(x[thousands]), 0, TRUE, TRUE, target)
  out[tens] <- format_num(x[tens], 1, TRUE, TRUE, target)
  out[small] <- format_num(x[small], digits, TRUE, TRUE, target)
  if (symbol) {
    out <- stat_string("ER", out)
  }
  out
}

# ---- apa_rhat_ess -----------------------------------------------------

#' Format convergence diagnostics
#'
#' `*R̂* = 1.00, bulk ESS = 1,240, tail ESS = 980` for one parameter, or
#' for the extremes over a fit. Parts that are `NULL` are omitted; the
#' order is always R-hat, bulk ESS, tail ESS.
#'
#' @param rhat,ess_bulk,ess_tail Numeric vectors or `NULL`. Vectors must
#'   share one length, or have length 1. At least one must be given.
#' @param digits Decimals for R-hat (effective sample sizes print as
#'   integers with a thousands separator).
#' @inheritParams apa_num
#'
#' @return A character vector of the common length; an `NA` in any given
#'   part gives `NA_character_` for that element.
#' @examples
#' apa_rhat_ess(1.003, 1240, 980)
#' apa_rhat_ess(1.003, digits = 3, markup = "plain")
#' apa_rhat_ess(ess_bulk = c(1240, 400))
#' @export
apa_rhat_ess <- function(rhat = NULL, ess_bulk = NULL, ess_tail = NULL,
                         digits = 2, markup = NULL) {
  parts <- list(rhat = rhat, ess_bulk = ess_bulk, ess_tail = ess_tail)
  given <- !vapply(parts, is.null, logical(1))
  if (!any(given)) {
    cli::cli_abort(
      "Supply at least one of {.arg rhat}, {.arg ess_bulk} or {.arg ess_tail}."
    )
  }
  for (nm in names(parts)[given]) {
    parts[[nm]] <- check_numeric(parts[[nm]], arg = nm)
  }
  check_digits(digits)
  lens <- lengths(parts[given])
  n <- max(lens)
  if (!all(lens %in% c(1L, n))) {
    cli::cli_abort(
      "{.arg rhat}, {.arg ess_bulk} and {.arg ess_tail} must have the same length." # nolint: line_length_linter. One cli message string.
    )
  }
  target <- markup_target(markup)
  if (n == 0) {
    return(character(0))
  }
  values <- list()
  if (given[["rhat"]]) {
    values$rhat <- format_num(rep_len(rhat, n), digits, TRUE, FALSE, target)
  }
  if (given[["ess_bulk"]]) {
    values$ess_bulk <- format_num(rep_len(ess_bulk, n), 0, TRUE, TRUE, target)
  }
  if (given[["ess_tail"]]) {
    values$ess_tail <- format_num(rep_len(ess_tail, n), 0, TRUE, TRUE, target)
  }
  labels <- c(
    rhat = symbol("rhat", target), ess_bulk = "bulk ESS",
    ess_tail = "tail ESS"
  )
  pieces <- lapply(names(values), function(nm) {
    paste(labels[[nm]], "=", values[[nm]])
  })
  out <- do.call(paste, c(pieces, sep = ", "))
  any_na <- Reduce(`|`, lapply(values, is.na))
  out[any_na] <- NA_character_
  out
}
