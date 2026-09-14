# apa_print() methods for papaja users (ARCHITECTURE.md decision 5, last
# paragraph; local/specs/spec-apa_print.md). papaja stays in Suggests:
# `@exportS3Method papaja::apa_print` registers each method when papaja's
# namespace loads. Measured 2026-09-14 (local/probes/probe_papaja.R):
# papaja names the results of a multi-row object by a sanitised term
# (`apa_print(lm)$full_result$wt`), and owns methods for BFBayesFactor
# and emmGrid, which apabayes therefore never registers.

#' Results for papaja's `apa_print()`
#'
#' When papaja is installed, `papaja::apa_print()` works on the objects
#' apabayes reports: a brms fit, an rstanarm fit, a lavaan or blavaan
#' fit, a brms hypothesis test, and a stored [apabayes_tidy] table. Each
#' method calls [apa_inline()] and returns its result in the shape papaja
#' users address, so `apa_print(fit)$full_result$wt` works as it does on
#' an `lm`.
#'
#' @section Shape:
#' Without `term`, `estimate`, `statistic` and `full_result` are named
#' lists with one string per row. The names follow papaja's rule:
#' parentheses, backticks and commas are removed and every other
#' character that is not a letter, digit or underscore becomes `_`, so
#' `(Intercept)` is `Intercept` and `visual =~ x1` is `visual_x1`. A
#' parameter is named by its label, a hypothesis by its string, a fit
#' table by its model. A name that would repeat an earlier one gets
#' `_2`, `_3`, …, skipping any name another row has; a row with no
#' usable name is `row` and its position.
#' With `term`, the three elements are plain strings and the result is
#' exactly `apa_inline(x, term, ...)`.
#'
#' The strings use apabayes' markup (see [apa_inline()]'s `markup`), not
#' the LaTeX math papaja writes, and print with a `knit_print` method, so
#' inline code needs no `$full_result` for a single result.
#'
#' @param x A `brmsfit`, `stanreg`, `lavaan`, `blavaan` or
#'   `brmshypothesis` object, or an [apabayes_tidy] table.
#' @param term The row to report, as in [apa_inline()]; `NULL` reports
#'   every row as named lists.
#' @param ... Passed to [apa_inline()]: `rhs`, `op`, `group` and the
#'   formatting options, and on a fitted model the arguments of its
#'   [apa_tidy()] route.
#' @param in_paren `TRUE` writes brackets for the parentheses in the
#'   strings (`χ²[24]`), for a result quoted inside parentheses, as
#'   papaja's argument of that name does.
#'
#' @return An [apa_results] object.
#' @seealso [apa_inline()], which these methods call.
#' @examplesIf rlang::is_installed(c("papaja", "lavaan"))
#' model <- "visual =~ x1 + x2 + x3
#'           textual =~ x4 + x5 + x6"
#' fit <- lavaan::cfa(model, lavaan::HolzingerSwineford1939)
#' r <- papaja::apa_print(fit, standardize = TRUE)
#' r$full_result$visual_x2
#' papaja::apa_print(apa_tidy_sem_fit(fit), in_paren = TRUE)
#' @name apabayes-papaja
NULL

#' @rdname apabayes-papaja
#' @exportS3Method papaja::apa_print
apa_print.apabayes_tidy <- function(x, term = NULL, ..., in_paren = FALSE) {
  apa_print_apabayes(x, term, ..., in_paren = in_paren)
}

#' @rdname apabayes-papaja
#' @exportS3Method papaja::apa_print
apa_print.brmsfit <- function(x, term = NULL, ..., in_paren = FALSE) {
  apa_print_apabayes(x, term, ..., in_paren = in_paren)
}

#' @rdname apabayes-papaja
#' @exportS3Method papaja::apa_print
apa_print.stanreg <- function(x, term = NULL, ..., in_paren = FALSE) {
  apa_print_apabayes(x, term, ..., in_paren = in_paren)
}

#' @rdname apabayes-papaja
#' @exportS3Method papaja::apa_print
apa_print.lavaan <- function(x, term = NULL, ..., in_paren = FALSE) {
  apa_print_apabayes(x, term, ..., in_paren = in_paren)
}

#' @rdname apabayes-papaja
#' @exportS3Method papaja::apa_print
apa_print.blavaan <- function(x, term = NULL, ..., in_paren = FALSE) {
  apa_print_apabayes(x, term, ..., in_paren = in_paren)
}

#' @rdname apabayes-papaja
#' @exportS3Method papaja::apa_print
apa_print.brmshypothesis <- function(x, term = NULL, ..., in_paren = FALSE) {
  apa_print_apabayes(x, term, ..., in_paren = in_paren)
}

# The shared body. `in_paren` is checked here rather than left to `...`:
# the lavaan route ignores an unknown argument (measured), so a typo'd
# flag would vanish.
apa_print_apabayes <- function(x, term, ..., in_paren,
                               call = rlang::caller_env()) {
  check_flag(in_paren, call = call)
  out <- apa_inline(x, term = term, ...)
  strings <- c("estimate", "statistic", "full_result")
  if (in_paren) {
    for (el in strings) {
      out[[el]] <- gsub(
        ")", "]", gsub("(", "[", out[[el]], fixed = TRUE),
        fixed = TRUE
      )
    }
  }
  if (!is.null(term)) {
    return(out)
  }
  row_names <- apa_print_names(out$table)
  for (el in strings) {
    out[[el]] <- stats::setNames(as.list(out[[el]]), row_names)
  }
  out
}

# One name per row: the id column the addresser matches first, with a
# parameter's label where it has one; sanitised, empty names replaced by
# the row position, repeats suffixed in row order.
apa_print_names <- function(table) {
  type <- attr(table, "type", exact = TRUE)
  id <- table[[inline_id_columns(type)[1]]]
  if (type == "parameters" && "label" %in% names(table)) {
    has_label <- !is.na(table$label)
    id[has_label] <- table$label[has_label]
  }
  out <- sanitize_print_names(id)
  empty <- is.na(out) | out == ""
  out[empty] <- paste0("row", which(empty))
  unique_print_names(out)
}

# The first occurrence keeps its name; a later one takes the lowest
# `_<k>` (k >= 2) that no row has as its own name and no earlier row was
# given, so a suffix never takes the name of a different row.
unique_print_names <- function(x) {
  out <- x
  for (i in seq_along(x)[-1]) {
    if (x[i] %in% out[seq_len(i - 1)]) {
      k <- 2L
      while (paste0(x[i], "_", k) %in% c(x, out[seq_len(i - 1)])) {
        k <- k + 1L
      }
      out[i] <- paste0(x[i], "_", k)
    }
  }
  out
}

# papaja's sanitize_terms() for a character vector (papaja 0.1.5), step
# for step and with the same regex engine, so names match what papaja
# users type on papaja's own objects.
sanitize_print_names <- function(x) {
  x <- gsub("\\(|\\)|`", "", x)
  x <- gsub("\\.0+$", "", x)
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub(" + ", "_", x, fixed = TRUE)
  x <- gsub("\\W", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_", "", x)
}
