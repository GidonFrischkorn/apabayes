# The inline result object (ARCHITECTURE.md decision 5): papaja's
# four-slot shape under an apabayes class. Measured 2026-09-14
# (local/probes/probe_inline.R): knitr's inline hook ignores
# `as.character`, `format` and `print` methods and prints every element
# of a list, so `knit_print` is the method that makes `` `r apa_inline() ` ``
# work; papaja's own object has no methods and a class vector of
# `c("apa_results", "list")`, which apabayes inherits rather than owns.

#' Inline results
#'
#' Every [apa_inline()] method returns an `apabayes_results` object: the
#' strings for the running text next to the numbers they were made from.
#' It has the four elements papaja's `apa_results` has, and inherits that
#' class, so `papaja::apa_table()` accepts it.
#'
#' @section Elements:
#' \describe{
#'   \item{`estimate`}{character; the estimate with its interval, one
#'     per reported row (`*b* = 0.31, 95% CrI [0.12, 0.50]`), or one for
#'     the whole table when the string describes all of it, as
#'     [apa_convergence()]'s does. `NA` for a table kind that has no
#'     estimate.}
#'   \item{`statistic`}{character; the statistics that follow it
#'     (`*pd* > .999`), or `NA` when the row carries none.}
#'   \item{`full_result`}{character; the two joined with a comma, which
#'     is what prints.}
#'   \item{`table`}{the reported rows, as an [apabayes_tidy] table with
#'     its attributes.}
#'   \item{`markup`}{the markup target the strings were written for.}
#' }
#'
#' @section In a document:
#' Inline code such as `` `r apa_inline(fit, "wt")` `` prints
#' `full_result` through a `knit_print` method; no `$full_result` is
#' needed. `print()`, `format()` and `as.character()` return the same
#' string at the console.
#'
#' @param x An object.
#' @param ... Ignored, or passed on by knitr.
#' @param inline Set by knitr: `TRUE` for inline code, `FALSE` for a
#'   chunk.
#' @return `is_apa_results()` returns a logical scalar; `format()` and
#'   `as.character()` return `full_result`; `print()` returns `x`
#'   invisibly.
#' @seealso [apa_inline()].
#' @examples
#' t <- apabayes_tidy(
#'   data.frame(
#'     term = "b_wt", estimate = -5.34, ci_low = -6.85,
#'     ci_high = -3.82, pd = 0.9995, component = "conditional"
#'   ),
#'   type = "parameters", centrality = "median", ci_method = "eti",
#'   ci_level = 0.95
#' )
#' r <- apa_inline(t, "b_wt")
#' r
#' is_apa_results(r)
#' r$estimate
#' r$statistic
#' @name apa_results
NULL

# The constructor. `table` is the rows the strings describe; the three
# string vectors have one element per row, or one element for the whole
# table (the convergence sentence of apa_convergence()). Extra named
# elements (its `passed` flag and `summary`) travel through `...`.
new_apa_results <- function(estimate, statistic, full_result, table,
                            markup, ...) {
  if (!is_apabayes_tidy(table)) {
    cli::cli_abort(
      "{.arg table} must be an {.cls apabayes_tidy} object, not
       {.cls {class(table)}}."
    )
  }
  validate_apabayes_tidy(table)
  n <- nrow(table)
  strings <- list(
    estimate = estimate, statistic = statistic, full_result = full_result
  )
  for (nm in names(strings)) {
    v <- strings[[nm]]
    if (!is.character(v) || !length(v) %in% c(n, 1L)) {
      # nolint next: object_usage_linter. Used in the cli string below.
      allowed <- if (n == 1L) "1" else paste(n, "or 1")
      cli::cli_abort(
        "{.arg {nm}} must be a character vector of length {allowed} (one
         per row of {.arg table}, or one for the whole table), not
         {.cls {class(v)}} of length {length(v)}."
      )
    }
  }
  extra <- rlang::list2(...)
  if (length(extra) > 0 && !rlang::is_named(extra)) {
    cli::cli_abort("Every extra element in {.arg ...} must be named.")
  }
  structure(
    c(strings, list(table = table, markup = markup_target(markup)), extra),
    class = c("apabayes_results", "apa_results", "list")
  )
}

#' @rdname apa_results
#' @export
is_apa_results <- function(x) {
  inherits(x, "apabayes_results")
}

#' @rdname apa_results
#' @export
print.apabayes_results <- function(x, ...) {
  cat(x$full_result, sep = "\n")
  invisible(x)
}

#' @rdname apa_results
#' @export
format.apabayes_results <- function(x, ...) {
  x$full_result
}

#' @rdname apa_results
#' @export
as.character.apabayes_results <- function(x, ...) {
  x$full_result
}

#' @rdname apa_results
#' @exportS3Method knitr::knit_print
knit_print.apabayes_results <- function(x, ..., inline = FALSE) {
  if (inline) {
    return(x$full_result)
  }
  knitr::normal_print(x)
}
