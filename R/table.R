# The table layer (ARCHITECTURE.md decision 6 as amended in
# spec-apa_table.md): a tidy table in, the character tibble that
# `apa7::apa_flextable()` renders out, with the note text attached. The
# tidy method is the whole layer; the default method extracts first and
# then calls it, as `apa_inline()` does. The per-type columns and notes
# live in the parts file.

#' Report a result as an APA table
#'
#' `apa_table()` turns a tidy table (or a fitted model) into the tibble
#' that [apa7::apa_flextable()] renders in an apaquarto manuscript: every
#' value already formatted as text, every header already its APA
#' markdown, and the text of the table note attached for [apa_note()].
#' It is the table counterpart of [apa_inline()] and takes the same
#' formatting options.
#'
#' @section What is printed:
#' A `parameters` table has one row per parameter: the label column
#' (`Path` when every term is a structural-equation path, else
#' `Predictor`), the estimate (`*Mdn*` for a posterior median, `*M*` for a
#' posterior mean, `Estimate` otherwise), the interval, and then, in this
#' order, whichever of `*pd*`, `% in ROPE`, `*BF*~10~` and `*p*` the
#' table carries. `*R̂*`, `ESS~bulk~` and `ESS~tail~` are added with
#' `stats`. The interval header reads `95% CrI`, `95% HDI` or `95% CI`,
#' from the rows' own `ci_method` and `ci_level`; rows that disagree on
#' the level carry it in their cells (`90% [0.12, 0.48]`), and rows that
#' disagree on the method carry their label (`HDI [0.12, 0.48]`) under
#' the header `95% Interval` (`Interval` when the levels differ too). A
#' bare `CI` label, which apa7 would format as a column of its own, moves
#' into the cells the same way, and a `ci_label` that apa7 would format
#' is refused. A `diagnostics` table from
#' [apa_tidy_diagnostics()] has a `Term` column and `*R̂*`, `ESS~bulk~`
#' and `ESS~tail~`.
#'
#' Every number goes through the format layer ([apa_num()], [apa_pd()],
#' [apa_prob()], [apa_bf()], [apa_p()]) and is decimal-aligned with
#' [apa7::align_chr()], except Bayes factors, whose markup the alignment
#' would count as digits. A missing value is an empty cell. The ROPE
#' share drops its `%`, which the header carries. No header equals a
#' column name apa7 formats itself, so `apa_flextable()` renders the table
#' as it is.
#'
#' The note defines the abbreviations the table shows (`*Mdn* = posterior
#' median; CrI = equal-tailed credible interval; *pd* = probability of
#' direction.`), states that the estimates are standardized when every
#' row is, and on a diagnostics table counts the divergent transitions.
#' Nothing printed judges the result.
#'
#' Hypotheses, model comparisons, Bayes factor tables, fit indices,
#' contrasts and correlations are not tabulated yet: `apa_table()`
#' refuses them by name, and [apa_inline()] reports them.
#'
#' @section Rendering in apaquarto:
#' Make the table in an earlier chunk, then name the note in the chunk
#' option `apa-note` of the chunk that renders it:
#'
#' ````
#' ```{r}
#' tab <- apa_table(fit)
#' ```
#'
#' ```{r}
#' #| label: tbl-fit
#' #| tbl-cap: Posterior summary of the regression
#' #| apa-note: !expr apa_note(tab)
#' apa7::apa_flextable(tab)
#' ```
#' ````
#'
#' The two chunks are needed because knitr evaluates an `!expr` chunk
#' option before it runs the chunk, so a table made in the same chunk does
#' not exist yet when the note is read. `apa_note()` refuses a table with
#' nothing to define rather than return `NULL` or `""`, because apaquarto
#' renders either as an empty note, `Note. {}`; remove the `apa-note`
#' option for such a table. `apa_flextable()` drops the attributes of the
#' tibble, so the note is read from the tibble, never from the flextable.
#'
#' @section papaja:
#' papaja also exports a function called `apa_table()`. Whichever of the
#' two packages is attached last masks the other's. With both attached,
#' call `apabayes::apa_table()`.
#'
#' @section The default method:
#' On a fitted model or any other object [apa_tidy()] accepts, the default
#' method calls `apa_tidy(x, ...)` and tabulates the result, so `...`
#' takes that route's arguments: `standardize = TRUE` on a lavaan or
#' blavaan fit, `effects = "all"` on a brms fit, `ci = "hdi"` or `rope =`
#' on any posterior.
#'
#' @param x An [apabayes_tidy] table, or an object [apa_tidy()] accepts.
#'   For `apa_note()`, a table `apa_table()` returned.
#' @param ... Tidy method: must be empty. Default method: passed to
#'   [apa_tidy()].
#' @param stats `NULL` for the default, or a character vector naming the
#'   statistic columns to show: `"pd"`, `"rope"`, `"bf"`, `"p"`,
#'   `"rhat"`, `"ess_bulk"`, `"ess_tail"` for a parameters table (the
#'   default shows each of the first four that has a value in some row),
#'   `"rhat"`, `"ess_bulk"`, `"ess_tail"` for a diagnostics table (the
#'   default shows each that has a value in some row). The column order is
#'   fixed, whatever the order of `stats`; `character()` shows the label,
#'   the estimate and the interval alone. Naming a statistic the table
#'   has no value of is an error.
#' @param interval `FALSE` drops the interval column.
#' @param ci_label `"auto"` labels the interval from the rows' `ci_method`
#'   (`CrI`, `HDI`, `HPD`, `SPI`, `BCI`, `CI`); a string replaces the label
#'   in the header and in the note, where the definition still follows
#'   `ci_method`. `NULL` is an error: an interval column needs a header
#'   that names it.
#' @param digits Decimals for estimates, interval bounds and R-hat; `NULL`
#'   is 2.
#' @param digits_prob Decimals for pd and p. The ROPE share keeps
#'   [apa_prob()]'s own percentage digits, as in [apa_inline()].
#' @param leading_zero `"auto"` drops the leading zero of the estimate and
#'   its bounds on standardized rows and keeps it elsewhere; `TRUE` or
#'   `FALSE` force one rule.
#' @param bf,bf_direction Passed to [apa_bf()] as `style` and `direction`;
#'   the direction also sets the subscript of the header.
#' @param group_rows `TRUE` adds a leading `Component` column (parameters
#'   tables only) for `apa7::apa_flextable(row_title_column = Component)`:
#'   `Population-level`, `Group-level (<group>)`, the component as
#'   recorded (`sigma`, `Loading`), or `Other`. Rows are reordered stably
#'   so that each title is one run.
#'
#' @return `apa_table()`: a tibble with one character column per shown
#'   column, named by its markdown header, one row per row of the tidy
#'   table, and the attributes `note` (a markdown string, or `NA` when the
#'   table shows nothing that needs defining) and `table_type`.
#'   `apa_note()`: the note, a single string.
#' @seealso [apa_inline()] for the running text, [apa_tidy()] for the
#'   tables, [apa7::apa_flextable()] for rendering.
#' @examples
#' t <- apabayes_tidy(
#'   data.frame(
#'     term = c("b_Intercept", "b_wt", "sigma"),
#'     label = c("(Intercept)", "wt", "sigma"),
#'     estimate = c(37.3, -5.34, 2.71), ci_low = c(31.2, -6.85, 2.09),
#'     ci_high = c(43.1, -3.82, 3.60), pd = c(1, 0.9995, 1),
#'     rhat = c(1.001, 1.002, 1.004), ess_bulk = c(1520, 1633, 2401),
#'     component = c("conditional", "conditional", "sigma")
#'   ),
#'   type = "parameters", centrality = "median", ci_method = "eti",
#'   ci_level = 0.95
#' )
#' tab <- apa_table(t)
#' tab
#' apa_note(tab)
#' apa_table(t, stats = c("pd", "rhat", "ess_bulk"), group_rows = TRUE)
#' @examplesIf rlang::is_installed("lavaan")
#' hs <- lavaan::HolzingerSwineford1939
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3", hs)
#' apa_table(fit, standardize = TRUE)
#' @export
apa_table <- function(x, ...) {
  UseMethod("apa_table")
}

#' @rdname apa_table
#' @export
apa_table.apabayes_tidy <- function(x, ..., stats = NULL, interval = TRUE,
                                    ci_label = "auto", digits = NULL,
                                    digits_prob = 3, leading_zero = "auto",
                                    bf = c("auto", "sci", "plain"),
                                    bf_direction = c("10", "01"),
                                    group_rows = FALSE) {
  rlang::check_dots_empty()
  validate_apabayes_tidy(x)
  opts <- table_options(
    x, stats, interval, ci_label, digits, digits_prob, leading_zero, bf,
    bf_direction, group_rows
  )
  parts <- table_parts(x, opts)
  new_apa_table(parts$columns, parts$note, opts$type)
}

#' @rdname apa_table
#' @export
apa_table.default <- function(x, ..., stats = NULL, interval = TRUE,
                              ci_label = "auto", digits = NULL,
                              digits_prob = 3, leading_zero = "auto",
                              bf = c("auto", "sci", "plain"),
                              bf_direction = c("10", "01"),
                              group_rows = FALSE) {
  tidy <- apa_tidy(x, ...)
  apa_table(
    tidy,
    stats = stats, interval = interval, ci_label = ci_label,
    digits = digits, digits_prob = digits_prob, leading_zero = leading_zero,
    bf = bf, bf_direction = bf_direction, group_rows = group_rows
  )
}

#' @rdname apa_table
#' @export
apa_note <- function(x) {
  note <- attr(x, "note", exact = TRUE)
  is_result <- !is.null(attr(x, "table_type", exact = TRUE)) &&
    is.character(note) && length(note) == 1
  if (!is_result) {
    cli::cli_abort(
      c(
        "{.arg x} is not an {.fn apa_table} result.",
        i = "It has no {.field note} attribute to read."
      )
    )
  }
  if (is.na(note)) {
    cli::cli_abort(
      c(
        "This table has nothing to define; remove the {.code apa-note}
         chunk option.",
        i = "apaquarto renders an empty note as {.code Note. {{}}}."
      )
    )
  }
  note
}

# A plain tibble (not `apabayes_tidy`: its cells are text, and the numeric
# table is the tidy one it came from) with the note and the type attached.
new_apa_table <- function(columns, note, type, call = rlang::caller_env()) {
  check_table_headers(names(columns), call)
  n <- length(columns[[1]])
  out <- tibble::new_tibble(columns, nrow = n)
  attr(out, "note") <- note
  attr(out, "table_type") <- type
  out
}

# No header may be a column name apa7 formats itself, nor contain `_`:
# `apa_flextable()` would re-format that column (a `p` header had its
# values rewritten, spec measured 3) or split it into a spanner (measured
# 4). The fixed headers are chosen to pass; a user's `ci_label` standing
# alone as a header is what this catches.
check_table_headers <- function(headers, call = rlang::caller_env()) {
  reserved <- names(apa7::column_formats())
  bad <- headers[headers %in% reserved | grepl("_", headers, fixed = TRUE)]
  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "The header {.val {bad[1]}} would be re-formatted by apa7.",
        i = "{.fn apa7::apa_flextable} formats a column with that name
             itself, or splits a name containing {.code _}; choose another
             {.arg ci_label}."
      ),
      call = call
    )
  }
  invisible(headers)
}

# ---- options -------------------------------------------------------------

# The table kinds built so far (slice 1 of spec-apa_table.md). The others
# abort by name until their slices land, as the inline layer did.
table_types <- function() {
  c("parameters", "diagnostics")
}

# The statistic columns a type can show, in column order.
table_stats_vocabulary <- function(type) {
  switch(type,
    parameters = c("pd", "rope", "bf", "p", "rhat", "ess_bulk", "ess_tail"),
    diagnostics = c("rhat", "ess_bulk", "ess_tail")
  )
}

# What `stats = NULL` may show, before the all-NA columns are dropped:
# the diagnostics are opt-in on a parameters table (decision 5 of the
# spec), and the whole point of a diagnostics table.
table_default_stats <- function(type) {
  switch(type,
    parameters = c("pd", "rope", "bf", "p"),
    diagnostics = c("rhat", "ess_bulk", "ess_tail")
  )
}

# The contract column behind a statistic.
table_stat_column <- function(stat) {
  if (stat == "rope") "rope_pct" else stat
}

# Validate every option once and return them as a list, `stats` resolved
# against the rows of `x`.
table_options <- function(x, stats, interval, ci_label, digits, digits_prob,
                          leading_zero, bf, bf_direction, group_rows,
                          call = rlang::caller_env()) {
  type <- attr(x, "type", exact = TRUE)
  if (!type %in% table_types()) {
    cli::cli_abort(
      "{.fn apa_table} does not yet report a table of type {.val {type}}.",
      call = call
    )
  }
  check_flag(group_rows, call = call)
  if (group_rows) {
    check_table_grouping(x, type, call)
  }
  stats <- check_table_stats(x, stats, type, call)
  check_flag(interval, call = call)
  check_table_ci_label(ci_label, call)
  if (!is.null(digits)) {
    check_digits(digits, call = call)
  }
  check_digits(digits_prob, min = 1, call = call)
  check_leading_zero(leading_zero, call)
  bf <- rlang::arg_match(bf, c("auto", "sci", "plain"), error_call = call)
  bf_direction <- rlang::arg_match(
    bf_direction, c("10", "01"),
    error_call = call
  )
  list(
    type = type, stats = stats, interval = interval, ci_label = ci_label,
    digits = digits %||% 2, digits_prob = digits_prob,
    leading_zero = leading_zero, bf = bf, bf_direction = bf_direction,
    group_rows = group_rows
  )
}

# `NULL` means the type's default, less every column with no value; a
# character vector must lie inside the vocabulary, and a column it names
# must have a value somewhere (an all-empty column would say nothing).
# The return value is the resolved vector, in column order.
check_table_stats <- function(x, stats, type, call = rlang::caller_env()) {
  vocabulary <- table_stats_vocabulary(type)
  filled <- vapply(vocabulary, function(stat) {
    any(!is.na(x[[table_stat_column(stat)]]))
  }, logical(1))
  if (is.null(stats)) {
    default <- table_default_stats(type)
    return(vocabulary[vocabulary %in% default & filled])
  }
  # One check for both ways to get it wrong: a value that is not a name
  # (`stats = 1`) is reported as an unknown name.
  unknown <- setdiff(as.character(stats), vocabulary)
  if (!is.character(stats) || length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg stats} must be NULL or name statistics a {.val {type}} table
         can show.",
        x = "Unknown: {.val {unknown}}.",
        i = "Available: {.val {vocabulary}}."
      ),
      call = call
    )
  }
  empty <- vocabulary[vocabulary %in% stats & !filled]
  if (length(empty) > 0) {
    cli::cli_abort(
      c(
        "{.arg stats} names {.val {empty[1]}}, which has no value in any
         row of this table.",
        i = table_stat_sources()[[empty[1]]]
      ),
      call = call
    )
  }
  vocabulary[vocabulary %in% stats]
}

# Where each statistic column is filled, for the all-NA error: the
# `apa_tidy()` argument when one fills it, the route otherwise.
table_stat_sources <- function() {
  diagnostics <- "It is filled by {.code apa_tidy(x, diagnostics = TRUE)}
                  on a posterior."
  c(
    pd = "It is filled by the posterior routes of {.fn apa_tidy}.",
    rope = "It is filled by {.code apa_tidy(x, rope = )}.",
    bf = "It is filled by {.fn apa_tidy} on a table computed with
          {.code test = \"bf\"}.",
    p = "It is filled by the lavaan route of {.fn apa_tidy}.",
    rhat = diagnostics, ess_bulk = diagnostics, ess_tail = diagnostics
  )
}

# Grouping needs a parameters table with something to group by.
check_table_grouping <- function(x, type, call = rlang::caller_env()) {
  if (type != "parameters") {
    cli::cli_abort(
      "{.arg group_rows} groups a {.val parameters} table, not a
       {.val {type}} table.",
      call = call
    )
  }
  if (all(is.na(x$component)) && all(is.na(x$effects))) {
    cli::cli_abort(
      c(
        "{.arg group_rows} has nothing to group by.",
        i = "{.field component} and {.field effects} are empty in every
             row."
      ),
      call = call
    )
  }
  invisible(x)
}

# Unlike `apa_inline()`, a table needs a label: the interval column's
# header names it.
check_table_ci_label <- function(ci_label, call = rlang::caller_env()) {
  if (!rlang::is_string(ci_label)) {
    cli::cli_abort(
      c(
        '{.arg ci_label} must be "auto" or a single string.',
        i = "An interval column needs a header that names the interval."
      ),
      call = call
    )
  }
  invisible(ci_label)
}
