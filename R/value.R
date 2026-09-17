# The query layer (local/specs/spec-apa_value.md): one cell of a tidy
# table, addressed the way `apa_inline()` addresses a row and returned
# unformatted. The addressing is `select_inline_rows()` itself, not a
# copy of it — decision 251 requires the two to share one grammar,
# because a reader will take them for one feature.

#' Read one value out of a tidy table
#'
#' `apa_value()` addresses a row exactly as [apa_inline()] does and
#' returns the value in one of its columns: the number itself, not the
#' string. It is the function for a value that goes into arithmetic, into
#' a comparison, or into a sentence whose author formats it themselves —
#' the cases a hand-rolled lookup helper is otherwise written for, and
#' those helpers index by position and report the wrong row the day the
#' table changes.
#'
#' @section Addressing a row:
#' `term`, `rhs`, `op` and `group` are [apa_inline()]'s, with
#' [apa_inline()]'s meaning and [apa_inline()]'s refusals: a term, a
#' label, a term with the brms class prefix removed, a hypothesis string,
#' a model, the two sides of a structural-equation path, a correlation's
#' pair in either order. No match, or more than one, is an error that
#' lists the candidates. `term = NULL` selects every row and returns the
#' whole column.
#'
#' @section What comes back:
#' The column as it is stored: a double from `estimate`, `ci_low` or
#' `pd`, a character from `term` or `ci_method`, a logical from `std`.
#' Nothing is rounded, formatted or marked up, and the value carries no
#' names. In prose, format it — `apa_num(apa_value(t, "wt"))` — or use
#' [apa_inline()], which writes the whole string. In arithmetic, use it
#' as it is.
#'
#' A column that is `NA` on every addressed row is an error rather than a
#' missing value: a value queried for a manuscript is one the table is
#' expected to hold, and `NA` reaching the prose is the defect this
#' refusal exists to stop. A column missing on some rows and present on
#' others comes back as it is.
#'
#' @inheritParams apa_inline
#' @param column The column to read, as a single string; any column of
#'   the table, contract or not. `NULL` reads the column the table's type
#'   is about — the value its contract requires beside the columns that
#'   name a row: `estimate` on a parameters, hypotheses, contrasts or
#'   correlations table, `elpd_diff` on a `loo` table, `bf` on a
#'   `bf_models` or `bf_inclusion` one. A `sem_fit` table carries several
#'   indices and a `diagnostics` table three statistics, so neither has a
#'   default and `column` must name one.
#' @param ... Tidy method: must be empty. Default method: passed to
#'   [apa_tidy()].
#'
#' @return The selected values of `column`, unnamed, in the table's own
#'   row order.
#' @seealso [apa_inline()] for the formatted string, [apa_tidy()] for the
#'   tables.
#' @examples
#' t <- apabayes_tidy(
#'   data.frame(
#'     term = c("b_Intercept", "b_wt"), label = c("(Intercept)", "wt"),
#'     estimate = c(37.3, -5.34), ci_low = c(31.2, -6.85),
#'     ci_high = c(43.1, -3.82), pd = c(1, 0.9995),
#'     component = "conditional"
#'   ),
#'   type = "parameters", centrality = "median", ci_method = "eti",
#'   ci_level = 0.95
#' )
#' apa_value(t, "wt")
#' apa_value(t, "wt", column = "ci_low")
#' apa_value(t, column = "term")
#' # the number behind the string
#' apa_num(apa_value(t, "wt") * 2)
#' @examplesIf rlang::is_installed("lavaan")
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
#' apa_value(fit, "visual", "x2", op = "=~", standardize = TRUE)
#' @export
apa_value <- function(x, ...) {
  UseMethod("apa_value")
}

#' @rdname apa_value
#' @export
apa_value.apabayes_tidy <- function(x, term = NULL, rhs = NULL, op = NULL,
                                    group = NULL, ..., column = NULL) {
  rlang::check_dots_empty()
  validate_apabayes_tidy(x)
  column <- resolve_value_column(column, x)
  rows <- select_inline_rows(x, term, rhs, op, group)
  out <- unname(x[[column]][rows])
  check_value_present(out, column)
  out
}

#' @rdname apa_value
#' @export
apa_value.default <- function(x, term = NULL, rhs = NULL, op = NULL,
                              group = NULL, ..., column = NULL) {
  tidy <- apa_tidy(x, ...)
  apa_value(
    tidy,
    term = term, rhs = rhs, op = op, group = group, column = column
  )
}

# The column a type is about: the value its contract requires beside the
# columns that name a row (`tidy_contracts()$required`). `sem_fit` and
# `diagnostics` require none — a fit table carries several indices, a
# diagnostics table three statistics — so those two are the types that
# have to be asked a question.
value_default_column <- function(type) {
  switch(type,
    parameters = "estimate",
    hypotheses = "estimate",
    contrasts = "estimate",
    correlations = "estimate",
    loo = "elpd_diff",
    bf_models = "bf",
    bf_inclusion = "bf",
    NULL
  )
}

resolve_value_column <- function(column, x, call = rlang::caller_env()) {
  # nolint next: object_usage_linter. Used in the cli strings below.
  type <- attr(x, "type", exact = TRUE)
  if (is.null(column)) {
    column <- value_default_column(type)
    if (is.null(column)) {
      cli::cli_abort(
        c(
          "A {.val {type}} table has no default column; name one with
           {.arg column}.",
          i = "Columns: {.field {names(x)}}."
        ),
        call = call
      )
    }
    return(column)
  }
  check_value_column(column, x, call)
  column
}

check_value_column <- function(column, x, call = rlang::caller_env()) {
  if (!rlang::is_string(column)) {
    cli::cli_abort(
      "{.arg column} must be a single column name, not
       {.obj_type_friendly {column}}.",
      call = call
    )
  }
  if (!column %in% names(x)) {
    cli::cli_abort(
      c(
        "A {.val {attr(x, 'type', exact = TRUE)}} table has no column
         {.val {column}}.",
        i = "Columns: {.field {names(x)}}."
      ),
      call = call
    )
  }
  invisible(column)
}

# A value queried for a manuscript is one the table is expected to hold.
# Finding 7 of the acceptance test is what this refusal is for: a
# mislabelled table reported `NA` inline and the `NA` reached the prose.
check_value_present <- function(out, column, call = rlang::caller_env()) {
  if (length(out) == 0 || !all(is.na(out))) {
    return(invisible(out))
  }
  cli::cli_abort(
    c(
      "Column {.field {column}} is {.code NA} on every row
       {.fn apa_value} selected.",
      i = "The table records no value there; nothing in a manuscript
           should print {.code NA}."
    ),
    call = call
  )
}
