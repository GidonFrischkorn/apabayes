# Row addressing for the inline layer (ARCHITECTURE.md decision 5): a
# row is found by name, never by position, and anything but exactly one
# hit is an error that lists the candidates. The rules, in order:
# `group` filters first; no `term` means every row; a `rhs` or `op`
# reads the term as a structural-equation path (`~~` is symmetric, the
# other operators are not); a bare `term` matches the term, then the
# label, then the term without the brms class prefix. A correlation is a
# pair of variables: `term` + `rhs` matches it in either order, and a bare
# `term` that is no `var1~~var2` string matches a variable on either side.

select_inline_rows <- function(x, term, rhs, op, group,
                               call = rlang::caller_env()) {
  check_inline_string(term, "term", call)
  check_inline_string(rhs, "rhs", call)
  check_inline_string(op, "op", call)
  check_inline_string(group, "group", call)
  type <- attr(x, "type", exact = TRUE)
  rows <- seq_len(nrow(x))
  if (!is.null(group)) {
    rows <- filter_inline_group(x, group, rows, type, call)
  }
  if (is.null(term) && is.null(rhs) && is.null(op)) {
    return(rows)
  }
  if (is.null(term)) {
    cli::cli_abort(
      "{.arg term} is needed when {.arg rhs} or {.arg op} is given.",
      call = call
    )
  }
  hits <- if (is.null(rhs) && is.null(op)) {
    match_inline_id(x, rows, term, type)
  } else {
    match_inline_path(x, rows, term, rhs, op, type, call)
  }
  if (length(hits) == 0) {
    abort_inline_no_match(x, rows, term, rhs, op, type, call)
  }
  if (length(hits) > 1) {
    abort_inline_many(x, hits, type, call)
  }
  hits
}

check_inline_string <- function(value, arg, call = rlang::caller_env()) {
  if (!(is.null(value) || rlang::is_string(value))) {
    cli::cli_abort(
      "{.arg {arg}} must be a single string or NULL, not
       {.cls {class(value)}} of length {length(value)}.",
      call = call
    )
  }
  invisible(value)
}

# The column(s) a bare `term` is matched against, per table kind. A
# Bayes-factor model is found by bayestestR's `Model`, then by the name
# it was passed as; a contrast by its string, which is emmeans's label
# of the row on a means grid.
inline_id_columns <- function(type) {
  switch(type,
    parameters = c("term", "label"),
    hypotheses = "hypothesis",
    sem_fit = "model",
    loo = "model",
    bf_models = c("model", "name"),
    contrasts = "contrast",
    correlations = "term",
    "term"
  )
}

filter_inline_group <- function(x, group, rows, type,
                                call = rlang::caller_env()) {
  if (!"group" %in% names(x)) {
    cli::cli_abort(
      "A {.val {type}} table has no {.field group} column to match
       {.arg group} against.",
      call = call
    )
  }
  hit <- rows[x$group[rows] %in% group]
  if (length(hit) == 0) {
    present <- unique(x$group[rows][!is.na(x$group[rows])])
    cli::cli_abort(c(
      "No row has group {.val {group}}.",
      "i" = if (length(present) > 0) {
        "Groups present: {.val {present}}."
      } else {
        "The table has no group values."
      }
    ), call = call)
  }
  hit
}

# A bare `term`: exact id column(s) first, then the term without a `b_`
# prefix (the brms class prefix the draws route keeps in the label).
match_inline_id <- function(x, rows, term, type) {
  for (column in inline_id_columns(type)) {
    hit <- rows[x[[column]][rows] %in% term]
    if (length(hit) > 0) {
      return(hit)
    }
  }
  if (type == "correlations") {
    return(rows[x$var1[rows] %in% term | x$var2[rows] %in% term])
  }
  if ("term" %in% names(x)) {
    return(rows[x$term[rows] %in% paste0("b_", term)])
  }
  integer()
}

# A structural-equation path. `~~` is the one symmetric operator, as in
# the seed manuscripts' lookup helpers; a loading or a regression has a
# direction and its reverse is not a row.
match_inline_path <- function(x, rows, term, rhs, op, type,
                              call = rlang::caller_env()) {
  if (type == "correlations") {
    return(match_inline_pair(x, rows, term, rhs, op))
  }
  if (type != "parameters") {
    cli::cli_abort(
      "{.arg rhs} and {.arg op} address a structural-equation path, and
       a {.val {type}} table has none.",
      call = call
    )
  }
  parts <- sem_term_parts(x$term[rows])
  has_op <- !is.na(parts$op)
  op_ok <- has_op & (is.null(op) | parts$op %in% op)
  if (is.null(rhs)) {
    return(rows[op_ok & parts$lhs %in% term])
  }
  direct <- parts$lhs %in% term & parts$rhs %in% rhs
  reverse <- parts$op %in% "~~" & parts$lhs %in% rhs & parts$rhs %in% term
  rows[op_ok & (direct | reverse)]
}

# A correlation, read from its `var1` and `var2` columns rather than split
# out of the term, so that a variable name is never parsed. A correlation
# is symmetric, and its only operator is `~~`.
match_inline_pair <- function(x, rows, term, rhs, op) {
  if (!is.null(op) && op != "~~") {
    return(integer())
  }
  v1 <- x$var1[rows]
  v2 <- x$var2[rows]
  if (is.null(rhs)) {
    return(rows[v1 %in% term | v2 %in% term])
  }
  rows[(v1 %in% term & v2 %in% rhs) | (v1 %in% rhs & v2 %in% term)]
}

# How a row is named in an error message: `term (label)` where the label
# adds something, else the id alone.
inline_row_names <- function(x, rows, type) {
  id <- inline_id_columns(type)[1]
  out <- x[[id]][rows]
  if (type == "parameters") {
    label <- x$label[rows]
    add <- !is.na(label) & label != out
    out[add] <- sprintf("%s (%s)", out[add], label[add])
  }
  out
}

abort_inline_no_match <- function(x, rows, term, rhs, op, type,
                                  call = rlang::caller_env()) {
  asked <- if (is.null(rhs) && is.null(op)) {
    "{.val {term}}"
  } else {
    "the path {.val {term}} {.val {op %||% '?'}} {.val {rhs %||% ''}}"
  }
  available <- inline_row_names(x, rows, type)
  shown <- utils::head(available, 20)
  # nolint next: object_usage_linter. Used in the cli string below.
  more <- length(available) - length(shown)
  cli::cli_abort(c(
    paste0("No row matches ", asked, "."),
    "i" = "Available: {.val {shown}}{if (more > 0) paste0(' and ', more, ' more') else ''}." # nolint: line_length_linter. One cli string.
  ), call = call)
}

abort_inline_many <- function(x, hits, type, call = rlang::caller_env()) {
  # nolint next: object_usage_linter. Used in the cli string below.
  found <- inline_row_names(x, hits, type)
  hints <- character()
  if ("group" %in% names(x) && length(unique(x$group[hits])) > 1) {
    hints <- c(hints, "i" = "They differ by group; pass {.arg group}.")
  }
  if (type == "parameters") {
    ops <- unique(sem_term_parts(x$term[hits])$op)
    if (length(ops) > 1) {
      hints <- c(hints, "i" = "They differ by operator; pass {.arg op}.")
    }
  }
  cli::cli_abort(c(
    "{length(hits)} rows match: {.val {found}}.",
    hints
  ), call = call)
}
