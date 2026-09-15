# The contrasts route (ARCHITECTURE.md decisions 2, 9, 12, 22): an
# emmeans grid computed from a Bayesian fit. Shapes measured 2026-09-14
# (local/specs/spec-apa_tidy_emmGrid.md, probes `probe_emmgrid*.R`). The
# three that shape the route: `parameters::model_parameters(emmGrid)`
# names its id columns after the grid (`contrast`, or the grid variables
# on a means grid) and carries no `ci_method` attribute, so the
# result-object route cannot read it; its default interval is the
# equal-tailed one, and `ci_method = "hdi"` is bit-identical to the HPD
# interval `summary(emmGrid)` prints; and emmeans's own test for a
# Bayesian grid is whether `post.beta` holds draws.

#' @describeIn apa_tidy An `emmGrid` from [emmeans::emmeans()] or
#'   [emmeans::contrast()] on a Bayesian fit: one row per grid row, the
#'   contrast string or, on a grid of marginal means, the row's label as
#'   emmeans writes it (`cyl_f4`, `cyl_f4 auto`), in the `contrast`
#'   column. The estimate, interval, pd and ROPE share come from
#'   [parameters::model_parameters()]. `ci = "hpd"`, the default, is the
#'   highest-density interval `summary(emmGrid)` prints, and is labelled
#'   as such; `ci = "eti"` gives the equal-tailed interval. A `by`
#'   variable's values go into the `group` column and its name into the
#'   `by` attribute; every grid variable is kept as an extra column. The
#'   grid is reported as it is: subset it with `[` first to report fewer
#'   rows. A grid without posterior draws (from a frequentist fit) is
#'   refused, and so is an `emm_list` (`emmeans(fit, pairwise ~ f)`),
#'   which holds two tables of different kinds — pass one of its parts.
#' @method apa_tidy emmGrid
#' @export
apa_tidy.emmGrid <- function(x,
                             centrality = c("median", "mean"),
                             ci = c("hpd", "eti"),
                             ci_level = 0.95,
                             rope = NULL,
                             rope_ci = 0.95,
                             ...) {
  rlang::check_dots_empty()
  # An emmGrid is an S4 object whose class is defined in emmeans; reading
  # its slots and `model_parameters()` on it both need the package.
  rlang::check_installed("emmeans", reason = "to read emmGrid objects.")
  centrality <- rlang::arg_match(centrality)
  ci <- rlang::arg_match(ci)
  check_ci_level(ci_level, allow_na = FALSE, strict = TRUE)
  rope <- check_rope(rope)
  if (!is.null(rope)) {
    check_rope_ci(rope_ci)
  }
  check_emm_grid(x)

  # The HPD interval is what emmeans computes and prints; easystats
  # calls the same interval "hdi" (measured identical), and the contract
  # keeps emmeans's name for it so the text says what the reader saw.
  mp <- call_model_parameters(
    x, centrality, if (ci == "hpd") "hdi" else ci, ci_level, rope, rope_ci
  )
  grid <- x@grid
  row <- match_emm_rows(grid, mp)
  by <- x@misc$by.vars

  out <- data.frame(
    contrast = emm_row_labels(grid, x@misc$pri.vars),
    group = emm_group(grid, by),
    estimate = mp[[route_estimate_column(centrality)]][row],
    ci_low = mp$CI_low[row],
    ci_high = mp$CI_high[row],
    ci_method = ci,
    ci_level = ci_level,
    pd = optional_numeric(mp, "pd", row),
    rope_pct = optional_numeric(mp, "ROPE_Percentage", row),
    stringsAsFactors = FALSE
  )
  extras <- emm_extra_columns(grid)
  if (ncol(extras) > 0) {
    out <- cbind(out, extras)
  }

  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = as.character(class(x)),
    packages = c("emmeans", "parameters", "bayestestR", "apabayes"),
    rope = rope, rope_ci = rope_ci
  )
  extra$type <- "contrasts"
  if (!is.null(by)) {
    extra$by <- by
  }
  extra$est_type <- x@misc$estType %||% NA_character_
  extra$contrast_method <- x@misc$methDesc %||% NA_character_
  rlang::exec(apabayes_tidy, out, !!!extra)
}

#' @describeIn apa_tidy An `emm_list`, which [emmeans::emmeans()] returns
#'   for a two-sided formula (`pairwise ~ f`), is refused: it holds a
#'   table of means and a table of contrasts, and one tidy table reports
#'   one kind. Pass `x$emmeans` or `x$contrasts`.
#' @method apa_tidy emm_list
#' @export
apa_tidy.emm_list <- function(x, ...) {
  parts <- names(x)
  # The hint is assembled *before* cli sees it: cli interpolates a
  # template once, so markup returned by an expression inside `{}`
  # would print as literal `{.code ...}` (found in review).
  hint <- paste0(
    "Pass one of its parts: ",
    paste0("{.code x$", parts, "}", collapse = " or "), "."
  )
  cli::cli_abort(c(
    "{.arg x} is an {.cls emm_list} holding {.field {parts}}, tables of
     different kinds.",
    i = hint
  ))
}

# ---- the grid's own validity -------------------------------------------

# emmeans's own test for a grid with posterior draws (`summary.emmGrid`,
# `as.mcmc.emmGrid`, measured): a frequentist grid keeps a 1 x 1 NA in
# `post.beta`. Neither `insight::model_info()` (NULL with a warning) nor
# bayestestR's internal test (no S4 dispatch) can decide it.
check_emm_grid <- function(x, call = rlang::caller_env()) {
  if (is.na(x@post.beta[1])) {
    cli::cli_abort(
      c(
        "{.arg x} carries no posterior draws; apabayes reports posterior
         summaries.",
        i = "Compute the grid from a Bayesian fit (brms, rstanarm)."
      ),
      call = call
    )
  }
  if (nrow(x@grid) == 0) {
    cli::cli_abort("{.arg x} has no rows to report.", call = call)
  }
  invisible(x)
}

# ---- rows ---------------------------------------------------------------

# `model_parameters()` returns the grid's id columns (its
# `parameter_names` attribute) and one row per grid row, in grid order
# (measured); the rows are still matched by their id values and never by
# position, so a row the summary dropped (a non-estimable one) is named
# rather than misaligned.
match_emm_rows <- function(grid, mp, call = rlang::caller_env()) {
  ids <- attr(mp, "parameter_names", exact = TRUE)
  ids <- intersect(ids, names(grid))
  if (length(ids) == 0) {
    cli::cli_abort(
      "The summary of {.arg x} names none of its grid columns.",
      call = call
    )
  }
  key <- function(df) {
    do.call(paste, c(lapply(df[ids], as.character), sep = "\r"))
  }
  grid_key <- key(grid)
  row <- match(grid_key, key(mp))
  if (anyNA(row) || anyDuplicated(grid_key) > 0) {
    cli::cli_abort(
      c(
        "The rows of {.arg x} cannot be matched to their summary.",
        i = "Every grid row must be estimable and distinct in
             {.field {ids}}."
      ),
      call = call
    )
  }
  row
}

# The row label emmeans itself would give a grid row: the primary
# variables' values pasted with a space, each variable whose values are
# all numeric-looking prefixed with its name (`contrast.emmGrid`'s
# `enhance.levels`, measured: `cyl_f4`, `wt2.5`, `cyl_f4 auto`). A
# contrast grid already carries the label in its `contrast` column, and
# `contrast(x, "identity")` returns exactly these labels for a means
# grid, which is the tests' oracle. The space is emmeans's default
# `sep` option, taken as given: a session that changes the option gets
# emmeans's own contrast strings with the new separator and these labels
# with a space, which is the one place the two could differ.
emm_row_labels <- function(grid, pri_vars) {
  if ("contrast" %in% names(grid)) {
    return(as.character(grid$contrast))
  }
  parts <- lapply(pri_vars, function(v) {
    values <- as.character(grid[[v]])
    numeric_like <- !anyNA(suppressWarnings(as.numeric(values)))
    if (numeric_like) paste0(v, values) else values
  })
  do.call(paste, c(parts, sep = " "))
}

# The `by` values of each row, pasted with a space when there are
# several; NA when the grid has no `by`.
emm_group <- function(grid, by) {
  if (is.null(by)) {
    return(rep(NA_character_, nrow(grid)))
  }
  do.call(paste, c(lapply(grid[by], as.character), sep = " "))
}

# The grid variables, kept after the contract columns so that a table can
# print them: everything but the contrast label and emmeans's weight and
# offset columns. Factors become character; a numeric variable (a `by`
# or `at` covariate) stays numeric.
emm_extra_columns <- function(grid) {
  keep <- setdiff(names(grid), c("contrast", ".wgt.", ".offset."))
  out <- lapply(grid[keep], function(v) {
    if (is.factor(v)) as.character(v) else v
  })
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
}
