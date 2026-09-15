# The modelbased route (ARCHITECTURE.md decision 22, second backend of the
# contrasts contract): a table from `modelbased::estimate_contrasts()` or
# `modelbased::estimate_means()` on a Bayesian fit. Shapes measured
# 2026-09-15 (local/specs/spec-apa_tidy_modelbased.md, probes
# `probe_modelbased*.R`). As on the result-object route nothing is
# computed: the numbers are the table's, and the settings are read off
# the object. The three facts that shape the route: the table records its
# centrality only as a column name and its interval nowhere but in the
# call (a default ETI, or whatever `ci_method` the call passed on to
# bayestestR); modelbased's Bayesian marker is `model_info$is_bayesian`;
# and on `backend = "emmeans"` the estimate column is named `Mean` or
# `Difference` whichever centrality it holds, so that backend is refused.

#' @describeIn apa_tidy A table of contrasts from
#'   [modelbased::estimate_contrasts()] on a Bayesian fit, as a `contrasts`
#'   table. A row is named as modelbased names it, `Level1 - Level2`
#'   (`"6 - 4"`, or `"6, auto - 4, auto"` for two variables), or by its
#'   `Parameter` for a custom comparison; a `by` variable's values go into
#'   the `group` column. The numbers are read from the table, not
#'   recomputed, and so is the ROPE share with its bounds when the table
#'   has one. The table records its interval only in its call: with no
#'   `ci_method` there it is modelbased's default equal-tailed interval,
#'   otherwise the one named. When the call gives `ci_method` as a
#'   variable, or the table has lost its call, name the interval with
#'   `ci = "eti"` or `ci = "hdi"`; a `ci` that contradicts the call is
#'   refused. Tables computed with `backend = "emmeans"` are refused, since
#'   their estimate column does not say which centrality it holds: pass
#'   the emmeans grid to `apa_tidy()` instead.
#' @method apa_tidy estimate_contrasts
#' @export
apa_tidy.estimate_contrasts <- function(x, centrality = NULL, ci = NULL,
                                        ...) {
  rlang::check_dots_empty()
  mp <- check_modelbased_table(x)
  contrast <- modelbased_contrast_labels(mp)
  by <- modelbased_by(x, mp, required = FALSE)
  keep <- c(intersect(c("Level1", "Level2"), names(mp)), by)
  modelbased_tidy(
    x, mp,
    contrast = contrast,
    group = if (is.null(by)) NA_character_ else modelbased_join(mp, by),
    extras = mp[keep], by = by, centrality = centrality, ci = ci
  )
}

#' @describeIn apa_tidy A table of marginal means from
#'   [modelbased::estimate_means()] on a Bayesian fit, as a `contrasts`
#'   table whose rows are named by the values of their `by` variables
#'   (`"4"`, `"4, auto"`). Everything else is read as for
#'   `estimate_contrasts` tables.
#' @method apa_tidy estimate_means
#' @export
apa_tidy.estimate_means <- function(x, centrality = NULL, ci = NULL, ...) {
  rlang::check_dots_empty()
  mp <- check_modelbased_table(x)
  by <- modelbased_by(x, mp, required = TRUE)
  modelbased_tidy(
    x, mp,
    contrast = modelbased_join(mp, by), group = NA_character_,
    extras = mp[by], by = by, centrality = centrality, ci = ci
  )
}

# The contract table both methods return. The ROPE bounds and the ROPE's
# own level are read as on the result-object route; `ROPE_CI` is not the
# interval level (measured: it stays .95 under `ci = 0.9`).
modelbased_tidy <- function(x, mp, contrast, group, extras, by, centrality,
                            ci, call = rlang::caller_env()) {
  centrality <- result_centrality(mp, centrality, call = call)
  ci <- modelbased_ci_method(x, ci, call = call)
  ci_level <- modelbased_ci_level(x, call = call)
  row <- seq_len(nrow(mp))

  out <- data.frame(
    contrast = contrast,
    group = group,
    estimate = mp[[route_estimate_column(centrality)]],
    ci_low = mp$CI_low,
    ci_high = mp$CI_high,
    ci_method = ci,
    ci_level = ci_level,
    pd = optional_numeric(mp, "pd", row),
    rope_pct = optional_numeric(mp, "ROPE_Percentage", row),
    stringsAsFactors = FALSE
  )
  if (ncol(extras) > 0) {
    out <- cbind(out, emm_extra_columns(extras))
  }

  rope <- result_rope_range(mp)
  extra <- parameters_attributes(
    centrality, ci, ci_level,
    source_class = class(x),
    # The table is a data frame and loads without modelbased; its version
    # is recorded when it is installed.
    packages = c(
      if (rlang::is_installed("modelbased")) "modelbased",
      "bayestestR", "apabayes"
    ),
    rope = rope, rope_ci = if (is.null(rope)) NULL else result_rope_ci(mp)
  )
  extra$type <- "contrasts"
  extra$by <- by
  extra$predict <- attr(x, "predict", exact = TRUE)
  extra$marginalization <- attr(x, "estimate", exact = TRUE)
  rlang::exec(apabayes_tidy, out, !!!extra)
}

# ---- the table's own validity --------------------------------------------

# What must hold before any column is read, in the order a reader would
# want to hear it; returns the table as a plain data frame, attributes
# kept, so no modelbased method is dispatched on it.
check_modelbased_table <- function(x, call = rlang::caller_env()) {
  if (identical(attr(x, "backend", exact = TRUE), "emmeans")) {
    cli::cli_abort(
      c(
        "{.arg x} was computed with {.code backend = \"emmeans\"}, whose
         estimate column is named {.field Mean} or {.field Difference}
         whichever centrality it holds.",
        i = "Compute it with modelbased's default backend, or pass the
             emmeans grid to {.fn apa_tidy} directly."
      ),
      call = call
    )
  }
  info <- attr(x, "model_info", exact = TRUE)
  if (is.null(info)) {
    cli::cli_abort(
      "{.arg x} records no model information, so apabayes cannot tell
       whether it summarises a Bayesian model.",
      call = call
    )
  }
  if (!isTRUE(info$is_bayesian)) {
    cli::cli_abort(
      "{.arg x} summarises a model that is not Bayesian; apabayes reports
       posterior summaries.",
      call = call
    )
  }
  if (nrow(x) == 0) {
    cli::cli_abort("{.arg x} has no rows to report.", call = call)
  }
  class(x) <- "data.frame"
  x
}

# ---- rows -----------------------------------------------------------------

# modelbased's own row names: `Level1 - Level2` (the difference is
# Level1 minus Level2, measured), or the custom comparison's `Parameter`.
modelbased_contrast_labels <- function(mp, call = rlang::caller_env()) {
  if (all(c("Level1", "Level2") %in% names(mp))) {
    return(paste(as.character(mp$Level1), "-", as.character(mp$Level2)))
  }
  if ("Parameter" %in% names(mp)) {
    return(as.character(mp$Parameter))
  }
  cli::cli_abort(
    "{.arg x} has neither {.field Level1} and {.field Level2} nor a
     {.field Parameter} column to name its contrasts.",
    call = call
  )
}

# The `by` variables and their columns. A means table always names them
# (they identify its rows); a contrasts table only when it has groups.
modelbased_by <- function(x, mp, required, call = rlang::caller_env()) {
  by <- attr(x, "by", exact = TRUE)
  if (is.null(by)) {
    if (required) {
      cli::cli_abort(
        "{.arg x} names no {.field by} variables to label its rows.",
        call = call
      )
    }
    return(NULL)
  }
  missing <- setdiff(by, names(mp))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg x} has no column for its {.field by} variable{?s}
       {.field {missing}}.",
      call = call
    )
  }
  by
}

# Values of several variables joined as modelbased joins them in
# `Level1` (`"6, auto"`).
modelbased_join <- function(mp, vars) {
  do.call(paste, c(lapply(mp[vars], as.character), sep = ", "))
}

# ---- settings -------------------------------------------------------------

# The interval, which the table records only in its call (measured). The
# argument that set it is any name a prefix of `ci_method` from `ci_` on,
# because R's partial matching passes `ci_m = "hdi"` on to bayestestR
# (measured); without one, the interval is bayestestR's default ETI. A
# value that is not a string would have to be evaluated in an environment
# the table does not carry, so it is not read: `ci` names the interval
# instead, and is checked against the call wherever the call can say.
modelbased_ci_method <- function(x, ci, call = rlang::caller_env()) {
  if (!is.null(ci)) {
    ci <- rlang::arg_match(ci, c("eti", "hdi"), error_call = call)
  }
  ask <- "Name the interval the table was computed with:
          {.code ci = \"eti\"} or {.code ci = \"hdi\"}."
  table_call <- attr(x, "call", exact = TRUE)
  if (!is.call(table_call)) {
    if (is.null(ci)) {
      cli::cli_abort(
        c("{.arg x} carries no call to read its interval from.", i = ask),
        call = call
      )
    }
    return(ci)
  }
  args <- as.list(table_call)[-1]
  arg_names <- names(args) %||% rep("", length(args))
  hit <- nchar(arg_names) >= 3 & startsWith("ci_method", arg_names)
  if (sum(hit) > 1) {
    cli::cli_abort(
      "The call of {.arg x} names the interval more than once:
       {.code {arg_names[hit]}}.",
      call = call
    )
  }
  if (!any(hit)) {
    recorded <- "eti"
  } else {
    value <- args[[which(hit)]]
    if (!rlang::is_string(value)) {
      if (is.null(ci)) {
        # nolint next: object_usage_linter. Used in the cli string below.
        given <- paste(
          arg_names[hit], "=",
          paste(rlang::expr_deparse(value), collapse = " ")
        )
        cli::cli_abort(
          c(
            "The call of {.arg x} gives its interval as {.code {given}},
             which apabayes cannot read without evaluating it.",
            i = ask
          ),
          call = call
        )
      }
      return(ci)
    }
    recorded <- tolower(value)
  }
  if (!recorded %in% c("eti", "hdi")) {
    cli::cli_abort(
      "{.arg x} reports a {.val {recorded}} interval; this route reports
       {.val eti} and {.val hdi}.",
      call = call
    )
  }
  if (!is.null(ci) && ci != recorded) {
    cli::cli_abort(
      c(
        "{.arg ci} is {.val {ci}}, but {.arg x} was computed with the
         {.val {recorded}} interval.",
        i = if (!any(hit)) {
          "Its call passes no {.arg ci_method}, and modelbased's default
           is the equal-tailed interval."
        }
      ),
      call = call
    )
  }
  recorded
}

# The interval level is the `ci` attribute (measured; `ROPE_CI` is the
# ROPE's). modelbased itself refuses several levels, so more than one
# means an edited table.
modelbased_ci_level <- function(x, call = rlang::caller_env()) {
  level <- attr(x, "ci", exact = TRUE)
  if (!is.numeric(level) || length(level) == 0 || anyNA(level)) {
    cli::cli_abort("{.arg x} carries no interval level.", call = call)
  }
  if (length(level) > 1) {
    abort_ci_levels(level, call)
  }
  as.double(level)
}
