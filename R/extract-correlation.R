# The correlation route (ARCHITECTURE.md decision 23): a table of Bayesian
# correlations from `correlation::correlation()` or `cor_test()`, as a
# `correlations` table. Shapes measured 2026-09-15
# (local/specs/spec-apa_tidy_correlation.md, probes `probe_correlation*.R`).
# As on the result-object route nothing is computed; every number is the
# table's. What shapes the route is what the table does *not* record: it
# computes its numbers with `parameters::model_parameters()` on a
# BayesFactor fit, then renames the one centrality column `rho` and keeps
# neither the interval method nor a call, so both are named by the caller;
# and it writes the requested `ci` into its `CI` column without ever
# passing it on, so every interval is 95 % whatever the column says
# (correlation 0.8.8 and its main branch), and any other level is refused.

#' @describeIn apa_tidy A table of Bayesian correlations from
#'   [correlation::correlation()] or [correlation::cor_test()] with
#'   `bayesian = TRUE`, as a `correlations` table: one row per pair,
#'   named `var1~~var2`, with the posterior estimate of the correlation,
#'   its interval, pd, the ROPE share, the Bayes factor and the pairwise
#'   n, all read from the table. The table does not record which interval
#'   or which centrality it holds, so `ci` and `centrality` must name what
#'   it was computed with; they default to correlation's own defaults
#'   (`bayesian_ci_method = "hdi"`, the median) and apabayes cannot check
#'   them. A table computed with `centrality = "all"` has no `rho` column
#'   and is read from the column `centrality` names. correlation writes
#'   its `ci` argument into the table without applying it (the bounds are
#'   95 % whatever `ci` was), so a table labelled with any other level is
#'   refused. The ROPE share is reported without bounds, which the table
#'   does not carry. Frequentist tables and tables with diagonal rows
#'   (`redundant = TRUE`) are refused.
#' @method apa_tidy easycorrelation
#' @export
apa_tidy.easycorrelation <- function(x, centrality = c("median", "mean"),
                                     ci = c("hdi", "eti"), ...) {
  rlang::check_dots_empty()
  centrality <- rlang::arg_match(centrality)
  ci <- rlang::arg_match(ci)
  mp <- check_correlation_table(x)
  ci_level <- correlation_ci_level(x, mp)
  estimate <- correlation_estimate(mp, centrality)
  row <- seq_len(nrow(mp))

  out <- data.frame(
    term = paste0(mp$Parameter1, "~~", mp$Parameter2),
    var1 = as.character(mp$Parameter1),
    var2 = as.character(mp$Parameter2),
    group = optional_column(mp, "Group", row),
    estimate = estimate,
    ci_low = mp$CI_low,
    ci_high = mp$CI_high,
    ci_method = ci,
    ci_level = ci_level,
    pd = optional_numeric(mp, "pd", row),
    rope_pct = optional_numeric(mp, "ROPE_Percentage", row),
    bf = optional_numeric(mp, "BF", row),
    n = optional_numeric(mp, "n_Obs", row),
    stringsAsFactors = FALSE
  )
  extras <- c(
    method = "Method", prior_distribution = "Prior_Distribution",
    prior_location = "Prior_Location", prior_scale = "Prior_Scale"
  )
  for (name in names(extras)) {
    if (extras[[name]] %in% names(mp)) {
      out[[name]] <- mp[[extras[[name]]]]
    }
  }

  # The table is a data frame and loads without correlation or
  # BayesFactor; their versions are recorded when they are installed.
  packages <- c(
    installed_packages_of(c("correlation", "BayesFactor")),
    "parameters", "bayestestR", "apabayes"
  )
  rlang::exec(
    apabayes_tidy, out,
    type = "correlations",
    centrality = centrality,
    ci_method = ci,
    ci_level = ci_level,
    source_class = class(x),
    package_versions = package_versions_of(packages),
    prior = attr(x, "bayesian_prior", exact = TRUE),
    method = attr(x, "method", exact = TRUE)
  )
}

# The packages of `pkgs` that are installed, in order.
installed_packages_of <- function(pkgs) {
  pkgs[vapply(pkgs, rlang::is_installed, logical(1))]
}

# ---- the table's own validity --------------------------------------------

# What must hold before any number is read; returns the table as a plain
# data frame, attributes kept, so no correlation method is dispatched.
check_correlation_table <- function(x, call = rlang::caller_env()) {
  if (!correlation_is_bayesian(x)) {
    cli::cli_abort(
      c(
        "{.arg x} is a frequentist correlation table, not Bayesian;
         apabayes reports posterior summaries.",
        i = "Compute it with {.code bayesian = TRUE}."
      ),
      call = call
    )
  }
  if (nrow(x) == 0) {
    cli::cli_abort("{.arg x} has no rows to report.", call = call)
  }
  needed <- c("Parameter1", "Parameter2", "CI_low", "CI_high")
  missing <- setdiff(needed, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg x} has no {.field {missing}} column{?s}.",
      call = call
    )
  }
  diagonal <- as.character(x$Parameter1) == as.character(x$Parameter2)
  if (any(diagonal)) {
    # nolint next: object_usage_linter. Used in the cli string below.
    vars <- unique(as.character(x$Parameter1[diagonal]))
    cli::cli_abort(
      c(
        "{.arg x} correlates {.field {vars}} with {?itself/themselves}:
         correlation fills those rows with fixed values, not estimates.",
        i = "Compute the table with {.code redundant = FALSE}."
      ),
      call = call
    )
  }
  class(x) <- "data.frame"
  x
}

# A `correlation()` table says so in its `bayesian` attribute; a
# `cor_test()` result has no such attribute, and says so only in `Method`
# ("Bayesian Pearson"). The `rho` column is no marker: a frequentist
# Spearman table has one too (measured).
correlation_is_bayesian <- function(x) {
  flag <- attr(x, "bayesian", exact = TRUE)
  if (!is.null(flag)) {
    return(isTRUE(flag))
  }
  if (!"Method" %in% names(x)) {
    return(FALSE)
  }
  # `isTRUE()`: a missing `Method` value makes `startsWith()` NA, which is
  # no marker either.
  isTRUE(all(startsWith(as.character(x$Method), "Bayesian")))
}

# ---- settings -------------------------------------------------------------

# `rho` holds whichever single centrality the table was computed with; a
# table computed with several has none and keeps `Median` and `Mean`
# (measured), and one computed with the MAP has no estimate column at all,
# because parameters returns no MAP for a BayesFactor correlation.
correlation_estimate <- function(mp, centrality, call = rlang::caller_env()) {
  if ("rho" %in% names(mp)) {
    return(mp$rho)
  }
  column <- route_estimate_column(centrality)
  if (column %in% names(mp)) {
    return(mp[[column]])
  }
  if (!any(c("Median", "Mean") %in% names(mp))) {
    cli::cli_abort(
      c(
        "{.arg x} has no estimate column: neither {.field rho} nor
         {.field Median} or {.field Mean}.",
        i = "A table of MAP estimates ({.code centrality = \"map\"}) has
             none; compute it with the median or the mean."
      ),
      call = call
    )
  }
  cli::cli_abort(
    "{.arg x} has no {.field rho} column and no {.field {column}} column
     for {.code centrality = \"{centrality}\"}.",
    call = call
  )
}

# The interval level, from the `ci` attribute and else the `CI` column.
# correlation 0.8.8 records the level it was asked for but never passes it
# to the interval (measured: `ci = 0.9` and `ci = 0.5` give the 95 %
# bounds), so a level other than .95 labels bounds that are not at that
# level, and is refused rather than reported.
correlation_ci_level <- function(x, mp, call = rlang::caller_env()) {
  level <- attr(x, "ci", exact = TRUE)
  if (is.null(level) && "CI" %in% names(mp)) {
    level <- mp$CI
  }
  level <- unique(level[!is.na(level)])
  if (!is.numeric(level) || length(level) == 0) {
    cli::cli_abort("{.arg x} carries no interval level.", call = call)
  }
  if (length(level) > 1) {
    abort_ci_levels(level, call)
  }
  if (level != 0.95) {
    cli::cli_abort(
      c(
        "{.arg x} is labelled as a {.val {level}} interval, but its bounds
         are 95% intervals.",
        i = "correlation records {.arg ci} without applying it to Bayesian
             correlations, so only a table computed with
             {.code ci = 0.95} can be reported."
      ),
      call = call
    )
  }
  as.double(level)
}
