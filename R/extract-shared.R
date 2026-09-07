# Shape shared by every route of the extract layer.
#
# A route is always the same four steps: validate the reporting arguments,
# call easystats once, match the returned rows to the requested terms *by
# name*, and hand the assembled contract columns to `apabayes_tidy()`.
# What differs between routes is only which easystats function is called
# and which columns come back; the validation and the attribute assembly
# are identical, so they live here rather than being repeated per route.
#
# These helpers were extracted once the second route (brmsfit) landed and
# the shared shape was visible, not before (ARCHITECTURE.md open items).

# The reporting arguments every parameters route takes. Called after the
# route's own `arg_match()` calls, so that an invalid `centrality` is
# still reported before an invalid `ci_level`: the order of these checks
# is part of the routes' error behaviour and is preserved here.
#
# Returns the checked `rope`, which `check_rope()` normalises; `rope_ci`
# is checked only when a ROPE was asked for, so the default call carries
# exactly the easystats defaults.
check_route_args <- function(ci_level, diagnostics, rope, rope_ci) {
  check_ci_level(ci_level, allow_na = FALSE, strict = TRUE)
  check_flag(diagnostics)
  rope <- check_rope(rope)
  if (!is.null(rope)) {
    check_rope_ci(rope_ci)
  }
  rope
}

# `effects = "random"` asked of a model that has no random effects. Left
# to easystats this is not one behaviour but two, both measured
# 2026-09-07: `model_parameters(brmsfit)` aborts inside a `merge()` with
# `'by' must specify a uniquely valid column`, and
# `model_parameters(stanreg)` silently returns the fixed rows instead.
# The check is `insight::is_mixed_model()`, which reads the model's own
# formula, so the message never depends on the wording of an upstream
# error. `insight` moved from Suggests to Imports for this (decision 13).
check_random_effects <- function(x, effects, call = rlang::caller_env()) {
  if (identical(effects, "random") && !insight::is_mixed_model(x)) {
    cli::cli_abort(
      c(
        "{.arg effects} is {.val random}, but {.arg x} has no random
         effects.",
        i = "Report a model with a grouping term, or use
             {.code effects = \"fixed\"} or {.code effects = \"all\"}."
      ),
      call = call
    )
  }
  invisible(effects)
}

# The attributes a "parameters" table carries. `rope_range` and `rope_ci`
# appear only when a ROPE was computed, so a table note cannot state
# bounds that were never applied.
parameters_attributes <- function(centrality, ci, ci_level, source_class,
                                  packages, rope, rope_ci) {
  out <- list(
    type = "parameters",
    centrality = centrality,
    ci_method = ci,
    ci_level = ci_level,
    source_class = source_class,
    package_versions = package_versions_of(packages)
  )
  if (!is.null(rope)) {
    out$rope_range <- rope
    out$rope_ci <- rope_ci
  }
  out
}

# The versions a table note reports. Named so that the note can say which
# package produced which number.
package_versions_of <- function(packages) {
  vapply(
    packages,
    function(pkg) as.character(utils::packageVersion(pkg)),
    character(1)
  )
}

# A column of a `model_parameters()` table that may or may not exist:
# `Effects` and `Group` appear only when the model has random effects and
# `effects = "all"`, and `Component` only when a component was asked for
# (measured on brmsfit and stanreg). An absent column is the typed NA,
# not an error.
optional_column <- function(mp, name, row) {
  if (!name %in% names(mp)) {
    return(rep(NA_character_, length(row)))
  }
  as.character(mp[[name]])[row]
}

# The display label of a `model_parameters()` row. `pretty_names` is a
# character vector named by `Parameter`, so it is looked up by name; its
# values are not unique (`b_Intercept` and `sd_cyl_f__Intercept` are both
# "(Intercept)" on brmsfit, `(Intercept)` and `Sigma[...]` on stanreg),
# and two rows with the same label would break the term-or-label
# addressing of the inline layer. A duplicated label is therefore
# qualified with its group, and falls back to the term if that still does
# not separate it.
parameters_labels <- function(mp, terms, group, labels) {
  pretty <- attr(mp, "pretty_names")
  out <- if (is.character(pretty) && !is.null(names(pretty))) {
    unname(pretty[terms])
  } else {
    rep(NA_character_, length(terms))
  }
  out[is.na(out)] <- terms[is.na(out)]

  repeated <- out %in% out[duplicated(out)]
  qualify <- repeated & !is.na(group)
  out[qualify] <- sprintf("%s (%s)", out[qualify], group[qualify])
  repeated <- out %in% out[duplicated(out)]
  out[repeated] <- terms[repeated]

  if (is.null(labels)) {
    return(out)
  }
  named <- resolve_draws_labels(labels, terms)
  out[named != terms] <- named[named != terms]
  out
}

# The `variables =` selection of a parameters route: `NULL` keeps every
# row easystats returned, in its order; names are matched verbatim
# against `Parameter`.
resolve_parameters_variables <- function(variables, available,
                                         call = rlang::caller_env()) {
  if (length(available) == 0) {
    cli::cli_abort("{.arg x} has no parameters to report.", call = call)
  }
  if (is.null(variables)) {
    return(available)
  }
  if (!is.character(variables)) {
    cli::cli_abort(
      "{.arg variables} must be a character vector or NULL, not
       {.cls {class(variables)}}.",
      call = call
    )
  }
  # Checked before `unknown`: an empty selection has no offending term to
  # name, and folding it into the branch below reported `NA` as the term
  # that was not found.
  if (length(variables) == 0) {
    cli::cli_abort(
      "{.arg variables} selects no parameter to report.",
      call = call
    )
  }
  unknown <- setdiff(variables, available)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg variables} must name reported parameters;
         {.val {unknown[1]}} is not one.",
        i = "Available: {.val {available}}."
      ),
      call = call
    )
  }
  variables
}

# The `test =` argument of both `describe_posterior()` and
# `model_parameters()`: pd is always asked for, the ROPE only on request.
route_test_arg <- function(rope) {
  if (is.null(rope)) "pd" else c("pd", "rope")
}

# The estimate column of an easystats table, which is named after the
# centrality that was asked for.
route_estimate_column <- function(centrality) {
  switch(centrality,
    median = "Median",
    mean = "Mean"
  )
}
