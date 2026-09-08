# The comparison routes (ARCHITECTURE.md § File organization):
# `brmshypothesis` here, `compare.loo` and `bayesfactor_models` to come.
#
# The brmshypothesis route is the first that calls no easystats function.
# Measured 2026-09-08 (dev/specs/spec-apa_tidy_brmshypothesis.md, probes
# `probe_hypothesis*.R`): there is no `model_parameters.brmshypothesis`,
# `describe_posterior()` reports the class as not implemented, and
# `insight::model_info()` returns NULL. So the numbers are brms's own
# `$hypothesis` data frame, and apabayes selects, renames and derives the
# two things decision 3 asks for.
#
# The route reads brms's own table, but calls no brms function; brms is
# still required to be *installed*, because `package_versions` names the
# version that produced the numbers.
#
# Both derivations exist because brms stores neither: `robust = TRUE`
# leaves no marker of the centrality, and a *named* hypothesis loses its
# operator, so neither the kind of the hypothesis nor what `Estimate`
# holds can be read from the object's metadata. Both can be read from its
# numbers, exactly, because it carries the draws.

# ---- the parameters of the object --------------------------------------

# The methods brms uses for the evidence ratio, one per kind of
# hypothesis. Reported in the `bf_method` attribute so that a table note
# can name them (decision 16).
hypothesis_bf_methods <- function() {
  c(point = "Savage-Dickey density ratio", directional = "posterior odds")
}

#' @describeIn apa_tidy The output of [brms::hypothesis()]. Estimates,
#'   interval, evidence ratio and posterior probability are brms's own;
#'   apabayes adds `bf10` (`1 / evid_ratio` for a point hypothesis, the
#'   evidence ratio itself for a directional one) and `directional`.
#'   easystats has no method for this class, so no easystats function is
#'   called, and `package_versions` credits `brms` alone.
#'
#' @param directional `NULL` to read from `x` which rows test a
#'   directional hypothesis, or a logical vector of length 1 or one per
#'   row saying so. `brms::hypothesis()` records the operator only in the
#'   `Hypothesis` string, and replaces that string with the user's name
#'   when the hypothesis was named, so the derivation reads the interval
#'   instead: brms uses the quantiles at `alpha` and `1 - alpha` for a
#'   directional hypothesis and at `alpha / 2` and `1 - alpha / 2` for a
#'   point one. A row where neither rule fits, or both (a posterior with
#'   no spread), is an error rather than a missing Bayes factor.
#' @export
apa_tidy.brmshypothesis <- function(x, directional = NULL, ...) {
  # The guard is here for a narrower reason than on the other routes.
  # This one calls no brms *function*: every value is a list element and
  # the arithmetic is `quantile()`, `mean()` and `median()`. But
  # `package_versions` must credit the package that produced the
  # numbers, and `utils::packageVersion("brms")` aborts with a bare base
  # error when brms is absent — so the requirement is real and is stated
  # here rather than reached at the end of the function.
  rlang::check_installed(
    "brms",
    reason = "to record the version that produced these numbers."
  )
  hyp <- check_brmshypothesis(x)
  directional <- resolve_directional(directional, hyp, x$samples, x$alpha)
  ci_level <- hypothesis_ci_level(x$alpha, directional)
  evid_ratio <- as.double(hyp$Evid.Ratio)

  out <- data.frame(
    hypothesis = as.character(hyp$Hypothesis),
    group = hypothesis_group(hyp),
    estimate = as.double(hyp$Estimate),
    ci_low = as.double(hyp$CI.Lower),
    ci_high = as.double(hyp$CI.Upper),
    ci_method = "eti",
    ci_level = ci_level,
    evid_ratio = evid_ratio,
    post_prob = as.double(hyp$Post.Prob),
    # Decision 3: brms reports the Savage-Dickey ratio *for* a point
    # hypothesis and the posterior odds *for* a directional one, so only
    # the first needs inverting to be a Bayes factor against the null.
    bf10 = ifelse(directional, evid_ratio, 1 / evid_ratio),
    directional = directional,
    stringsAsFactors = FALSE
  )

  apabayes_tidy(
    out,
    type = "hypotheses",
    centrality = hypothesis_centrality(hyp, x$samples),
    ci_method = "eti",
    # One object can hold rows of two different levels, so the attribute
    # states a level only when every row agrees on it. The column always
    # says what each row is (decision 2).
    ci_level = if (length(unique(ci_level)) == 1) ci_level[1] else NA_real_,
    source_class = class(x),
    package_versions = package_versions_of(c("brms", "apabayes")),
    bf_method = unname(
      hypothesis_bf_methods()[c(any(!directional), any(directional))]
    )
  )
}

# `Star` and `Est.Error` are not read. `Star` is "*" exactly when the
# interval excludes zero (measured), which is a verdict, and decisions 15
# and 24 keep verdicts out of everything apabayes prints; `Est.Error` has
# no column in any contract, as `SE` has none on the parameters routes.

# The `Group` column exists only under `scope = "coef"` or `"ranef"`,
# where the object has one row per hypothesis and group level (measured).
# It is a factor there; the contract's column is character.
hypothesis_group <- function(hyp) {
  if (!"Group" %in% names(hyp)) {
    return(rep(NA_character_, nrow(hyp)))
  }
  as.character(hyp$Group)
}

# ---- the object's own validity -----------------------------------------

# A `brmshypothesis` is a plain list carrying a plain data frame, so the
# class attribute alone says nothing about the components. Checked here
# rather than left to fail inside the arithmetic, so that a truncated or
# hand-built object is named as what it is.
check_brmshypothesis <- function(x, call = rlang::caller_env()) {
  missing <- setdiff(c("hypothesis", "samples", "alpha"), names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg x} must be a {.cls brmshypothesis} object from
         {.fn brms::hypothesis}.",
        i = "It has no {.field {missing}} component{?s}."
      ),
      call = call
    )
  }
  hyp <- as.data.frame(x$hypothesis)
  needed <- c(
    "Hypothesis", "Estimate", "CI.Lower", "CI.Upper", "Evid.Ratio",
    "Post.Prob"
  )
  absent <- setdiff(needed, names(hyp))
  if (length(absent) > 0) {
    cli::cli_abort(
      "The {.field hypothesis} table of {.arg x} has no {.field {absent}}
       column{?s}.",
      call = call
    )
  }
  if (nrow(hyp) == 0) {
    cli::cli_abort("{.arg x} has no hypotheses to report.", call = call)
  }
  # `$samples` column i belongs to row i and is matched by position: the
  # columns are named H1..Hn whatever the rows are called (measured).
  if (ncol(x$samples) != nrow(hyp)) {
    cli::cli_abort(
      "{.arg x} has {ncol(x$samples)} samples column{?s} for {nrow(hyp)}
       hypothesis row{?s}.",
      call = call
    )
  }
  # Checked here, before anything derived from it: `derive_directional()`
  # feeds `alpha` to `stats::quantile()`, which aborts with a bare
  # `'probs' outside [0,1]` naming nothing the user can act on. A real
  # `brms::hypothesis()` object cannot carry an alpha outside (0, 1) --
  # brms's own `quantile()` call would have failed first -- but a
  # hand-built or truncated one can, and that is what this function is
  # for.
  check_ci_level(
    x$alpha,
    allow_na = FALSE, strict = TRUE, arg = "alpha", call = call
  )
  hyp
}

# ---- which rows are directional ----------------------------------------

resolve_directional <- function(directional, hyp, samples, alpha,
                                call = rlang::caller_env()) {
  n <- nrow(hyp)
  if (is.null(directional)) {
    return(derive_directional(hyp, samples, alpha, call = call))
  }
  if (!is.logical(directional)) {
    cli::cli_abort(
      "{.arg directional} must be a logical vector or {.code NULL}, not
       {.obj_type_friendly {directional}}.",
      call = call
    )
  }
  if (anyNA(directional)) {
    cli::cli_abort(
      c(
        "{.arg directional} must not contain missing values.",
        i = "A row of unknown kind has no Bayes factor: {.field bf10}
             inverts the evidence ratio for a point hypothesis and not
             for a directional one."
      ),
      call = call
    )
  }
  if (!length(directional) %in% c(1L, n)) {
    cli::cli_abort(
      "{.arg directional} must have length 1 or {n}, not
       {length(directional)}.",
      call = call
    )
  }
  rep(directional, length.out = n)
}

# Which quantile pair brms used for each row's interval. The comparison
# is `identical()` and needs no tolerance: the bounds are bit-exactly
# `stats::quantile()`'s default type 7 of the samples column, measured at
# three alphas, on both kinds of row, with and without `robust`.
derive_directional <- function(hyp, samples, alpha,
                               call = rlang::caller_env()) {
  out <- vapply(
    seq_len(nrow(hyp)),
    function(i) {
      bounds <- c(hyp$CI.Lower[i], hyp$CI.Upper[i])
      s <- samples[[i]]
      dir <- identical(bounds, quantile_pair(s, c(alpha, 1 - alpha)))
      point <- identical(bounds, quantile_pair(s, c(alpha / 2, 1 - alpha / 2)))
      if (dir && !point) TRUE else if (point && !dir) FALSE else NA
    },
    logical(1)
  )
  if (anyNA(out)) {
    # nolint next: object_usage_linter. Used in the cli string below.
    rows <- which(is.na(out))
    cli::cli_abort(
      c(
        "The kind of hypothesis in {cli::qty(length(rows))}row{?s}
         {rows} cannot be read from {.arg x}.",
        i = "{.fn brms::hypothesis} records the operator only in the
             {.field Hypothesis} string, which a named hypothesis
             replaces, and the interval matches neither the point nor
             the directional rule.",
        i = "Pass {.arg directional} to say which rows are directional."
      ),
      call = call
    )
  }
  out
}

# `quantile()` keeps the probabilities as names; the interval bounds on
# the object do not have them.
quantile_pair <- function(x, probs) {
  unname(stats::quantile(x, probs))
}

# ---- the interval level -------------------------------------------------

# Measured: brms takes the quantiles at alpha/2 and 1 - alpha/2 for a
# point hypothesis and at alpha and 1 - alpha for a directional one, so
# one object holds intervals of two masses. brms validates `alpha`
# nowhere: at 0.5 a directional row is an interval of level 0, and above
# it the bounds come back inverted, at a negative level. The contract
# forbids both, and would report it as a bad `ci_level` — a value the
# user never typed — so the route names `alpha` instead. The check runs
# after `directional`, because whether a level is impossible depends on
# the kind of the row.
hypothesis_ci_level <- function(alpha, directional,
                                call = rlang::caller_env()) {
  # `alpha` is already known to lie in (0, 1), so the level can only fall
  # off the bottom, and only on a directional row.
  level <- ifelse(directional, 1 - 2 * alpha, 1 - alpha)
  bad <- level <= 0
  if (any(bad)) {
    # nolint next: object_usage_linter. Used in the cli string below.
    worst <- level[bad][1]
    # nolint next: object_usage_linter. Used in the cli string below.
    rows <- which(bad)
    cli::cli_abort(
      c(
        "The {.field alpha} of {.arg x} is {.val {alpha}}, which gives
         {cli::qty(length(rows))}row{?s} {rows} an interval of level
         {.val {worst}}.",
        i = "{.fn brms::hypothesis} halves the tail area for a point
             hypothesis and not for a directional one, so a directional
             row needs {.code alpha < 0.5}."
      ),
      call = call
    )
  }
  level
}

# ---- what the estimate holds -------------------------------------------

# `robust = TRUE` makes `Estimate` the median and `Est.Error` the MAD,
# and leaves no marker of itself on the object (measured: the two objects
# have the same components). Since the draws are on the object, the
# answer is a bit-exact comparison rather than a default: `Estimate` is
# `identical()` to `mean()` of its samples column, or to `median()`. A
# column with no spread matches both and is scored as brms's own default.
hypothesis_centrality <- function(hyp, samples) {
  matches <- function(f) {
    all(vapply(
      seq_len(nrow(hyp)),
      function(i) identical(hyp$Estimate[i], f(samples[[i]])),
      logical(1)
    ))
  }
  if (matches(mean)) {
    "mean"
  } else if (matches(stats::median)) {
    "median"
  } else {
    NA_character_
  }
}
