# The comparison routes (ARCHITECTURE.md § File organization):
# `brmshypothesis`, `compare.loo` and `bayesfactor_models`.
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

# ---- compare.loo ---------------------------------------------------------

# Measured 2026-09-14 (local/specs/spec-apa_tidy_compare_loo.md, probes
# `probe_loo*.R`): loo >= 2.10.0 returns a data frame with a `model`
# column, loo 2.9.0 a double matrix with the models as row names; rows
# are sorted best first in both. Neither carries a weight, so weights are
# an argument.

#' @describeIn apa_tidy The output of [loo::loo_compare()], of either
#'   shape: the data frame loo 2.10.0 and later return, or the matrix of
#'   earlier versions. One row per model, best first, with loo's own
#'   numbers: the ELPD difference to the best model and its standard
#'   error, the model's ELPD, `p_loo` and LOOIC. No loo function is
#'   called; `loo` must be installed to record its version. apabayes
#'   never turns an ELPD difference into a Bayes factor or a weight into
#'   evidence (ARCHITECTURE decision 17).
#'
#' @param weights `NULL`, or model weights to report in the `weight`
#'   column: the result of [loo::loo_model_weights()] (its kind,
#'   `"stacking"`, `"pseudo-BMA+"` or `"pseudo-BMA"`, becomes the
#'   `weight_method` attribute), or a numeric vector named by model. The
#'   weights are matched to rows by model name, never by position, and
#'   must name exactly the models of `x`. The comparison object carries
#'   no weights; `performance::compare_performance()`'s `LOOIC_wt` is the
#'   stacking weight.
#' @param reference `NULL` for loo's own reference — the model in the
#'   first row, the one with the highest ELPD, against which
#'   [loo::loo_compare()] signs every difference — or the name of another
#'   model of `x` to take the differences from, so that a model behind
#'   the reference prints a positive `elpd_diff`. Naming loo's own
#'   reference changes nothing: the object's numbers are returned
#'   untouched.
#'
#'   Under another reference `elpd_diff` becomes `elpd` minus the
#'   reference's `elpd` — a difference of two numbers loo reported,
#'   computed by apabayes, which is why it is named here — and the
#'   standard error of the difference survives only where loo measured
#'   it: `0` on the reference row, loo's own `se_diff` on the model that
#'   was loo's reference (the same pair, read the other way round), and
#'   `NA` on every other row, because a `compare.loo` object does not
#'   carry the pointwise ELPDs another pair would need. Run
#'   [loo::loo_compare()] on two models to get that standard error.
#'   `p_worse` and `diag_diff` qualify a difference against loo's
#'   reference and become `NA` for the same reason; every column that
#'   describes a model rather than a pair is unchanged, as is the row
#'   order.
#' @method apa_tidy compare.loo
#' @export
apa_tidy.compare.loo <- function(x, weights = NULL, reference = NULL, ...) {
  rlang::check_dots_empty()
  rlang::check_installed(
    "loo",
    reason = "to record the version that produced these numbers."
  )
  referenced <- compare_loo_reference(compare_loo_frame(x), reference)
  cmp <- referenced$frame
  w <- compare_loo_weights(weights, cmp$model)
  n <- nrow(cmp)
  extra <- function(name, na) {
    if (name %in% names(cmp)) cmp[[name]] else rep(na, n)
  }
  out <- data.frame(
    model = cmp$model,
    elpd_diff = cmp$elpd_diff,
    se_diff = cmp$se_diff,
    elpd = cmp$elpd_loo,
    se_elpd = cmp$se_elpd_loo,
    p_loo = cmp$p_loo,
    looic = cmp$looic,
    weight = w$weight,
    se_p_loo = extra("se_p_loo", NA_real_),
    se_looic = extra("se_looic", NA_real_),
    # loo 2.10.0 added these three; on the matrix shape they were never
    # computed, which is NA and not "no flag" ("").
    p_worse = extra("p_worse", NA_real_),
    diag_diff = extra("diag_diff", NA_character_),
    diag_elpd = extra("diag_elpd", NA_character_),
    stringsAsFactors = FALSE
  )
  apabayes_tidy(
    out,
    type = "loo",
    centrality = NA_character_,
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = class(x),
    package_versions = package_versions_of(c("loo", "apabayes")),
    reference = referenced$reference,
    weight_method = w$method
  )
}

# Which model the differences are taken from. `NULL` is loo's own
# reference, the first row (loo sorts best first, in both shapes), and
# then nothing is recomputed: measured 2026-09-17, re-deriving
# `elpd_loo - elpd_loo[1]` differs from loo's own `elpd_diff` by up to
# 1.2e-14, because loo sums the pointwise differences rather than
# differencing the sums. loo's number is the one to report.
compare_loo_reference <- function(frame, reference,
                                  call = rlang::caller_env()) {
  if (is.null(reference)) {
    return(list(frame = frame, reference = frame$model[1]))
  }
  if (!rlang::is_string(reference)) {
    cli::cli_abort(
      "{.arg reference} must be the name of one model of {.arg x} or
       {.code NULL}, not {.obj_type_friendly {reference}}.",
      call = call
    )
  }
  row <- match(reference, frame$model)
  if (is.na(row)) {
    cli::cli_abort(
      c(
        "{.arg x} has no model called {.val {reference}}.",
        i = "Models: {.val {frame$model}}."
      ),
      call = call
    )
  }
  if (row == 1L) {
    return(list(frame = frame, reference = reference))
  }
  se <- rep(NA_real_, nrow(frame))
  se[row] <- 0
  # The pair loo measured is the same pair read the other way round, so
  # its standard error is loo's, not apabayes'. Only if the object is
  # still loo's own: a subset or reordered comparison no longer has its
  # reference in row 1, and then no pair is known.
  if (identical(frame$elpd_diff[1], 0) && identical(frame$se_diff[1], 0)) {
    se[1] <- frame$se_diff[row]
  }
  frame$elpd_diff <- frame$elpd_loo - frame$elpd_loo[row]
  frame$se_diff <- se
  # Both qualify a difference against loo's reference and say nothing
  # about any other pair.
  if ("p_worse" %in% names(frame)) {
    frame$p_worse <- rep(NA_real_, nrow(frame))
  }
  if ("diag_diff" %in% names(frame)) {
    frame$diag_diff <- rep(NA_character_, nrow(frame))
  }
  list(frame = frame, reference = reference)
}

# The LOO columns every row needs; `model` is checked apart because the
# matrix shape keeps it in the row names.
compare_loo_columns <- function() {
  c("elpd_diff", "se_diff", "elpd_loo", "se_elpd_loo", "p_loo", "looic")
}

# Either shape as a plain data frame with a `model` column, checked.
compare_loo_frame <- function(x, call = rlang::caller_env()) {
  if (is.matrix(x)) {
    m <- unclass(x)
    if (is.null(rownames(m))) {
      cli::cli_abort(
        "{.arg x} is a matrix-shaped {.cls compare.loo} without row names,
         which is where loo versions before 2.10.0 keep the model names.",
        call = call
      )
    }
    frame <- as.data.frame(m, stringsAsFactors = FALSE)
    frame$model <- rownames(m)
    rownames(frame) <- NULL
  } else if (is.data.frame(x)) {
    frame <- x
    class(frame) <- "data.frame"
  } else {
    cli::cli_abort(
      "{.arg x} must be a {.cls compare.loo} data frame or matrix, not
       {.obj_type_friendly {x}}.",
      call = call
    )
  }
  check_compare_loo_columns(frame, call)
  if (nrow(frame) == 0) {
    cli::cli_abort("{.arg x} has no models to report.", call = call)
  }
  # nolint next: object_usage_linter. Used in the cli string below.
  dup <- unique(frame$model[duplicated(frame$model)])
  if (length(dup) > 0) {
    cli::cli_abort(
      c(
        "{.arg x} names the model{?s} {.val {dup}} more than once.",
        i = "Rows are addressed and weights matched by model name; name
             the elements of the list given to {.fn loo::loo_compare}."
      ),
      call = call
    )
  }
  frame
}

check_compare_loo_columns <- function(frame, call = rlang::caller_env()) {
  missing <- setdiff(compare_loo_columns(), names(frame))
  if (length(missing) > 0) {
    # A WAIC or k-fold comparison is also a `compare.loo`, with the
    # criterion in its column names (measured: `elpd_waic`, `waic`).
    other <- setdiff(
      grep("^elpd_", names(frame), value = TRUE),
      c("elpd_diff", "elpd_loo")
    )
    if (length(other) > 0) {
      # nolint next: object_usage_linter. Used in the cli string below.
      criterion <- toupper(sub("^elpd_", "", other[1]))
      cli::cli_abort(
        c(
          "{.arg x} is a {criterion} comparison; the {.val loo} table
           reports LOO comparisons only.",
          i = "Compare the models with {.fn loo::loo} objects."
        ),
        call = call
      )
    }
    cli::cli_abort(
      "{.arg x} has no {.field {missing}} column{?s}.",
      call = call
    )
  }
  if (!"model" %in% names(frame)) {
    cli::cli_abort(
      "{.arg x} has no {.field model} column naming the models.",
      call = call
    )
  }
  invisible(frame)
}

# The `weight` column and the `weight_method` attribute.
compare_loo_weights <- function(weights, models, call = rlang::caller_env()) {
  if (is.null(weights)) {
    return(list(weight = rep(NA_real_, length(models)), method = NA_character_))
  }
  if (!is.numeric(weights)) {
    cli::cli_abort(
      "{.arg weights} must be a {.fn loo::loo_model_weights} result or a
       numeric vector named by model, not {.obj_type_friendly {weights}}.",
      call = call
    )
  }
  nms <- names(weights)
  if (is.null(nms) || any(is.na(nms) | !nzchar(nms))) {
    cli::cli_abort(
      c(
        "{.arg weights} must be named by model.",
        i = "Weights are matched to models by name, never by position."
      ),
      call = call
    )
  }
  # nolint next: object_usage_linter. Used in the cli string below.
  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0) {
    cli::cli_abort(
      "{.arg weights} names {.val {dup}} more than once.",
      call = call
    )
  }
  no_weight <- setdiff(models, nms)
  no_model <- setdiff(nms, models)
  if (length(no_weight) > 0 || length(no_model) > 0) {
    cli::cli_abort(
      c(
        "The names of {.arg weights} must be the models of {.arg x}.",
        i = if (length(no_weight) > 0) "No weight for {.val {no_weight}}.",
        i = if (length(no_model) > 0) "No model called {.val {no_model}}."
      ),
      call = call
    )
  }
  values <- as.numeric(weights)
  if (anyNA(values)) {
    cli::cli_abort(
      "{.arg weights} must not contain missing values.",
      call = call
    )
  }
  check_range01(values, arg = "weights", call = call)
  # nolint next: object_usage_linter. Used in the cli string below.
  total <- sum(values)
  if (abs(total - 1) > 1e-6) {
    cli::cli_abort(
      c(
        "{.arg weights} must sum to 1, not {.val {total}}.",
        i = "Pass the {.fn loo::loo_model_weights} result itself rather
             than rounded values."
      ),
      call = call
    )
  }
  list(
    weight = values[match(models, nms)],
    method = loo_weight_method(weights)
  )
}

# The kind of weight, from the class `loo_model_weights()` gives it
# (measured). A plain numeric vector says nothing about how it was made.
loo_weight_method <- function(weights) {
  kinds <- c(
    stacking_weights = "stacking",
    pseudobma_bb_weights = "pseudo-BMA+",
    pseudobma_weights = "pseudo-BMA"
  )
  hit <- intersect(class(weights), names(kinds))
  if (length(hit) == 0) NA_character_ else unname(kinds[hit[1]])
}

# ---- bayesfactor_models --------------------------------------------------

# Measured 2026-09-14 (local/specs/spec-apa_tidy_bayesfactor_models.md,
# probes `probe_bf_models*.R`): `Model` and `log_BF` columns, the
# denominator as a row index attribute, and `[` keeping that index
# unchanged. bayestestR offers no posterior model probability for the
# class, so the route computes it at equal prior odds from `log_BF`.

#' @describeIn apa_tidy The output of [bayestestR::bayesfactor_models()].
#'   `bf` is `exp(log_BF)`, kept next to `log_bf` because the exponential
#'   overflows above a log Bayes factor of about 709; `denominator` marks
#'   the row every Bayes factor is taken against; `method` is how they
#'   were computed (bridge sampling, the BIC approximation, BayesFactor's
#'   JZS). `post_prob` is the posterior probability of each model under
#'   **equal prior odds**, the assumption the `prior_odds` attribute
#'   records. `model` is the model as bayestestR names it (the formula's
#'   right-hand side for most classes) and the extra column `name` the
#'   argument it was passed as.
#' @method apa_tidy bayesfactor_models
#' @export
apa_tidy.bayesfactor_models <- function(x, ...) {
  rlang::check_dots_empty()
  check_bayesfactor_models(x)
  log_bf <- as.double(x$log_BF)
  denominator <- attr(x, "denominator", exact = TRUE)
  method <- attr(x, "BF_method", exact = TRUE)
  out <- data.frame(
    model = as.character(x$Model),
    bf = exp(log_bf),
    log_bf = log_bf,
    denominator = seq_along(log_bf) == denominator,
    method = method,
    post_prob = posterior_model_probs(log_bf),
    name = rownames(x),
    stringsAsFactors = FALSE
  )
  apabayes_tidy(
    out,
    type = "bf_models",
    centrality = NA_character_,
    ci_method = NA_character_,
    ci_level = NA_real_,
    source_class = class(x),
    package_versions = package_versions_of(c("bayestestR", "apabayes")),
    bf_method = method,
    prior_odds = "equal",
    denominator_model = out$model[denominator]
  )
}

check_bayesfactor_models <- function(x, call = rlang::caller_env()) {
  missing <- setdiff(c("Model", "log_BF"), names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg x} has no {.field {missing}} column{?s}.",
      call = call
    )
  }
  if (!is.numeric(x$log_BF)) {
    cli::cli_abort(
      "Column {.field log_BF} of {.arg x} must be numeric, not
       {.cls {class(x$log_BF)}}.",
      call = call
    )
  }
  if (nrow(x) == 0) {
    cli::cli_abort("{.arg x} has no models to report.", call = call)
  }
  if (!rlang::is_string(attr(x, "BF_method", exact = TRUE))) {
    cli::cli_abort(
      "{.arg x} has no {.field BF_method} attribute saying how its Bayes
       factors were computed.",
      call = call
    )
  }
  d <- attr(x, "denominator", exact = TRUE)
  ok <- is.numeric(d) && length(d) == 1 && !is.na(d) && d == round(d)
  if (!ok) {
    cli::cli_abort(
      "{.arg x} has no usable {.field denominator} attribute (the row
       index of the model every Bayes factor is taken against).",
      call = call
    )
  }
  # `[` keeps the attribute as it was (measured), so a subset or a
  # reordered object points at the wrong row or past the last one. The
  # denominator's own log Bayes factor is 0 by construction.
  if (d < 1 || d > nrow(x) || !identical(as.double(x$log_BF[d]), 0)) {
    cli::cli_abort(
      c(
        "The {.field denominator} of {.arg x} (row {d}) is not a row with a
         log Bayes factor of 0; the object looks subset or edited.",
        i = "Re-run {.fn bayestestR::bayesfactor_models}, or use
             {.code update(x, reference = )} on the whole object."
      ),
      call = call
    )
  }
  invisible(x)
}

# Posterior model probabilities at equal prior odds,
# exp(log_bf - logsumexp(log_bf)): the same numbers as the naive
# exp(log_bf) / sum(exp(log_bf)) and finite where that overflows
# (measured). An unknown or infinite log Bayes factor leaves every
# probability unknown; -Inf is a probability of 0.
posterior_model_probs <- function(log_bf) {
  if (anyNA(log_bf) || any(log_bf == Inf)) {
    return(rep(NA_real_, length(log_bf)))
  }
  top <- max(log_bf)
  exp(log_bf - (top + log(sum(exp(log_bf - top)))))
}
