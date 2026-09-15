# The contract between the two layers (ARCHITECTURE.md decisions 1 and 2).
# The extract layer produces an `apabayes_tidy` tibble; the format, inline
# and table layers consume one and never see a fit object.

# ---- the column contracts ---------------------------------------------

# One entry per table kind: which columns must be supplied by the caller
# and, in order, every column of the contract with its storage type.
tidy_contracts <- function() {
  list(
    parameters = list(
      required = c("term", "estimate"),
      columns = c(
        term = "chr", label = "chr", estimate = "dbl", ci_low = "dbl",
        ci_high = "dbl", ci_method = "chr", ci_level = "dbl", pd = "dbl",
        rope_pct = "dbl", rhat = "dbl", ess_bulk = "dbl",
        ess_tail = "dbl", bf = "dbl", component = "chr", group = "chr",
        effects = "chr", std = "lgl", p = "dbl"
      )
    ),
    diagnostics = list(
      required = "term",
      columns = c(
        term = "chr", rhat = "dbl", ess_bulk = "dbl", ess_tail = "dbl"
      )
    ),
    hypotheses = list(
      required = c("hypothesis", "estimate"),
      columns = c(
        hypothesis = "chr", group = "chr", estimate = "dbl",
        ci_low = "dbl", ci_high = "dbl", ci_method = "chr",
        ci_level = "dbl", evid_ratio = "dbl", post_prob = "dbl",
        bf10 = "dbl", directional = "lgl"
      )
    ),
    loo = list(
      required = c("model", "elpd_diff"),
      columns = c(
        model = "chr", elpd_diff = "dbl", se_diff = "dbl", elpd = "dbl",
        se_elpd = "dbl", p_loo = "dbl", looic = "dbl", weight = "dbl"
      )
    ),
    bf_models = list(
      required = c("model", "bf"),
      columns = c(
        model = "chr", bf = "dbl", log_bf = "dbl", denominator = "lgl",
        method = "chr", post_prob = "dbl"
      )
    ),
    sem_fit = list(
      required = "model",
      columns = c(
        model = "chr", chisq = "dbl", df = "dbl", p = "dbl", cfi = "dbl",
        tli = "dbl", rmsea = "dbl", rmsea_low = "dbl",
        rmsea_high = "dbl", rmsea_level = "dbl", srmr = "dbl",
        ppp = "dbl", brmsea = "dbl", brmsea_low = "dbl",
        brmsea_high = "dbl", bgammahat = "dbl", bgammahat_low = "dbl",
        bgammahat_high = "dbl"
      )
    ),
    # `group` and `rope_pct` joined with the emmGrid route (session 22):
    # a contrast computed within `by` groups repeats its name across
    # them, so the inline layer needs `group` to address one, and
    # `model_parameters(emmGrid)` returns the ROPE share (measured, which
    # decision 22 had assumed it did not).
    contrasts = list(
      required = c("contrast", "estimate"),
      columns = c(
        contrast = "chr", group = "chr", estimate = "dbl", ci_low = "dbl",
        ci_high = "dbl", ci_method = "chr", ci_level = "dbl", pd = "dbl",
        rope_pct = "dbl"
      )
    )
  )
}

tidy_types <- function() names(tidy_contracts())

na_of <- function(type) {
  switch(type,
    chr = NA_character_,
    dbl = NA_real_,
    lgl = NA
  )
}

# Coerce a supplied column to its contract type, or abort naming the
# column and the class that could not be coerced. An all-NA column of any
# type counts as the typed NA (a `data.frame()` column of bare NAs is
# logical).
coerce_tidy_column <- function(value, type, name, table_type,
                               call = rlang::caller_env()) {
  if (is.logical(value) && all(is.na(value))) {
    return(rep(na_of(type), length(value)))
  }
  ok <- switch(type,
    chr = is.character(value) || is.factor(value),
    dbl = is.numeric(value),
    lgl = is.logical(value)
  )
  if (!ok) {
    # nolint next: object_usage_linter. Used in the cli string below.
    wanted <- switch(type,
      chr = "character",
      dbl = "numeric",
      lgl = "logical"
    )
    cli::cli_abort(
      "Column {.field {name}} of a {.val {table_type}} table must be
       {wanted}, not {.cls {class(value)}}.",
      call = call
    )
  }
  switch(type,
    chr = as.character(value),
    dbl = as.double(value),
    lgl = as.logical(value)
  )
}

check_ci_method <- function(x, allow_na = TRUE, call = rlang::caller_env()) {
  if (allow_na && length(x) == 1 && is.na(x)) {
    return(invisible(NA_character_))
  }
  if (!(is.character(x) && length(x) == 1 && x %in% ci_methods())) {
    cli::cli_abort(
      '{.arg ci_method} must be one of "eti", "hdi", "hpd", "spi",
       "bci", "wald" or "boot", not {.val {x}}.',
      call = call
    )
  }
  invisible(x)
}

# The interval types the contract knows: five credible intervals and,
# since the lavaan route, two confidence intervals (Wald on standard or
# robust SEs; percentile bootstrap).
#
# `spi` and `bci` are here because this column is *descriptive*: its job
# is to say truthfully what an interval is, and the result-object route
# accepts a table apabayes did not compute, so the user chooses the
# method. `describe_posterior()` computes both (measured), and refusing
# to describe one would make a user recompute their analysis to report
# it. No route *offers* them: every `ci` argument is still "eti" or
# "hdi", exactly as `wald` and `boot` are describable but not offered.
ci_methods <- function() {
  c("eti", "hdi", "hpd", "spi", "bci", "wald", "boot")
}

# `centrality` is "median" or "mean" for a posterior summary and NA for a
# point estimate that is not one (the lavaan route). NA is a value here,
# not an absence: the print header omits the part, and the format layer
# picks the estimate label from it.
check_centrality <- function(x, call = rlang::caller_env()) {
  if (length(x) == 1 && is.na(x)) {
    return(invisible(NA_character_))
  }
  ok <- is.character(x) && length(x) == 1 && x %in% c("median", "mean")
  if (!ok) {
    cli::cli_abort(
      '{.arg centrality} must be "median", "mean" or NA, not {.val {x}}.',
      call = call
    )
  }
  invisible(x)
}

check_ci_level <- function(x, allow_na = TRUE, strict = FALSE,
                           arg = "ci_level", call = rlang::caller_env()) {
  if (allow_na && length(x) == 1 && is.na(x)) {
    return(invisible(NA_real_))
  }
  upper <- if (strict) x < 1 else x <= 1
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0 && upper
  if (!ok) {
    # nolint next: object_usage_linter. Used in the cli string below.
    bound <- if (strict) "strictly less than 1" else "at most 1"
    cli::cli_abort(
      "{.arg {arg}} must be a single number greater than 0 and
       {bound}.",
      call = call
    )
  }
  invisible(x)
}

# ---- constructor -------------------------------------------------------

#' The apabayes tidy contract
#'
#' `apabayes_tidy()` builds the object the extract layer hands to the
#' format, inline and table layers: a tibble whose column names and types
#' are fixed by the kind of table, carrying the reporting metadata as
#' attributes. Every `apa_tidy()` method returns one, and every apabayes
#' function that reports numbers takes one.
#'
#' @section Column contracts:
#' `type` selects the contract. Columns the caller does not supply are
#' filled with the typed `NA`, so a consumer can select a column by name
#' without testing whether it exists.
#'
#' `"parameters"` — one row per model parameter. Required: `term`,
#' `estimate`.
#'
#' \describe{
#'   \item{`term`}{character; the parameter as its source names it
#'     (`b_wt`, `visual=~x1`, `mu`).}
#'   \item{`label`}{character; the display label. Defaults to `term`.}
#'   \item{`estimate`}{double; the posterior median or mean, per the
#'     `centrality` attribute.}
#'   \item{`ci_low`, `ci_high`}{double; interval bounds.}
#'   \item{`ci_method`}{character; `"eti"`, `"hdi"`, `"hpd"`, `"spi"`
#'     (shortest probability interval) or `"bci"` (bias-corrected and
#'     accelerated) for a credible interval, `"wald"` or `"boot"`
#'     (percentile bootstrap) for a frequentist confidence interval
#'     (lavaan).}
#'   \item{`ci_level`}{double; the interval mass, e.g. `0.95`.}
#'   \item{`pd`}{double; probability of direction.}
#'   \item{`rope_pct`}{double; percentage of the posterior inside the
#'     ROPE, `NA` unless a ROPE was requested.}
#'   \item{`rhat`, `ess_bulk`, `ess_tail`}{double; convergence
#'     diagnostics.}
#'   \item{`bf`}{double; a Bayes factor for the parameter, `NA` unless
#'     one was requested.}
#'   \item{`component`, `group`, `effects`}{character; the structure a
#'     model puts a parameter in.}
#'   \item{`std`}{logical; whether the row is a standardized estimate.}
#'   \item{`p`}{double; a frequentist p value (lavaan only).}
#' }
#'
#' The other contracts are `"diagnostics"` (`term`, `rhat`, `ess_bulk`,
#' `ess_tail`), `"hypotheses"`, `"loo"`, `"bf_models"`, `"sem_fit"` and
#' `"contrasts"` (`contrast`, `group` for the `by` variable's value,
#' `estimate`, the interval columns, `pd`, `rope_pct`); the print method
#' lists the columns of each, and every extract method documents the
#' ones it fills. Columns beyond the contract are kept, after the
#' contract columns.
#'
#' @section Metadata:
#' `type`, `centrality`, `ci_method`, `ci_level`, `source_class` and
#' `package_versions` are attributes of the table, plus anything passed
#' through `...` (`rope_range`, `rope_ci`, `bf_method`, `note`).
#' `ci_method` and `ci_level` are *both* attributes and columns: a
#' report can mix intervals of different kinds in one table, so each row
#' says what it is, while the attribute says what was asked for.
#'
#' @section Validity:
#' A tibble subclass survives `[` and the dplyr verbs, and nothing
#' re-validates on the way, so a table you have edited can carry the
#' class without satisfying the contract. Every apabayes function that
#' consumes a tidy table therefore validates it on entry;
#' `validate_apabayes_tidy()` is that check.
#'
#' @param x A data frame with at least the required columns of `type`.
#' @param type Which column contract applies: one of `"parameters"`,
#'   `"diagnostics"`, `"hypotheses"`, `"loo"`, `"bf_models"`,
#'   `"sem_fit"`, `"contrasts"`.
#' @param centrality `"median"` or `"mean"`; what `estimate` holds. `NA`
#'   when the estimate is not a posterior summary (a lavaan
#'   maximum-likelihood estimate).
#' @param ci_method `"eti"`, `"hdi"`, `"hpd"`, `"spi"`, `"bci"`,
#'   `"wald"`, `"boot"` or `NA`; seeds the `ci_method` column when the
#'   caller supplies none. The column describes intervals apabayes did
#'   not necessarily compute, so it knows more methods than any route
#'   offers through its own `ci` argument.
#' @param ci_level A number in (0, 1] or `NA`; seeds the `ci_level`
#'   column the same way.
#' @param source_class Character; `class()` of the object the numbers
#'   were extracted from.
#' @param package_versions Named character vector of the packages that
#'   produced the numbers.
#' @param ... Further attributes, all named.
#'
#' @return A tibble of class `apabayes_tidy` with the contract columns
#'   first, in contract order, then any extra columns.
#'   `validate_apabayes_tidy()` returns its input invisibly or aborts;
#'   `is_apabayes_tidy()` returns `TRUE` or `FALSE`.
#'
#' @examples
#' apabayes_tidy(data.frame(term = "b_wt", estimate = -5.34))
#' apabayes_tidy(
#'   data.frame(
#'     term = "b_wt", estimate = -5.34, ci_low = -6.85,
#'     ci_high = -3.82, pd = 1
#'   ),
#'   ci_method = "hdi", source_class = "stanfit"
#' )
#' @export
apabayes_tidy <- function(x,
                          ...,
                          type = c(
                            "parameters", "diagnostics",
                            "hypotheses", "loo", "bf_models",
                            "sem_fit", "contrasts"
                          ),
                          centrality = c("median", "mean"),
                          ci_method = "eti",
                          ci_level = 0.95,
                          source_class = NA_character_,
                          package_versions = character()) {
  # Checked before `...` is forced, so that a non-data-frame `x` is still
  # reported ahead of anything that errors while evaluating `...`.
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame, not {.cls {class(x)}}.")
  }
  # `...` sits before the named arguments so that an unnamed extra cannot
  # be absorbed positionally by `type`; everything but `x` is named.
  extra_attrs <- rlang::list2(...)
  check_tidy_constructor_args(extra_attrs, package_versions)
  type <- rlang::arg_match(type)
  # NA is a value of `centrality` (a point estimate that is no posterior
  # summary); `arg_match()` would reject it, so it is checked apart. The
  # validator uses `check_centrality()` for the same domain: here the
  # value is a user-facing argument and `arg_match()`'s "did you mean"
  # earns its place, there it is a value already on the object.
  centrality <- if (length(centrality) == 1 && is.na(centrality)) {
    NA_character_
  } else {
    rlang::arg_match(centrality)
  }
  check_ci_method(ci_method)
  check_ci_level(ci_level)

  contract <- tidy_contracts()[[type]]
  missing <- setdiff(contract$required, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "A tidy table of type {.val {type}} needs the column{?s}
       {.field {missing}}.",
      i = "Supplied: {.field {names(x)}}."
    ))
  }

  cols <- assemble_tidy_columns(x, contract, type, ci_method, ci_level)
  out <- tibble::new_tibble(cols, nrow = nrow(x), class = "apabayes_tidy")
  attributes_to_set <- c(
    list(
      type = type,
      centrality = centrality,
      ci_method = ci_method,
      ci_level = ci_level,
      source_class = source_class,
      package_versions = package_versions
    ),
    extra_attrs
  )
  for (nm in names(attributes_to_set)) {
    attr(out, nm) <- attributes_to_set[[nm]]
  }
  validate_apabayes_tidy(out)
  # Not `validate_apabayes_tidy(out)` as the last expression: it returns
  # invisibly, and the constructor's value must be visible so that a bare
  # `apabayes_tidy(x)` reaches `print.apabayes_tidy()`.
  out
}

# The constructor's remaining input checks, in the order it reported them
# before they were extracted. The data-frame check stays in the
# constructor: it must run before `...` is forced.
check_tidy_constructor_args <- function(extra_attrs, package_versions) {
  unnamed_attrs <- length(extra_attrs) > 0 &&
    (is.null(names(extra_attrs)) || any(!nzchar(names(extra_attrs))))
  if (unnamed_attrs) {
    cli::cli_abort(c(
      "Every attribute passed through {.arg ...} must be named.",
      i = "Every argument of {.fn apabayes_tidy} except {.arg x} is named."
    ))
  }
  bad_versions <- length(package_versions) > 0 &&
    (!is.character(package_versions) || is.null(names(package_versions)))
  if (bad_versions) {
    cli::cli_abort("{.arg package_versions} must be a named character vector.")
  }
  invisible(NULL)
}

# The contract's columns, in contract order, then whatever else the caller
# supplied. A column the caller gave is coerced to the contract's type; a
# column the contract seeds (`ci_method`, `ci_level`) is recycled from the
# constructor's argument; anything else is the typed NA.
assemble_tidy_columns <- function(x, contract, type, ci_method, ci_level) {
  n <- nrow(x)
  seeds <- list(ci_method = ci_method, ci_level = ci_level)
  cols <- list()
  for (nm in names(contract$columns)) {
    ctype <- contract$columns[[nm]]
    if (nm %in% names(x)) {
      cols[[nm]] <- coerce_tidy_column(x[[nm]], ctype, nm, type)
    } else if (nm %in% names(seeds)) {
      cols[[nm]] <- rep(coerce_tidy_column(seeds[[nm]], ctype, nm, type), n)
    } else {
      cols[[nm]] <- rep(na_of(ctype), n)
    }
  }
  # `label` names the parameter for a reader; without one, the term does.
  if (all(c("label", "term") %in% names(cols))) {
    blank <- is.na(cols$label)
    cols$label[blank] <- cols$term[blank]
  }
  for (nm in setdiff(names(x), names(contract$columns))) {
    cols[[nm]] <- x[[nm]]
  }
  cols
}

#' @rdname apabayes_tidy
#' @export
validate_apabayes_tidy <- function(x) {
  if (!is_apabayes_tidy(x)) {
    cli::cli_abort(
      "{.arg x} must be an {.cls apabayes_tidy} object, not
       {.cls {class(x)}}."
    )
  }
  type <- attr(x, "type")
  if (!(is.character(type) && length(type) == 1 && type %in% tidy_types())) {
    cli::cli_abort(
      "The {.field type} attribute must be one of {.val {tidy_types()}},
       not {.val {type}}."
    )
  }
  contract <- tidy_contracts()[[type]]
  missing <- setdiff(names(contract$columns), names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      "A tidy table of type {.val {type}} is missing the column{?s}
       {.field {missing}}."
    )
  }
  for (nm in names(contract$columns)) {
    coerce_tidy_column(x[[nm]], contract$columns[[nm]], nm, type)
  }
  check_centrality(attr(x, "centrality"))
  check_ci_method(attr(x, "ci_method"))
  check_ci_level(attr(x, "ci_level"))
  if ("pd" %in% names(x)) {
    check_range01(x$pd, arg = "pd")
  }
  if ("ci_level" %in% names(x)) {
    level <- x$ci_level[!is.na(x$ci_level)]
    if (any(level <= 0 | level > 1)) {
      cli::cli_abort(
        "Column {.field ci_level} must lie between 0 and 1; found
         {.val {level[level <= 0 | level > 1][1]}}."
      )
    }
  }
  invisible(x)
}

#' @rdname apabayes_tidy
#' @export
is_apabayes_tidy <- function(x) {
  inherits(x, "apabayes_tidy")
}

#' @param n,width Passed to the tibble print method.
#' @rdname apabayes_tidy
#' @export
print.apabayes_tidy <- function(x, n = NULL, width = NULL, ...) {
  rows <- nrow(x)
  cat(sprintf(
    "# apabayes tidy table: %s (%d row%s)\n",
    attr(x, "type"), rows, if (rows == 1) "" else "s"
  ))
  meta <- tidy_header_meta(x)
  if (nzchar(meta)) {
    cat("# ", meta, "\n", sep = "")
  }
  plain <- x
  class(plain) <- c("tbl_df", "tbl", "data.frame")
  print(plain, n = n, width = width, ...)
  invisible(x)
}

# The second header line: what the numbers are, in plain text. Console
# output never carries markup (see `markup()`).
tidy_header_meta <- function(x) {
  parts <- character()
  centrality <- attr(x, "centrality")
  if (is.character(centrality) && !is.na(centrality)) {
    parts <- c(parts, centrality)
  }
  method <- attr(x, "ci_method")
  level <- attr(x, "ci_level")
  if (is.character(method) && !is.na(method) && !is.na(level)) {
    parts <- c(parts, sprintf(
      "%s%% %s",
      format(round(level * 100, 2)),
      switch(method,
        eti = "CrI (equal-tailed)",
        hdi = "HDI",
        hpd = "HPD interval",
        spi = "SPI (shortest probability)",
        bci = "BCI (bias-corrected accelerated)",
        wald = "CI (Wald)",
        boot = "CI (percentile bootstrap)"
      )
    ))
  }
  source_class <- attr(x, "source_class")
  line <- paste(parts, collapse = ", ")
  if (length(source_class) > 0 && !is.na(source_class[1])) {
    line <- paste0(
      if (nzchar(line)) paste0(line, "; ") else "",
      "source: ", source_class[1]
    )
  }
  line
}
