# Tests for the brmshypothesis route
# (dev/specs/spec-apa_tidy_brmshypothesis.md).
#
# No expected number is typed. Every one is computed inside the test from
# the object under test, with brms's own components as the oracle:
# `h$hypothesis` for the columns, `h$samples` for the interval and the
# centrality, and the operator brms leaves in the `Hypothesis` string for
# `directional` — the one thing the route deliberately does not read, so
# that the string can check the derivation instead of feeding it.
#
# The live fits come from `test_brms_fit()` and stay off CRAN. The guard
# and edge-case tests build a `brmshypothesis` object by hand, with the
# measured rule for the interval (spec point 6), and run everywhere.

# ---- helpers -----------------------------------------------------------

# The operator brms leaves in an *unnamed* hypothesis string: it rewrites
# every one to "(expr) <op> 0" (measured, probe 3). NA when the string is
# a user-supplied name and carries no operator.
op_of <- function(h) {
  s <- h$hypothesis$Hypothesis
  out <- rep(NA_character_, length(s))
  hit <- grepl("^\\(.*\\) [<>=] 0$", s)
  out[hit] <- sub("^\\(.*\\) ([<>=]) 0$", "\\1", s[hit])
  out
}

# A `brmshypothesis` object built from numbers alone, so that a test can
# reach the guards without a Stan fit and without brms. The interval
# follows the measured rule: quantiles at alpha/2 for a point row and at
# alpha for a directional one.
fake_hypothesis <- function(samples, directional, alpha = 0.05,
                            hypothesis = NULL, robust = FALSE,
                            evid_ratio = NULL, group = NULL) {
  n <- length(samples)
  probs <- function(i) {
    if (directional[i]) c(alpha, 1 - alpha) else c(alpha / 2, 1 - alpha / 2)
  }
  ci <- vapply(
    seq_len(n),
    function(i) unname(stats::quantile(samples[[i]], probs(i))),
    numeric(2)
  )
  centre <- if (robust) stats::median else mean
  spread <- if (robust) stats::mad else stats::sd
  hyp <- data.frame(
    Hypothesis = hypothesis %||% paste0("(h", seq_len(n), ") = 0"),
    Estimate = vapply(samples, centre, numeric(1)),
    Est.Error = vapply(samples, spread, numeric(1)),
    CI.Lower = ci[1, ],
    CI.Upper = ci[2, ],
    Evid.Ratio = evid_ratio %||% rep(2, n),
    Post.Prob = rep(2 / 3, n),
    Star = rep("", n),
    stringsAsFactors = FALSE
  )
  if (!is.null(group)) {
    hyp <- cbind(Group = factor(group), hyp, stringsAsFactors = FALSE)
  }
  draws <- as.data.frame(samples)
  names(draws) <- paste0("H", seq_len(n))
  structure(
    list(
      hypothesis = hyp, samples = draws, prior_samples = draws,
      class = "b", alpha = alpha
    ),
    class = "brmshypothesis"
  )
}

# A deterministic, non-constant sample, so that no test depends on RNG
# state and the two quantile pairs of the measured rule differ.
probe_draws <- function(shift = 0, n = 400) {
  z <- stats::qnorm(stats::ppoints(n))
  shift + z[order(sin(seq_along(z)))]
}

# ---- contract ----------------------------------------------------------

test_that("apa_tidy() on a brmshypothesis returns the hypotheses contract", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))
  out <- apa_tidy(h)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "hypotheses")
  expect_true(all(
    names(tidy_contracts()$hypotheses$columns) %in% names(out)
  ))
  expect_identical(nrow(out), nrow(h$hypothesis))
  expect_identical(attr(out, "source_class"), "brmshypothesis")
  expect_identical(attr(out, "ci_method"), "eti")
  expect_identical(attr(out, "centrality"), "mean")
  expect_true(all(
    c("brms", "apabayes") %in% names(attr(out, "package_versions"))
  ))
  expect_identical(
    unname(attr(out, "package_versions")[["brms"]]),
    as.character(utils::packageVersion("brms"))
  )
})

test_that("apa_tidy() on a brmshypothesis returns visibly", {
  h <- brms::hypothesis(test_brms_fit("full"), "wt < 0")
  expect_true(withVisible(apa_tidy(h))$visible)
})

test_that("the columns are brms's own numbers, renamed", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))
  out <- apa_tidy(h)

  expect_identical(out$hypothesis, h$hypothesis$Hypothesis)
  expect_identical(out$estimate, h$hypothesis$Estimate)
  expect_identical(out$ci_low, h$hypothesis$CI.Lower)
  expect_identical(out$ci_high, h$hypothesis$CI.Upper)
  expect_identical(out$evid_ratio, h$hypothesis$Evid.Ratio)
  expect_identical(out$post_prob, h$hypothesis$Post.Prob)
  expect_true(all(is.na(out$group)))
})

test_that("Star and Est.Error reach no column", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))
  out <- apa_tidy(h)

  holds <- function(value) {
    any(vapply(
      out, function(col) any(col %in% value, na.rm = TRUE), logical(1)
    ))
  }
  expect_false(holds(h$hypothesis$Est.Error))
  expect_false(holds("*"))
})

# ---- directional, against brms's own operator --------------------------

test_that("directional is derived to agree with brms's operator", {
  h <- brms::hypothesis(
    test_brms_fit("full"),
    c("wt < 0", "am = 0", "wt + am > 0", "wt = am")
  )
  out <- apa_tidy(h)

  ops <- op_of(h)
  expect_false(anyNA(ops))
  expect_identical(out$directional, ops %in% c("<", ">"))
})

test_that("a named hypothesis, whose operator is not kept, is classified", {
  fit <- test_brms_fit("full")
  named <- brms::hypothesis(fit, c(weight = "wt < 0", trans = "am = 0"))
  unnamed <- brms::hypothesis(fit, c("wt < 0", "am = 0"))

  expect_true(all(is.na(op_of(named))))
  expect_identical(apa_tidy(named)$directional, apa_tidy(unnamed)$directional)
  expect_identical(apa_tidy(named)$directional, c(TRUE, FALSE))
})

test_that("a name that contains an operator does not make a row directional", {
  h <- brms::hypothesis(test_brms_fit("full"), c("a < b" = "wt = 0"))

  expect_identical(h$hypothesis$Hypothesis, "a < b")
  expect_false(apa_tidy(h)$directional)
})

test_that("directional = overrides the derivation", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))

  expect_identical(apa_tidy(h, directional = TRUE)$directional, c(TRUE, TRUE))
  expect_identical(
    apa_tidy(h, directional = c(FALSE, TRUE))$directional,
    c(FALSE, TRUE)
  )
  # bf10 follows the value it was given, not the object.
  expect_identical(
    apa_tidy(h, directional = TRUE)$bf10,
    h$hypothesis$Evid.Ratio
  )
})

test_that("directional = is checked", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))

  expect_error(apa_tidy(h, directional = "yes"), "must be a logical")
  expect_error(apa_tidy(h, directional = c(TRUE, NA)), "missing value")
  expect_error(apa_tidy(h, directional = c(TRUE, TRUE, TRUE)), "length")
  expect_error(apa_tidy(h, directional = logical()), "length")
})

test_that("a row whose kind cannot be derived is refused, and rescued", {
  const <- fake_hypothesis(
    list(probe_draws(), rep(1, 200)),
    directional = c(TRUE, FALSE)
  )

  expect_error(apa_tidy(const), "directional")
  expect_error(apa_tidy(const), "row 2")
  out <- apa_tidy(const, directional = c(TRUE, FALSE))
  expect_identical(out$directional, c(TRUE, FALSE))
})

test_that("a constant hypothesis from a real fit is the ambiguous case", {
  h <- suppressWarnings(
    brms::hypothesis(test_brms_fit("full"), "wt - wt = 0")
  )

  expect_length(unique(h$samples[[1]]), 1L)
  expect_error(apa_tidy(h), "directional")
})

# ---- the interval ------------------------------------------------------

test_that("the interval is the equal-tailed interval brms computed", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))
  out <- apa_tidy(h)

  expect_identical(out$ci_method, rep("eti", 2))
  expect_identical(
    c(out$ci_low[1], out$ci_high[1]),
    unname(stats::quantile(h$samples[[1]], c(h$alpha, 1 - h$alpha)))
  )
  expect_identical(
    c(out$ci_low[2], out$ci_high[2]),
    unname(stats::quantile(h$samples[[2]], c(h$alpha / 2, 1 - h$alpha / 2)))
  )
})

test_that("ci_level is 1 - alpha on a point row, 1 - 2 alpha directional", {
  fit <- test_brms_fit("full")

  for (a in c(0.05, 0.10)) {
    out <- apa_tidy(brms::hypothesis(fit, c("wt < 0", "am = 0"), alpha = a))
    expect_equal(out$ci_level, c(1 - 2 * a, 1 - a))
    # The rows disagree, so the object cannot claim one level.
    expect_identical(attr(out, "ci_level"), NA_real_)
  }
})

test_that("the ci_level attribute is the row value when the rows agree", {
  fit <- test_brms_fit("full")

  level_of <- function(...) {
    attr(apa_tidy(brms::hypothesis(fit, ...)), "ci_level")
  }

  expect_identical(level_of("am = 0"), 0.95)
  expect_identical(level_of("wt < 0"), 0.90)
  expect_identical(level_of(c("wt < 0", "wt + am > 0")), 0.90)
})

test_that("an alpha that makes a directional interval impossible is refused", {
  fit <- test_brms_fit("full")

  expect_error(
    apa_tidy(brms::hypothesis(fit, "wt < 0", alpha = 0.6)),
    "alpha"
  )
  expect_error(
    apa_tidy(brms::hypothesis(fit, "wt < 0", alpha = 0.5)),
    "alpha"
  )
  # The same alpha is harmless when no row is directional.
  point <- apa_tidy(brms::hypothesis(fit, "am = 0", alpha = 0.6))
  expect_equal(point$ci_level, 0.4)
})

# ---- the Bayes factor --------------------------------------------------

test_that("bf10 inverts the evidence ratio for a point hypothesis only", {
  h <- brms::hypothesis(test_brms_fit("full"), c("wt < 0", "am = 0"))
  out <- apa_tidy(h)

  expect_identical(out$bf10[1], h$hypothesis$Evid.Ratio[1])
  expect_identical(out$bf10[2], 1 / h$hypothesis$Evid.Ratio[2])
})

test_that("an infinite evidence ratio survives as an infinite bf10", {
  h <- brms::hypothesis(test_brms_fit("full"), "wt < 100")
  out <- apa_tidy(h)

  expect_true(is.infinite(out$evid_ratio))
  expect_true(is.infinite(out$bf10))
})

test_that("bf_method names the method behind each kind of row", {
  fit <- test_brms_fit("full")

  expect_identical(
    attr(apa_tidy(brms::hypothesis(fit, "am = 0")), "bf_method"),
    "Savage-Dickey density ratio"
  )
  expect_identical(
    attr(apa_tidy(brms::hypothesis(fit, "wt < 0")), "bf_method"),
    "posterior odds"
  )
  expect_identical(
    attr(apa_tidy(brms::hypothesis(fit, c("wt < 0", "am = 0"))), "bf_method"),
    c("Savage-Dickey density ratio", "posterior odds")
  )
})

test_that("a point hypothesis without prior draws reports NA, and no more", {
  # The fit is forced first, and its own warnings suppressed: at this
  # size brms warns about divergent transitions and about ESS, and
  # neither is what this test is about.
  fit <- suppressWarnings(test_brms_fit("mixed"))
  h <- expect_no_warning(brms::hypothesis(fit, "wt = 0"))
  out <- expect_no_warning(apa_tidy(h))

  expect_true(is.na(out$evid_ratio))
  expect_true(is.na(out$post_prob))
  expect_true(is.na(out$bf10))
  expect_false(out$directional)
  expect_false(is.na(out$estimate))
  expect_false(is.na(out$ci_low))
})

# ---- centrality --------------------------------------------------------

test_that("centrality is read off the estimate, not assumed", {
  fit <- test_brms_fit("full")
  plain <- brms::hypothesis(fit, c("wt < 0", "am = 0"))
  robust <- brms::hypothesis(fit, c("wt < 0", "am = 0"), robust = TRUE)

  expect_identical(attr(apa_tidy(plain), "centrality"), "mean")
  expect_identical(attr(apa_tidy(robust), "centrality"), "median")

  expect_identical(
    apa_tidy(plain)$estimate,
    vapply(plain$samples, mean, numeric(1), USE.NAMES = FALSE)
  )
  expect_identical(
    apa_tidy(robust)$estimate,
    vapply(robust$samples, stats::median, numeric(1), USE.NAMES = FALSE)
  )
  # robust = TRUE moves the estimate but not the interval (measured).
  expect_identical(apa_tidy(plain)$ci_low, apa_tidy(robust)$ci_low)
})

test_that("an estimate that is neither mean nor median leaves centrality NA", {
  fake <- fake_hypothesis(list(probe_draws()), directional = FALSE)
  fake$hypothesis$Estimate <- fake$hypothesis$Estimate + 1

  expect_identical(attr(apa_tidy(fake), "centrality"), NA_character_)
})

test_that("a sample whose mean and median coincide is scored as brms does", {
  fake <- fake_hypothesis(list(rep(2, 100)), directional = FALSE)
  out <- apa_tidy(fake, directional = FALSE)

  expect_identical(attr(out, "centrality"), "mean")
})

# ---- the grouped object ------------------------------------------------

test_that("scope = 'coef' fills the group column", {
  h <- brms::hypothesis(
    test_brms_fit("mixed"), c(a = "Intercept > 30", b = "Intercept < 40"),
    group = "cyl_f", scope = "coef"
  )
  out <- apa_tidy(h)

  expect_identical(nrow(out), nrow(h$hypothesis))
  expect_identical(out$group, as.character(h$hypothesis$Group))
  expect_identical(out$hypothesis, h$hypothesis$Hypothesis)
  expect_true(all(out$directional))
  # `$samples` column i belongs to row i (measured); the estimates say so.
  expect_identical(
    out$estimate,
    vapply(h$samples, mean, numeric(1), USE.NAMES = FALSE)
  )
})

# ---- the object, not the package ---------------------------------------

test_that("the route reads the object and calls no brms function", {
  fake <- fake_hypothesis(
    list(probe_draws(), probe_draws(1)),
    directional = c(TRUE, FALSE)
  )
  # Nothing here came from brms, and nothing in brms is called on it: the
  # route reads list elements. brms must still be *installed*, because
  # `package_versions` reads its version, and the guard says so.
  asked <- character()
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) {
      asked <<- c(asked, pkg)
      invisible(TRUE)
    },
    .package = "rlang"
  )
  out <- apa_tidy(fake)

  expect_identical(asked, "brms")
  expect_identical(nrow(out), 2L)
  expect_identical(out$directional, c(TRUE, FALSE))
  expect_identical(out$estimate, fake$hypothesis$Estimate)
  expect_identical(
    unname(attr(out, "package_versions")[["brms"]]),
    as.character(utils::packageVersion("brms"))
  )
})

test_that("a hypothesis computed from a data frame of draws is reported too", {
  fit <- test_brms_fit("full")
  draws <- as.data.frame(brms::as_draws_df(fit))[, c("b_wt", "b_am")]
  h <- brms::hypothesis(draws, "b_wt < 0")
  out <- apa_tidy(h)

  expect_identical(out$hypothesis, "(b_wt) < 0")
  expect_true(out$directional)
  expect_identical(out$estimate, mean(h$samples[[1]]))
})

# ---- malformed input ---------------------------------------------------

test_that("an object that is not a brmshypothesis is refused by name", {
  fake <- fake_hypothesis(list(probe_draws()), directional = FALSE)

  for (missing in c("hypothesis", "samples", "alpha")) {
    broken <- fake
    broken[[missing]] <- NULL
    expect_error(apa_tidy(broken), missing)
  }
  empty <- fake
  empty$hypothesis <- empty$hypothesis[0, ]
  expect_error(apa_tidy(empty), "no hypothes")
})

test_that("a samples frame that does not match the rows is refused", {
  fake <- fake_hypothesis(
    list(probe_draws(), probe_draws(1)),
    directional = c(TRUE, FALSE)
  )
  fake$samples <- fake$samples[, 1, drop = FALSE]

  expect_error(apa_tidy(fake), "column")
})

test_that("a hypothesis table missing a column is refused by name", {
  fake <- fake_hypothesis(list(probe_draws()), directional = FALSE)
  fake$hypothesis$Evid.Ratio <- NULL

  expect_error(apa_tidy(fake), "Evid\\.Ratio")
})

test_that("an alpha outside (0, 1) is refused by name, however it is reached", {
  fake <- fake_hypothesis(list(probe_draws()), directional = FALSE)
  bad <- function(alpha) {
    out <- fake
    out$alpha <- alpha
    out
  }
  # Checked before the derivation, which would otherwise hand the value
  # to `stats::quantile()` and abort with `'probs' outside [0,1]`, naming
  # nothing the user typed. Both the derived and the supplied
  # `directional` path must reach the same message.
  expect_error(apa_tidy(bad(-0.1)), "alpha")
  expect_error(apa_tidy(bad(-0.1), directional = FALSE), "alpha")
  expect_error(apa_tidy(bad(1.5)), "alpha")
  expect_error(apa_tidy(bad(1)), "alpha")
  expect_error(apa_tidy(bad(0)), "alpha")
  expect_error(apa_tidy(bad(c(0.05, 0.10))), "single number")
  expect_error(apa_tidy(bad(NA_real_)), "single number")
})
