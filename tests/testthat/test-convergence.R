# Tests for apa_convergence() (local/specs/spec-apa_convergence.md).
#
# The sentence is tested on hand-built diagnostics tables, so that every
# branch runs on CRAN and each value is chosen to sit on the rule under
# test. Rules are asserted on the plain target, where the string is
# ASCII and can be read in the test; the other targets are composed from
# the format layer. The default method is tested on live fits at the end.

# A diagnostics table from vectors, with an optional divergences
# attribute (absent when NULL, as on a table stored before slice 2).
diag_table <- function(rhat, ess_bulk, ess_tail = ess_bulk,
                       divergences = NULL) {
  extra <- if (is.null(divergences)) list() else list(divergences = divergences)
  rlang::exec(
    apabayes_tidy,
    data.frame(
      term = paste0("v", seq_along(rhat)), rhat = rhat,
      ess_bulk = ess_bulk, ess_tail = ess_tail
    ),
    type = "diagnostics", centrality = NA_character_,
    ci_method = NA_character_, ci_level = NA_real_, !!!extra
  )
}

plain <- function(x, ...) apa_convergence(x, ..., markup = "plain")$full_result

# ---- the passing sentence ------------------------------------------------

test_that("a passing table states the extremes as bounds", {
  t <- diag_table(
    c(1.0012, 1.0036), c(1234.2, 2000), c(980.7, 1500),
    divergences = 0L
  )
  expect_identical(
    plain(t),
    paste(
      "Rhat <= 1.004, bulk ESS >= 1,234, tail ESS >= 980,",
      "no divergent transitions"
    )
  )
})

test_that("the bound is rounded away from the data, never past it", {
  # The m3 seed's sprintf("%.3f") prints 1.003 for 1.0034, and
  # "R-hat <= 1.003" is then false; the bound rounds up instead.
  expect_match(plain(diag_table(1.0034, 500)), "Rhat <= 1.004", fixed = TRUE)
  # A value already at the precision stays where it is.
  expect_match(plain(diag_table(1.004, 500)), "Rhat <= 1.004", fixed = TRUE)
  expect_match(plain(diag_table(0.9996, 500)), "Rhat <= 1.000", fixed = TRUE)
  expect_match(
    plain(diag_table(1.001, 1240.9)), "bulk ESS >= 1,240",
    fixed = TRUE
  )
  expect_match(
    plain(diag_table(1.001, 500, 512)), "tail ESS >= 512",
    fixed = TRUE
  )
  expect_match(
    plain(diag_table(1.0034, 500), digits = 2), "Rhat <= 1.01",
    fixed = TRUE
  )
})

test_that("the m3 seed's numbers appear where its rounding is a bound", {
  # tutorial-m3-bmm.qmd: `$\hat{R} \leq$ val_rhat, bulk ESS $>$ val_ess`,
  # with val_rhat = sprintf("%.3f", max) and val_ess = the rounded
  # minimum. For these values rounding and bounding agree.
  rhat <- c(1.0011, 1.0036)
  ess <- c(1234.2, 4000)
  val_rhat <- sprintf("%.3f", max(rhat))
  val_ess <- format(round(min(ess)), big.mark = ",")
  out <- apa_convergence(
    diag_table(rhat, ess, divergences = 0L),
    markup = "latex"
  )$full_result
  expect_identical(
    out,
    paste0(
      "$\\hat{R}$ $\\leq$ ", val_rhat, ", bulk ESS $\\geq$ ", val_ess,
      ", tail ESS $\\geq$ ", val_ess, ", no divergent transitions"
    )
  )
})

test_that("every markup target is honoured", {
  t <- diag_table(c(1.0012, 1.0036), c(1234.2, 2000), divergences = 0L)
  for (target in c("md", "latex", "typst")) {
    rhat <- stat_string(
      symbol("rhat", target),
      paste(symbol("leq", target), "1.004")
    )
    expected <- paste0(
      rhat, ", bulk ESS ", symbol("geq", target), " 1,234, tail ESS ",
      symbol("geq", target), " 1,234, no divergent transitions"
    )
    expect_identical(
      apa_convergence(t, markup = target)$full_result, expected,
      info = target
    )
  }
})

# ---- violations ------------------------------------------------------------

test_that("an R-hat at or above the threshold is counted, with its maximum", {
  t <- diag_table(c(1.001, 1.0100, 1.0184, 1.002), rep(900, 4))
  out <- apa_convergence(t, markup = "plain")
  expect_identical(
    out$full_result,
    "2 of 4 Rhat >= 1.01, maximum 1.018, bulk ESS >= 900, tail ESS >= 900"
  )
  expect_false(out$passed)
})

test_that("an ESS at or below the threshold is counted and its minimum given", {
  t <- diag_table(rep(1.001, 3), c(400, 152.6, 2000), c(900, 850, 1000))
  out <- apa_convergence(t, markup = "plain")
  expect_identical(
    out$full_result,
    "Rhat <= 1.001, 2 of 3 bulk ESS <= 400, minimum 153, tail ESS >= 850"
  )
  expect_false(out$passed)
  tail <- diag_table(rep(1.001, 2), c(900, 900), c(399, 1000))
  expect_match(plain(tail), "1 of 2 tail ESS <= 400, minimum 399", fixed = TRUE)
})

test_that("the thresholds are arguments", {
  t <- diag_table(c(1.004, 1.02), c(600, 800))
  expect_match(plain(t, rhat = 1.05), "Rhat <= 1.020", fixed = TRUE)
  expect_match(
    plain(t, ess = 700), "1 of 2 bulk ESS <= 700, minimum 600",
    fixed = TRUE
  )
  expect_true(apa_convergence(t, rhat = 1.05, ess = 100)$passed)
})

test_that("a threshold prints at its own precision, whatever digits is", {
  t <- diag_table(c(1.02, 1.03), c(900, 900))
  expect_match(
    plain(t, digits = 0), "2 of 2 Rhat >= 1.01, maximum 1,",
    fixed = TRUE
  )
  expect_match(
    plain(t, rhat = 1.005, digits = 2), "Rhat >= 1.005, maximum 1.03",
    fixed = TRUE
  )
})

test_that("divergences are counted, with the singular for one", {
  t <- function(k) diag_table(1.001, 900, divergences = k)
  expect_match(plain(t(0L)), ", no divergent transitions$")
  expect_match(plain(t(1L)), ", 1 divergent transition$")
  expect_match(plain(t(1234L)), ", 1,234 divergent transitions$")
  expect_true(apa_convergence(t(0L))$passed)
  expect_false(apa_convergence(t(1L))$passed)
})

test_that("unknown divergences print nothing and do not decide passed", {
  absent <- diag_table(1.001, 900)
  unknown <- diag_table(1.001, 900, divergences = NA_integer_)
  for (t in list(absent, unknown)) {
    out <- apa_convergence(t, markup = "plain")
    expect_false(grepl("divergent", out$full_result))
    expect_true(out$passed)
    expect_identical(out$summary$divergences, NA_integer_)
  }
})

test_that("NA diagnostics are left out of the counts and the extremes", {
  t <- diag_table(c(1.001, NA, 1.05), c(900, NA, 950))
  out <- apa_convergence(t, markup = "plain")
  expect_match(
    out$full_result, "1 of 2 Rhat >= 1.01, maximum 1.050",
    fixed = TRUE
  )
  expect_identical(out$summary$variables, 3L)
  expect_identical(out$summary$rhat_n, 2L)
  all_na <- diag_table(c(NA_real_, NA_real_), c(900, 950))
  expect_identical(plain(all_na), "bulk ESS >= 900, tail ESS >= 900")
})

# ---- the object ------------------------------------------------------------

test_that("the result object carries the whole table, passed and a summary", {
  t <- diag_table(
    c(1.001, 1.0184), c(152.6, 2000), c(980, 400),
    divergences = 3L
  )
  out <- apa_convergence(t)
  expect_s3_class(out, "apabayes_results")
  expect_identical(out$estimate, NA_character_)
  expect_identical(out$statistic, out$full_result)
  expect_length(out$full_result, 1L)
  expect_identical(out$table, t)
  expect_false(out$passed)
  expect_identical(out$markup, "md")
  expect_true(withVisible(apa_convergence(t))$visible)

  s <- out$summary
  expect_identical(nrow(s), 1L)
  expect_named(s, c(
    "variables", "rhat_n", "rhat_max", "rhat_flagged", "ess_bulk_n",
    "ess_bulk_min", "ess_bulk_flagged", "ess_tail_n", "ess_tail_min",
    "ess_tail_flagged", "divergences", "rhat_threshold", "ess_threshold"
  ))
  expect_identical(s$variables, 2L)
  expect_identical(s$rhat_max, 1.0184)
  expect_identical(s$rhat_flagged, 1L)
  expect_identical(s$ess_bulk_min, 152.6)
  expect_identical(s$ess_bulk_flagged, 1L)
  expect_identical(s$ess_tail_min, 400)
  expect_identical(s$ess_tail_flagged, 1L)
  expect_identical(s$divergences, 3L)
  expect_identical(s$rhat_threshold, 1.01)
  expect_identical(s$ess_threshold, 400)
})

test_that("no decision word appears in any sentence", {
  strings <- c(
    plain(diag_table(c(1.001, 1.02), c(100, 900), divergences = 2L)),
    apa_convergence(diag_table(1.001, 900, divergences = 0L))$full_result,
    apa_convergence(fixture("diag_brms_full"))$full_result
  )
  for (word in c(decision_words, "converged", "good", "acceptable", "fail")) {
    expect_false(any(grepl(word, strings, ignore.case = TRUE)), info = word)
  }
})

test_that("a stored diagnostics fixture is reported", {
  d <- fixture("diag_brms_full")
  out <- apa_convergence(d, markup = "plain")
  expect_match(out$full_result, "^Rhat <= ")
  expect_identical(out$summary$variables, nrow(d))
})

# ---- refusals --------------------------------------------------------------

test_that("only a diagnostics table is reported", {
  expect_error(
    apa_convergence(fixture("tidy_brms_full")),
    "diagnostics"
  )
  expect_error(
    apa_convergence(fixture("tidy_brms_full")),
    "apa_tidy_diagnostics"
  )
  empty <- diag_table(1.001, 900)[0, ]
  expect_error(apa_convergence(empty), "no variables")
})

test_that("the arguments are validated", {
  t <- diag_table(1.001, 900)
  expect_error(apa_convergence(t, rhat = 0.99), "rhat")
  expect_error(apa_convergence(t, rhat = c(1.01, 1.05)), "rhat")
  expect_error(apa_convergence(t, rhat = NA), "rhat")
  expect_error(apa_convergence(t, ess = 0), "ess")
  expect_error(apa_convergence(t, ess = Inf), "ess")
  expect_error(apa_convergence(t, digits = -1), "digits")
  expect_error(apa_convergence(t, markup = "html"), "markup")
  expect_error(apa_convergence(t, variables = "v1"), "must be empty")
})

# ---- the default method ----------------------------------------------------

test_that("the default method reports the diagnostics of a draws object", {
  draws <- fixture("draws_brms")
  expect_identical(
    apa_convergence(draws)$full_result,
    apa_convergence(apa_tidy_diagnostics(draws))$full_result
  )
  out <- apa_convergence(draws, variables = "b_wt", markup = "plain")
  expect_identical(out$table$term, "b_wt")
  expect_false(grepl("divergent", out$full_result))
})

test_that("the default method fails with the diagnostics route's message", {
  expect_error(apa_convergence(stats::lm(mpg ~ wt, mtcars)), "posterior")
})

test_that("the default method reports a brms fit, divergences included", {
  fit <- test_brms_fit("full")
  out <- apa_convergence(fit, markup = "plain")
  expect_identical(
    out$full_result,
    apa_convergence(apa_tidy_diagnostics(fit), markup = "plain")$full_result
  )
  expect_match(out$full_result, "no divergent transitions$")
  expect_identical(out$summary$variables, nrow(apa_tidy_diagnostics(fit)))
})

test_that("the default method reports a blavaan fit and its divergences", {
  fit <- test_blavaan_fit("divergent")
  out <- apa_convergence(fit, markup = "plain")
  k <- attr(apa_tidy_diagnostics(fit), "divergences")
  expect_gt(k, 1L)
  expect_match(
    out$full_result,
    paste0(format(k, big.mark = ","), " divergent transitions$")
  )
  expect_false(out$passed)
})
