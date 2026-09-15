# Tests for the bayesfactor_models route
# (local/specs/spec-apa_tidy_bayesfactor_models.md).
#
# No expected number is typed: every one is computed from the object
# under test (`Model`, `log_BF`, its attributes). The lm trio runs
# everywhere; the brms pair is bridge sampling, computed once per run
# under a fixed RNG state (`test_bf_models_brms()`), off CRAN.

# An object with its log Bayes factors replaced, keeping the denominator
# row at 0 so the object stays one bayestestR could have made.
with_log_bf <- function(b, log_bf) {
  b$log_BF <- log_bf
  b
}

# ---- contract ------------------------------------------------------------

test_that("apa_tidy() on a bayesfactor_models returns the bf_models contract", {
  b <- test_bf_models_lm()
  out <- apa_tidy(b)
  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "bf_models")
  expect_identical(
    names(out),
    c(
      "model", "bf", "log_bf", "denominator", "method", "post_prob",
      "error", "name"
    )
  )
  expect_identical(out$error, rep(NA_real_, nrow(out)))
  expect_identical(out$model, b$Model)
  expect_identical(out$log_bf, b$log_BF)
  expect_identical(out$bf, exp(b$log_BF))
  expect_identical(out$name, rownames(b))
  expect_identical(
    out$denominator,
    seq_len(nrow(b)) == attr(b, "denominator")
  )
  expect_identical(out$method, rep(attr(b, "BF_method"), nrow(b)))
})

test_that("the attributes name the method, prior odds and denominator", {
  b <- test_bf_models_lm()
  out <- apa_tidy(b)
  expect_identical(attr(out, "bf_method"), attr(b, "BF_method"))
  expect_identical(attr(out, "prior_odds"), "equal")
  expect_identical(
    attr(out, "denominator_model"),
    b$Model[attr(b, "denominator")]
  )
  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(attr(out, "ci_method"), NA_character_)
  expect_identical(attr(out, "ci_level"), NA_real_)
  expect_identical(attr(out, "source_class"), class(b))
  expect_named(attr(out, "package_versions"), c("bayestestR", "apabayes"))
})

test_that("apa_tidy() on a bayesfactor_models returns visibly", {
  expect_true(withVisible(apa_tidy(test_bf_models_lm()))$visible)
})

# ---- posterior model probabilities ---------------------------------------

test_that("post_prob is the normalised Bayes factor at equal prior odds", {
  b <- test_bf_models_lm()
  out <- apa_tidy(b)
  expect_equal(out$post_prob, exp(b$log_BF) / sum(exp(b$log_BF)))
  expect_equal(sum(out$post_prob), 1)
})

test_that("post_prob does not depend on which model is the denominator", {
  b <- test_bf_models_lm()
  moved <- update(b, reference = 1)
  expect_equal(attr(moved, "denominator"), 1)
  out <- apa_tidy(moved)
  expect_equal(out$post_prob, apa_tidy(b)$post_prob)
  expect_identical(out$denominator, c(TRUE, FALSE, FALSE))
  expect_identical(out$log_bf, moved$log_BF)
})

test_that("post_prob stays finite where exp() of the log BF overflows", {
  b <- with_log_bf(test_bf_models_lm(), c(800, 790, 0))
  out <- apa_tidy(b)
  expect_identical(out$bf[1], Inf)
  expect_identical(out$log_bf[1], 800)
  expect_true(all(is.finite(out$post_prob)))
  expect_equal(out$post_prob[1:2], c(1, exp(-10)) / (1 + exp(-10)))
})

test_that("a missing log BF makes every post_prob missing", {
  b <- with_log_bf(test_bf_models_lm(), c(NA, 2, 0))
  out <- apa_tidy(b)
  expect_identical(out$bf[1], NA_real_)
  expect_identical(out$post_prob, rep(NA_real_, 3))
})

test_that("a log BF of -Inf is a probability of 0", {
  b <- with_log_bf(test_bf_models_lm(), c(-Inf, 2, 0))
  out <- apa_tidy(b)
  expect_identical(out$post_prob[1], 0)
  expect_equal(out$post_prob[2:3], exp(c(2, 0)) / sum(exp(c(2, 0))))
})

test_that("a log BF of +Inf leaves post_prob missing", {
  b <- with_log_bf(test_bf_models_lm(), c(Inf, 2, 0))
  expect_identical(apa_tidy(b)$post_prob, rep(NA_real_, 3))
})

# ---- what the route refuses ----------------------------------------------

test_that("a subset whose denominator index is stale is refused", {
  b <- test_bf_models_lm()
  expect_error(apa_tidy(b[1:2, ]), "subset or edited")
  expect_error(apa_tidy(b[c(3, 1, 2), ]), "subset or edited")
})

test_that("a missing or malformed denominator is refused", {
  b <- test_bf_models_lm()
  none <- b
  attr(none, "denominator") <- NULL
  expect_error(apa_tidy(none), "denominator")
  half <- b
  attr(half, "denominator") <- 1.5
  expect_error(apa_tidy(half), "denominator")
})

test_that("a missing method is refused", {
  b <- test_bf_models_lm()
  attr(b, "BF_method") <- NULL
  expect_error(apa_tidy(b), "BF_method")
})

test_that("missing columns and empty objects are refused", {
  b <- test_bf_models_lm()
  no_log <- b
  no_log$log_BF <- NULL
  expect_error(apa_tidy(no_log), "log_BF")
  text_log <- b
  text_log$log_BF <- as.character(text_log$log_BF)
  expect_error(apa_tidy(text_log), "numeric")
  expect_error(apa_tidy(b[0, ]), "no models")
})

test_that("an argument the route does not take is refused", {
  expect_error(
    apa_tidy(test_bf_models_lm(), post_prob = TRUE),
    class = "rlib_error_dots_nonempty"
  )
})

# ---- live fits -----------------------------------------------------------

test_that("the brms pair reports bridge sampling and the right-hand sides", {
  b <- test_bf_models_brms()
  out <- apa_tidy(b)
  expect_identical(out$model, b$Model)
  expect_identical(out$model, c("wt + am", "wt"))
  expect_identical(out$name, c("full", "reduced"))
  expect_identical(out$log_bf, b$log_BF)
  expect_identical(out$method, rep(attr(b, "BF_method"), 2))
  expect_match(attr(out, "bf_method"), "bridgesampling")
})
