# Tests for the draws route (dev/specs/spec-apa_tidy_draws.md,
# ARCHITECTURE.md decision 18). Written before R/extract-draws.R.
#
# Expected values are computed from the fixtures with bayestestR and
# posterior, never typed: the contract is "apabayes selects, arranges and
# formats", so a test that hard-coded a number would be testing
# easystats, not apabayes.

skip_if_no_draws <- function() {
  testthat::skip_if_not_installed("posterior")
  testthat::skip_if_not_installed("bayestestR")
}

test_that("the draws method returns the tidy contract", {
  skip_if_no_draws()
  out <- apa_tidy(fixture("draws_brms"))

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "parameters")
  expect_true(all(
    c("term", "label", "estimate", "ci_low", "ci_high", "pd", "rhat") %in%
      names(out)
  ))
})

test_that("the draws method returns its table visibly", {
  skip_if_no_draws()
  # The route ends in the constructor, so it inherits whatever visibility
  # the constructor has; `apa_tidy(draws)` must print at the console.
  expect_true(withVisible(apa_tidy(fixture("draws_brms")))$visible)
})

test_that("the default variable rule drops lp__, lprior and prior_*", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  expect_true(all(
    c("prior_b", "lprior", "lp__") %in% posterior::variables(draws)
  ))
  expect_identical(
    apa_tidy(draws)$term,
    c("b_Intercept", "b_wt", "b_am", "sigma", "Intercept")
  )
})

test_that("variables selects, orders and accepts internal names", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  expect_identical(
    apa_tidy(draws, variables = c("sigma", "b_wt"))$term,
    c("sigma", "b_wt")
  )
  expect_identical(apa_tidy(draws, variables = "lp__")$term, "lp__")
  expect_identical(nrow(apa_tidy(draws, variables = "b_wt")), 1L)
})

test_that("estimate is the median or the mean of the draws", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  expect_equal(
    apa_tidy(draws, variables = c("b_wt", "sigma"))$estimate,
    c(stats::median(draws$b_wt), stats::median(draws$sigma))
  )
  expect_equal(
    apa_tidy(draws, variables = "b_wt", centrality = "mean")$estimate,
    mean(draws$b_wt)
  )
  expect_identical(
    attr(apa_tidy(draws, centrality = "mean"), "centrality"), "mean"
  )
})

test_that("the interval is the one bayestestR computes, at the level asked", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  eti <- bayestestR::eti(draws$b_wt, ci = 0.95)
  out <- apa_tidy(draws, variables = "b_wt")
  expect_equal(out$ci_low, eti$CI_low)
  expect_equal(out$ci_high, eti$CI_high)
  expect_identical(out$ci_method, "eti")
  expect_identical(out$ci_level, 0.95)

  hdi <- bayestestR::hdi(draws$b_wt, ci = 0.9)
  out90 <- apa_tidy(draws, variables = "b_wt", ci = "hdi", ci_level = 0.9)
  expect_equal(out90$ci_low, hdi$CI_low)
  expect_equal(out90$ci_high, hdi$CI_high)
  expect_identical(out90$ci_method, "hdi")
  expect_identical(attr(out90, "ci_method"), "hdi")
  expect_identical(attr(out90, "ci_level"), 0.9)
})

test_that("pd is bayestestR's p_direction", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  expect_equal(
    apa_tidy(draws, variables = "b_am")$pd,
    as.numeric(bayestestR::p_direction(draws$b_am)$pd)
  )
})

test_that("ROPE is opt-in and carries its bounds in the attributes", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  off <- apa_tidy(draws, variables = c("b_wt", "b_am"))
  expect_identical(off$rope_pct, c(NA_real_, NA_real_))
  expect_null(attr(off, "rope_range"))

  on <- apa_tidy(draws, variables = "b_am", rope = c(-0.1, 0.1), rope_ci = 1)
  expect_equal(
    on$rope_pct,
    as.numeric(bayestestR::rope(
      draws$b_am,
      range = c(-0.1, 0.1), ci = 1
    )$ROPE_Percentage)
  )
  expect_identical(attr(on, "rope_range"), c(-0.1, 0.1))
  expect_identical(attr(on, "rope_ci"), 1)
})

test_that("diagnostics come from summarise_draws and can be turned off", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  expected <- posterior::summarise_draws(
    posterior::subset_draws(draws, variable = c("b_wt", "sigma")),
    "rhat", "ess_bulk", "ess_tail"
  )
  out <- apa_tidy(draws, variables = c("b_wt", "sigma"))
  expect_equal(out$rhat, expected$rhat)
  expect_equal(out$ess_bulk, expected$ess_bulk)
  expect_equal(out$ess_tail, expected$ess_tail)

  off <- apa_tidy(draws, variables = c("b_wt", "sigma"), diagnostics = FALSE)
  expect_identical(off$rhat, c(NA_real_, NA_real_))
  expect_identical(off$ess_bulk, c(NA_real_, NA_real_))
})

test_that("labels map by name and fall back to term", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  out <- apa_tidy(
    draws,
    variables = c("b_wt", "b_am"),
    labels = c(b_wt = "Weight")
  )
  expect_identical(out$label, c("Weight", "b_am"))
  expect_identical(out$term, c("b_wt", "b_am"))
})

test_that("columns a draws object cannot fill are typed NA", {
  skip_if_no_draws()
  out <- apa_tidy(fixture("draws_brms"), variables = "b_wt")

  expect_identical(out$bf, NA_real_)
  expect_identical(out$component, NA_character_)
  expect_identical(out$group, NA_character_)
  expect_identical(out$effects, NA_character_)
  expect_identical(out$std, NA)
  expect_identical(out$p, NA_real_)
})

test_that("the attributes record where the numbers came from", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")
  out <- apa_tidy(draws)

  expect_identical(attr(out, "source_class"), class(draws))
  versions <- attr(out, "package_versions")
  expect_true(
    all(c("posterior", "bayestestR", "apabayes") %in% names(versions))
  )
  expect_type(versions, "character")
  expect_identical(
    versions[["posterior"]],
    as.character(utils::packageVersion("posterior"))
  )
})

test_that("a BayesFactor posterior keeps its variable names verbatim", {
  skip_if_no_draws()
  post <- fixture("draws_bf")

  out <- apa_tidy(post)
  expect_identical(
    out$term,
    c("mu", "beta (x - y)", "sig2", "delta", "g")
  )
  expect_identical(attr(out, "source_class"), "mcmc")
  expect_equal(
    out$estimate[[2]],
    stats::median(as.matrix(post)[, "beta (x - y)"])
  )
  expect_false(anyNA(out$rhat))
})

test_that("a JAGS mcmc.list goes through the default method", {
  skip_if_no_draws()
  ml <- fixture("draws_jags")

  out <- apa_tidy(ml)
  expect_identical(out$term, c("mu", "sigma"))
  expect_identical(attr(out, "source_class"), "mcmc.list")
  expect_equal(
    out$estimate,
    unname(apply(as.matrix(posterior::as_draws_matrix(ml)), 2, stats::median))
  )
  expect_false(anyNA(out$rhat))
})

test_that("coercion adds nothing: a live stanfit equals its draws_df", {
  skip_if_no_draws()
  fit <- test_brms_fit("full")

  from_fit <- apa_tidy(fit$fit)
  from_draws <- apa_tidy(posterior::as_draws_df(fit$fit))

  expect_equal(from_fit$estimate, from_draws$estimate)
  expect_identical(from_fit$term, from_draws$term)
  expect_identical(attr(from_fit, "source_class"), "stanfit")
})

test_that("the runjags method reads $mcmc", {
  skip_if_no_draws()
  skip_if_not_installed("runjags")
  skip_on_cran()
  # An installed runjags is not an installed JAGS. The binary is taken
  # from the PATH wherever there is one and only falls back to the
  # Homebrew location this package is developed against; pointing
  # `jagspath` at that path unconditionally would fail on any machine
  # that has JAGS somewhere else.
  jags <- Sys.which("jags")
  if (!nzchar(jags) && file.exists("/opt/homebrew/bin/jags")) {
    jags <- "/opt/homebrew/bin/jags"
  }
  skip_if(!nzchar(jags), "JAGS binary not available")
  runjags::runjags.options(
    jagspath = unname(jags),
    silent.jags = TRUE, silent.runjags = TRUE
  )
  rj <- skip_if_cannot_build("runjags", "normal mean", suppressWarnings(
    runjags::run.jags(
      "model {
         for (i in 1:N) { y[i] ~ dnorm(mu, tau) }
         mu ~ dnorm(0, 0.001)
         tau ~ dgamma(0.01, 0.01)
       }",
      monitor = "mu",
      data = list(y = mtcars$mpg, N = nrow(mtcars)),
      n.chains = 2, burnin = 200, sample = 500,
      inits = list(list(mu = 20, tau = 0.05), list(mu = 15, tau = 0.02)),
      method = "simple"
    )
  ))

  out <- apa_tidy(rj)
  expect_identical(out$term, "mu")
  expect_identical(attr(out, "source_class"), "runjags")
  expect_equal(out$estimate, apa_tidy(rj$mcmc)$estimate)
})

test_that("bad input aborts with the class or the value named", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  expect_error(apa_tidy(stats::lm(mpg ~ wt, mtcars)), "lm")
  expect_error(apa_tidy(draws, variables = "nope"), "nope")
  expect_error(apa_tidy(draws, variables = "nope"), "b_wt")
  expect_error(apa_tidy(draws, variables = 1), "character")
  expect_error(apa_tidy(draws, labels = "Weight"), "named")
  expect_error(apa_tidy(draws, labels = c(nope = "Weight")), "nope")
  expect_error(apa_tidy(draws, rope = 0.1), "two numbers")
  expect_error(apa_tidy(draws, rope = c(0.1, -0.1)), "two numbers")
  expect_error(apa_tidy(draws, rope = c(-0.1, 0.1), rope_ci = 0), "rope_ci")
  expect_error(apa_tidy(draws, ci_level = 1), "ci_level")
  expect_error(apa_tidy(draws, ci_level = 0), "ci_level")
  expect_error(apa_tidy(draws, centrality = "mode"), "centrality")
  expect_error(apa_tidy(draws, ci = "quantile"), "ci")
})

test_that("a plain data frame of numeric draws is still read as draws", {
  skip_if_no_draws()
  df <- data.frame(mu = c(0.1, 0.4, -0.2, 0.3), sigma = c(1, 1.2, 0.9, 1.1))

  expected <- apa_tidy(posterior::as_draws_df(df), diagnostics = FALSE)
  out <- apa_tidy(df, diagnostics = FALSE)
  expect_identical(out$estimate, expected$estimate)
  expect_identical(attr(out, "source_class"), "data.frame")
  tbl <- structure(df, class = c("tbl_df", "tbl", "data.frame"))
  expect_identical(apa_tidy(tbl, diagnostics = FALSE)$ci_low, expected$ci_low)
  # A data.table and a grouped tibble of draws convert as their plain data
  # frame does (measured with data.table and dplyr, session 23); the
  # class vectors are built here so the tests need neither package.
  dt <- structure(df, class = c("data.table", "data.frame"))
  expect_identical(apa_tidy(dt, diagnostics = FALSE)$ci_low, expected$ci_low)
  grouped <- structure(
    df,
    class = c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
  expect_identical(
    apa_tidy(grouped, diagnostics = FALSE)$estimate,
    expected$estimate
  )
})

test_that("a data frame that is not draws is refused, not coerced", {
  skip_if_no_draws()
  # Measured (session 23): posterior::as_draws_df() accepts character,
  # factor and logical columns and any table class, so a summary table
  # became a nonsense parameters table with no error.
  df <- data.frame(mu = c(0.1, 0.4), sigma = c(1, 1.2))

  # A summary class with no route of its own (modelbased's classes have
  # one since session 23, bayesfactor_inclusion since session 25).
  summary_table <- structure(
    df,
    class = c("compare_performance", "see_compare_performance", "data.frame")
  )
  expect_error(apa_tidy(summary_table), "compare_performance")
  expect_error(apa_tidy(summary_table), "not draws", fixed = TRUE)

  labelled <- data.frame(mu = c(0.1, 0.4), term = c("a", "b"))
  expect_error(apa_tidy(labelled), "term")
  factored <- data.frame(mu = c(0.1, 0.4), g = factor(c("a", "b")))
  expect_error(apa_tidy(factored), "g")
  flagged <- data.frame(mu = c(0.1, 0.4), ok = c(TRUE, FALSE))
  expect_error(apa_tidy(flagged), "ok")
  expect_error(apa_tidy(flagged), "numeric", fixed = TRUE)
})

test_that("a summary matrix too short to be draws is refused, not coerced", {
  skip_if_no_draws()
  # Finding 1 of the miniQmetrics acceptance test
  # (local/findings-2026-09-16.md, decided by Gidon 2026-09-16): a plain
  # matrix carries no class to refuse on, unlike the data-frame case
  # above, so a 2 x 6 matrix of per-model max-Rhat/min-ESS summaries was
  # silently read as two draws of six variables. The estimate it produced
  # was the mean of an R-hat and an ESS: confident and meaningless.
  conv <- matrix(
    c(1.0014, 2578.1, 1.002, 3000, 1.001, 2900),
    nrow = 2, dimnames = list(c("max_rhat", "min_ess"), paste0("m", 1:3))
  )
  expect_error(apa_tidy(conv), "too few draws")
})

test_that("posterior is required, not assumed", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  local_mocked_bindings(
    check_installed = function(...) {
      cli::cli_abort("posterior is not installed.")
    },
    .package = "rlang"
  )
  expect_error(apa_tidy(draws), "posterior")
})

test_that("the runjags method reads $mcmc without a JAGS binary", {
  skip_if_no_draws()
  ml <- fixture("draws_jags")
  fake <- structure(list(mcmc = ml), class = "runjags")

  out <- apa_tidy(fake)
  expect_identical(out$term, c("mu", "sigma"))
  expect_identical(attr(out, "source_class"), "runjags")
  expect_equal(out$estimate, apa_tidy(ml)$estimate)
})

test_that("a selection with nothing left to report aborts", {
  skip_if_no_draws()
  draws <- fixture("draws_brms")

  internal <- posterior::as_draws_df(
    data.frame(lp__ = as.double(seq_len(20)))
  )
  expect_error(apa_tidy(internal), "internal")
  expect_error(apa_tidy(draws, variables = character()), "no draws variable")
  expect_error(
    resolve_draws_variables(NULL, character()),
    "no draws variable"
  )
})
