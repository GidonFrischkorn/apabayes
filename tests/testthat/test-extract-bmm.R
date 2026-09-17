# Tests for bmm fits on the brmsfit route (spec-apa_tidy_brmsfit-upstream.md).
#
# A bmmfit is a brmsfit (class c("bmmfit", "brmsfit")) and reaches
# apa_tidy.brmsfit() without any bmm code in apabayes. Until session 34
# that rested on the class vector of a mock fit; these are the first
# fitted bmm objects the suite has held. The fits come from
# test_bmm_fit() in setup.R and skip without bmm and on CRAN.

test_that("a bmm M3 fit is a brmsfit and fails upstream as a multinomial fit", {
  fit <- test_bmm_fit("m3")

  expect_identical(class(fit), c("bmmfit", "brmsfit"))
  expect_identical(fit$family$family, "multinomial")
  expect_error(
    apa_tidy(fit), "apa_tidy(brms::as_draws_df(fit))",
    fixed = TRUE
  )
  expect_error(apa_tidy(fit), class = "rlang_error")
})

test_that("the draws route reports a bmm M3 fit under its brms names", {
  fit <- test_bmm_fit("m3")
  out <- apa_tidy(brms::as_draws_df(fit))

  expect_identical(attr(out, "type"), "parameters")
  # The M3 model parameters are brms distributional parameters, so the
  # term is b_<parameter>_<term>; nothing splits it (decision 249).
  expect_setequal(
    out$term, c("b_c_Intercept", "b_a_Intercept", "b_b_Intercept")
  )
  expect_true(all(is.na(out$component)))
  # `b` is fixed for scaling in every M3 (bmm's fixed_parameters): every
  # draw is 0, so it has an estimate and no R-hat. The free ones have both.
  free <- out$term != "b_b_Intercept"
  expect_false(anyNA(out$rhat[free]))
  expect_true(is.na(out$rhat[!free]))
  expect_identical(out$estimate[!free], 0)
})

test_that("diagnostics and the convergence statement work on a bmm M3 fit", {
  fit <- test_bmm_fit("m3")
  diag <- apa_tidy_diagnostics(fit)

  expect_identical(attr(diag, "type"), "diagnostics")
  expect_true(all(c("b_c_Intercept", "b_a_Intercept") %in% diag$term))
  # bmm samples through cmdstanr when it is installed and through rstan
  # otherwise; either way the sampler record exists and is counted.
  expect_type(attr(diag, "divergences"), "integer")
  expect_false(is.na(attr(diag, "divergences")))
  expect_no_error(apa_convergence(fit))
})

test_that("a bmm fit with a numeric response goes through the brmsfit route", {
  # Decision 249 as a test: the bmm model parameter is the brms
  # distributional parameter, and easystats already labels `Component`
  # with it, so `component` carries `c` and `kappa` with no bmm code.
  fit <- test_bmm_fit("sdm")
  out <- apa_tidy(fit)

  expect_identical(attr(out, "source_class"), c("bmmfit", "brmsfit"))
  expect_true(all(c("b_c_Intercept", "b_kappa_Intercept") %in% out$term))
  expect_identical(out$component[out$term == "b_c_Intercept"], "c")
  expect_identical(out$component[out$term == "b_kappa_Intercept"], "kappa")
  # `mu` is fixed internally to 0 in the sdm (bmm's fixed_parameters), so
  # its `b_Intercept` row is a constant with no R-hat; the free rows have one.
  free <- out$term %in% c("b_c_Intercept", "b_kappa_Intercept")
  expect_false(anyNA(out$rhat[free]))
  expect_false(is.na(attr(apa_tidy_diagnostics(fit), "divergences")))
})

test_that("apa_inline() addresses a bmm parameter by its term", {
  fit <- test_bmm_fit("sdm")
  out <- apa_inline(fit, "b_kappa_Intercept", markup = "plain")

  expect_s3_class(out, "apa_results")
  expect_match(out$full_result[[1]], "CrI")
})
