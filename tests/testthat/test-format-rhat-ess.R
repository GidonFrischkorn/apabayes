# Contract under test: dev/specs/spec-apa_rhat_ess.md

test_that("apa_rhat_ess() prints the parts given, in order, per target", {
  expect_identical(
    apa_rhat_ess(1.003, 1240, 980),
    "*R̂* = 1.00, bulk ESS = 1,240, tail ESS = 980"
  )
  expect_identical(
    apa_rhat_ess(1.003, 1240, 980, markup = "plain"),
    "Rhat = 1.00, bulk ESS = 1,240, tail ESS = 980"
  )
  expect_identical(
    apa_rhat_ess(1.003, 1240, 980, markup = "latex"),
    "$\\hat{R}$ = 1.00, bulk ESS = 1,240, tail ESS = 980"
  )
  expect_identical(
    apa_rhat_ess(1.003, 1240, 980, markup = "typst"),
    "*R̂* = 1.00, bulk ESS = 1,240, tail ESS = 980"
  )
  expect_identical(apa_rhat_ess(1.003), "*R̂* = 1.00")
  expect_identical(apa_rhat_ess(1.003, digits = 3), "*R̂* = 1.003")
  expect_identical(apa_rhat_ess(ess_bulk = 1240), "bulk ESS = 1,240")
  expect_identical(apa_rhat_ess(ess_tail = 979.6), "tail ESS = 980")
  expect_identical(
    apa_rhat_ess(1.003, ess_tail = 980),
    "*R̂* = 1.00, tail ESS = 980"
  )
})

test_that("apa_rhat_ess() is vectorised, recycles scalars, keeps NA", {
  expect_identical(
    apa_rhat_ess(c(1.00, 1.02), c(1240, 400), markup = "plain"),
    c("Rhat = 1.00, bulk ESS = 1,240", "Rhat = 1.02, bulk ESS = 400")
  )
  expect_identical(
    apa_rhat_ess(1.01, c(1240, 400), markup = "plain"),
    c("Rhat = 1.01, bulk ESS = 1,240", "Rhat = 1.01, bulk ESS = 400")
  )
  expect_identical(apa_rhat_ess(NA, 1240), NA_character_)
  expect_identical(
    apa_rhat_ess(c(1.00, NA), c(1240, 400), markup = "plain"),
    c("Rhat = 1.00, bulk ESS = 1,240", NA)
  )
  expect_identical(apa_rhat_ess(numeric(0)), character(0))
})

test_that("apa_rhat_ess() validates", {
  expect_error(apa_rhat_ess(), "at least one")
  expect_error(apa_rhat_ess("1.00"), "numeric")
  expect_error(apa_rhat_ess(1, ess_bulk = "400"), "numeric")
  expect_error(apa_rhat_ess(c(1, 1, 1), c(400, 400)), "same length")
  expect_error(apa_rhat_ess(1, digits = -1), "digits")
})

test_that("apa_rhat_ess() reproduces the m3 three-decimal R-hat", {
  x <- c(1.0034, 1.0106, 0.9996)
  expect_identical(
    sub("^Rhat = ", "", apa_rhat_ess(x, digits = 3, markup = "plain")),
    sprintf("%.3f", x)
  )
})
