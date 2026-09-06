# Contract under test: dev/specs/spec-apa_p_pd_prob.md

test_that("apa_p() prints three decimals without a leading zero", {
  expect_identical(apa_p(0.0234), ".023")
  expect_identical(apa_p(0.05), ".050")
  expect_identical(apa_p(0.5), ".500")
  expect_identical(apa_p(0.0234, digits = 2), ".02")
})

test_that("apa_p() floors and caps", {
  expect_identical(apa_p(0.0004), "< .001")
  expect_identical(apa_p(0), "< .001")
  expect_identical(apa_p(0.001), ".001")
  expect_identical(apa_p(0.999), ".999")
  expect_identical(apa_p(0.9996), "> .999")
  expect_identical(apa_p(1), "> .999")
  expect_identical(apa_p(0.004, digits = 2), "< .01")
  expect_identical(apa_p(0.996, digits = 2), "> .99")
})

test_that("apa_p() prepends the symbol per target", {
  expect_identical(apa_p(0.05, symbol = TRUE), "*p* = .050")
  expect_identical(apa_p(0.0004, symbol = TRUE), "*p* < .001")
  expect_identical(apa_p(1, symbol = TRUE, markup = "latex"), "*p* > .999")
  expect_identical(apa_p(0.0004, symbol = TRUE, markup = "plain"), "p < .001")
  expect_identical(apa_p(0.05, symbol = TRUE, markup = "plain"), "p = .050")
})

test_that("apa_p() passes NA through and validates", {
  expect_identical(apa_p(NA), NA_character_)
  expect_identical(apa_p(c(0.02, NA), symbol = TRUE), c("*p* = .020", NA))
  expect_identical(apa_p(numeric(0)), character(0))
  expect_error(apa_p(1.2), "between 0 and 1")
  expect_error(apa_p(-0.1), "between 0 and 1")
  expect_error(apa_p("0.05"), "numeric")
  expect_error(apa_p(0.05, digits = 0), "digits")
})

test_that("apa_p() agrees with papaja::apa_p() and insight::format_p()", {
  x <- c(0, 0.0004, 0.001, 0.0234, 0.05, 0.5, 0.999, 0.9996, 1)
  skip_if_not_installed("papaja")
  expect_identical(apa_p(x, markup = "plain"), papaja::apa_p(x))
  skip_if_not_installed("insight")
  expect_identical(
    apa_p(x, markup = "plain"),
    sub("^0\\.", ".", insight::format_p(x, name = NULL))
  )
})

test_that("apa_p() reproduces the SDVWM fmt_p() helper", {
  x <- c(0.0004, 0.001, 0.0234, 0.05, 0.5, 0.999)
  expect_identical(
    apa_p(x, markup = "plain"),
    vapply(x, seed_sdvwm$fmt_p, character(1))
  )
})

test_that("apa_pd() prints and caps like the miniQ helper", {
  expect_identical(apa_pd(0.9874), ".987")
  expect_identical(apa_pd(0.5), ".500")
  expect_identical(apa_pd(0.999), ".999")
  expect_identical(apa_pd(0.9995), "> .999")
  expect_identical(apa_pd(1), "> .999")
  expect_identical(apa_pd(0.9995, symbol = TRUE), "*pd* > .999")
  expect_identical(apa_pd(0.9874, symbol = TRUE), "*pd* = .987")
  expect_identical(apa_pd(0.9874, symbol = TRUE, markup = "plain"), "pd = .987")
  expect_identical(apa_pd(NA), NA_character_)
  expect_error(apa_pd(1.5), "between 0 and 1")
  x <- c(0.9874, 0.5, 0.999, 0.9995, 1, 0.75)
  expect_identical(
    apa_pd(x, symbol = TRUE, markup = "plain"),
    paste("pd", vapply(x, seed_miniq$fmt_pd, character(1)))
  )
})

test_that("apa_prob() formats proportions and percentages", {
  expect_identical(apa_prob(0.1234), ".123")
  expect_identical(apa_prob(0.5, digits = 2), ".50")
  expect_identical(apa_prob(0.0004), "< .001")
  expect_identical(apa_prob(0), "< .001")
  expect_identical(apa_prob(1), "> .999")
  expect_identical(apa_prob(0.1234, percent = TRUE), "12.3%")
  expect_identical(apa_prob(0.1234, percent = TRUE, digits = 2), "12.34%")
  expect_identical(apa_prob(0, percent = TRUE), "< 0.1%")
  expect_identical(apa_prob(0.0004, percent = TRUE), "< 0.1%")
  expect_identical(apa_prob(1, percent = TRUE), "> 99.9%")
  expect_identical(apa_prob(0.9996, percent = TRUE), "> 99.9%")
  expect_identical(apa_prob(0.5, percent = TRUE), "50.0%")
  expect_identical(apa_prob(NA, percent = TRUE), NA_character_)
  expect_identical(apa_prob(c(0.25, NA)), c(".250", NA))
  expect_error(apa_prob(2), "between 0 and 1")
  expect_error(apa_prob(0.5, percent = NA), "percent")
})
