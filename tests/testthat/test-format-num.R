# Contract under test: dev/specs/spec-apa_num.md

test_that("apa_num() prints fixed decimals with the leading-zero rule", {
  expect_identical(apa_num(0.4712), "0.47")
  expect_identical(apa_num(0.4712, leading_zero = FALSE), ".47")
  expect_identical(
    apa_num(-0.4712, leading_zero = FALSE, markup = "plain"),
    "-.47"
  )
  expect_identical(apa_num(1.004, leading_zero = FALSE), "1.00")
  expect_identical(apa_num(0.4712, digits = 3), "0.471")
  expect_identical(apa_num(3, digits = 0), "3")
})

test_that("apa_num() uses the minus of the target and zaps negative zero", {
  expect_identical(apa_num(-0.4712, markup = "md"), "−0.47")
  expect_identical(apa_num(-0.4712, markup = "latex"), "−0.47")
  expect_identical(apa_num(-0.4712, markup = "typst"), "−0.47")
  expect_identical(apa_num(-0.4712, markup = "plain"), "-0.47")
  expect_identical(apa_num(-0.001), "0.00")
  expect_identical(apa_num(-0.001, leading_zero = FALSE), ".00")
  expect_identical(apa_num(-0.4, digits = 0, markup = "plain"), "0")
})

test_that("apa_num() applies the thousands separator", {
  expect_identical(apa_num(1234.5678), "1,234.57")
  expect_identical(apa_num(1234.5678, big_mark = FALSE), "1234.57")
  expect_identical(apa_num(1240, digits = 0), "1,240")
  expect_identical(apa_num(-1240, digits = 0, markup = "plain"), "-1,240")
  expect_identical(apa_num(999.99, digits = 1), "1,000.0")
})

test_that("apa_num() passes NA through and prints infinities per target", {
  expect_identical(apa_num(NA), NA_character_)
  expect_identical(
    apa_num(c(NA, Inf, -Inf), markup = "plain"),
    c(NA, "Inf", "-Inf")
  )
  expect_identical(apa_num(c(Inf, -Inf), markup = "md"), c("∞", "−∞"))
  expect_identical(
    apa_num(c(Inf, -Inf), markup = "latex"),
    c("$\\infty$", "$-\\infty$")
  )
  expect_identical(apa_num(numeric(0)), character(0))
  expect_identical(apa_num(c(a = 1.5)), "1.50")
  expect_identical(apa_num(NaN), NA_character_)
})

test_that("apa_num() validates its arguments", {
  expect_error(apa_num("0.5"), "numeric")
  expect_error(apa_num(TRUE), "numeric")
  expect_error(apa_num(0.5, digits = -1), "digits")
  expect_error(apa_num(0.5, digits = 1.5), "digits")
  expect_error(apa_num(0.5, digits = c(1, 2)), "digits")
  expect_error(apa_num(0.5, leading_zero = NA), "leading_zero")
  expect_error(apa_num(0.5, big_mark = "yes"), "big_mark")
})

test_that("apa_num() agrees with papaja::apa_num() where they coincide", {
  skip_if_not_installed("papaja")
  # papaja rounds through round() before formatting, which settles decimal
  # ties (0.005, 12.345) differently from printf rounding; ties stay out
  x <- c(0.4712, 0, 0.006, 1.5, 12.344, 999.999, 1234.5678, 100000)
  expect_identical(apa_num(x, markup = "plain"), papaja::apa_num(x, digits = 2))
  expect_identical(
    apa_num(x, digits = 3, markup = "plain"),
    papaja::apa_num(x, digits = 3)
  )
  bounded <- c(0.4712, 0.006, 0.99, 0.994, 0)
  expect_identical(
    apa_num(bounded, leading_zero = FALSE, markup = "plain"),
    papaja::apa_num(bounded, digits = 2, gt1 = FALSE)
  )
})

test_that("apa_num() agrees with insight::format_value() where they coincide", {
  skip_if_not_installed("insight")
  x <- c(0.4712, 0.05, 1.5, -12.345, 999.999, 123.4)
  expect_identical(
    apa_num(x, big_mark = FALSE, markup = "plain"),
    insight::format_value(x, digits = 2)
  )
})

test_that("apa_num() reproduces the SDVWM fmt_r() and miniQ fmt_r() helpers", {
  x <- c(0.4712, -0.4712, 0.005, -0.005, 0.999, 1.234)
  expect_identical(
    apa_num(x, leading_zero = FALSE, big_mark = FALSE, markup = "plain"),
    vapply(x, seed_sdvwm$fmt_r, character(1))
  )
  # miniQ keeps "-.00" for a negative zero; apabayes zaps it, so compare
  # away from zero only
  x2 <- c(0.4712, -0.4712, 0.999, -0.12)
  expect_identical(
    apa_num(x2, leading_zero = FALSE, markup = "plain"),
    vapply(x2, seed_miniq$fmt_r, character(1))
  )
})
