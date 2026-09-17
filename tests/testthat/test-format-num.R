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
  # Both helpers round through the platform's printf, so neither is
  # defined at a tie: on Windows they print .005 as ".00" and on macOS
  # and Linux as ".01" (measured, CI run 35209064098). apabayes rounds
  # for itself (spec-rounding-0.1.0.md), so the comparison runs where the
  # helpers agree with themselves and the rule is asserted directly in
  # "apa_num() rounds ties away from zero on every platform" below.
  x <- c(0.4712, -0.4712, 0.006, -0.006, 0.999, 1.234)
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

# ---- rounding ---------------------------------------------------------
# Contract under test: local/specs/spec-rounding-0.1.0.md. apabayes
# rounds in R and hands formatC() an already-rounded number, so the
# printed string does not depend on whose printf runs.

test_that("round_half_up() rounds the number as written, ties away from zero", {
  x <- c(0.005, 0.015, 0.045, 0.125, 0.145, 1.005, 2.675, 12.345)
  expected <- c(0.01, 0.02, 0.05, 0.13, 0.15, 1.01, 2.68, 12.35)
  expect_equal(round_half_up(x, 2), expected)
  expect_equal(round_half_up(-x, 2), -expected)

  # digits = 0 and digits = 3, the two other decimals the package uses
  expect_equal(round_half_up(c(0.5, 1.5, 2.5, -0.5), 0), c(1, 2, 3, -1))
  expect_equal(round_half_up(c(0.0005, 0.0015), 3), c(0.001, 0.002))

  # away from the midpoint nothing moves
  expect_equal(
    round_half_up(c(0.4712, -0.4712, 0.006), 2), c(0.47, -0.47, 0.01)
  )

  # the rule is not R's round(), which settles the decimal tie to even
  expect_false(isTRUE(all.equal(round_half_up(0.005, 2), round(0.005, 2))))

  # nothing that has no fraction at this scale is touched, and the
  # non-finite values pass through
  big <- 2^53
  expect_identical(round_half_up(big, 2), big)
  expect_identical(
    round_half_up(c(NA_real_, NaN, Inf, -Inf), 2),
    c(NA_real_, NaN, Inf, -Inf)
  )
})

test_that("apa_num() rounds ties away from zero on every platform", {
  # The two strings the first CI run disagreed about (class C of
  # local/ci-analysis-2026-09-17.md): Windows printed ".00" where macOS
  # and Linux printed ".01".
  expect_identical(apa_num(0.005, digits = 2), "0.01")
  expect_identical(apa_num(0.005, leading_zero = FALSE), ".01")
  expect_identical(
    apa_num(-0.005, leading_zero = FALSE, markup = "plain"), "-.01"
  )

  x <- c(0.005, 0.015, 0.045, 0.125, 0.145, 2.675, 12.345)
  expect_identical(
    apa_num(x, big_mark = FALSE, markup = "plain"),
    c("0.01", "0.02", "0.05", "0.13", "0.15", "2.68", "12.35")
  )
  # and the string is the rounded number laid out, not a second rounding
  expect_identical(
    apa_num(x, big_mark = FALSE, markup = "plain"),
    formatC(round_half_up(x, 2), digits = 2, format = "f")
  )
})

test_that("a value that rounds to zero keeps no sign, at any digits", {
  # Unchanged behaviour, asserted directly rather than through a seed
  # helper: the helpers keep "-.00" and apabayes does not.
  expect_identical(apa_num(-0.004, leading_zero = FALSE), ".00")
  expect_identical(apa_num(-0.004, markup = "plain"), "0.00")
  expect_identical(apa_num(-0.0004, digits = 3, markup = "plain"), "0.000")
  expect_identical(apa_num(-0.4, digits = 0, markup = "plain"), "0")
  expect_identical(apa_num(-1e-9, digits = 2, markup = "plain"), "0.00")
  # the negative zero itself, not only a value that becomes one
  expect_identical(apa_num(-0, markup = "plain"), "0.00")
})

test_that("the bounded formatters round by the same rule", {
  expect_identical(apa_p(0.0455, markup = "plain"), ".046")
  expect_identical(apa_p(0.1235, markup = "plain"), ".124")
  expect_identical(apa_prob(0.125, digits = 2, markup = "plain"), ".13")
  expect_identical(
    apa_prob(0.125, digits = 2, percent = TRUE, markup = "plain"), "12.50%"
  )
})

test_that("a regime boundary is decided by the number that gets printed", {
  # as_printed() decides which regime a value falls in; it has to round
  # the way the printing does, or a value can be put in one regime and
  # printed as if it were in the other.
  expect_identical(as_printed(0.125, 2), round_half_up(0.125, 2))
  expect_identical(as_printed(9.995, 2), round_half_up(9.995, 2))
  # 9.995 rounds to 10.00, so it is in the "10 or more" regime and prints
  # with one decimal; under the platform's rounding it stayed at 9.99 and
  # printed with two.
  expect_identical(apa_er(9.995, markup = "plain"), "10.0")
  expect_identical(apa_bf(9.995, markup = "plain"), "10.0")
})
