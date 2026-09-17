# Contract under test: dev/specs/spec-apa_bf.md

test_that("apa_bf() picks the regime by size under style = 'auto'", {
  expect_identical(apa_bf(5.3412), "5.34")
  expect_identical(apa_bf(0.05), "0.05")
  expect_identical(apa_bf(0.01), "0.01")
  expect_identical(apa_bf(9.994), "9.99")
  expect_identical(apa_bf(9.999), "10.0")
  expect_identical(apa_bf(10), "10.0")
  expect_identical(apa_bf(20.86), "20.9")
  expect_identical(apa_bf(9999), "9,999.0")
  expect_identical(apa_bf(9999.94), "9,999.9")
  expect_identical(apa_bf(9999.99), "1.00 × 10^4^")
  expect_identical(apa_bf(10000), "1.00 × 10^4^")
  expect_identical(apa_bf(123456), "1.23 × 10^5^")
  expect_identical(apa_bf(0.0004), "4.00 × 10^−4^")
  expect_identical(apa_bf(0.009), "9.00 × 10^−3^")
  expect_identical(apa_bf(5.3412, digits = 3), "5.341")
  expect_identical(apa_bf(0.004, digits = 3), "0.004")
})

test_that("apa_bf() honours digits in style = 'auto's scientific regime", {
  # Finding 2 of the miniQmetrics acceptance test
  # (local/findings-2026-09-16.md): `digits` was silently ignored here,
  # always printing a one-decimal mantissa. Decided (GF, 2026-09-16):
  # honour it, so `apa_bf()` is a true drop-in for a caller's own
  # two-decimal `fmt_bf()` with no `style =`.
  expect_identical(apa_bf(4.975034e14, digits = 1), "5.0 × 10^14^")
  expect_identical(apa_bf(4.975034e14, digits = 2), "4.98 × 10^14^")
  expect_identical(apa_bf(4.975034e14, digits = 3), "4.975 × 10^14^")
})

test_that("apa_bf() composes scientific notation per target", {
  expect_identical(apa_bf(123456, markup = "md"), "1.23 × 10^5^")
  expect_identical(apa_bf(123456, markup = "typst"), "1.23 × 10^5^")
  expect_identical(apa_bf(123456, markup = "latex"), "1.23 $\\times$ 10^5^")
  expect_identical(apa_bf(123456, markup = "plain"), "1.23 x 10^5")
  expect_identical(apa_bf(0.0004, markup = "plain"), "4.00 x 10^-4")
  expect_identical(apa_bf(0.0004, markup = "latex"), "4.00 $\\times$ 10^−4^")
})

test_that("apa_bf() handles infinity, zero and NA", {
  expect_identical(apa_bf(Inf), "∞")
  expect_identical(apa_bf(Inf, markup = "latex"), "$\\infty$")
  expect_identical(apa_bf(Inf, markup = "plain"), "Inf")
  expect_identical(apa_bf(0), "0")
  expect_identical(apa_bf(NA), NA_character_)
  expect_identical(apa_bf(c(5.3412, NA, Inf)), c("5.34", NA, "∞"))
  expect_identical(apa_bf(numeric(0)), character(0))
})

test_that("apa_bf() inverts for direction = '01'", {
  expect_identical(apa_bf(20, direction = "01"), "0.05")
  expect_identical(apa_bf(0.05, direction = "01"), "20.0")
  expect_identical(apa_bf(0.0004, direction = "01"), "2,500.0")
  expect_identical(apa_bf(Inf, direction = "01"), "0")
  expect_identical(apa_bf(0, direction = "01"), "∞")
  expect_identical(apa_bf(NA, direction = "01"), NA_character_)
  expect_identical(apa_bf(1e-6, direction = "01"), "1.00 × 10^6^")
})

test_that("apa_bf() prepends the symbol with the matching subscript", {
  expect_identical(apa_bf(20.86, symbol = TRUE), "*BF*~10~ = 20.9")
  expect_identical(
    apa_bf(20.86, direction = "01", symbol = TRUE),
    "*BF*~01~ = 0.05"
  )
  expect_identical(
    apa_bf(20.86, symbol = TRUE, markup = "plain"),
    "BF10 = 20.9"
  )
  expect_identical(
    apa_bf(20.86, direction = "01", symbol = TRUE, markup = "plain"),
    "BF01 = 0.05"
  )
  expect_identical(
    apa_bf(Inf, symbol = TRUE, markup = "latex"),
    "*BF*~10~ = $\\infty$"
  )
  expect_identical(apa_bf(NA, symbol = TRUE), NA_character_)
})

test_that("apa_bf() style = 'sci' is scientific with `digits` decimals", {
  expect_identical(apa_bf(20.86, style = "sci"), "2.09 × 10^1^")
  expect_identical(apa_bf(5.3412, style = "sci"), "5.34 × 10^0^")
  expect_identical(apa_bf(0.05, style = "sci"), "5.00 × 10^−2^")
  expect_identical(apa_bf(9960, style = "sci", digits = 1), "1.0 × 10^4^")
  expect_identical(apa_bf(9.996, style = "sci"), "1.00 × 10^1^")
  expect_identical(apa_bf(Inf, style = "sci"), "∞")
  expect_identical(apa_bf(0, style = "sci"), "0")
  expect_identical(
    apa_bf(20.86, style = "sci", direction = "01"),
    "4.79 × 10^−2^"
  )
})

test_that("apa_bf() style = 'plain' never uses scientific notation", {
  expect_identical(apa_bf(123456, style = "plain"), "123,456.0")
  expect_identical(apa_bf(20.86, style = "plain"), "20.9")
  expect_identical(apa_bf(5.3412, style = "plain"), "5.34")
  expect_identical(apa_bf(0.0004, style = "plain"), "0.00")
  expect_identical(apa_bf(Inf, style = "plain", markup = "plain"), "Inf")
})

test_that("apa_bf() validates", {
  expect_error(apa_bf(-1), "0 or more")
  expect_error(apa_bf("3"), "numeric")
  expect_error(apa_bf(3, direction = "1"), "direction")
  expect_error(apa_bf(3, style = "words"), "style")
  expect_error(apa_bf(3, digits = -1), "digits")
})

test_that("apa_bf() reproduces the m3, miniQ and SDVWM fmt_bf() helpers", {
  # m3: three regimes; the seed writes the scientific regime as one LaTeX
  # math expression, apabayes as pandoc markup around a math times sign.
  x <- c(0.5, 5.3412, 9.99, 10, 20.86, 999.9, 9999)
  expect_identical(
    apa_bf(x, big_mark = FALSE, markup = "plain"),
    vapply(x, seed_m3$fmt_bf, character(1))
  )
  big <- c(10000, 123456, 2.5e7)
  m3_big <- vapply(big, seed_m3$fmt_bf, character(1))
  m3_big <- sub(
    "^\\$(.+) \\\\times 10\\^\\{(-?\\d+)\\}\\$$", "\\1 $\\\\times$ 10^\\2^",
    m3_big
  )
  # m3's own fmt_bf() hard-codes one mantissa decimal in this regime; now
  # that `digits` is honoured here (finding 2), `digits = 1` is what
  # matches it, not apa_bf()'s own default of 2.
  expect_identical(apa_bf(big, digits = 1, markup = "latex"), m3_big)
  # miniQ: every value scientific, two decimals, Unicode times; the seed
  # writes a negative exponent with a hyphen, apabayes with the minus
  # sign, so the comparison stays at exponents of 0 or more
  x2 <- c(1.5, 5.3412, 20.86, 123456)
  expect_identical(
    apa_bf(x2, style = "sci", markup = "md"),
    vapply(x2, seed_miniq$fmt_bf, character(1))
  )
  expect_identical(
    apa_bf(Inf, style = "sci", markup = "latex"),
    seed_miniq$fmt_bf(Inf)
  )
  # SDVWM: the prefix and two decimals below 10
  x3 <- c(0.5, 5.3412, 0.02)
  expect_identical(
    apa_bf(x3, symbol = TRUE, markup = "md"),
    vapply(x3, seed_sdvwm$fmt_bf, character(1))
  )
})

test_that("no numeric formatter emits a decision word (decision 15)", {
  sweep <- c(
    0, 1e-6, 0.0004, 0.05, 0.5, 1, 2.9, 3, 3.1, 9.99, 10, 30, 100,
    150, 1000, 12345, 1e7, Inf, NA
  )
  out <- c(
    apa_bf(sweep), apa_bf(sweep, direction = "01"),
    apa_bf(sweep, style = "sci"),
    apa_bf(sweep, style = "plain"), apa_bf(sweep, symbol = TRUE),
    apa_er(sweep), apa_er(sweep, symbol = TRUE),
    apa_num(sweep), apa_pd(c(0.5, 0.9, 0.999, 1, NA), symbol = TRUE),
    apa_p(c(0, 0.0004, 0.05, 1), symbol = TRUE),
    apa_prob(c(0, 0.5, 1)), apa_ci(-1, 1), apa_rhat_ess(1.01, 400, 400)
  )
  out <- out[!is.na(out)]
  pattern <- paste0("\\b(", paste(decision_words, collapse = "|"), ")\\b")
  expect_false(any(grepl(pattern, out, ignore.case = TRUE)))
})
