# Contract under test: dev/specs/spec-apa_er.md

test_that("apa_er() picks the regime by size", {
  expect_identical(apa_er(5.3412), "5.34")
  expect_identical(apa_er(0), "0.00")
  expect_identical(apa_er(9.994), "9.99")
  expect_identical(apa_er(9.999), "10.0")
  expect_identical(apa_er(10), "10.0")
  expect_identical(apa_er(23.456), "23.5")
  expect_identical(apa_er(999.94), "999.9")
  expect_identical(apa_er(999.96), "1,000")
  expect_identical(apa_er(1000), "1,000")
  expect_identical(apa_er(2345.6), "2,346")
  expect_identical(apa_er(9999.4), "9,999")
  expect_identical(apa_er(9999.6), "≥ 10,000")
  expect_identical(apa_er(10000), "≥ 10,000")
  expect_identical(apa_er(12345), "≥ 10,000")
  expect_identical(apa_er(5.3412, digits = 3), "5.341")
})

test_that("apa_er() prints the bound per target and handles Inf and NA", {
  expect_identical(apa_er(Inf, markup = "md"), "≥ 10,000")
  expect_identical(apa_er(Inf, markup = "latex"), "$\\geq$ 10,000")
  expect_identical(apa_er(Inf, markup = "typst"), "≥ 10,000")
  expect_identical(apa_er(Inf, markup = "plain"), ">= 10,000")
  expect_identical(apa_er(NA), NA_character_)
  expect_identical(
    apa_er(c(5.3412, NA, Inf), markup = "plain"),
    c("5.34", NA, ">= 10,000")
  )
  expect_identical(apa_er(numeric(0)), character(0))
})

test_that("apa_er() prepends ER with the joiner rule", {
  expect_identical(apa_er(5.3412, symbol = TRUE), "ER = 5.34")
  expect_identical(apa_er(12345, symbol = TRUE), "ER ≥ 10,000")
  expect_identical(
    apa_er(12345, symbol = TRUE, markup = "plain"),
    "ER >= 10,000"
  )
  expect_identical(
    apa_er(12345, symbol = TRUE, markup = "latex"),
    "ER $\\geq$ 10,000"
  )
  expect_identical(apa_er(NA, symbol = TRUE), NA_character_)
})

test_that("apa_er() validates", {
  expect_error(apa_er(-1), "0 or more")
  expect_error(apa_er("3"), "numeric")
  expect_error(apa_er(3, digits = 1.5), "digits")
})

test_that("apa_er() reproduces the m3 fmt_er() helper", {
  x <- c(
    0.5, 5.3412, 9.99, 10, 23.456, 999.9, 1000, 2345.6, 9999.4, 10000,
    12345, Inf
  )
  expect_identical(
    apa_er(x, markup = "latex"),
    vapply(x, seed_m3$fmt_er, character(1))
  )
})
