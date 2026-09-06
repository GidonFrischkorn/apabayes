# Contract under test: dev/specs/spec-apa_ci.md

test_that("apa_ci() prints level, label and bounds", {
  expect_identical(apa_ci(0.2, 0.74), "95% CrI [0.20, 0.74]")
  expect_identical(
    apa_ci(0.2, 0.74, leading_zero = FALSE),
    "95% CrI [.20, .74]"
  )
  expect_identical(
    apa_ci(0.2, 0.74, label = "HDI", level = 0.9),
    "90% HDI [0.20, 0.74]"
  )
  expect_identical(apa_ci(0.2, 0.74, label = "CI"), "95% CI [0.20, 0.74]")
  expect_identical(apa_ci(0.2, 0.74, label = NULL), "[0.20, 0.74]")
  expect_identical(apa_ci(0.2, 0.74, level = 0.955), "95.5% CrI [0.20, 0.74]")
  expect_identical(apa_ci(0.2, 0.74, level = 0.89), "89% CrI [0.20, 0.74]")
  expect_identical(apa_ci(0.2, 0.74, digits = 3), "95% CrI [0.200, 0.740]")
  expect_identical(apa_ci(1200, 1800, digits = 0), "95% CrI [1,200, 1,800]")
  expect_identical(
    apa_ci(1200, 1800, digits = 0, big_mark = FALSE),
    "95% CrI [1200, 1800]"
  )
})

test_that("apa_ci() uses the minus and infinity of the target", {
  expect_identical(apa_ci(-0.5, 0.74, markup = "md"), "95% CrI [−0.50, 0.74]")
  expect_identical(
    apa_ci(-0.5, 0.74, markup = "plain"),
    "95% CrI [-0.50, 0.74]"
  )
  expect_identical(
    apa_ci(-Inf, 0.74, markup = "latex"),
    "95% CrI [$-\\infty$, 0.74]"
  )
  expect_identical(apa_ci(-Inf, 0.74, markup = "md"), "95% CrI [−∞, 0.74]")
  expect_identical(apa_ci(0.2, Inf, markup = "plain"), "95% CrI [0.20, Inf]")
})

test_that("apa_ci() is vectorised, recycles a scalar bound, keeps NA", {
  expect_identical(
    apa_ci(c(0.2, NA), c(0.74, 0.9)),
    c("95% CrI [0.20, 0.74]", NA)
  )
  expect_identical(
    apa_ci(c(0.2, 0.3), 0.74),
    c("95% CrI [0.20, 0.74]", "95% CrI [0.30, 0.74]")
  )
  expect_identical(apa_ci(numeric(0), numeric(0)), character(0))
})

test_that("apa_ci() validates", {
  expect_error(apa_ci(0.74, 0.2), "below")
  expect_error(apa_ci(c(0.1, 0.9), c(0.5, 0.5)), "below")
  expect_error(apa_ci("a", 0.2), "numeric")
  expect_error(apa_ci(0.2, "b"), "numeric")
  expect_error(apa_ci(c(0.1, 0.2, 0.3), c(0.5, 0.6)), "same length")
  expect_error(apa_ci(0.2, 0.74, level = 95), "level")
  expect_error(apa_ci(0.2, 0.74, level = c(0.9, 0.95)), "level")
  expect_error(apa_ci(0.2, 0.74, label = c("a", "b")), "label")
  expect_error(apa_ci(0.2, 0.74, label = 1), "label")
})

test_that("apa_ci() agrees with insight::format_ci()", {
  skip_if_not_installed("insight")
  expect_identical(
    apa_ci(0.2, 0.74, label = "CI", markup = "plain"),
    insight::format_ci(0.2, 0.74, ci = 0.95, digits = 2)
  )
  expect_identical(
    apa_ci(0.2, 0.74, label = "CI", level = 0.9, markup = "plain"),
    insight::format_ci(0.2, 0.74, ci = 0.9, digits = 2)
  )
})
