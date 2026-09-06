# Contract under test: dev/specs/spec-markup.md

targets <- c("md", "latex", "typst", "plain")

test_that("markup_target() defaults to md outside knitr", {
  withr::local_options(apabayes.markup = NULL)
  expect_identical(markup_target(), "md")
})

test_that("markup_target() honours the option, and the argument beats it", {
  withr::local_options(apabayes.markup = "latex")
  expect_identical(markup_target(), "latex")
  expect_identical(markup_target("plain"), "plain")
})

test_that("markup_target() detects latex and typst inside knitr", {
  skip_if_not_installed("knitr")
  withr::local_options(apabayes.markup = NULL)
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  withr::defer(knitr::opts_knit$set(rmarkdown.pandoc.to = old))
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  expect_identical(markup_target(), "latex")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "typst")
  expect_identical(markup_target(), "typst")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "docx")
  expect_identical(markup_target(), "md")
  # the option still wins over detection
  withr::local_options(apabayes.markup = "plain")
  expect_identical(markup_target(), "plain")
})

test_that("markup_target() falls back to md when knitr is not installed", {
  withr::local_options(apabayes.markup = NULL)
  testthat::local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang"
  )
  expect_identical(markup_target(), "md")
})

test_that("markup_target() rejects unknown targets", {
  expect_error(markup_target("html"), "html")
  expect_error(markup_target(c("md", "latex")))
  withr::local_options(apabayes.markup = "docx")
  expect_error(markup_target(), "docx")
})

test_that("symbol() returns every table entry for every target", {
  expected <- list(
    minus = c("−", "−", "−", "-"),
    times = c("×", "$\\times$", "×", "x"),
    infinity = c("∞", "$\\infty$", "∞", "Inf"),
    neg_infinity = c("−∞", "$-\\infty$", "−∞", "-Inf"),
    geq = c("≥", "$\\geq$", "≥", ">="),
    leq = c("≤", "$\\leq$", "≤", "<="),
    chisq = c("χ²", "$\\chi^2$", "χ²", "chi2"),
    rhat = c("*R̂*", "$\\hat{R}$", "*R̂*", "Rhat"),
    delta = c("Δ", "$\\Delta$", "Δ", "Delta")
  )
  expected <- lapply(expected, stats::setNames, targets)
  for (name in names(expected)) {
    for (target in targets) {
      expect_identical(symbol(name, target), unname(expected[[name]][[target]]),
        info = paste(name, target)
      )
    }
  }
})

test_that("symbol() is vectorised and rejects unknown names", {
  expect_identical(symbol(c("times", "infinity"), "plain"), c("x", "Inf"))
  expect_error(symbol("nope", "md"), "nope")
  expect_error(symbol("nope", "md"), "times")
})

test_that("markup() applies italic, subscript and superscript per target", {
  expect_identical(
    markup("BF", "md", italic = TRUE, subscript = "10"),
    "*BF*~10~"
  )
  expect_identical(
    markup("BF", "latex", italic = TRUE, subscript = "10"),
    "*BF*~10~"
  )
  expect_identical(
    markup("BF", "typst", italic = TRUE, subscript = "10"),
    "*BF*~10~"
  )
  expect_identical(
    markup("BF", "plain", italic = TRUE, subscript = "10"),
    "BF10"
  )
  expect_identical(markup("p", "md", italic = TRUE), "*p*")
  expect_identical(markup("p", "plain", italic = TRUE), "p")
  expect_identical(markup("10", "md", superscript = "5"), "10^5^")
  expect_identical(markup("10", "plain", superscript = "5"), "10^5")
  expect_identical(markup("H", "md", subscript = "1"), "H~1~")
  expect_identical(markup("x", "md"), "x")
})

test_that("markup() keeps length and passes NA through", {
  expect_identical(markup(c("a", NA), "md", italic = TRUE), c("*a*", NA))
  expect_identical(markup(character(0), "md", italic = TRUE), character(0))
})

test_that("stat_string() joins with = unless the value is a comparison", {
  expect_identical(
    stat_string("*p*", c(".023", "< .001")),
    c("*p* = .023", "*p* < .001")
  )
  expect_identical(stat_string("*pd*", "> .999"), "*pd* > .999")
  expect_identical(stat_string("ER", ">= 10,000"), "ER >= 10,000")
  expect_identical(stat_string("ER", "<= 1"), "ER <= 1")
  expect_identical(stat_string("ER", "≥ 10,000"), "ER ≥ 10,000")
  expect_identical(stat_string("ER", "$\\geq$ 10,000"), "ER $\\geq$ 10,000")
  expect_identical(stat_string("*p*", NA_character_), NA_character_)
  expect_identical(stat_string("*p*", character(0)), character(0))
})
