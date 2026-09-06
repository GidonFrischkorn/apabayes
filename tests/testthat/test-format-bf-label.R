# Contract under test: dev/specs/spec-apa_bf_label.md

test_that("apa_bf_label() names the category and the favoured hypothesis", {
  expect_identical(
    apa_bf_label(5, scheme = "jeffreys"),
    "moderate evidence for H~1~"
  )
  expect_identical(
    apa_bf_label(0.2, scheme = "jeffreys"),
    "moderate evidence for H~0~"
  )
  expect_identical(
    apa_bf_label(5, scheme = "jeffreys", markup = "plain"),
    "moderate evidence for H1"
  )
  expect_identical(
    apa_bf_label(1, scheme = "jeffreys"),
    "no evidence for either hypothesis"
  )
  expect_identical(
    apa_bf_label(Inf, scheme = "jeffreys"),
    "extreme evidence for H~1~"
  )
  expect_identical(
    apa_bf_label(0, scheme = "jeffreys"),
    "extreme evidence for H~0~"
  )
  expect_identical(
    apa_bf_label(Inf, scheme = "raftery"),
    "very strong evidence for H~1~"
  )
  expect_identical(apa_bf_label(NA, scheme = "jeffreys"), NA_character_)
  expect_identical(apa_bf_label(numeric(0), scheme = "jeffreys"), character(0))
})

test_that("apa_bf_label() places the boundaries as effectsize documents them", {
  # upper bound inclusive: 3 is still the lowest category
  j <- c(1.5, 3, 3.01, 10, 10.01, 30, 30.01, 100, 100.01)
  expect_identical(
    apa_bf_label(j, scheme = "jeffreys", markup = "plain"),
    paste(c(
      "anecdotal", "anecdotal", "moderate", "moderate", "strong", "strong",
      "very strong", "very strong", "extreme"
    ), "evidence for H1")
  )
  r <- c(1.5, 3, 3.01, 20, 20.01, 150, 150.01)
  expect_identical(
    apa_bf_label(r, scheme = "raftery", markup = "plain"),
    paste(
      c(
        "weak", "weak", "positive", "positive", "strong", "strong",
        "very strong"
      ),
      "evidence for H1"
    )
  )
  expect_identical(
    apa_bf_label(1 / j, scheme = "jeffreys", markup = "plain"),
    sub("H1", "H0", apa_bf_label(j, scheme = "jeffreys", markup = "plain"))
  )
})

test_that("apa_bf_label() requires an explicit scheme and validates", {
  expect_error(apa_bf_label(5), "scheme")
  expect_error(apa_bf_label(5), "continuous")
  expect_error(apa_bf_label(5, scheme = "lee_wagenmakers"), "scheme")
  expect_error(apa_bf_label(-1, scheme = "jeffreys"), "0 or more")
  expect_error(apa_bf_label("3", scheme = "jeffreys"), "numeric")
})

test_that("apa_bf_label() agrees with effectsize::interpret_bf()", {
  skip_if_not_installed("effectsize")
  # effectsize compares on the log scale, so exact bounds fall on either
  # side by floating-point luck (measured 2026-09-06: 3 -> moderate,
  # 150 -> strong); the comparison stays away from them
  x <- c(0.005, 0.02, 0.05, 0.2, 0.5, 2, 3.5, 15, 25, 50, 200, 1000)
  words <- function(s) {
    out <- apa_bf_label(x, scheme = s, markup = "plain")
    sub(" evidence for H[01]$", "", out)
  }
  es <- function(rules) {
    sub(
      " evidence (in favour of|against)$", "",
      as.character(effectsize::interpret_bf(x, rules = rules))
    )
  }
  expect_identical(words("jeffreys"), es("jeffreys1961"))
  expect_identical(words("raftery"), es("raftery1995"))
  # direction agrees too
  labels <- apa_bf_label(x, scheme = "jeffreys", markup = "plain")
  dir_apa <- sub("^.* evidence for H([01])$", "\\1", labels)
  es_out <- as.character(effectsize::interpret_bf(x))
  dir_es <- ifelse(grepl("in favour of$", es_out), "1", "0")
  expect_identical(dir_apa, dir_es)
})
