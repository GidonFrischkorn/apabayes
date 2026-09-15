# apa_inline() on correlations tables
# (local/specs/spec-apa_tidy_correlation.md, section "Inline").
#
# The tables come from the correlation route on the fixtures. Numbers
# reach the expected strings through the format layer's own functions,
# which *are* the rule (decision 8); the words and symbols around them are
# the spec's literals.

# nolint start: object_usage_linter.
correlation_table <- function(name = "cor_default", ...) {
  apa_tidy(fixture(name), ...)
}

# The expected estimate part of one row: `*r* = −.82, 95% HDI [...]`.
expected_r <- function(row, m, label = "HDI", leading_zero = FALSE,
                       symbol = "r") {
  est <- apa_num(row$estimate, 2, leading_zero, markup = m)
  if (!is.null(symbol)) {
    est <- paste0(markup(symbol, m, italic = TRUE), " = ", est)
  }
  paste0(
    est, ", ",
    apa_ci(
      row$ci_low, row$ci_high,
      level = row$ci_level, label = label, digits = 2,
      leading_zero = leading_zero, markup = m
    )
  )
}
# nolint end

# ---- type ----------------------------------------------------------------

test_that("the correlations type is built", {
  expect_true("correlations" %in% inline_types())
  expect_identical(
    inline_stats_vocabulary("correlations"),
    c("pd", "rope", "bf", "n")
  )
  expect_identical(inline_default_stats("correlations"), c("pd", "bf"))
})

# ---- strings -------------------------------------------------------------

test_that("a correlation row prints r without a leading zero, pd and BF", {
  tab <- correlation_table()
  for (m in c("md", "latex", "typst", "plain")) {
    r <- apa_inline(tab, markup = m)
    est <- vapply(
      seq_len(nrow(tab)),
      function(i) expected_r(tab[i, ], m),
      character(1)
    )
    stat <- paste0(
      apa_pd(tab$pd, 3, markup = m, symbol = TRUE), ", ",
      apa_bf(tab$bf, "10", "auto", markup = m, symbol = TRUE)
    )
    expect_identical(r$estimate, est, label = m)
    expect_identical(r$statistic, stat, label = m)
    expect_identical(r$full_result, paste0(est, ", ", stat), label = m)
  }
  expect_identical(
    apa_inline(tab, "mpg~~wt", markup = "md")$estimate,
    "*r* = −.82, 95% HDI [−.92, −.66]"
  )
})

test_that("the ROPE share and n print on request, in vocabulary order", {
  tab <- correlation_table()
  r <- apa_inline(tab, "mpg~~wt", stats = c("n", "rope"), markup = "md")
  expect_identical(
    r$statistic,
    paste0(
      apa_prob(tab$rope_pct[1], percent = TRUE, markup = "md"),
      " in ROPE, *n* = 32"
    )
  )
  none <- apa_inline(tab, "mpg~~wt", stats = character(), markup = "md")
  expect_identical(none$full_result, none$estimate)
  expect_error(apa_inline(tab, stats = "p"), "cannot print")
})

test_that("a missing statistic is left out of the string", {
  tab <- correlation_table("cor_bf")
  r <- apa_inline(tab, "mpg~~wt", markup = "md")
  expect_identical(
    r$statistic,
    apa_bf(tab$bf[1], "10", "auto", markup = "md", symbol = TRUE)
  )
})

test_that("symbol, leading_zero and the interval label can be changed", {
  tab <- correlation_table()
  row <- tab[1, ]
  expect_identical(
    apa_inline(
      tab, "mpg~~wt",
      symbol = FALSE, stats = character(), markup = "md"
    )$full_result,
    expected_r(row, "md", symbol = NULL)
  )
  expect_identical(
    apa_inline(
      tab, "mpg~~wt",
      symbol = "ρ", stats = character(), markup = "md"
    )$full_result,
    expected_r(row, "md", symbol = "ρ")
  )
  expect_identical(
    apa_inline(
      tab, "mpg~~wt",
      leading_zero = TRUE, stats = character(), markup = "md"
    )$full_result,
    expected_r(row, "md", leading_zero = TRUE)
  )
  eti <- correlation_table("cor_eti", ci = "eti")
  expect_identical(
    apa_inline(eti, "mpg~~wt", stats = character(), markup = "md")$estimate,
    expected_r(eti[1, ], "md", label = "CrI")
  )
})

# ---- addressing ----------------------------------------------------------

test_that("a pair is found by its term or its variables in either order", {
  tab <- correlation_table()
  ref <- apa_inline(tab, "mpg~~wt")
  expect_identical(nrow(ref$table), 1L)
  expect_identical(apa_inline(tab, "mpg", "wt"), ref)
  expect_identical(apa_inline(tab, "wt", "mpg"), ref)
  expect_identical(apa_inline(tab, "wt", "mpg", op = "~~"), ref)
  expect_error(apa_inline(tab, "mpg", "wt", op = "=~"), "No row matches")
  expect_error(apa_inline(tab, "mpg", "gear"), "No row matches")
})

test_that("a single variable finds its one pair, or lists several", {
  two <- correlation_table("cor_select2")
  expect_error(apa_inline(two, "mpg"), "2 rows match")
  expect_identical(
    apa_inline(two, "hp")$table$term,
    "mpg~~hp"
  )
  expect_error(apa_inline(two, "gear"), "mpg~~wt")
  expect_identical(apa_inline(two, "hp", op = "~~")$table$term, "mpg~~hp")
})

test_that("a grouped table is addressed with group", {
  tab <- correlation_table("cor_grouped")
  expect_error(apa_inline(tab, "mpg", "wt"), "pass `group`")
  r <- apa_inline(tab, "mpg", "wt", group = "manual")
  expect_identical(r$table$group, "manual")
  expect_identical(r$table$n, tab$n[4])
})

# ---- papaja --------------------------------------------------------------

test_that("apa_print() on a correlation table matches its tidy table", {
  x <- fixture("cor_default")
  r <- apa_print.easycorrelation(x)
  expect_identical(r, apa_print.apabayes_tidy(apa_tidy(x)))
  expect_named(r$full_result, c("mpg_wt", "mpg_hp", "wt_hp"))
  expect_identical(
    apa_print.easycorrelation(x, "wt", "mpg"),
    apa_inline(x, "wt", "mpg")
  )
})
