# apa_inline() on contrasts tables (local/specs/spec-apa_inline-contrasts.md).
#
# The tables come from the emmGrid route on the qdrg() grids. Numbers
# reach the expected strings through the format layer's own functions,
# which *are* the rule (decision 8); the words and symbols around them
# are the spec's literals.

# The helpers read setup.R's fixtures and the package's functions, which
# lintr does not see.
# nolint start: object_usage_linter.
contrast_table <- function(name = "pairs", ...) {
  apa_tidy(test_emm_grid(name), ...)
}

# The expected estimate part of one row.
expected_estimate <- function(row, m, label = "HDI", digits = 2,
                              leading_zero = TRUE) {
  paste0(
    apa_num(row$estimate, digits, leading_zero, markup = m), ", ",
    apa_ci(
      row$ci_low, row$ci_high,
      level = row$ci_level, label = label, digits = digits,
      leading_zero = leading_zero, markup = m
    )
  )
}
# nolint end

# ---- type ----------------------------------------------------------------

test_that("the contrasts type is built", {
  expect_true("contrasts" %in% inline_types())
  expect_identical(inline_stats_vocabulary("contrasts"), c("pd", "rope"))
  expect_identical(inline_default_stats("contrasts"), c("pd", "rope"))
})

test_that("a type without a builder is still refused by name", {
  # Every contract has a builder now; the guard stays for a contract
  # added before its builder, so it is exercised with the list shortened.
  tab <- contrast_table()
  local_mocked_bindings(inline_types = function() "parameters")
  expect_error(
    apa_inline(tab),
    'does not yet report a table of type "contrasts"'
  )
})

# ---- strings -------------------------------------------------------------

test_that("a contrast row prints its estimate, HDI and pd", {
  tab <- contrast_table()
  for (m in c("md", "latex", "plain")) {
    r <- apa_inline(tab, markup = m)
    est <- vapply(
      seq_len(nrow(tab)),
      function(i) expected_estimate(tab[i, ], m),
      character(1)
    )
    stat <- apa_pd(tab$pd, 3, markup = m, symbol = TRUE)
    expect_identical(r$estimate, est, label = m)
    expect_identical(r$statistic, stat, label = m)
    expect_identical(r$full_result, paste0(est, ", ", stat), label = m)
  }
  first <- apa_inline(tab, "cyl_f4 - cyl_f6", markup = "md")$full_result
  number <- "[-−0-9.]+"
  expect_match(first, paste0(
    "^", number, ", 95% HDI \\[", number, ", ", number, "\\], ",
    "\\*pd\\* [=<>] \\.[0-9]{3}$"
  ))
})

test_that("an equal-tailed table is labelled CrI, or not labelled", {
  tab <- contrast_table(ci = "eti")
  row <- tab[1, ]
  expect_identical(
    apa_inline(
      tab, row$contrast,
      stats = character(), markup = "md"
    )$full_result,
    expected_estimate(row, "md", label = "CrI")
  )
  expect_identical(
    apa_inline(
      tab, row$contrast,
      stats = character(), ci_label = NULL, markup = "md"
    )$full_result,
    expected_estimate(row, "md", label = NULL)
  )
  expect_identical(
    apa_inline(
      tab, row$contrast,
      stats = character(), interval = FALSE, markup = "md"
    )$full_result,
    apa_num(row$estimate, markup = "md")
  )
})

test_that("a symbol is printed only when asked for", {
  tab <- contrast_table()
  row <- tab[2, ]
  with_symbol <- apa_inline(
    tab, row$contrast,
    symbol = "d", stats = character(), markup = "md"
  )
  expect_identical(
    with_symbol$full_result,
    paste0("*d* = ", expected_estimate(row, "md"))
  )
  plain <- apa_inline(
    tab, row$contrast,
    symbol = "d", stats = character(), markup = "plain"
  )
  expect_identical(
    plain$full_result,
    paste0("d = ", expected_estimate(row, "plain"))
  )
  for (symbol in list(NULL, FALSE)) {
    r <- apa_inline(
      tab, row$contrast,
      symbol = symbol, stats = character(), markup = "md"
    )
    expect_identical(r$full_result, expected_estimate(row, "md"))
  }
})

test_that("the ROPE share prints when the table carries one", {
  tab <- contrast_table(rope = c(-1, 1), rope_ci = 1)
  row <- tab[3, ]
  r <- apa_inline(tab, row$contrast, markup = "md")
  expect_identical(
    r$statistic,
    paste0(
      apa_pd(row$pd, 3, markup = "md", symbol = TRUE), ", ",
      apa_prob(row$rope_pct, percent = TRUE, markup = "md"), " in ROPE"
    )
  )
  only_pd <- apa_inline(tab, row$contrast, stats = "pd", markup = "md")
  expect_identical(
    only_pd$statistic,
    apa_pd(row$pd, 3, markup = "md", symbol = TRUE)
  )
  none <- apa_inline(tab, row$contrast, stats = character(), markup = "md")
  expect_identical(none$statistic, NA_character_)
  expect_identical(none$full_result, expected_estimate(row, "md"))
  without <- contrast_table()
  expect_false(grepl("ROPE", apa_inline(without, row$contrast)$full_result))
})

test_that("a statistic a contrasts table cannot print is refused", {
  expect_error(apa_inline(contrast_table(), stats = "bf"), "pd")
  expect_error(apa_inline(contrast_table(), stats = "bf"), "rope")
})

test_that("digits, digits_prob and leading_zero reach the numbers", {
  tab <- contrast_table()
  row <- tab[1, ]
  r <- apa_inline(
    tab, row$contrast,
    digits = 1, digits_prob = 2, leading_zero = FALSE, markup = "md"
  )
  expect_identical(
    r$full_result,
    paste0(
      expected_estimate(row, "md", digits = 1, leading_zero = FALSE), ", ",
      apa_pd(row$pd, 2, markup = "md", symbol = TRUE)
    )
  )
})

# ---- addressing ----------------------------------------------------------

test_that("a contrast row is addressed by its contrast string", {
  tab <- contrast_table()
  r <- apa_inline(tab, "cyl_f6 - cyl_f8")
  expect_identical(nrow(r$table), 1L)
  expect_identical(r$table$contrast, "cyl_f6 - cyl_f8")
  expect_error(apa_inline(tab, "cyl_f4 - cyl_f5"), "cyl_f4 - cyl_f6")
  expect_error(apa_inline(tab, "cyl_f4", op = "=~"), "structural-equation")
})

test_that("a contrast that repeats across by groups needs group", {
  tab <- contrast_table("by")
  expect_error(apa_inline(tab, "cyl_f4 - cyl_f6"), "2 rows match")
  expect_error(apa_inline(tab, "cyl_f4 - cyl_f6"), "differ by group")
  r <- apa_inline(tab, "cyl_f4 - cyl_f6", group = "manual")
  expect_identical(r$table$group, "manual")
  expect_identical(r$table$am_f, "manual")
  all_manual <- apa_inline(tab, group = "manual")
  expect_identical(nrow(all_manual$table), 3L)
  expect_error(apa_inline(tab, group = "semi"), "auto")
})

test_that("a means row is addressed by its emmeans label", {
  tab <- contrast_table("means")
  expect_identical(apa_inline(tab, "cyl_f6")$table$cyl_f, "6")
})

# ---- default method ------------------------------------------------------

test_that("the default method passes route arguments through", {
  g <- test_emm_grid("pairs")
  expect_identical(
    apa_inline(g, "cyl_f4 - cyl_f6", ci = "eti"),
    apa_inline(apa_tidy(g, ci = "eti"), "cyl_f4 - cyl_f6")
  )
  expect_identical(
    apa_inline(g, "cyl_f4 - cyl_f6", centrality = "mean")$table$estimate,
    apa_tidy(g, centrality = "mean")$estimate[1]
  )
})

# ---- apa_print -----------------------------------------------------------

test_that("apa_print() on a stored contrasts table names rows as papaja does", {
  skip_if_not_installed("papaja")
  tab <- contrast_table()
  r <- papaja::apa_print(tab)
  expect_identical(
    names(r$full_result),
    c("cyl_f4_cyl_f6", "cyl_f4_cyl_f8", "cyl_f6_cyl_f8")
  )
  expect_identical(
    papaja::apa_print(tab, "cyl_f4 - cyl_f8"),
    apa_inline(tab, "cyl_f4 - cyl_f8")
  )
})

test_that("a Bayesian emmGrid still reaches papaja's own apa_print()", {
  skip_if_not_installed("papaja")
  method <- utils::getS3method(
    "apa_print", "emmGrid",
    envir = asNamespace("papaja")
  )
  expect_identical(environmentName(environment(method)), "papaja")
})

# ---- modelbased tables ---------------------------------------------------

test_that("a modelbased contrast prints with its own interval label", {
  x <- fixture("mb_contrasts")
  tab <- apa_tidy(x)
  row <- tab[1, ]
  r <- apa_inline(x, "6 - 4", markup = "md")
  expect_identical(r$estimate, expected_estimate(row, "md", label = "CrI"))
  expect_identical(
    r$statistic,
    paste0(
      apa_pd(row$pd, 3, markup = "md", symbol = TRUE), ", ",
      apa_prob(row$rope_pct, percent = TRUE, markup = "md"), " in ROPE"
    )
  )
  hdi <- apa_inline(fixture("mb_contrasts_hdi"), "6 - 4", markup = "md")
  expect_match(hdi$estimate, "95% HDI [", fixed = TRUE)
  expect_error(apa_inline(x, "4 - 6"), "6 - 4")
})

test_that("a modelbased contrast within by groups is addressed by group", {
  x <- fixture("mb_contrasts_by")
  expect_error(apa_inline(x, "6 - 4"), "differ by group")
  r <- apa_inline(x, "6 - 4", group = "manual")
  expect_identical(r$table$am_f, "manual")
  expect_identical(apa_inline(fixture("mb_means"), "8")$table$cyl_f, "8")
})

test_that("apa_print() on a modelbased table is its tidy table's", {
  skip_if_not_installed("papaja")
  for (name in c("mb_contrasts", "mb_means")) {
    x <- fixture(name)
    expect_identical(
      papaja::apa_print(x),
      papaja::apa_print(apa_tidy(x)),
      label = name
    )
  }
  x <- fixture("mb_contrasts")
  expect_named(papaja::apa_print(x)$full_result, c("6_4", "8_4", "8_6"))
})
