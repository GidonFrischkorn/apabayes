# apa_inline() and apa_print() on the comparison tables
# (local/specs/spec-apa_inline-comparisons.md).
#
# The tables come from the routes on CRAN-safe objects: the simulated
# LOO trio and the lm Bayes-factor trio. Numbers reach the expected
# strings through the format layer's own functions, which *are* the rule
# (decision 8); the words and symbols around them are the decided
# literals of the spec.

# The two helpers read setup.R's fixtures, which lintr does not see.
# nolint start: object_usage_linter.
loo_table <- function(weights = NULL) {
  apa_tidy(loo::loo_compare(test_loo_list()), weights = weights)
}

bf_table <- function(b = test_bf_models_lm()) {
  apa_tidy(b)
}
# nolint end

# ---- types ---------------------------------------------------------------

test_that("the comparison types are built", {
  expect_true(all(c("loo", "bf_models") %in% inline_types()))
  expect_identical(inline_default_stats("loo"), "elpd_diff")
  expect_identical(inline_default_stats("bf_models"), "bf")
})

# ---- loo -----------------------------------------------------------------

test_that("a loo row prints the ELPD difference and its SE by default", {
  tab <- loo_table()
  for (m in c("md", "latex", "plain")) {
    r <- apa_inline(tab, markup = m)
    se <- if (m == "plain") "SE" else "*SE*"
    expected <- paste0(
      symbol("delta", m), "ELPD = ", apa_num(tab$elpd_diff, markup = m),
      ", ", se, " = ", apa_num(tab$se_diff, markup = m)
    )
    expect_identical(r$full_result, expected, label = m)
    expect_identical(r$statistic, expected)
    expect_identical(r$estimate, rep(NA_character_, nrow(tab)))
  }
})

test_that("the reference row prints its zero difference", {
  r <- apa_inline(loo_table(), "good", markup = "md")
  expect_identical(r$full_result, "ΔELPD = 0.00, *SE* = 0.00")
})

test_that("every loo statistic prints in vocabulary order", {
  loos <- test_loo_list()
  w <- loo::loo_model_weights(loos, method = "pseudobma", BB = FALSE)
  tab <- loo_table(weights = w)
  r <- apa_inline(
    tab, "shifted",
    stats = c("weight", "looic", "elpd", "p_loo", "elpd_diff"),
    markup = "md"
  )
  row <- tab[tab$model == "shifted", ]
  expected <- paste0(
    "ΔELPD = ", apa_num(row$elpd_diff, markup = "md"),
    ", *SE* = ", apa_num(row$se_diff, markup = "md"),
    ", ELPD = ", apa_num(row$elpd, markup = "md"),
    ", *SE* = ", apa_num(row$se_elpd, markup = "md"),
    ", *p*~loo~ = ", apa_num(row$p_loo, markup = "md"),
    ", LOOIC = ", apa_num(row$looic, markup = "md"),
    ", ", stat_string("*w*", apa_prob(row$weight, 3))
  )
  expect_identical(r$full_result, expected)
})

test_that("the plain target spells the loo symbols in ASCII", {
  tab <- loo_table()
  r <- apa_inline(
    tab, "wide",
    stats = c("elpd_diff", "p_loo"), markup = "plain"
  )
  row <- tab[tab$model == "wide", ]
  expect_identical(
    r$full_result,
    paste0(
      "DeltaELPD = ", apa_num(row$elpd_diff, markup = "plain"),
      ", SE = ", apa_num(row$se_diff, markup = "plain"),
      ", ploo = ", apa_num(row$p_loo, markup = "plain")
    )
  )
})

test_that("a missing weight or SE drops that part alone", {
  tab <- loo_table()
  r <- apa_inline(tab, "wide", stats = c("elpd_diff", "weight"), markup = "md")
  row <- tab[tab$model == "wide", ]
  expect_false(grepl("*w*", r$full_result, fixed = TRUE))
  tab$se_diff <- NA_real_
  r <- apa_inline(tab, "wide", markup = "md")
  expect_identical(
    r$full_result,
    paste0("ΔELPD = ", apa_num(row$elpd_diff, markup = "md"))
  )
  tab$elpd_diff <- NA_real_
  expect_identical(apa_inline(tab, "wide")$full_result, NA_character_)
})

test_that("digits, digits_prob and leading_zero reach the loo numbers", {
  w <- c(good = 0.4567, shifted = 0.3333, wide = 0.21)
  tab <- loo_table(weights = w)
  r <- apa_inline(
    tab, "wide",
    stats = c("elpd_diff", "weight"), digits = 1, digits_prob = 2,
    leading_zero = FALSE, markup = "md"
  )
  row <- tab[tab$model == "wide", ]
  expect_identical(
    r$full_result,
    paste0(
      "ΔELPD = ", apa_num(row$elpd_diff, 1, FALSE, markup = "md"),
      ", *SE* = ", apa_num(row$se_diff, 1, FALSE, markup = "md"),
      ", *w* = ", apa_prob(0.21, 2)
    )
  )
})

test_that("a statistic a loo table cannot print is refused", {
  expect_error(apa_inline(loo_table(), stats = "bf"), "elpd_diff")
})

# ---- bf_models -----------------------------------------------------------

test_that("a bf_models row prints its Bayes factor against the denominator", {
  tab <- bf_table()
  for (m in c("md", "latex", "plain")) {
    r <- apa_inline(tab, markup = m)
    expect_identical(
      r$full_result,
      apa_bf(tab$bf, markup = m, symbol = TRUE),
      label = m
    )
    expect_identical(r$estimate, rep(NA_character_, nrow(tab)))
  }
  expect_identical(
    apa_inline(tab, "1", markup = "md")$full_result,
    "*BF*~10~ = 1.00"
  )
  expect_identical(
    apa_inline(tab, "1", markup = "plain")$full_result,
    "BF10 = 1.00"
  )
})

test_that("bf_direction = '01' inverts the Bayes factor and its log", {
  tab <- bf_table()
  r <- apa_inline(
    tab, "wt",
    stats = c("bf", "log_bf"), bf_direction = "01", markup = "md"
  )
  row <- tab[tab$model == "wt", ]
  expect_identical(
    r$full_result,
    paste0(
      apa_bf(row$bf, "01", markup = "md", symbol = TRUE),
      ", log(*BF*~01~) = ", apa_num(-row$log_bf, markup = "md")
    )
  )
})

test_that("every bf_models statistic prints in vocabulary order", {
  tab <- bf_table()
  r <- apa_inline(
    tab, "wt",
    stats = c("post_prob", "log_bf", "bf"), markup = "md"
  )
  row <- tab[tab$model == "wt", ]
  expect_identical(
    r$full_result,
    paste0(
      apa_bf(row$bf, markup = "md", symbol = TRUE),
      ", log(*BF*~10~) = ", apa_num(row$log_bf, markup = "md"),
      ", ", stat_string("*P*(M | D)", apa_prob(row$post_prob, 3))
    )
  )
  plain <- apa_inline(
    tab, "wt",
    stats = c("log_bf", "post_prob"), markup = "plain"
  )
  expect_identical(
    plain$full_result,
    paste0(
      "log(BF10) = ", apa_num(row$log_bf, markup = "plain"),
      ", ", stat_string("P(M | D)", apa_prob(row$post_prob, 3))
    )
  )
})

test_that("a Bayes factor that overflows prints its log instead", {
  b <- test_bf_models_lm()
  b$log_BF <- c(800, 790, 0)
  tab <- bf_table(b)
  r <- apa_inline(tab, "wt + am", markup = "md")
  expect_identical(
    r$full_result,
    paste0("log(*BF*~10~) = ", apa_num(800, markup = "md"))
  )
  both <- apa_inline(tab, "wt + am", stats = c("bf", "log_bf"), markup = "md")
  expect_identical(both$full_result, r$full_result)
})

test_that("a Bayes factor that underflows to 0 prints its log instead", {
  b <- test_bf_models_lm()
  b$log_BF <- c(-800, 2, 0)
  r <- apa_inline(bf_table(b), "wt + am", markup = "plain")
  expect_identical(
    r$full_result,
    paste0("log(BF10) = ", apa_num(-800, markup = "plain"))
  )
})

test_that("bf = 'sci' reaches apa_bf()", {
  tab <- bf_table()
  row <- tab[tab$model == "wt", ]
  expect_identical(
    apa_inline(tab, "wt", bf = "sci", markup = "md")$full_result,
    apa_bf(row$bf, style = "sci", markup = "md", symbol = TRUE)
  )
})

# ---- bf_models: error -----------------------------------------------------

test_that("the bf_models inline vocabulary gains error, default stays bf", {
  expect_identical(
    inline_stats_vocabulary("bf_models"),
    c("bf", "error", "log_bf", "post_prob")
  )
  expect_identical(inline_default_stats("bf_models"), "bf")
})

test_that("stats = 'error' alone is refused: it qualifies the Bayes factor", {
  skip_if_not_installed("BayesFactor")
  tab <- apa_tidy(fixture("bf_anova"))
  expect_error(apa_inline(tab, stats = "error"), "error")
  expect_error(apa_inline(tab, stats = "error"), "qualifies")
})

test_that("the error rides along with the Bayes factor, in a percent", {
  skip_if_not_installed("BayesFactor")
  tab <- apa_tidy(fixture("bf_anova"))
  row <- tab[tab$model == "am_f + cyl_f", ]
  r <- apa_inline(tab, "am_f + cyl_f", stats = c("bf", "error"), markup = "md")
  expected <- paste0(
    apa_bf(row$bf, markup = "md", symbol = TRUE),
    " ± ", apa_prob(row$error, percent = TRUE, markup = "md")
  )
  expect_identical(r$full_result, expected)

  small <- tab[tab$model == "am_f", ]
  r2 <- apa_inline(tab, "am_f", stats = c("bf", "error"), markup = "md")
  expected2 <- paste0(
    apa_bf(small$bf, markup = "md", symbol = TRUE),
    " ± ", apa_prob(small$error, percent = TRUE, markup = "md")
  )
  expect_identical(r2$full_result, expected2)
  expect_identical(
    apa_prob(small$error, percent = TRUE, markup = "md"), "< 0.1%"
  )
})

test_that("a zero error prints literally as 0%, and NA adds nothing", {
  skip_if_not_installed("BayesFactor")
  cont_tab <- apa_tidy(fixture("bf_contingency"))
  row <- cont_tab[!cont_tab$denominator, ]
  expect_identical(row$error, 0)
  r <- apa_inline(
    cont_tab, row$model[1],
    stats = c("bf", "error"), markup = "md"
  )
  expected <- paste0(apa_bf(row$bf[1], markup = "md", symbol = TRUE), " ± 0%")
  expect_identical(r$full_result, expected)

  anova_tab <- apa_tidy(fixture("bf_anova"))
  denom_row <- anova_tab[anova_tab$denominator, ]
  expect_true(is.na(denom_row$error))
  r_denom <- apa_inline(
    anova_tab, denom_row$model[1],
    stats = c("bf", "error"), markup = "md"
  )
  expected_denom <- apa_bf(denom_row$bf[1], markup = "md", symbol = TRUE)
  expect_identical(r_denom$full_result, expected_denom)
})

test_that("bf_direction = '01' keeps the same error", {
  skip_if_not_installed("BayesFactor")
  tab <- apa_tidy(fixture("bf_anova"))
  row <- tab[tab$model == "am_f + cyl_f", ]
  r <- apa_inline(
    tab, "am_f + cyl_f",
    stats = c("bf", "error"), bf_direction = "01", markup = "md"
  )
  expected <- paste0(
    apa_bf(row$bf, "01", markup = "md", symbol = TRUE),
    " ± ", apa_prob(row$error, percent = TRUE, markup = "md")
  )
  expect_identical(r$full_result, expected)
})

test_that("an overflowing row prints its log Bayes factor with the error", {
  skip_if_not_installed("BayesFactor")
  tab <- apa_tidy(fixture("bf_overflow"))
  row <- tab[!tab$denominator, ]
  r <- apa_inline(tab, row$model[1], stats = c("bf", "error"), markup = "md")
  expected <- paste0(
    "log(*BF*~10~) = ", apa_num(row$log_bf[1], markup = "md"),
    " ± ", apa_prob(row$error[1], percent = TRUE, markup = "md")
  )
  expect_identical(r$full_result, expected)
})

# ---- bf_inclusion -----------------------------------------------------------

test_that("the bf_inclusion type is built", {
  expect_true("bf_inclusion" %in% inline_types())
  expect_identical(
    inline_stats_vocabulary("bf_inclusion"),
    c("bf", "p_prior", "p_posterior")
  )
  expect_identical(inline_default_stats("bf_inclusion"), "bf")
})

test_that("a bf_inclusion row prints its inclusion Bayes factor by default", {
  tab <- apa_tidy(fixture("inc_anova"))
  row <- tab[tab$term == "am_f", ]
  r <- apa_inline(tab, "am_f", markup = "md")
  expected <- stat_string(
    markup("BF", "md", italic = TRUE, subscript = "incl"),
    apa_bf(row$bf, markup = "md")
  )
  expect_identical(r$full_result, expected)
  expect_identical(r$estimate, NA_character_)
})

test_that("bf_direction = '01' prints the exclusion Bayes factor", {
  tab <- apa_tidy(fixture("inc_anova"))
  row <- tab[tab$term == "am_f", ]
  r <- apa_inline(tab, "am_f", bf_direction = "01", markup = "md")
  expected <- stat_string(
    markup("BF", "md", italic = TRUE, subscript = "excl"),
    apa_bf(1 / row$bf, markup = "md")
  )
  expect_identical(r$full_result, expected)
})

test_that("p_prior and p_posterior print with apa_prob()", {
  tab <- apa_tidy(fixture("inc_anova"))
  row <- tab[tab$term == "am_f", ]
  r <- apa_inline(
    tab, "am_f",
    stats = c("p_prior", "p_posterior"), markup = "md"
  )
  expected <- paste0(
    stat_string(
      paste0(markup("P", "md", italic = TRUE), "(incl)"),
      apa_prob(row$p_prior, 3, markup = "md")
    ),
    ", ",
    stat_string(
      paste0(markup("P", "md", italic = TRUE), "(incl | D)"),
      apa_prob(row$p_posterior, 3, markup = "md")
    )
  )
  expect_identical(r$full_result, expected)
})

test_that("an NA log Bayes factor aborts naming the term and 'every model'", {
  tab <- apa_tidy(fixture("inc_random"))
  expect_error(apa_inline(tab, "id"), "id")
  expect_error(apa_inline(tab, "id"), "every model")
  expect_no_error(apa_inline(tab, "id", stats = "p_prior"))
})

test_that("an infinite log Bayes factor aborts naming the term and 'rounded'", {
  tab <- apa_tidy(fixture("inc_inf"))
  expect_error(apa_inline(tab, "x"), "x")
  expect_error(apa_inline(tab, "x"), "rounded")
})

# ---- addressing ----------------------------------------------------------

test_that("a loo row is addressed by its model", {
  tab <- loo_table()
  expect_identical(apa_inline(tab, "wide")$table$model, "wide")
  expect_error(apa_inline(tab, "narrow"), "good")
})

test_that("a bf_models row is addressed by model, then by name", {
  tab <- bf_table()
  expect_identical(apa_inline(tab, "wt")$table$name, "m0")
  expect_identical(apa_inline(tab, "m0")$table$model, "wt")
  expect_error(apa_inline(tab, "am"), "wt \\+ am")
})

test_that("identical right-hand sides are told apart by name", {
  b <- test_bf_models_lm()
  b$Model[2] <- b$Model[1]
  tab <- bf_table(b)
  expect_error(apa_inline(tab, "wt + am"), "2 rows match")
  expect_identical(apa_inline(tab, "m0")$table$name, "m0")
  expect_identical(apa_inline(tab, "m1")$table$name, "m1")
})

test_that("path and group arguments are refused on the comparison types", {
  for (tab in list(loo_table(), bf_table())) {
    expect_error(apa_inline(tab, "x", op = "=~"), "structural-equation")
    expect_error(apa_inline(tab, group = "a"), "no .*group")
  }
})

# ---- default method ------------------------------------------------------

test_that("the default method passes route arguments through", {
  loos <- test_loo_list()
  cmp <- loo::loo_compare(loos)
  w <- loo::loo_model_weights(loos, method = "pseudobma", BB = FALSE)
  expect_identical(
    apa_inline(cmp, "shifted", weights = w, stats = "weight"),
    apa_inline(apa_tidy(cmp, weights = w), "shifted", stats = "weight")
  )
  b <- test_bf_models_lm()
  expect_identical(apa_inline(b, "wt"), apa_inline(apa_tidy(b), "wt"))
})

# ---- apa_print -----------------------------------------------------------

test_that("apa_print() names comparison rows by model", {
  skip_if_not_installed("papaja")
  cmp <- loo::loo_compare(test_loo_list())
  r <- papaja::apa_print(cmp)
  expect_identical(names(r$full_result), cmp$model)
  expect_identical(
    unlist(r$full_result, use.names = FALSE),
    apa_inline(cmp)$full_result
  )
  b <- test_bf_models_lm()
  rb <- papaja::apa_print(b)
  expect_identical(names(rb$full_result), c("wt_am", "wt", "1"))
  expect_identical(papaja::apa_print(b, "wt"), apa_inline(b, "wt"))
})

test_that("in_paren turns the comparison parentheses into brackets", {
  skip_if_not_installed("papaja")
  b <- test_bf_models_lm()
  r <- papaja::apa_print(
    b, "wt",
    stats = c("log_bf", "post_prob"), markup = "plain", in_paren = TRUE
  )
  expect_match(r$full_result, "^log\\[BF10\\] = .*, P\\[M \\| D\\]")
})
