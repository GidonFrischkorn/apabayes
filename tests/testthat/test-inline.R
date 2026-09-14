# Tests for apa_inline() (spec-apa_inline.md): the strings.
#
# No expected string is typed. Each is composed in the test from the
# fixture row through the format layer, or reproduced by a seed helper
# from helper-seeds.R on the same numbers. The fixtures are tidy tables
# built once from the setup.R fits (local/data-raw/fixtures-tidy.R), so
# everything here except the live-fit tests at the end runs on CRAN.

# The row of a fixture as a one-row data frame, by term.
row_of <- function(x, term) {
  as.data.frame(x[x$term == term, ])
}

# A parameters table with every optional column filled, so that the
# order of the statistic parts can be asserted on one row.
full_row_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("b_x", "b_z"), label = c("x", "z"),
      estimate = c(0.3, -0.1), ci_low = c(0.1, -0.4),
      ci_high = c(0.5, 0.2), pd = c(0.98, 0.6),
      rope_pct = c(0.02, 0.4), bf = c(20.86, 0.3), p = c(0.012, 0.4),
      component = "conditional", effects = "fixed"
    ),
    type = "parameters", centrality = "median", ci_method = "eti",
    ci_level = 0.95
  )
}

# ---- parameters ----------------------------------------------------------

test_that("a brms coefficient prints symbol, estimate, interval and pd", {
  t <- fixture("tidy_brms_full")
  r <- apa_inline(t, "wt")
  row <- row_of(t, "b_wt")
  est <- paste0(
    "*b* = ", apa_num(row$estimate), ", ",
    apa_ci(row$ci_low, row$ci_high, level = row$ci_level, label = "CrI")
  )
  expect_identical(r$estimate, est)
  expect_identical(r$statistic, apa_pd(row$pd, symbol = TRUE))
  expect_identical(r$full_result, paste0(est, ", ", r$statistic))
  expect_identical(nrow(r$table), 1L)
  expect_identical(r$table$term, "b_wt")
  expect_identical(attr(r$table, "ci_method"), attr(t, "ci_method"))
  expect_true(withVisible(apa_inline(t, "wt"))$visible)
})

test_that("the default symbol is *b* on coefficients only", {
  t <- fixture("tidy_brms_full")
  expect_match(apa_inline(t, "sigma")$estimate, "^[^*]")
  expect_match(apa_inline(t, "(Intercept)")$estimate, "^\\*b\\* = ")
  mixed <- fixture("tidy_brms_mixed")
  sd_row <- mixed$term[mixed$effects %in% "random"][1]
  expect_match(apa_inline(mixed, sd_row)$estimate, "^[^*]")
  lav <- fixture("tidy_lavaan_std")
  expect_match(apa_inline(lav, "visual", "x2")$estimate, "^[^*]")
  draws <- apa_tidy(fixture("draws_brms"), diagnostics = FALSE)
  expect_match(apa_inline(draws, "b_wt")$estimate, "^[^*]")
})

test_that("symbol = FALSE drops it and a string replaces it", {
  t <- fixture("tidy_brms_full")
  row <- row_of(t, "b_wt")
  expect_match(
    apa_inline(t, "wt", symbol = FALSE)$estimate,
    paste0("^", apa_num(row$estimate))
  )
  expect_match(apa_inline(t, "wt", symbol = "β")$estimate, "^\\*β\\* = ")
  expect_match(apa_inline(t, "sigma", symbol = "s")$estimate, "^\\*s\\* = ")
  expect_error(
    apa_inline(t, "wt", symbol = 1),
    "`symbol` must be NULL, FALSE or a single string"
  )
})

test_that("interval, ci_label and the row's method control the interval", {
  t <- fixture("tidy_brms_full")
  row <- row_of(t, "b_wt")
  expect_identical(
    apa_inline(t, "wt", interval = FALSE)$estimate,
    paste0("*b* = ", apa_num(row$estimate))
  )
  bare <- apa_inline(t, "wt", ci_label = NULL)$estimate
  expect_match(bare, ", [", fixed = TRUE)
  expect_false(grepl("CrI", bare))
  hdi <- apa_inline(t, "wt", ci_label = "HDI")$estimate
  expect_match(hdi, "95% HDI [", fixed = TRUE)
  lav <- fixture("tidy_lavaan_std")
  wald <- apa_inline(lav, "visual", "x2")$estimate
  expect_match(wald, "95% CI [", fixed = TRUE)
  expect_error(apa_inline(t, "wt", ci_label = 1), "`ci_label`")
  expect_error(apa_inline(t, "wt", interval = NA), "`interval`")
})

test_that("a row without an interval prints the estimate alone", {
  t <- fixture("tidy_brms_full")
  t$ci_low[2] <- NA
  row <- row_of(t, "b_wt")
  expect_identical(
    apa_inline(t, "wt")$estimate,
    paste0("*b* = ", apa_num(row$estimate))
  )
})

test_that("a row with an interval but no level prints bare brackets", {
  t <- fixture("tidy_brms_full")
  t$ci_level[2] <- NA
  attr(t, "ci_level") <- NA_real_
  row <- row_of(t, "b_wt")
  bare <- paste0(
    apa_num(row$estimate), ", ",
    apa_ci(row$ci_low, row$ci_high, label = NULL)
  )
  expect_identical(apa_inline(t, "wt", symbol = FALSE)$estimate, bare)
  # An explicit label does not restore one: no level, no label.
  expect_identical(
    apa_inline(t, "wt", symbol = FALSE, ci_label = "HDI")$estimate,
    bare
  )
})

test_that("leading_zero = 'auto' follows std; TRUE and FALSE force it", {
  std <- fixture("tidy_blavaan_std")
  row <- row_of(std, "visual~~textual")
  auto <- apa_inline(std, "visual", "textual", op = "~~")
  expect_identical(
    auto$estimate,
    paste0(
      apa_num(row$estimate, leading_zero = FALSE), ", ",
      apa_ci(row$ci_low, row$ci_high, leading_zero = FALSE)
    )
  )
  forced <- apa_inline(
    std, "visual", "textual",
    op = "~~", leading_zero = TRUE
  )
  expect_identical(
    forced$estimate,
    paste0(apa_num(row$estimate), ", ", apa_ci(row$ci_low, row$ci_high))
  )
  t <- fixture("tidy_brms_full")
  expect_match(apa_inline(t, "wt")$estimate, "= −5\\.")
  brow <- row_of(t, "b_wt")
  expect_match(
    apa_inline(t, "wt", leading_zero = FALSE)$estimate,
    apa_num(brow$estimate, leading_zero = FALSE),
    fixed = TRUE
  )
  expect_error(apa_inline(t, "wt", leading_zero = "yes"), "`leading_zero`")
})

test_that("digits and digits_prob change the right pieces", {
  t <- fixture("tidy_brms_full")
  row <- row_of(t, "b_wt")
  r <- apa_inline(t, "wt", digits = 3, digits_prob = 2)
  expect_identical(
    r$estimate,
    paste0(
      "*b* = ", apa_num(row$estimate, 3), ", ",
      apa_ci(row$ci_low, row$ci_high, digits = 3)
    )
  )
  expect_identical(r$statistic, apa_pd(row$pd, digits = 2, symbol = TRUE))
  expect_error(apa_inline(t, "wt", digits = -1), "whole number")
  expect_error(
    apa_inline(t, "wt", digits_prob = 0),
    "whole number of 1 or more"
  )
})

test_that("stats selects parts in a fixed order; unknown names abort", {
  t <- full_row_table()
  r <- apa_inline(t, "x")
  expect_identical(
    r$statistic,
    paste(
      apa_pd(0.98, symbol = TRUE),
      paste0(apa_prob(0.02, percent = TRUE), " in ROPE"),
      apa_bf(20.86, symbol = TRUE),
      apa_p(0.012, symbol = TRUE),
      sep = ", "
    )
  )
  expect_identical(
    apa_inline(t, "x", stats = c("p", "pd"))$statistic,
    paste(apa_pd(0.98, symbol = TRUE), apa_p(0.012, symbol = TRUE), sep = ", ")
  )
  only <- apa_inline(t, "x", stats = character())
  expect_identical(only$statistic, NA_character_)
  expect_identical(only$full_result, only$estimate)
  expect_error(apa_inline(t, "x", stats = "er"), "cannot print")
  expect_error(
    apa_inline(t, "x", stats = 1),
    "`stats` must be NULL or a character vector"
  )
  expect_error(
    apa_inline(fixture("diag_brms_full"), "b_wt", stats = "pd"),
    "prints no statistic"
  )
})

test_that("bf and bf_direction reach apa_bf()", {
  t <- full_row_table()
  expect_match(
    apa_inline(t, "x", stats = "bf", bf = "sci")$statistic,
    apa_bf(20.86, style = "sci", symbol = TRUE),
    fixed = TRUE
  )
  expect_identical(
    apa_inline(t, "x", stats = "bf", bf_direction = "01")$statistic,
    apa_bf(20.86, direction = "01", symbol = TRUE)
  )
  expect_error(apa_inline(t, "x", bf = "wide"), "`bf`")
  expect_error(apa_inline(t, "x", bf_direction = "11"), "`bf_direction`")
})

test_that("all four markup targets are honoured", {
  t <- fixture("tidy_brms_full")
  row <- row_of(t, "b_wt")
  for (target in c("md", "latex", "typst", "plain")) {
    r <- apa_inline(t, "wt", markup = target)
    expect_identical(r$markup, target)
    expect_identical(
      r$full_result,
      paste0(
        stat_string(
          markup("b", target, italic = TRUE),
          apa_num(row$estimate, markup = target)
        ),
        ", ", apa_ci(row$ci_low, row$ci_high, markup = target),
        ", ", apa_pd(row$pd, symbol = TRUE, markup = target)
      )
    )
  }
  expect_match(apa_inline(t, "wt", markup = "plain")$full_result, "^b = -")
  expect_error(apa_inline(t, "wt", markup = "html"), "`markup`")
})

test_that("the miniQ seed helpers are reproduced on a blavaan std row", {
  std <- fixture("tidy_blavaan_std")
  row <- row_of(std, "visual~~textual")
  r <- apa_inline(std, "visual", "textual", op = "~~")
  seed <- paste0(
    seed_miniq$fmt_r(row$estimate), ", 95% CrI [",
    seed_miniq$fmt_r(row$ci_low), ", ", seed_miniq$fmt_r(row$ci_high),
    "], *pd* ", seed_miniq$fmt_pd(row$pd)
  )
  expect_identical(r$full_result, seed)
  # The seed writes an ASCII hyphen in a negative exponent where apabayes
  # writes the minus sign (decision 8), so the finite case is compared as
  # BF01, whose exponent is positive, exactly as test-format-bf.R does.
  hyp <- fixture("tidy_hypotheses")
  point <- as.data.frame(hyp[!hyp$directional, ])
  r <- apa_inline(hyp, point$hypothesis, bf = "sci", bf_direction = "01")
  expect_identical(
    r$statistic,
    paste0("*BF*~01~ = ", seed_miniq$fmt_bf(1 / point$bf10))
  )
  inf <- as.data.frame(hyp[hyp$directional, ])
  expect_identical(
    apa_inline(hyp, inf$hypothesis, bf = "sci", markup = "latex")$statistic,
    paste0("*BF*~10~ = ", seed_miniq$fmt_bf(inf$bf10))
  )
})

test_that("the SDVWM fmt_r_full helper is reproduced on a lavaan fit", {
  fit <- test_lavaan_fit("cfa")
  t <- apa_tidy(fit, standardize = TRUE)
  paths <- list(c("visual", "textual", "~~"), c("visual", "x2", "=~"))
  for (path in paths) {
    seed <- seed_sdvwm$fmt_r_full(fit, path[1], path[2], op = path[3])
    got <- apa_inline(t, path[1], path[2], op = path[3])$full_result
    expect_identical(got, seed)
  }
  expect_identical(
    apa_inline(t, "textual", "visual", op = "~~", interval = FALSE)$full_result,
    seed_sdvwm$fmt_r_full(fit, "visual", "textual", ci = FALSE)
  )
})

test_that("no decision word appears in any string", {
  strings <- c(
    apa_inline(fixture("tidy_brms_full"))$full_result,
    apa_inline(fixture("tidy_blavaan_std"))$full_result,
    apa_inline(
      fixture("tidy_hypotheses"),
      stats = c("bf", "er", "post_prob")
    )$full_result,
    apa_inline(fixture("diag_brms_full"))$full_result,
    apa_inline(full_row_table())$full_result
  )
  for (word in decision_words) {
    expect_false(any(grepl(word, strings, ignore.case = TRUE)), info = word)
  }
})

# ---- hypotheses ----------------------------------------------------------

test_that("a hypotheses row prints the estimate, its own level and the BF", {
  hyp <- fixture("tidy_hypotheses")
  dir <- as.data.frame(hyp[hyp$directional, ])
  point <- as.data.frame(hyp[!hyp$directional, ])
  r <- apa_inline(hyp, dir$hypothesis)
  expect_identical(
    r$estimate,
    paste0(
      apa_num(dir$estimate), ", ",
      apa_ci(dir$ci_low, dir$ci_high, level = dir$ci_level)
    )
  )
  expect_match(r$estimate, "90% CrI", fixed = TRUE)
  expect_identical(r$statistic, apa_bf(dir$bf10, symbol = TRUE))
  p <- apa_inline(hyp, point$hypothesis)
  expect_match(p$estimate, "95% CrI", fixed = TRUE)
  expect_identical(p$statistic, apa_bf(point$bf10, symbol = TRUE))
  both <- apa_inline(hyp)
  expect_length(both$full_result, 2L)
})

test_that("stats on a hypotheses row adds the ER and the probability", {
  hyp <- fixture("tidy_hypotheses")
  point <- as.data.frame(hyp[!hyp$directional, ])
  r <- apa_inline(hyp, point$hypothesis, stats = c("post_prob", "er", "bf"))
  expect_identical(
    r$statistic,
    paste(
      apa_bf(point$bf10, symbol = TRUE),
      apa_er(point$evid_ratio, symbol = TRUE),
      paste0("*P*(H) = ", apa_prob(point$post_prob)),
      sep = ", "
    )
  )
  plain <- apa_inline(hyp, point$hypothesis, stats = "er", markup = "plain")
  expect_identical(
    plain$statistic,
    apa_er(point$evid_ratio, symbol = TRUE, markup = "plain")
  )
})

test_that("a hypothesis without a Bayes factor prints the estimate alone", {
  hyp <- fixture("tidy_hypotheses")
  hyp$bf10 <- NA_real_
  hyp$evid_ratio <- NA_real_
  r <- apa_inline(hyp, hyp$hypothesis[1])
  expect_identical(r$statistic, NA_character_)
  expect_identical(r$full_result, r$estimate)
  with_symbol <- apa_inline(hyp, hyp$hypothesis[1], symbol = "d")
  expect_match(with_symbol$estimate, "^\\*d\\* = ")
})

# ---- diagnostics ---------------------------------------------------------

test_that("a diagnostics row prints R-hat and both ESS", {
  d <- fixture("diag_brms_full")
  row <- row_of(d, "b_wt")
  r <- apa_inline(d, "b_wt")
  expect_identical(r$estimate, NA_character_)
  expect_identical(
    r$statistic,
    apa_rhat_ess(row$rhat, row$ess_bulk, row$ess_tail)
  )
  expect_identical(r$full_result, r$statistic)
  expect_identical(
    apa_inline(d, "b_wt", digits = 3, markup = "plain")$full_result,
    apa_rhat_ess(
      row$rhat, row$ess_bulk, row$ess_tail,
      digits = 3, markup = "plain"
    )
  )
})

# ---- types not yet built -------------------------------------------------

test_that("a table kind the layer does not build yet is refused by name", {
  fit <- test_lavaan_fit("cfa")
  expect_error(
    apa_inline(apa_tidy_sem_fit(fit)),
    'does not yet report a table of type "sem_fit"'
  )
})

# ---- object handling -----------------------------------------------------

test_that("a stored table gives the same string after a round trip", {
  std <- fixture("tidy_blavaan_std")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(std, path)
  back <- readRDS(path)
  expect_identical(
    apa_inline(back, "visual", "x2", op = "=~")$full_result,
    apa_inline(std, "visual", "x2", op = "=~")$full_result
  )
})

test_that("the tidy method rejects dots and an invalid table", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_inline(t, "wt", foo = 1), "must be empty")
  expect_error(apa_inline(t, "wt", standardize = TRUE), "must be empty")
  broken <- t
  attr(broken, "type") <- "nonsense"
  expect_error(apa_inline(broken, "wt"), "type")
})

test_that("bare inline knitr code renders the string", {
  skip_if_not_installed("knitr")
  env <- new.env()
  env$t <- fixture("tidy_brms_full")
  text <- "`r apa_inline(t, 'wt')`"
  out <- knitr::knit(text = text, quiet = TRUE, envir = env)
  expect_identical(out, apa_inline(env$t, "wt")$full_result)
})

# ---- the default method --------------------------------------------------

test_that("the default method extracts and then reports (lavaan)", {
  fit <- test_lavaan_fit("cfa")
  direct <- apa_inline(
    fit, "visual", "textual",
    op = "~~", standardize = TRUE
  )
  std <- apa_tidy(fit, standardize = TRUE)
  via_tidy <- apa_inline(std, "visual", "textual", op = "~~")
  expect_identical(direct$full_result, via_tidy$full_result)
  expect_identical(direct$table, via_tidy$table)
  expect_true(withVisible(apa_inline(fit, "visual", "x2"))$visible)
  unstd <- apa_inline(fit, "visual", "x2", op = "=~")
  expect_false(isTRUE(unstd$table$std))
})

test_that("the default method forwards route arguments (brms, off CRAN)", {
  fit <- test_brms_fit("full")
  t <- apa_tidy(fit)
  expect_identical(
    apa_inline(fit, "wt")$full_result,
    apa_inline(t, "wt")$full_result
  )
  hdi <- apa_inline(fit, "wt", ci = "hdi")
  expect_match(hdi$estimate, "95% HDI [", fixed = TRUE)
  expect_identical(attr(hdi$table, "ci_method"), "hdi")
  h <- brms::hypothesis(fit, "wt < 0")
  expect_identical(
    apa_inline(h)$full_result,
    apa_inline(apa_tidy(h))$full_result
  )
})

test_that("the default method fails with the extract route's own message", {
  expect_error(apa_inline(lm(mpg ~ wt, mtcars), "wt"), "lm")
})
