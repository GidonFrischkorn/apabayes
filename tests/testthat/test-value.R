# Tests for apa_value() (local/specs/spec-apa_value.md): the number
# behind the string.
#
# No expected value is typed: every one is read from the table under
# test. The addressing is apa_inline()'s, from the same code, so these
# tests assert that the two agree rather than re-deriving the rules.

test_that("apa_value() returns the addressed value, unformatted", {
  t <- fixture("tidy_brms_full")
  value <- apa_value(t, "wt")
  expect_identical(value, t$estimate[t$term == "b_wt"])
  expect_type(value, "double")
  expect_length(value, 1L)
  expect_null(names(value))
  expect_true(withVisible(apa_value(t, "wt"))$visible)
})

test_that("column selects any column, at its own type", {
  t <- fixture("tidy_brms_full")
  row <- which(t$term == "b_wt")
  expect_identical(apa_value(t, "wt", column = "ci_low"), t$ci_low[row])
  expect_identical(apa_value(t, "wt", column = "ci_high"), t$ci_high[row])
  expect_identical(apa_value(t, "wt", column = "pd"), t$pd[row])
  expect_identical(apa_value(t, "wt", column = "term"), "b_wt")
  expect_type(apa_value(t, "wt", column = "ci_method"), "character")
  expect_identical(apa_value(t, "wt", column = "ci_level"), t$ci_level[row])
})

test_that("no term returns the whole column in the table's row order", {
  t <- fixture("tidy_brms_full")
  expect_identical(apa_value(t), t$estimate)
  expect_identical(apa_value(t, column = "term"), t$term)
})

test_that("the addressing is apa_inline()'s, rule for rule", {
  t <- fixture("tidy_brms_full")
  # label, and the brms class prefix removed
  expect_identical(apa_value(t, "wt"), apa_value(t, "b_wt"))
  lav <- fixture("tidy_lavaan_std")
  path <- apa_value(lav, "visual", "x2", op = "=~")
  expect_identical(path, lav$estimate[lav$term == "visual=~x2"])
  blav <- fixture("tidy_blavaan_std")
  # `~~` is symmetric, every other operator is not
  expect_identical(
    apa_value(blav, "visual", "textual", op = "~~"),
    apa_value(blav, "textual", "visual", op = "~~")
  )
  hyp <- fixture("tidy_hypotheses")
  expect_identical(
    apa_value(hyp, hyp$hypothesis[1], column = "bf10"),
    hyp$bf10[1]
  )
  groups <- fixture("tidy_lavaan_groups")
  group <- groups$group[[1]]
  expect_length(apa_value(groups, column = "estimate", group = group), sum(
    groups$group == group
  ))
})

test_that("a correlation is addressed by its pair, in either order", {
  cor <- apa_tidy(fixture("cor_default"))
  expect_identical(
    apa_value(cor, cor$var1[1], cor$var2[1]),
    apa_value(cor, cor$var2[1], cor$var1[1])
  )
  expect_identical(apa_value(cor, cor$term[1]), cor$estimate[1])
})

test_that("a loo row is addressed by model", {
  skip_if_not_installed("loo")
  t <- apa_tidy(loo::loo_compare(test_loo_list()))
  expect_identical(apa_value(t, t$model[2]), t$elpd_diff[2])
  expect_identical(apa_value(t, t$model[2], column = "se_diff"), t$se_diff[2])
})

test_that("the default column is the value the table's type is about", {
  t <- fixture("tidy_brms_full")
  expect_identical(apa_value(t, "wt"), apa_value(t, "wt", column = "estimate"))
  hyp <- fixture("tidy_hypotheses")
  expect_identical(apa_value(hyp, hyp$hypothesis[1]), hyp$estimate[1])
  bf <- apa_tidy(fixture("inc_anova"))
  expect_identical(apa_value(bf, bf$term[1]), bf$bf[1])
  skip_if_not_installed("loo")
  cmp <- apa_tidy(loo::loo_compare(test_loo_list()))
  expect_identical(apa_value(cmp, cmp$model[2]), cmp$elpd_diff[2])
})

test_that("every type's default column is the one its contract requires", {
  # The two types the fixtures above do not reach: a contrast is about
  # its estimate, a model Bayes factor about its Bayes factor.
  con <- apabayes_tidy(
    data.frame(contrast = "a - b", estimate = 4.28),
    type = "contrasts", ci_method = "hdi"
  )
  expect_identical(apa_value(con, "a - b"), 4.28)
  bfm <- apabayes_tidy(
    data.frame(model = c("m1", "m2"), bf = c(1, 6.38)),
    type = "bf_models", centrality = NA, ci_method = NA, ci_level = NA
  )
  expect_identical(apa_value(bfm, "m2"), 6.38)
})

test_that("a table about no single value asks for a column", {
  # A sem_fit table carries several indices, a diagnostics table three
  # statistics; neither has a value column to default to.
  fit <- fixture("sem_fit_blavaan")
  expect_error(apa_value(fit), "no default column")
  expect_error(apa_value(fit), "Columns")
  expect_identical(apa_value(fit, column = "ppp"), fit$ppp)
  diag <- fixture("diag_brms_full")
  expect_error(apa_value(diag, diag$term[1]), "no default column")
  expect_identical(
    apa_value(diag, diag$term[1], column = "rhat"),
    diag$rhat[1]
  )
})

test_that("the addressing refusals are apa_inline()'s", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_value(t, "nope"), "No row matches")
  expect_error(apa_value(t, "nope"), "Available")
  expect_error(apa_value(t, 1), "`term`")
  expect_error(apa_value(t, "wt", group = "nope"), "group")
  skip_if_not_installed("loo")
  cmp <- apa_tidy(loo::loo_compare(test_loo_list()))
  expect_error(apa_value(cmp, "good", "wide", op = "~~"), "path")
})

test_that("more than one match is an error that lists them", {
  t <- apabayes_tidy(
    data.frame(
      term = c("a", "a"), estimate = c(1, 2), group = c("g1", "g2")
    ),
    type = "parameters"
  )
  expect_error(apa_value(t, "a"), "2 rows match")
  expect_identical(apa_value(t, "a", group = "g2"), 2)
})

test_that("an unknown or malformed column is refused", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_value(t, "wt", column = "estimte"), "estimte")
  expect_error(apa_value(t, "wt", column = "estimte"), "Columns")
  expect_error(apa_value(t, "wt", column = 1), "`column`")
  expect_error(apa_value(t, "wt", column = c("estimate", "pd")), "`column`")
  expect_error(apa_value(t, "wt", column = NA), "`column`")
})

test_that("a value that is NA on every selected row is refused", {
  # A query helper exists to put a number into prose or arithmetic, and
  # NA in either is the defect that made finding 7 a release blocker.
  t <- fixture("tidy_brms_full")
  expect_error(apa_value(t, "wt", column = "rope_pct"), "rope_pct")
  expect_error(apa_value(t, "wt", column = "rope_pct"), "on every row")
  # Some rows missing is a table with a value for some rows; returned.
  partial <- t
  partial$rope_pct <- c(0.02, rep(NA_real_, nrow(t) - 1))
  expect_identical(apa_value(partial, column = "rope_pct"), partial$rope_pct)
})

test_that("the tidy method takes no other argument", {
  t <- fixture("tidy_brms_full")
  expect_error(
    apa_value(t, "wt", digits = 2),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("the default method extracts first and passes route arguments", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(
    "visual =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939
  )
  expect_identical(
    apa_value(fit, "visual", "x2", op = "=~"),
    apa_value(apa_tidy(fit), "visual", "x2", op = "=~")
  )
  std <- apa_value(fit, "visual", "x2", op = "=~", standardize = TRUE)
  expect_identical(std, apa_value(
    apa_tidy(fit, standardize = TRUE), "visual", "x2",
    op = "=~"
  ))
  expect_false(identical(std, apa_value(fit, "visual", "x2", op = "=~")))
})

test_that("apa_value() and apa_inline() report the same row", {
  t <- fixture("tidy_brms_full")
  string <- apa_inline(t, "wt")
  expect_identical(apa_value(t, "wt"), string$table$estimate)
  expect_match(
    string$estimate,
    apa_num(apa_value(t, "wt")),
    fixed = TRUE
  )
})
