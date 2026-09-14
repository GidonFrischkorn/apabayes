# Tests for the row addressing of apa_inline() (spec-apa_inline.md,
# "Row selection"). Every case runs on the checked-in tidy fixtures, so
# the file runs on CRAN.

term_of <- function(...) apa_inline(...)$table$term

test_that("a bare term matches term, then label, then the b_-stripped term", {
  t <- fixture("tidy_brms_full")
  expect_identical(term_of(t, "b_wt"), "b_wt")
  expect_identical(term_of(t, "wt"), "b_wt")
  expect_identical(term_of(t, "(Intercept)"), "b_Intercept")
  draws <- apa_tidy(fixture("draws_brms"), diagnostics = FALSE)
  expect_identical(term_of(draws, "wt"), "b_wt")
  expect_identical(term_of(draws, "b_wt"), "b_wt")
})

test_that("term = NULL reports every row", {
  t <- fixture("tidy_brms_full")
  r <- apa_inline(t)
  expect_identical(nrow(r$table), nrow(t))
  expect_length(r$full_result, nrow(t))
  expect_identical(r$table$term, t$term)
})

test_that("a SEM path is found by both sides and the operator", {
  std <- fixture("tidy_lavaan_std")
  expect_identical(term_of(std, "visual", "x2", op = "=~"), "visual=~x2")
  expect_identical(term_of(std, "visual", "x2"), "visual=~x2")
  cov <- "visual~~textual"
  expect_identical(term_of(std, "visual", "textual", op = "~~"), cov)
  expect_identical(term_of(std, "textual", "visual", op = "~~"), cov)
  expect_identical(term_of(std, "textual", "visual"), cov)
  expect_error(apa_inline(std, "x2", "visual"), "No row matches the path")
  expect_error(apa_inline(std, "x2", "visual", op = "=~"), "No row matches")
  expect_error(apa_inline(std, "visual", "x2", op = "~~"), "No row matches")
})

test_that("a spaced label finds a SEM row too", {
  std <- fixture("tidy_lavaan_std")
  expect_identical(term_of(std, "visual =~ x2"), "visual=~x2")
  expect_identical(term_of(std, "visual=~x2"), "visual=~x2")
})

test_that("op without rhs finds an intercept or a lone left-hand side", {
  t <- apabayes_tidy(
    data.frame(
      term = c("x1~1", "x1~~x1", "visual=~x1"),
      estimate = c(4.9, 0.5, 0.9)
    ),
    type = "parameters", centrality = NA, ci_method = "wald",
    ci_level = 0.95
  )
  expect_identical(term_of(t, "x1", op = "~1"), "x1~1")
  expect_identical(term_of(t, "visual", op = "=~"), "visual=~x1")
  expect_error(apa_inline(t, "x1", op = "=~"), "No row matches")
  # A bare term never reads a path: "x1" is a side, not a row.
  expect_error(apa_inline(t, "x1"), "No row matches")
  expect_identical(term_of(t, "x1", "x1"), "x1~~x1")
})

test_that("group restricts the search first and is named in the hint", {
  g <- fixture("tidy_lavaan_groups")
  expect_error(
    apa_inline(g, "visual", "x2", op = "=~"),
    "2 rows match.*differ by group; pass `group`"
  )
  r <- apa_inline(g, "visual", "x2", op = "=~", group = "Grant-White")
  expect_identical(r$table$term, "visual=~x2.g2")
  expect_identical(r$table$group, "Grant-White")
  all_gw <- apa_inline(g, group = "Grant-White")
  expect_true(all(all_gw$table$group == "Grant-White"))
  expect_error(
    apa_inline(g, group = "Nowhere"),
    "No row has group.*Groups present"
  )
  expect_error(
    apa_inline(fixture("diag_brms_full"), group = "a"),
    "has no group column"
  )
  expect_error(
    apa_inline(fixture("tidy_brms_full"), group = "a"),
    "The table has no group values"
  )
})

test_that("ambiguity by operator points at op", {
  std <- fixture("tidy_lavaan_std")
  t <- std[std$term %in% c("visual~~visual", "visual=~x1"), ]
  t$term[2] <- "visual~~x1"
  expect_error(apa_inline(t, "visual", "x1"), "2 rows match")
  t2 <- std[std$term %in% c("visual~~textual"), ]
  t2 <- rbind(t2, t2)
  t2$term[2] <- "visual~textual"
  expect_error(
    apa_inline(t2, "visual", "textual"),
    "differ by operator; pass `op`"
  )
})

test_that("no match lists the available rows, capped at twenty", {
  g <- fixture("tidy_lavaan_groups")
  expect_error(apa_inline(g, "nothing"), "and [0-9]+ more")
  t <- fixture("tidy_brms_full")
  err <- tryCatch(
    apa_inline(t, "nothing"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "No row matches \"nothing\"")
  expect_match(err, "b_wt (wt)", fixed = TRUE)
  expect_false(grepl("more", err))
})

test_that("a hypotheses table is addressed by its hypothesis string", {
  hyp <- fixture("tidy_hypotheses")
  h2 <- hyp$hypothesis[2]
  expect_identical(apa_inline(hyp, h2)$table$hypothesis, h2)
  expect_error(apa_inline(hyp, "wt", "am"), "has none")
  expect_error(apa_inline(hyp, "nothing"), "No row matches")
})

test_that("a grouped hypotheses table needs group", {
  hyp <- fixture("tidy_hypotheses")[c(1, 1), ]
  hyp$group <- c("4", "6")
  h1 <- hyp$hypothesis[1]
  expect_error(apa_inline(hyp, h1), "differ by group")
  expect_identical(apa_inline(hyp, h1, group = "6")$table$group, "6")
})

test_that("rhs or op without term, and non-string arguments, abort", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_inline(t, rhs = "x"), "`term` is needed")
  expect_error(apa_inline(t, op = "~~"), "`term` is needed")
  expect_error(apa_inline(t, c("a", "b")), "`term` must be a single string")
  expect_error(apa_inline(t, "wt", rhs = 1), "`rhs` must be a single string")
  expect_error(apa_inline(t, "wt", op = NA), "`op` must be a single string")
  expect_error(
    apa_inline(t, "wt", group = 1),
    "`group` must be a single string"
  )
})
