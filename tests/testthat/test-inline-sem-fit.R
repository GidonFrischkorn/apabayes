# Tests for apa_inline() on a sem_fit table
# (local/specs/spec-apa_inline-sem_fit.md).
#
# The lavaan row comes from a live lavaan fit, which runs on CRAN; the
# blavaan row from a checked-in fixture built by
# local/data-raw/fixtures-tidy.R. Every expected piece is composed from
# the row through the format layer, or reproduced by a seed helper.

# nolint next: object_usage_linter. test_lavaan_fit() is defined in setup.R.
lavaan_fit_row <- function() apa_tidy_sem_fit(test_lavaan_fit("cfa"))

# ---- lavaan --------------------------------------------------------------

test_that("a lavaan row prints chi-square, p and the four indices in order", {
  t <- lavaan_fit_row()
  row <- as.data.frame(t)
  r <- apa_inline(t)
  m <- "md"
  idx <- function(x) apa_num(x, 3, leading_zero = FALSE, markup = m)
  expected <- paste0(
    symbol("chisq", m), "(", apa_num(row$df, 0, markup = m), ") = ",
    apa_num(row$chisq, 2, markup = m), ", ",
    apa_p(row$p, markup = m, symbol = TRUE),
    ", CFI = ", idx(row$cfi), ", TLI = ", idx(row$tli),
    ", RMSEA = ", idx(row$rmsea), ", ",
    apa_ci(
      row$rmsea_low, row$rmsea_high,
      level = row$rmsea_level, label = "CI",
      digits = 3, leading_zero = FALSE, markup = m
    ),
    ", SRMR = ", idx(row$srmr)
  )
  expect_s3_class(r, "apabayes_results")
  expect_identical(r$full_result, expected)
  expect_identical(r$statistic, expected)
  expect_identical(r$estimate, NA_character_)
  expect_identical(r$table, t)
})

test_that("the SDVWM sem_fit_row() pieces are reproduced", {
  fit <- test_lavaan_fit("cfa")
  seed <- seed_sdvwm$sem_fit_row("x", fit)
  got <- apa_inline(apa_tidy_sem_fit(fit), markup = "plain")$full_result
  # The seed writes "85.31(24)"; the sentence writes "chi2(24) = 85.31".
  chisq <- sub("^([^(]*)\\((.*)\\)$", "(\\2) = \\1", seed$chisq_df)
  expect_match(got, paste0("chi2", chisq), fixed = TRUE)
  expect_match(got, paste("p", seed$p), fixed = TRUE)
  expect_match(got, paste0("CFI = ", seed$cfi), fixed = TRUE)
  expect_match(
    got, paste0("RMSEA = ", seed$rmsea, ", 90% CI ", seed$ci),
    fixed = TRUE
  )
  expect_match(got, paste0("SRMR = ", seed$srmr), fixed = TRUE)
})

test_that("digits sets both groups; NULL gives chi-square two decimals", {
  t <- lavaan_fit_row()
  row <- as.data.frame(t)
  two <- apa_inline(t, digits = 2, markup = "plain")$full_result
  expect_match(two, paste0("= ", apa_num(row$chisq, 2), ","), fixed = TRUE)
  expect_match(
    two, paste0("CFI = ", apa_num(row$cfi, 2, leading_zero = FALSE)),
    fixed = TRUE
  )
  four <- apa_inline(t, digits = 4, markup = "plain")$full_result
  expect_match(four, paste0("= ", apa_num(row$chisq, 4), ","), fixed = TRUE)
  expect_match(
    apa_inline(t, digits_prob = 2, markup = "plain")$full_result,
    apa_p(row$p, digits = 2, markup = "plain", symbol = TRUE),
    fixed = TRUE
  )
})

test_that("stats selects parts, keeps the fixed order, and is validated", {
  t <- lavaan_fit_row()
  row <- as.data.frame(t)
  idx <- function(x) apa_num(x, 3, leading_zero = FALSE, markup = "plain")
  expect_identical(
    apa_inline(t, stats = c("srmr", "cfi"), markup = "plain")$full_result,
    paste0("CFI = ", idx(row$cfi), ", SRMR = ", idx(row$srmr))
  )
  expect_error(apa_inline(t, stats = "bf"), "cannot print")
  expect_error(apa_inline(t, stats = "bf"), "chisq")
})

test_that("interval, ci_label and leading_zero reach the RMSEA part", {
  t <- lavaan_fit_row()
  row <- as.data.frame(t)
  no_ci <- apa_inline(t, interval = FALSE, markup = "plain")$full_result
  expect_false(grepl("[", no_ci, fixed = TRUE))
  expect_match(
    apa_inline(
      t,
      ci_label = "CrI", stats = "rmsea", markup = "plain"
    )$full_result,
    "90% CrI [",
    fixed = TRUE
  )
  # An index drops the separator with its label, as an estimate does
  # (finding 3): `RMSEA = .091 [.071, .114]`.
  bare <- apa_inline(t, ci_label = NULL, stats = "rmsea", markup = "plain")
  expect_match(
    bare$full_result,
    paste0(apa_num(row$rmsea, 3, leading_zero = FALSE), " ["),
    fixed = TRUE
  )
  expect_false(grepl(", [", bare$full_result, fixed = TRUE))
  expect_match(
    apa_inline(
      t,
      leading_zero = TRUE, stats = "cfi", markup = "plain"
    )$full_result,
    paste0("CFI = ", apa_num(row$cfi, 3)),
    fixed = TRUE
  )
})

test_that("fractional df, a missing p and a missing level are handled", {
  t <- apabayes_tidy(
    data.frame(
      model = "m", chisq = 0.5, df = 23.4, p = NA, rmsea = 0.05,
      rmsea_low = 0.01, rmsea_high = 0.08
    ),
    type = "sem_fit", centrality = NA_character_,
    ci_method = NA_character_, ci_level = NA_real_
  )
  expect_identical(
    apa_inline(t, markup = "plain")$full_result,
    "chi2(23.40) = 0.50, RMSEA = .050, [.010, .080]"
  )
  expect_identical(
    apa_inline(t, leading_zero = FALSE, markup = "plain")$full_result,
    "chi2(23.40) = .50, RMSEA = .050, [.010, .080]"
  )
})

test_that("a chi-square without df prints without parentheses", {
  t <- apabayes_tidy(
    data.frame(model = "m", chisq = 12.345, p = 0.03),
    type = "sem_fit", centrality = NA_character_,
    ci_method = NA_character_, ci_level = NA_real_
  )
  expect_identical(
    apa_inline(t, markup = "plain")$full_result,
    paste0("chi2 = 12.35, ", apa_p(0.03, markup = "plain", symbol = TRUE))
  )
})

# ---- blavaan -----------------------------------------------------------

test_that("a blavaan row prints PPP, BRMSEA and BGammaHat with their HDIs", {
  t <- fixture("sem_fit_blavaan")
  row <- as.data.frame(t)
  m <- "md"
  idx <- function(x) apa_num(x, 3, leading_zero = FALSE, markup = m)
  ci <- function(lo, hi) {
    apa_ci(lo, hi,
      level = attr(t, "ci_level"), label = "HDI", digits = 3,
      leading_zero = FALSE, markup = m
    )
  }
  expected <- paste0(
    "PPP = ", idx(row$ppp),
    ", BRMSEA = ", idx(row$brmsea), ", ", ci(row$brmsea_low, row$brmsea_high),
    ", ", symbol("bgammahat", m), " = ", idx(row$bgammahat), ", ",
    ci(row$bgammahat_low, row$bgammahat_high)
  )
  expect_identical(apa_inline(t)$full_result, expected)
})

test_that("miniQ's fmt_bfit() is reproduced on a stored blavaan row", {
  t <- fixture("sem_fit_blavaan")
  row <- as.data.frame(t)
  fitind <- c(ppp = row$ppp, BRMSEA = row$brmsea, BGammaHat = row$bgammahat)
  expect_identical(
    apa_inline(t, interval = FALSE, digits = 2, markup = "latex")$full_result,
    seed_miniq$fmt_bfit(fitind)
  )
})

test_that("miniQ's fmt_bfit() is reproduced from the fit, numbers included", {
  # miniQ's bsem_fit_indices(): posterior means from
  # summary(blavFitIndices(), central.tendency = "mean") and ppp.
  two <- test_blavaan_fit("two")
  s <- summary(
    suppressWarnings(blavaan::blavFitIndices(two)),
    central.tendency = "mean"
  )
  fitind <- c(
    ppp = unname(lavaan::fitMeasures(two, "ppp")),
    stats::setNames(s[, "EAP"], rownames(s))
  )
  t <- suppressWarnings(apa_tidy_sem_fit(two, centrality = "mean"))
  expect_identical(
    apa_inline(t, interval = FALSE, digits = 2, markup = "latex")$full_result,
    seed_miniq$fmt_bfit(fitind)
  )
})

test_that("the Bayesian interval label follows the table's ci_method", {
  t <- fixture("sem_fit_blavaan")
  attr(t, "ci_method") <- "eti"
  expect_match(
    apa_inline(t, stats = "brmsea", markup = "plain")$full_result,
    "90% CrI [",
    fixed = TRUE
  )
  attr(t, "ci_level") <- NA_real_
  expect_match(
    apa_inline(t, stats = "brmsea", markup = "plain")$full_result,
    "^BRMSEA = [.0-9]+, \\[[.0-9]+, [.0-9]+\\]$"
  )
})

test_that("the Gamma-hat symbol follows the markup target", {
  t <- fixture("sem_fit_blavaan")
  for (m in c("md", "latex", "typst", "plain")) {
    got <- apa_inline(t, stats = "bgammahat", interval = FALSE, markup = m)
    expect_true(
      startsWith(got$full_result, paste0(symbol("bgammahat", m), " = ")),
      info = m
    )
  }
  expect_identical(symbol("bgammahat", "latex"), "B$\\hat{\\Gamma}$")
  expect_identical(symbol("bgammahat", "plain"), "BGammaHat")
  expect_identical(symbol("bgammahat", "md"), "B\u0393\u0302")
})

# ---- addressing and wording --------------------------------------------

test_that("a sem_fit row is addressed by model", {
  t <- apa_tidy_sem_fit(test_lavaan_fit("cfa"), model = "Three factors")
  expect_identical(
    apa_inline(t, "Three factors")$full_result,
    apa_inline(t)$full_result
  )
  expect_error(apa_inline(t, "Two factors"), "Three factors")
  expect_error(apa_inline(t, "visual", "x1"), "structural-equation path")
})

test_that("no decision word appears in a fit-index string", {
  strings <- c(
    apa_inline(lavaan_fit_row())$full_result,
    apa_inline(fixture("sem_fit_blavaan"))$full_result
  )
  judgements <- c("good", "acceptable", "adequate", "fit well")
  for (word in c(decision_words, judgements)) {
    expect_false(any(grepl(word, strings, ignore.case = TRUE)), info = word)
  }
})
