# Tests for apa_table(), apa_note() (spec-apa_table.md): the table
# counterpart of apa_inline(). Slice 1 builds `parameters` and
# `diagnostics`; every other type aborts by name.
#
# No expected cell is typed. Every cell is composed in the test from the
# fixture row (or a hand-built apabayes_tidy() table) through the format
# layer (apa_num(), apa_pd(), apa_prob(), apa_bf(), apa_p()) with
# markup = "md" and apa7::align_chr(), exactly as the spec's column
# table says; only header names and note wording are typed literally,
# because those are the contract.

# ---- composition helpers (mirror the spec's column table) ---------------

# A single numeric column, `leading_zero` allowed to vary by row (apa_num()
# itself only takes one flag), then decimal-aligned.
num_cell <- function(x, digits = 2, leading_zero = TRUE, big_mark = TRUE) {
  leading_zero <- rep_len(leading_zero, length(x))
  s <- vapply(seq_along(x), function(i) {
    apa_num(x[i], digits, leading_zero[i], big_mark, markup = "md")
  }, character(1))
  apa7::align_chr(s)
}

# The interval cell: `apa_num()` on each bound, then aligned the way
# apa7's own CI formatter aligns two already-formatted bounds (measured 7
# of the spec) and wrapped in brackets; empty when either bound is NA.
ci_cell <- function(lo, hi, digits = 2, leading_zero = TRUE) {
  leading_zero <- rep_len(leading_zero, length(lo))
  lo_s <- vapply(seq_along(lo), function(i) {
    apa_num(lo[i], digits, leading_zero[i], markup = "md")
  }, character(1))
  hi_s <- vapply(seq_along(hi), function(i) {
    apa_num(hi[i], digits, leading_zero[i], markup = "md")
  }, character(1))
  out <- paste0(
    "[",
    apa7::align_chr(
      paste0(apa7::align_chr(lo_s), ", ", apa7::align_chr(hi_s)),
      center = ", "
    ),
    "]"
  )
  out[is.na(lo_s) | is.na(hi_s)] <- ""
  out
}

# `pd`/`p` cells: format_bounded() output, aligned.
pd_cell <- function(x, digits_prob = 3) {
  apa7::align_chr(apa_pd(x, digits_prob, markup = "md"))
}
p_cell <- function(x, digits_prob = 3) {
  apa7::align_chr(apa_p(x, digits_prob, markup = "md"))
}

# `% in ROPE`: apa_prob(percent = TRUE) with the trailing "%" dropped
# (the unit lives in the header), then aligned.
rope_cell <- function(x, digits_prob = 1) {
  s <- apa_prob(x, digits_prob, percent = TRUE, markup = "md")
  apa7::align_chr(sub("%$", "", s))
}

# Bayes factor cells are never aligned (measured 7: align_chr() counts
# markup characters and would mis-pad `> .999`-style prefixes and the
# `10^n^` markup).
bf_cell <- function(x, direction = "10", style = "auto", digits = 2) {
  apa_bf(x, direction, style, digits, markup = "md")
}

rhat_cell <- function(x, digits = 2) {
  apa7::align_chr(apa_num(x, digits, markup = "md"))
}
ess_cell <- function(x) {
  apa7::align_chr(apa_num(x, 0, big_mark = TRUE, markup = "md"))
}

# The label header: "Path" when every term is an SEM term
# (sem_term_parts() finds an operator), else "Predictor".
label_header <- function(terms) {
  if (all(!is.na(sem_term_parts(terms)$op))) "Path" else "Predictor"
}

# The estimate header: centrality read from the attribute.
estimate_header <- function(centrality) {
  switch(centrality %||% "NA",
    median = "*Mdn*",
    mean = "*M*",
    "Estimate"
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# ---- the per-table contract check ---------------------------------------

# For any table `apa_table()` produces: apa7::apa_format_columns() leaves
# it untouched, apa7::apa_flextable() runs with col_keys == names(), and
# no header contains "_" (measured 3, 4 of the spec: a header equal to an
# apa7 format name is re-formatted, and "_" makes a spanner).
assert_table_contract <- function(tab) {
  strip <- function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    rownames(x) <- NULL
    x
  }
  formatted <- apa7::apa_format_columns(tab)
  expect_identical(strip(formatted), strip(tab))
  ft <- apa7::apa_flextable(tab)
  expect_true(inherits(ft, "flextable"))
  expect_identical(ft$col_keys, names(tab))
  expect_false(any(grepl("_", names(tab), fixed = TRUE)))
}

# ---- hand-built tables (apabayes_tidy(), checked to validate) -----------

# Every optional "parameters" column filled and non-NA in both rows: the
# default-stats and digits/digits_prob tests need a table where nothing
# is dropped for being all-NA. `ess_bulk` carries a five-digit value for
# the thousands-separator check.
full_stats_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("b_x", "b_z"), label = c("x", "z"),
      estimate = c(0.3, -0.1), ci_low = c(0.1, -0.4),
      ci_high = c(0.5, 0.2), pd = c(0.98, 0.6),
      rope_pct = c(0.123, 0.0005), bf = c(20.86, 0.3), p = c(0.012, 0.4),
      rhat = c(1.0036, 1.0092), ess_bulk = c(517, 12345),
      ess_tail = c(508, 683),
      component = "conditional", effects = "fixed"
    ),
    type = "parameters", centrality = "median", ci_method = "eti",
    ci_level = 0.95
  )
}

mean_centrality_table <- function() {
  apabayes_tidy(
    data.frame(term = "b_x", estimate = 0.5),
    type = "parameters", centrality = "mean"
  )
}

mixed_levels_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"), estimate = c(1.2, -3.4),
      ci_low = c(0.5, -6.7), ci_high = c(1.9, -1.0),
      ci_method = c("eti", "eti"), ci_level = c(0.90, 0.95)
    ),
    type = "parameters", centrality = "median"
  )
}

mixed_methods_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"), estimate = c(1.2, -3.4),
      ci_low = c(0.5, -6.7), ci_high = c(1.9, -1.0),
      ci_method = c("eti", "hdi"), ci_level = c(0.95, 0.95)
    ),
    type = "parameters", centrality = "median"
  )
}

hdi_level_table <- function() {
  apabayes_tidy(
    data.frame(term = "x", estimate = 1, ci_low = 0, ci_high = 2),
    type = "parameters", centrality = "median",
    ci_method = "hdi", ci_level = 0.89
  )
}

no_group_table <- function() {
  apabayes_tidy(
    data.frame(term = c("a", "b"), estimate = c(1, 2)),
    type = "parameters"
  )
}

# Titles interleave: Population-level, Group-level (g1), Population-level
# (second run of a title already seen), Other (NA component and effects),
# Group-level (a random row with NA group). The stable reorder groups
# each title into one run, ordered by first appearance.
interleaved_group_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("r1", "r2", "r3", "r4", "r5"),
      estimate = c(1, 2, 3, 4, 5),
      component = c("conditional", NA, "conditional", NA, NA),
      effects = c("fixed", "random", "fixed", NA, "random"),
      group = c(NA, "g1", NA, NA, NA)
    ),
    type = "parameters"
  )
}

rope_with_attrs_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"), estimate = c(0.3, -0.1),
      rope_pct = c(0.123, 0.0005)
    ),
    type = "parameters", centrality = "median",
    rope_range = c(-0.1, 0.1), rope_ci = 0.89
  )
}

rope_without_attrs_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"), estimate = c(0.3, -0.1),
      rope_pct = c(0.123, 0.0005)
    ),
    type = "parameters", centrality = "median"
  )
}

partial_na_table <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"), estimate = c(0.3, -0.1),
      pd = c(0.98, NA), rope_pct = c(NA, 0.05), p = c(0.012, NA)
    ),
    type = "parameters", centrality = "median"
  )
}

diag_table <- function(divergences) {
  args <- list(
    data.frame(
      term = c("a", "b"), rhat = c(1.0, 1.01),
      ess_bulk = c(500, 600), ess_tail = c(400, 450)
    ),
    type = "diagnostics"
  )
  if (!missing(divergences)) args$divergences <- divergences
  do.call(apabayes_tidy, args)
}

# ---- return shape ---------------------------------------------------------

test_that("apa_table() returns a plain character tibble with table_type", {
  t <- fixture("tidy_brms_full")
  tab <- apa_table(t)
  expect_false(is_apabayes_tidy(tab))
  expect_identical(class(tab), c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(tab), nrow(t))
  expect_true(all(vapply(tab, is.character, logical(1))))
  expect_identical(attr(tab, "table_type"), "parameters")
  expect_true(withVisible(apa_table(t))$visible)
  d <- fixture("diag_brms_full")
  dtab <- apa_table(d)
  expect_false(is_apabayes_tidy(dtab))
  expect_true(all(vapply(dtab, is.character, logical(1))))
  expect_identical(attr(dtab, "table_type"), "diagnostics")
  expect_true(withVisible(apa_table(d))$visible)
  assert_table_contract(tab)
  assert_table_contract(dtab)
})

# ---- parameters: brms full -------------------------------------------------

test_that("a brms full table has the expected names and cells", {
  t <- fixture("tidy_brms_full")
  tab <- apa_table(t)
  expect_identical(names(tab), c("Predictor", "*Mdn*", "95% CrI", "*pd*"))
  expect_identical(tab[["Predictor"]], t$label)
  expect_identical(tab[["Predictor"]][1], "(Intercept)")
  expect_identical(tab[["*Mdn*"]], num_cell(t$estimate))
  expect_identical(tab[["95% CrI"]], ci_cell(t$ci_low, t$ci_high))
  expect_identical(tab[["*pd*"]], pd_cell(t$pd))
  assert_table_contract(tab)
})

test_that("mean centrality gives *M*; NA centrality gives Estimate", {
  m <- apa_table(mean_centrality_table())
  expect_identical(names(m), c("Predictor", "*M*"))
  lav <- apa_table(fixture("tidy_lavaan_std"))
  expect_identical(names(lav)[2], "Estimate")
  assert_table_contract(m)
  assert_table_contract(lav)
})

# ---- parameters: SEM fixtures ----------------------------------------------

test_that("an SEM fixture heads its label column Path", {
  lav <- fixture("tidy_lavaan_std")
  tab <- apa_table(lav)
  expect_identical(names(tab)[1], "Path")
  expect_identical(tab[["Path"]], lav$label)
  assert_table_contract(tab)
})

test_that("lavaan std shows 95% CI and *p*, drops the leading zero", {
  lav <- fixture("tidy_lavaan_std")
  tab <- apa_table(lav)
  expect_identical(names(tab), c("Path", "Estimate", "95% CI", "*p*"))
  expect_identical(
    tab[["Estimate"]], num_cell(lav$estimate, leading_zero = FALSE)
  )
  expect_identical(
    tab[["95% CI"]], ci_cell(lav$ci_low, lav$ci_high, leading_zero = FALSE)
  )
  expect_identical(tab[["*p*"]], p_cell(lav$p))
  forced <- apa_table(lav, leading_zero = TRUE)
  expect_identical(
    forced[["Estimate"]], num_cell(lav$estimate, leading_zero = TRUE)
  )
  assert_table_contract(tab)
  assert_table_contract(forced)
})

test_that("blavaan std shows Path, *Mdn*, 95% CrI, *pd*, no zero", {
  std <- fixture("tidy_blavaan_std")
  tab <- apa_table(std)
  expect_identical(names(tab), c("Path", "*Mdn*", "95% CrI", "*pd*"))
  expect_identical(tab[["*Mdn*"]], num_cell(std$estimate, leading_zero = FALSE))
  expect_identical(
    tab[["95% CrI"]], ci_cell(std$ci_low, std$ci_high, leading_zero = FALSE)
  )
  expect_identical(tab[["*pd*"]], pd_cell(std$pd))
  assert_table_contract(tab)
})

# ---- stats selection --------------------------------------------------------

test_that("default stats shows pd/rope/bf/p exactly when non-NA somewhere", {
  full <- full_stats_table()
  tab <- apa_table(full)
  expect_identical(
    names(tab),
    c("Predictor", "*Mdn*", "95% CrI", "*pd*", "% in ROPE", "*BF*~10~", "*p*")
  )
  expect_false("*R̂*" %in% names(tab))
  expect_false(any(grepl("ESS", names(tab))))
  assert_table_contract(tab)
})

test_that("stats order is fixed whatever order stats is given in", {
  full <- full_stats_table()
  a <- apa_table(full, stats = c("bf", "pd"))
  expect_identical(
    names(a), c("Predictor", "*Mdn*", "95% CrI", "*pd*", "*BF*~10~")
  )
  b <- apa_table(full, stats = c("ess_tail", "ess_bulk", "rhat"))
  expect_identical(
    names(b),
    c("Predictor", "*Mdn*", "95% CrI", "*R̂*", "ESS~bulk~", "ESS~tail~")
  )
  assert_table_contract(a)
  assert_table_contract(b)
})

test_that("stats = character() gives label, estimate and interval only", {
  t <- fixture("tidy_brms_full")
  tab <- apa_table(t, stats = character())
  expect_identical(names(tab), c("Predictor", "*Mdn*", "95% CrI"))
  assert_table_contract(tab)
})

test_that("rhat and ESS columns and their cells", {
  full <- full_stats_table()
  tab <- apa_table(full, stats = c("rhat", "ess_bulk", "ess_tail"))
  expect_identical(
    names(tab),
    c("Predictor", "*Mdn*", "95% CrI", "*R̂*", "ESS~bulk~", "ESS~tail~")
  )
  expect_identical(tab[["*R̂*"]], rhat_cell(full$rhat))
  expect_identical(tab[["ESS~bulk~"]], ess_cell(full$ess_bulk))
  expect_match(tab[["ESS~bulk~"]][2], "12,345", fixed = TRUE)
  expect_identical(tab[["ESS~tail~"]], ess_cell(full$ess_tail))
  assert_table_contract(tab)
})

test_that("the rope cell drops the trailing %", {
  full <- full_stats_table()
  tab <- apa_table(full, stats = "rope")
  cells <- tab[["% in ROPE"]]
  expect_false(any(grepl("%", cells)))
  expect_identical(cells, rope_cell(full$rope_pct))
  expect_match(cells[1], "12.3", fixed = TRUE)
  expect_match(cells[2], "< 0.1", fixed = TRUE)
  assert_table_contract(tab)
})

test_that("bf cells are unaligned; the subscript follows bf_direction", {
  full <- full_stats_table()
  ten <- apa_table(full, stats = "bf")
  expect_identical(names(ten), c("Predictor", "*Mdn*", "95% CrI", "*BF*~10~"))
  expect_identical(ten[["*BF*~10~"]], bf_cell(full$bf, "10"))
  one <- apa_table(full, stats = "bf", bf_direction = "01")
  expect_identical(names(one), c("Predictor", "*Mdn*", "95% CrI", "*BF*~01~"))
  expect_identical(one[["*BF*~01~"]], bf_cell(full$bf, "01"))
  sci <- apa_table(full, stats = "bf", bf = "sci")
  expect_identical(sci[["*BF*~10~"]], bf_cell(full$bf, "10", "sci"))
  assert_table_contract(ten)
  assert_table_contract(one)
  assert_table_contract(sci)
})

# ---- interval column --------------------------------------------------------

test_that("interval = FALSE drops the column; ci_label relabels", {
  t <- fixture("tidy_brms_full")
  no_ci <- apa_table(t, interval = FALSE)
  expect_false("95% CrI" %in% names(no_ci))
  expect_identical(names(no_ci), c("Predictor", "*Mdn*", "*pd*"))
  relabelled <- apa_table(t, ci_label = "HDI")
  expect_true("95% HDI" %in% names(relabelled))
  expect_match(apa_note(relabelled), "HDI = equal-tailed credible interval")
  expect_error(apa_table(t, ci_label = NULL), regexp = "ci_label|interval")
  assert_table_contract(no_ci)
  assert_table_contract(relabelled)
})

test_that("the interval header follows ci_method and ci_level", {
  hdi <- apa_table(hdi_level_table())
  expect_true("89% HDI" %in% names(hdi))
  assert_table_contract(hdi)
})

test_that("mixed levels drop the level from the header and prefix cells", {
  tab <- apa_table(mixed_levels_table())
  expect_true("CrI" %in% names(tab))
  cells <- tab[["CrI"]]
  expect_match(cells[1], "^90% \\[", perl = TRUE)
  expect_match(cells[2], "^95% \\[", perl = TRUE)
  assert_table_contract(tab)
})

test_that("mixed methods read Interval and prefix cells with their own label", {
  # The shared level stays in the header (session 26 fix: it was lost).
  tab <- apa_table(mixed_methods_table())
  expect_true("95% Interval" %in% names(tab))
  cells <- tab[["95% Interval"]]
  expect_match(cells[1], "^CrI \\[", perl = TRUE)
  expect_match(cells[2], "^HDI \\[", perl = TRUE)
  assert_table_contract(tab)
  both <- mixed_methods_table()
  both$ci_level <- c(0.90, 0.95)
  tab2 <- apa_table(both)
  expect_true("Interval" %in% names(tab2))
  expect_match(tab2[["Interval"]][1], "^90% CrI \\[", perl = TRUE)
  expect_match(tab2[["Interval"]][2], "^95% HDI \\[", perl = TRUE)
  assert_table_contract(tab2)
})

test_that("NA bounds give an empty interval cell", {
  t <- fixture("tidy_brms_full")
  t$ci_low[2] <- NA
  tab <- apa_table(t)
  expect_identical(tab[["95% CrI"]][2], "")
  assert_table_contract(tab)
})

# ---- empty cells for NA values -----------------------------------------

test_that("a NA value in a shown column gives an empty cell", {
  tab <- apa_table(partial_na_table())
  expect_identical(
    names(tab), c("Predictor", "*Mdn*", "*pd*", "% in ROPE", "*p*")
  )
  expect_identical(tab[["*pd*"]][2], "")
  expect_identical(tab[["% in ROPE"]][1], "")
  expect_identical(tab[["*p*"]][2], "")
  assert_table_contract(tab)
})

# ---- all-NA column aborts ---------------------------------------------

test_that("stats naming an all-NA column aborts, naming the statistic", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, stats = "rope"), regexp = "rope")
  expect_error(apa_table(t, stats = "bf"), regexp = "bf")
  lav <- fixture("tidy_lavaan_std")
  expect_error(apa_table(lav, stats = "rhat"), regexp = "rhat")
})

# ---- digits / digits_prob ------------------------------------------------

test_that("digits and digits_prob change only the right cells", {
  full <- full_stats_table()
  tab <- apa_table(
    full,
    digits = 4, digits_prob = 1,
    stats = c("pd", "rope", "bf", "p", "rhat")
  )
  expect_identical(tab[["*Mdn*"]], num_cell(full$estimate, digits = 4))
  expect_identical(
    tab[["95% CrI"]], ci_cell(full$ci_low, full$ci_high, digits = 4)
  )
  expect_identical(tab[["*R̂*"]], rhat_cell(full$rhat, digits = 4))
  expect_identical(tab[["*pd*"]], pd_cell(full$pd, digits_prob = 1))
  expect_identical(
    tab[["% in ROPE"]], rope_cell(full$rope_pct, digits_prob = 1)
  )
  expect_identical(tab[["*p*"]], p_cell(full$p, digits_prob = 1))
  # bf keeps apa_bf()'s own default digits: the column table gives
  # apa_bf(bf, bf_direction, bf) with no digits argument.
  expect_identical(tab[["*BF*~10~"]], bf_cell(full$bf, "10"))
  digits_default <- apa_table(full, stats = "ess_bulk")
  expect_identical(digits_default[["ESS~bulk~"]], ess_cell(full$ess_bulk))
  assert_table_contract(tab)
})

# ---- grouping ---------------------------------------------------------------

test_that("group_rows adds Component with the spec's titles", {
  mixed <- fixture("tidy_brms_mixed")
  tab <- apa_table(mixed, group_rows = TRUE)
  expect_identical(names(tab)[1], "Component")
  expect_identical(
    tab[["Component"]],
    c("Population-level", "Population-level", "Group-level (cyl_f)", "sigma")
  )
  ft <- apa7::apa_flextable(tab, row_title_column = Component)
  expect_true(inherits(ft, "flextable"))
  assert_table_contract(tab)
})

test_that("interleaved titles are reordered stably by first appearance", {
  tab <- apa_table(interleaved_group_table(), group_rows = TRUE)
  expect_identical(
    tab[["Component"]],
    c(
      "Population-level", "Population-level", "Group-level (g1)",
      "Other", "Group-level"
    )
  )
  expect_identical(tab[["Predictor"]], c("r1", "r3", "r2", "r4", "r5"))
  assert_table_contract(tab)
})

test_that("group_rows aborts with nothing to group by, or on another type", {
  expect_error(
    apa_table(no_group_table(), group_rows = TRUE),
    regexp = "group"
  )
  expect_error(
    apa_table(fixture("diag_brms_full"), group_rows = TRUE),
    regexp = "diagnostics"
  )
})

# ---- note: parameters -------------------------------------------------------

test_that("the brms full note matches the pass/fail example exactly", {
  t <- fixture("tidy_brms_full")
  note <- apa_note(apa_table(t))
  expect_identical(
    note,
    paste(
      "*Mdn* = posterior median; CrI = equal-tailed credible interval;",
      "*pd* = probability of direction."
    )
  )
})

test_that("the ROPE note states level and range when recorded", {
  with_attrs <- apa_table(rope_with_attrs_table(), stats = "rope")
  note <- apa_note(with_attrs)
  lo <- apa_num(-0.1, 2, markup = "md")
  hi <- apa_num(0.1, 2, markup = "md")
  expect_identical(
    note,
    paste0(
      "*Mdn* = posterior median; % in ROPE = percentage of the 89% ",
      "posterior interval inside the region of practical equivalence [",
      lo, ", ", hi, "]."
    )
  )
  without_attrs <- apa_table(rope_without_attrs_table(), stats = "rope")
  note2 <- apa_note(without_attrs)
  expect_identical(
    note2,
    paste0(
      "*Mdn* = posterior median; % in ROPE = percentage of the ",
      "posterior interval inside the region of practical equivalence."
    )
  )
})

test_that("the BF note names the direction of the Bayes factor", {
  full <- full_stats_table()
  ten <- apa_note(apa_table(full, stats = "bf"))
  expect_match(
    ten,
    "\\*BF\\*~10~ = Bayes factor of the alternative over the null hypothesis"
  )
  one <- apa_note(apa_table(full, stats = "bf", bf_direction = "01"))
  expect_match(
    one, "\\*BF\\*~01~ = .*of the null over the alternative"
  )
})

test_that("the rhat/ESS note lines depend on which are shown", {
  full <- full_stats_table()
  rhat_only <- apa_note(apa_table(full, stats = "rhat"))
  expect_match(rhat_only, "potential scale reduction factor", fixed = TRUE)
  expect_false(grepl("ESS", rhat_only))
  bulk_only <- apa_note(apa_table(full, stats = "ess_bulk"))
  expect_match(
    bulk_only, "ESS~bulk~ = bulk effective sample size",
    fixed = TRUE
  )
  expect_false(grepl("ESS~tail~", bulk_only))
  both <- apa_note(apa_table(full, stats = c("ess_bulk", "ess_tail")))
  expect_match(
    both, "ESS~bulk~ and ESS~tail~ = bulk and tail effective sample size",
    fixed = TRUE
  )
})

test_that("the lavaan std note names the CI and standardization", {
  note <- apa_note(apa_table(fixture("tidy_lavaan_std")))
  expect_identical(
    note, "CI = Wald confidence interval. Estimates are standardized."
  )
})

test_that("nothing shown that needs defining gives an NA note", {
  lav <- fixture("tidy_lavaan_std")
  tab <- apa_table(lav, stats = character(), interval = FALSE)
  expect_true(is.na(attr(tab, "note")))
  expect_error(apa_note(tab), regexp = "nothing to define")
})

# ---- note: diagnostics -------------------------------------------------

test_that("the diagnostics note counts divergent transitions", {
  d <- fixture("diag_brms_full")
  note <- apa_note(apa_table(d))
  expect_identical(
    note,
    paste0(
      "*R̂* = potential scale reduction factor; ESS~bulk~ and ",
      "ESS~tail~ = bulk and tail effective sample size.",
      " 0 divergent transitions."
    )
  )
})

test_that("one divergent transition is singular; absent/NA adds no sentence", {
  one <- apa_note(apa_table(diag_table(1L)))
  expect_match(one, "1 divergent transition\\.$")
  expect_false(grepl("transitions\\.$", one))
  absent <- apa_note(apa_table(diag_table()))
  expect_false(grepl("divergent", absent))
  na_div <- apa_note(apa_table(diag_table(NA_integer_)))
  expect_false(grepl("divergent", na_div))
})

test_that("diagnostics: Term, three columns, inapplicable options ignored", {
  d <- fixture("diag_brms_full")
  tab <- apa_table(d)
  expect_identical(names(tab), c("Term", "*R̂*", "ESS~bulk~", "ESS~tail~"))
  expect_identical(tab[["Term"]], d$term)
  expect_identical(tab[["*R̂*"]], rhat_cell(d$rhat))
  same <- apa_table(
    d,
    interval = FALSE, ci_label = "X", bf = "sci", bf_direction = "01",
    digits_prob = 1
  )
  expect_identical(same, tab)
  assert_table_contract(tab)
})

# ---- decision words -----------------------------------------------------

test_that("no cell or note contains a decision (verdict) word", {
  tabs <- list(
    apa_table(fixture("tidy_brms_full")),
    apa_table(fixture("tidy_blavaan_std")),
    apa_table(fixture("diag_brms_full")),
    apa_table(full_stats_table())
  )
  strings <- unlist(c(
    lapply(tabs, function(x) unlist(x, use.names = FALSE)),
    lapply(tabs, apa_note)
  ))
  strings <- strings[!is.na(strings)]
  for (word in decision_words) {
    expect_false(any(grepl(word, strings, ignore.case = TRUE)), info = word)
  }
})

# ---- apa_note() error conditions -----------------------------------------

test_that("apa_note() aborts without a note attribute or on an NA note", {
  expect_error(apa_note(data.frame(x = 1)), regexp = "apa_table")
  lav <- fixture("tidy_lavaan_std")
  na_note <- apa_table(lav, stats = character(), interval = FALSE)
  expect_error(apa_note(na_note), regexp = "nothing to define")
})

# ---- unbuilt types --------------------------------------------------------

test_that("a type not built in slice 1 aborts naming the type", {
  expect_error(
    apa_table(fixture("tidy_hypotheses")),
    regexp = "does not yet report.*hypotheses"
  )
  expect_error(
    apa_table(fixture("sem_fit_blavaan")),
    regexp = "does not yet report.*sem_fit"
  )
})

# ---- option validation ----------------------------------------------------

test_that("the tidy method rejects a non-empty ... ", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, foo = 1), class = "rlang_error")
})

test_that("stats outside the vocabulary aborts and lists it", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, stats = "nonsense"), regexp = "nonsense")
})

test_that("interval and group_rows must be flags", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, interval = NA), class = "rlang_error")
  expect_error(apa_table(t, interval = "yes"), class = "rlang_error")
  expect_error(apa_table(t, group_rows = NA), class = "rlang_error")
})

test_that("ci_label must be auto, a string, or aborts (NULL included)", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, ci_label = 1), regexp = "ci_label")
  expect_error(apa_table(t, ci_label = c("a", "b")), regexp = "ci_label")
})

test_that("digits, digits_prob and leading_zero are validated", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, digits = -1), regexp = "whole number")
  expect_error(apa_table(t, digits_prob = 0), regexp = "whole number")
  expect_error(apa_table(t, leading_zero = "yes"), regexp = "leading_zero")
})

test_that("bf and bf_direction are validated against their sets", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, bf = "wide"), class = "rlang_error")
  expect_error(apa_table(t, bf_direction = "11"), class = "rlang_error")
})

# ---- round trip -----------------------------------------------------------

test_that("a stored tidy table gives an identical apa_table()", {
  t <- fixture("tidy_brms_full")
  tab <- apa_table(t)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(t, path)
  back <- readRDS(path)
  expect_identical(apa_table(back), tab)
})

# ---- the default method ----------------------------------------------------

test_that("the default method equals the tidy method (lavaan)", {
  skip_if_not_installed("lavaan")
  fit <- test_lavaan_fit("cfa")
  direct <- apa_table(fit, standardize = TRUE)
  via_tidy <- apa_table(apa_tidy(fit, standardize = TRUE))
  expect_identical(direct, via_tidy)
  assert_table_contract(direct)
})

test_that("the default method forwards route arguments (brms, off CRAN)", {
  fit <- test_brms_fit("full")
  hdi <- apa_table(fit, ci = "hdi")
  expect_true(any(grepl("^95% HDI$", names(hdi))))
  same <- apa_table(fit)
  expect_identical(same, apa_table(apa_tidy(fit)))
  assert_table_contract(hdi)
})

test_that("the default method fails with the extract route's own message", {
  expect_error(apa_table(lm(mpg ~ wt, mtcars)), regexp = "lm")
})

# ---- review findings (session 26) ---------------------------------------

test_that("a header that apa7 would re-format aborts", {
  # A `ci_label` standing alone as a header (mixed levels) that equals an
  # apa7 format name, or contains "_", would be re-formatted or split.
  x <- mixed_levels_table()
  expect_error(apa_table(x, ci_label = "p"), regexp = "apa7")
  expect_error(apa_table(x, ci_label = "Cr_I"), regexp = "apa7")
  # Bare `CI` is an apa7 format name too (measured: apa_flextable() aborts).
  expect_error(apa_table(x, ci_label = "CI"), regexp = "apa7")
  expect_no_error(apa_table(x, ci_label = "Credible"))
  expect_no_error(apa_table(fixture("tidy_brms_full"), ci_label = "p"))
})

test_that("an automatic CI label never stands alone as a header", {
  # Wald rows at mixed levels would be headed bare `CI`, which apa7
  # formats itself; the label moves into the cells under `Interval`.
  x <- mixed_levels_table()
  x$ci_method <- c("wald", "wald")
  tab <- apa_table(x)
  expect_true("Interval" %in% names(tab))
  expect_match(tab[["Interval"]][1], "^90% CI \\[", perl = TRUE)
  expect_match(tab[["Interval"]][2], "^95% CI \\[", perl = TRUE)
  assert_table_contract(tab)
  none <- x
  none$ci_level <- NA_real_
  tab2 <- apa_table(none)
  expect_true("Interval" %in% names(tab2))
  expect_match(tab2[["Interval"]][1], "^CI \\[", perl = TRUE)
  assert_table_contract(tab2)
})

test_that("a logical divergences attribute other than NA aborts", {
  expect_error(apa_table(diag_table(TRUE)), regexp = "divergences")
})

test_that("a zero-row parameters table claims neither Path nor standardized", {
  x0 <- apabayes_tidy(
    data.frame(term = character(0), estimate = numeric(0)),
    type = "parameters", centrality = "median"
  )
  expect_no_warning(tab <- apa_table(x0, stats = character()))
  expect_no_warning(tab <- apa_table(x0))
  expect_identical(names(tab)[1], "Predictor")
  expect_identical(nrow(tab), 0L)
  expect_false(grepl("standardized", attr(tab, "note"), fixed = TRUE))
})

test_that("a malformed rope_range attribute aborts", {
  x <- apabayes_tidy(
    data.frame(term = "a", estimate = 0.3, rope_pct = 0.05),
    type = "parameters", centrality = "median", rope_range = -0.1
  )
  expect_error(apa_table(x, stats = "rope"), regexp = "rope_range")
})

test_that("a non-numeric divergences attribute aborts naming it", {
  expect_error(apa_table(diag_table("abc")), regexp = "divergences")
  expect_error(apa_table(diag_table(c(1, 2))), regexp = "divergences")
})
