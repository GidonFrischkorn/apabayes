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

test_that("lavaan std shows 95% CI (Wald) and *p*, drops the leading zero", {
  # S3-1: the table's label for a wald interval names the method, because
  # apa7 reads `95% CI` as its own CI column (slice-3 measured M1).
  lav <- fixture("tidy_lavaan_std")
  tab <- apa_table(lav)
  expect_identical(names(tab), c("Path", "Estimate", "95% CI (Wald)", "*p*"))
  expect_identical(
    tab[["Estimate"]], num_cell(lav$estimate, leading_zero = FALSE)
  )
  expect_identical(
    tab[["95% CI (Wald)"]],
    ci_cell(lav$ci_low, lav$ci_high, leading_zero = FALSE)
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
    note, "CI (Wald) = Wald confidence interval. Estimates are standardized."
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
  # S3-1: a wald interval is labelled `CI (Wald)` in a table, so no
  # automatic label is a name apa7 formats itself, at any level.
  x <- mixed_levels_table()
  x$ci_method <- c("wald", "wald")
  tab <- apa_table(x)
  expect_true("CI (Wald)" %in% names(tab))
  expect_match(tab[["CI (Wald)"]][1], "^90% \\[", perl = TRUE)
  expect_match(tab[["CI (Wald)"]][2], "^95% \\[", perl = TRUE)
  assert_table_contract(tab)
  none <- x
  none$ci_level <- NA_real_
  tab2 <- apa_table(none)
  expect_true("CI (Wald)" %in% names(tab2))
  expect_match(tab2[["CI (Wald)"]][1], "^\\[", perl = TRUE)
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


# =============================================================================
# Slice 2 (session 27): hypotheses, loo, bf_models, bf_inclusion
# (spec-apa_table.md, "Slice 2"). Every expected cell is composed from a
# fixture (or a hand-built apabayes_tidy() table) through the format
# layer and apa7::align_chr(), exactly as slice 1; only header names and
# note wording are typed, because those are the contract.
# =============================================================================

# ---- composition helpers (slice 2) -----------------------------------------

# Unaligned: Bayes factors, ER (measured 7, M4). NA -> "" (hypotheses'
# bf/er columns have no "lost" concept of their own).
bf_cell_na_empty <- function(x, direction = "10", style = "auto") {
  out <- apa_bf(x, direction, style, markup = "md")
  out[is.na(out)] <- ""
  out
}
er_cell <- function(x) {
  out <- apa_er(x, markup = "md")
  out[is.na(out)] <- ""
  out
}
prob_cell <- function(x, digits_prob = 3) {
  apa7::align_chr(apa_prob(x, digits_prob, markup = "md"))
}

# `value (SE)` (M3 of the spec): each part is apa_num()'d and aligned on
# its own, then pasted; the header drops "(*SE*)" when every SE is NA.
loo_num_cell <- function(x, digits = 2) {
  apa7::align_chr(apa_num(x, digits, markup = "md"))
}
loo_se_cell <- function(value, se, digits = 2) {
  v <- loo_num_cell(value, digits)
  if (all(is.na(se))) {
    return(v)
  }
  paste0(v, " (", loo_num_cell(se, digits), ")")
}

# Error (%): apa_prob(percent = TRUE) without the "%"; an exact 0 prints
# "0" (confirmed choice 5), never the floored "< 0.1".
bfm_error_cell <- function(x) {
  s <- apa_prob(x, percent = TRUE, markup = "md")
  s <- sub("%$", "", s)
  s[!is.na(x) & x == 0] <- "0"
  apa7::align_chr(s)
}
bfm_log_cell <- function(log_bf, direction = "10", digits = 2) {
  v <- if (direction == "01") -log_bf else log_bf
  apa7::align_chr(apa_num(v, digits, markup = "md"))
}

# A "lost" row (S2-3, the inline rule carried over): a Bayes factor whose
# exponential over- or underflowed, even though its log is finite.
lost_rows <- function(bf, log_bf) {
  !is.na(bf) & (is.infinite(bf) | bf == 0) & is.finite(log_bf)
}
bfm_bf_cell <- function(x, log_bf, direction = "10", style = "auto") {
  out <- apa_bf(x, direction, style, markup = "md")
  out[is.na(out) | lost_rows(x, log_bf)] <- ""
  out
}

# bf_inclusion's three row kinds (the spec's "Row kinds" paragraph): a
# missing log Bayes factor falls back to log(bf) before it is judged
# (check_inclusion_bf()'s rule).
inclusion_row_kind <- function(bf, log_bf) {
  eff <- ifelse(is.na(log_bf), log(bf), log_bf)
  kind <- rep("normal", length(bf))
  kind[is.na(eff)] <- "missing"
  kind[is.infinite(eff) & !is.na(eff)] <- "infinite"
  kind[lost_rows(bf, log_bf)] <- "lost"
  kind
}
bfi_bf_cell <- function(bf, kind, direction = "10", style = "auto") {
  out <- apa_bf(bf, direction, style, markup = "md")
  out[kind != "normal"] <- ""
  out
}
bfi_log_cell <- function(log_bf, kind, direction = "10", digits = 2) {
  v <- if (direction == "01") -log_bf else log_bf
  out <- apa7::align_chr(apa_num(v, digits, markup = "md"))
  out[kind != "lost"] <- ""
  out
}

# Terms quoted in backticks, joined "a, b and c" (the spec's own
# wording); has/have and its/their for one term versus more than one.
backtick_join <- function(terms) {
  q <- paste0("`", terms, "`")
  if (length(q) <= 1) {
    return(q)
  }
  paste0(paste(q[-length(q)], collapse = ", "), " and ", q[length(q)])
}
has_have <- function(terms) if (length(terms) == 1) "has" else "have"
its_their <- function(terms) if (length(terms) == 1) "its" else "their"
missing_sentence <- function(terms, noun = "inclusion") {
  paste0(
    backtick_join(terms), " ", has_have(terms),
    " no ", noun, " Bayes factor; a term in every model has none."
  )
}
infinite_sentence <- function(terms, noun = "inclusion") {
  paste0(
    backtick_join(terms), " ", has_have(terms),
    " an infinite ", noun, " Bayes factor; ", its_their(terms),
    " posterior inclusion probability rounds to 1 or 0."
  )
}
lost_sentence <-
  "A Bayes factor too large or too small to print is given as its log."

# ---- hand-built tables (slice 2) -------------------------------------------

hyp_point_only <- function() {
  apabayes_tidy(
    data.frame(
      hypothesis = c("(b1) = 0", "(b2) = 0"),
      estimate = c(1.2, -0.4), ci_low = c(0.5, -1.0), ci_high = c(1.9, 0.2),
      ci_method = c("eti", "eti"), ci_level = c(0.95, 0.95),
      evid_ratio = c(6.87, 2.5), post_prob = c(0.873, 0.7),
      bf10 = c(0.1455, 0.4), directional = c(FALSE, FALSE)
    ),
    type = "hypotheses", centrality = "mean"
  )
}

hyp_directional_only <- function() {
  apabayes_tidy(
    data.frame(
      hypothesis = c("(b1) < 0", "(b2) > 0"),
      estimate = c(-1.2, 0.4), ci_low = c(-1.9, 0.1), ci_high = c(-0.5, 0.9),
      ci_method = c("eti", "eti"), ci_level = c(0.90, 0.90),
      evid_ratio = c(Inf, 5), post_prob = c(1, 0.83),
      bf10 = c(Inf, 3.2), directional = c(TRUE, TRUE)
    ),
    type = "hypotheses", centrality = "mean"
  )
}

hyp_grouped <- function() {
  apabayes_tidy(
    data.frame(
      hypothesis = c("(b1) = 0", "(b1) = 0"),
      group = c("g1", "g2"),
      estimate = c(1, 2), ci_low = c(0.5, 1.5), ci_high = c(1.5, 2.5),
      ci_method = c("eti", "eti"), ci_level = c(0.95, 0.95),
      evid_ratio = c(2, 3), post_prob = c(0.6, 0.7), bf10 = c(0.5, 0.6),
      directional = c(FALSE, FALSE)
    ),
    type = "hypotheses", centrality = "median"
  )
}

# bf10/evid_ratio/post_prob all-NA: no prior draws, so every hypotheses
# stat is empty (the all-NA source test).
hyp_no_bf <- function() {
  apabayes_tidy(
    data.frame(
      hypothesis = c("(b1) = 0", "(b2) = 0"),
      estimate = c(1, 2), ci_low = c(0.5, 1.5), ci_high = c(1.5, 2.5),
      ci_method = c("eti", "eti"), ci_level = c(0.95, 0.95),
      evid_ratio = NA_real_, post_prob = NA_real_, bf10 = NA_real_,
      directional = c(FALSE, TRUE)
    ),
    type = "hypotheses", centrality = "median"
  )
}

loo_na_se_table <- function() {
  apabayes_tidy(
    data.frame(
      model = c("m1", "m2"),
      elpd_diff = c(0, -2.5), se_diff = NA_real_,
      elpd = c(-50, -52.5), se_elpd = NA_real_,
      p_loo = c(3.2, 4.1), looic = c(100, 105), weight = c(0.6, 0.4)
    ),
    type = "loo", centrality = NA
  )
}

loo_stacking_table <- function() {
  apabayes_tidy(
    data.frame(
      model = c("m1", "m2"),
      elpd_diff = c(0, -1.2), se_diff = c(0, 0.5),
      elpd = c(-40, -41.2), se_elpd = c(3, 3.1),
      p_loo = c(2, 2.5), looic = c(80, 82.4), weight = c(0.7, 0.3)
    ),
    type = "loo", centrality = NA, weight_method = "stacking"
  )
}

loo_big_looic_table <- function() {
  apabayes_tidy(
    data.frame(
      model = c("m1", "m2"),
      elpd_diff = c(0, -3), se_diff = c(0, 1.2),
      elpd = c(-5000, -5003), se_elpd = c(50, 51),
      p_loo = c(10, 12), looic = c(10000.4, 10006.4), weight = c(0.6, 0.4)
    ),
    type = "loo", centrality = NA
  )
}

# No `denominator_model` attribute (falls back to the TRUE row's model);
# `prior_odds = "custom"`; `bf_method` NA (no method parenthesis); an
# underflowed row (bf 0, finite log_bf) and an exact-0 error.
bf_models_underflow_table <- function() {
  apabayes_tidy(
    data.frame(
      model = c("Intercept only", "x"),
      bf = c(1, 0), log_bf = c(0, -800),
      denominator = c(TRUE, FALSE),
      method = c("JZS (BayesFactor)", "JZS (BayesFactor)"),
      post_prob = c(1, 0), error = c(NA_real_, 0)
    ),
    type = "bf_models", centrality = NA,
    prior_odds = "custom", bf_method = NA_character_
  )
}

# Neither a `denominator_model` attribute nor a TRUE row: the note's
# last-resort fallback, "the denominator model".
bf_models_no_denominator_flag <- function() {
  apabayes_tidy(
    data.frame(
      model = c("a", "b"),
      bf = c(1.2, 2.3), log_bf = c(log(1.2), log(2.3)),
      denominator = c(FALSE, FALSE),
      method = c("m", "m"),
      post_prob = c(0.4, 0.6), error = c(NA_real_, NA_real_)
    ),
    type = "bf_models", centrality = NA
  )
}

# One missing row ("b": log_bf NA).
inc_missing_row <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"),
      p_prior = c(0.5, 0.5), p_posterior = c(0.6, 1),
      bf = c(1.2, NA_real_), log_bf = c(log(1.2), NA_real_)
    ),
    type = "bf_inclusion", centrality = NA
  )
}
# Two missing rows ("n", "o"), for the plural sentence.
inc_missing_multi <- function() {
  apabayes_tidy(
    data.frame(
      term = c("m", "n", "o"),
      p_prior = c(0.5, 0.5, 0.5), p_posterior = c(0.6, 1, 1),
      bf = c(1.2, NA_real_, NA_real_),
      log_bf = c(log(1.2), NA_real_, NA_real_)
    ),
    type = "bf_inclusion", centrality = NA
  )
}
# Two infinite rows ("p", "q"), for the plural sentence.
inc_infinite_multi <- function() {
  apabayes_tidy(
    data.frame(
      term = c("p", "q", "r"),
      p_prior = c(0.5, 0.5, 0.5), p_posterior = c(1, 1, 0.5),
      bf = c(Inf, Inf, 2), log_bf = c(Inf, Inf, log(2))
    ),
    type = "bf_inclusion", centrality = NA
  )
}
# One lost row ("a": bf Inf, log_bf 800 finite; "b" normal).
inc_lost_row <- function() {
  apabayes_tidy(
    data.frame(
      term = c("a", "b"),
      p_prior = c(0.5, 0.5), p_posterior = c(0.9, 0.5),
      bf = c(Inf, 1.5), log_bf = c(800, log(1.5))
    ),
    type = "bf_inclusion", centrality = NA
  )
}

# Minimal skeletons of the two types still not built in either slice.
contrasts_stub <- function() {
  apabayes_tidy(
    data.frame(contrast = "a - b", estimate = 1),
    type = "contrasts", centrality = NA
  )
}
correlations_stub <- function() {
  apabayes_tidy(
    data.frame(term = "x~~y", var1 = "x", var2 = "y", estimate = 0.5),
    type = "correlations", centrality = NA
  )
}

# ---- every tidy type is now tabulated --------------------------------------

test_that("no tidy type aborts \"does not yet report\" any more", {
  skip_if_not_installed("loo")
  for (t in list(
    fixture("tidy_hypotheses"),
    apa_tidy(fixture("compare_loo_matrix")),
    bf_models_no_denominator_flag(),
    apa_tidy(fixture("inc_anova")),
    fixture("sem_fit_blavaan"),
    contrasts_stub(),
    correlations_stub()
  )) {
    expect_no_error(apa_table(t))
  }
  # Every type the contract knows has a vocabulary, so the slice-1
  # refusal has nothing left to refuse (slice 3).
  for (type in tidy_types()) {
    expect_type(table_stats_vocabulary(type), "character")
  }
})

# ---- hypotheses: names, order and cells ------------------------------------

test_that("hypotheses: names, order and cells from the fixture", {
  h <- fixture("tidy_hypotheses")
  tab <- apa_table(h)
  expect_identical(names(tab), c("Hypothesis", "*M*", "CrI", "*BF*~10~"))
  expect_identical(tab[["Hypothesis"]], h$hypothesis)
  expect_identical(tab[["*M*"]], num_cell(h$estimate))
  expect_identical(
    tab[["CrI"]],
    paste0(c("90% ", "95% "), ci_cell(h$ci_low, h$ci_high))
  )
  expect_identical(tab[["*BF*~10~"]], bf_cell_na_empty(h$bf10, "10"))
  expect_identical(tab[["*BF*~10~"]][1], "∞")
  assert_table_contract(tab)
})

test_that("hypotheses: Group is shown only when some row has one", {
  h <- fixture("tidy_hypotheses")
  no_group <- apa_table(h)
  expect_false("Group" %in% names(no_group))
  grouped <- apa_table(hyp_grouped())
  expect_true("Group" %in% names(grouped))
  expect_identical(grouped[["Group"]], c("g1", "g2"))
  assert_table_contract(grouped)
})

test_that("hypotheses: er and post_prob on request, in fixed order", {
  h <- fixture("tidy_hypotheses")
  tab <- apa_table(h, stats = c("post_prob", "er", "bf"))
  expect_identical(
    names(tab), c("Hypothesis", "*M*", "CrI", "*BF*~10~", "ER", "*P*(H)")
  )
  expect_identical(tab[["ER"]], er_cell(h$evid_ratio))
  expect_identical(tab[["*P*(H)"]], prob_cell(h$post_prob))
  assert_table_contract(tab)
})

test_that("hypotheses: a missing bf/er/post_prob value is an empty cell", {
  mixed <- apabayes_tidy(
    data.frame(
      hypothesis = c("(b1) = 0", "(b2) = 0"),
      estimate = c(1, 2), ci_low = c(0.5, 1.5), ci_high = c(1.5, 2.5),
      ci_method = c("eti", "eti"), ci_level = c(0.95, 0.95),
      evid_ratio = c(2, NA_real_), post_prob = c(0.6, NA_real_),
      bf10 = c(0.5, NA_real_), directional = c(FALSE, FALSE)
    ),
    type = "hypotheses", centrality = "median"
  )
  tab <- apa_table(mixed, stats = c("bf", "er", "post_prob"))
  expect_identical(tab[["*BF*~10~"]][2], "")
  expect_identical(tab[["ER"]][2], "")
  expect_identical(tab[["*P*(H)"]][2], "")
  assert_table_contract(tab)
})

test_that("hypotheses: bf_direction = '01' inverts the header and cells", {
  h <- fixture("tidy_hypotheses")
  tab <- apa_table(h, bf_direction = "01")
  expect_true("*BF*~01~" %in% names(tab))
  expect_identical(tab[["*BF*~01~"]], bf_cell_na_empty(h$bf10, "01"))
  assert_table_contract(tab)
})

# ---- hypotheses: note -------------------------------------------------------

test_that("hypotheses note: point-only, directional-only and both kinds", {
  point <- apa_note(apa_table(hyp_point_only(), stats = "bf"))
  expect_match(
    point,
    "*BF*~10~ = Bayes factor against the point hypothesis (Savage–Dickey density ratio)", # nolint: line_length_linter.
    fixed = TRUE
  )
  directional <- apa_note(apa_table(hyp_directional_only(), stats = "bf"))
  expect_match(
    directional,
    "*BF*~10~ = posterior odds of the hypothesis over its complement",
    fixed = TRUE
  )
  both <- apa_note(apa_table(fixture("tidy_hypotheses"), stats = "bf"))
  expect_match(
    both,
    paste0(
      "*BF*~10~ = Bayes factor against a point hypothesis (Savage–",
      "Dickey density ratio), or posterior odds of a directional ",
      "hypothesis over its complement"
    ),
    fixed = TRUE
  )
})

test_that("hypotheses note: the '01' point-only and directional forms", {
  point <- apa_note(
    apa_table(hyp_point_only(), stats = "bf", bf_direction = "01")
  )
  expect_match(
    point,
    "*BF*~01~ = Bayes factor in favour of the point hypothesis (Savage–Dickey density ratio)", # nolint: line_length_linter.
    fixed = TRUE
  )
  directional <- apa_note(
    apa_table(hyp_directional_only(), stats = "bf", bf_direction = "01")
  )
  expect_match(
    directional,
    "*BF*~01~ = posterior odds of the complement over the hypothesis",
    fixed = TRUE
  )
  both <- apa_note(
    apa_table(fixture("tidy_hypotheses"), stats = "bf", bf_direction = "01")
  )
  expect_match(
    both,
    paste0(
      "*BF*~01~ = Bayes factor in favour of a point hypothesis (Savage–",
      "Dickey density ratio), or posterior odds of the complement over a ",
      "directional hypothesis"
    ),
    fixed = TRUE
  )
})

test_that("hypotheses note: the fixture's note matches the pass/fail example", {
  expect_identical(
    apa_note(apa_table(fixture("tidy_hypotheses"))),
    paste0(
      "*M* = posterior mean; CrI = equal-tailed credible interval; ",
      "*BF*~10~ = Bayes factor against a point hypothesis (Savage–Dickey ",
      "density ratio), or posterior odds of a directional hypothesis over ",
      "its complement."
    )
  )
})

test_that("hypotheses: a default never shows an all-empty bf column", {
  tab <- apa_table(hyp_no_bf())
  expect_identical(names(tab), c("Hypothesis", "*Mdn*", "95% CrI"))
  assert_table_contract(tab)
})

test_that("hypotheses note: the er and post_prob definitions", {
  h <- fixture("tidy_hypotheses")
  note <- apa_note(apa_table(h, stats = c("bf", "er", "post_prob")))
  expect_match(note, "ER = evidence ratio for the hypothesis", fixed = TRUE)
  expect_match(
    note, "*P*(H) = posterior probability of the hypothesis",
    fixed = TRUE
  )
})

# ---- loo: names and cells ---------------------------------------------------

test_that("loo: default names with and without weights, and their cells", {
  skip_if_not_installed("loo")
  cmp <- fixture("compare_loo_matrix")
  out <- apa_tidy(cmp)
  tab <- apa_table(out)
  expect_identical(
    names(tab), c("Model", "ΔELPD (*SE*)", "ELPD (*SE*)", "*p*~loo~")
  )
  expect_identical(tab[["Model"]], out$model)
  expect_identical(
    tab[["ΔELPD (*SE*)"]], loo_se_cell(out$elpd_diff, out$se_diff)
  )
  expect_identical(tab[["ELPD (*SE*)"]], loo_se_cell(out$elpd, out$se_elpd))
  expect_identical(tab[["*p*~loo~"]], loo_num_cell(out$p_loo))
  assert_table_contract(tab)

  w <- c(good = 0.5, shifted = 0.3, wide = 0.2)
  weighted <- apa_tidy(cmp, weights = w)
  tab_w <- apa_table(weighted)
  expect_identical(
    names(tab_w),
    c("Model", "ΔELPD (*SE*)", "ELPD (*SE*)", "*p*~loo~", "*w*")
  )
  expect_identical(tab_w[["*w*"]], prob_cell(weighted$weight))
  assert_table_contract(tab_w)
})

test_that("loo: an NA-SE table drops (*SE*) from the header and the cell", {
  tab <- apa_table(loo_na_se_table())
  expect_identical(names(tab), c("Model", "ΔELPD", "ELPD", "*p*~loo~", "*w*"))
  expect_identical(tab[["ΔELPD"]], loo_num_cell(c(0, -2.5)))
  expect_identical(tab[["ELPD"]], loo_num_cell(c(-50, -52.5)))
  assert_table_contract(tab)
})

test_that("loo: looic is opt-in and prints with a thousands separator", {
  skip_if_not_installed("loo")
  out <- apa_tidy(fixture("compare_loo_matrix"))
  tab <- apa_table(out, stats = c("elpd_diff", "looic"))
  expect_identical(names(tab), c("Model", "ΔELPD (*SE*)", "LOOIC"))
  big <- apa_table(loo_big_looic_table(), stats = "looic")
  expect_identical(big[["LOOIC"]], loo_num_cell(c(10000.4, 10006.4)))
  expect_match(big[["LOOIC"]][1], "10,000.40", fixed = TRUE)
  assert_table_contract(tab)
  assert_table_contract(big)
})

test_that("loo: digits and digits_prob reach the value/SE and weight cells", {
  skip_if_not_installed("loo")
  out <- apa_tidy(
    fixture("compare_loo_matrix"),
    weights = c(good = 0.5, shifted = 0.3, wide = 0.2)
  )
  tab <- apa_table(out, digits = 1, digits_prob = 1)
  expect_identical(
    tab[["ΔELPD (*SE*)"]], loo_se_cell(out$elpd_diff, out$se_diff, digits = 1)
  )
  expect_identical(tab[["*w*"]], prob_cell(out$weight, digits_prob = 1))
  assert_table_contract(tab)
})

# ---- loo: note --------------------------------------------------------------

test_that("loo note: the reference model is named verbatim", {
  skip_if_not_installed("loo")
  out <- apa_tidy(fixture("compare_loo_matrix"))
  note <- apa_note(apa_table(out))
  expect_match(
    note,
    "ΔELPD = difference in expected log predictive density from good",
    fixed = TRUE
  )
})

test_that("loo note: the fixture's whole note, in column order", {
  skip_if_not_installed("loo")
  expect_identical(
    apa_note(apa_table(apa_tidy(fixture("compare_loo_matrix")))),
    paste0(
      "ΔELPD = difference in expected log predictive density from good; ",
      "*SE* = standard error; ELPD = expected log predictive density; ",
      "*p*~loo~ = effective number of parameters."
    )
  )
})

test_that("loo note: falls back to 'the model in the first row'", {
  note <- apa_note(apa_table(loo_stacking_table()))
  expect_match(
    note,
    paste0(
      "difference in expected log predictive density from the model in ",
      "the first row"
    ),
    fixed = TRUE
  )
})

test_that("loo note: *SE* is defined once; the weight kind and its fallback", {
  skip_if_not_installed("loo")
  out <- apa_tidy(fixture("compare_loo_matrix"))
  note_no_weight <- apa_note(apa_table(out))
  expect_false(grepl("*w*", note_no_weight, fixed = TRUE))
  se_definition <- "*SE* = standard error"
  count <- lengths(regmatches(
    note_no_weight,
    gregexpr(se_definition, note_no_weight, fixed = TRUE)
  ))
  expect_identical(count, 1L)
  stacked <- apa_note(apa_table(loo_stacking_table()))
  expect_match(stacked, "*w* = stacking model weight", fixed = TRUE)
  weighted <- apa_tidy(cmp <- fixture("compare_loo_matrix"), weights = c(
    good = 0.5, shifted = 0.3, wide = 0.2
  ))
  plain_w <- apa_note(apa_table(weighted))
  expect_match(plain_w, "*w* = model weight", fixed = TRUE)
})

# ---- bf_models: names and cells ---------------------------------------------

test_that("bf_models: default is Model, BF; an absent error drops the column", {
  out <- apa_tidy(test_bf_models_lm())
  tab <- apa_table(out)
  expect_identical(names(tab), c("Model", "*BF*~10~"))
  expect_identical(tab[["*BF*~10~"]], bfm_bf_cell(out$bf, out$log_bf, "10"))
  assert_table_contract(tab)
})

test_that("bf_models: Error (%) is shown when recorded; bf_ttest's denominator is empty", { # nolint: line_length_linter.
  skip_if_not_installed("BayesFactor")
  reg <- apa_tidy(fixture("bf_regression"))
  reg_tab <- apa_table(reg)
  expect_identical(names(reg_tab), c("Model", "*BF*~10~", "Error (%)"))
  expect_identical(reg_tab[["Error (%)"]], bfm_error_cell(reg$error))
  expect_true(all(grepl("< 0.1", reg_tab[["Error (%)"]][-1], fixed = TRUE)))

  tt <- apa_tidy(fixture("bf_ttest"))
  tt_tab <- apa_table(tt)
  expect_identical(tt_tab[["Error (%)"]][tt$denominator], "")
  assert_table_contract(reg_tab)
  assert_table_contract(tt_tab)
})

test_that("bf_models: an exact-0 error prints 0, never the floored '< 0.1'", {
  tab <- apa_table(bf_models_underflow_table())
  expect_identical(tab[["Error (%)"]], c("", "0"))
})

test_that("bf_models: an underflowed row empties BF, forcing a log", {
  tab <- apa_table(bf_models_underflow_table())
  expect_identical(
    names(tab), c("Model", "*BF*~10~", "Error (%)", "log(*BF*~10~)")
  )
  expect_identical(tab[["*BF*~10~"]], c("1.00", ""))
  expect_identical(tab[["log(*BF*~10~)"]][2], loo_num_cell(-800))
  expect_match(apa_note(tab), lost_sentence, fixed = TRUE)
  assert_table_contract(tab)
})

test_that("bf_models: the overflow fixture forces the log column too", {
  skip_if_not_installed("BayesFactor")
  out <- apa_tidy(fixture("bf_overflow"))
  tab <- apa_table(out)
  expect_identical(
    names(tab), c("Model", "*BF*~10~", "Error (%)", "log(*BF*~10~)")
  )
  expect_identical(tab[["*BF*~10~"]][2], "")
  # Aligned over the whole column, so composed over it before indexing.
  expect_identical(tab[["log(*BF*~10~)"]], bfm_log_cell(out$log_bf))
  expect_match(apa_note(tab), lost_sentence, fixed = TRUE)
  assert_table_contract(tab)
})

test_that("bf_models: log_bf and post_prob opt in; log negates on '01'", {
  out <- apa_tidy(test_bf_models_lm())
  tab <- apa_table(out, stats = c("post_prob", "log_bf", "bf"))
  expect_identical(
    names(tab), c("Model", "*BF*~10~", "log(*BF*~10~)", "*P*(M | D)")
  )
  expect_identical(tab[["log(*BF*~10~)"]], bfm_log_cell(out$log_bf, "10"))
  expect_identical(tab[["*P*(M | D)"]], prob_cell(out$post_prob))
  inverted <- apa_table(out, stats = c("bf", "log_bf"), bf_direction = "01")
  expect_identical(
    inverted[["log(*BF*~01~)"]], bfm_log_cell(out$log_bf, "01")
  )
  assert_table_contract(tab)
  assert_table_contract(inverted)
})

test_that("bf_models: stats = 'error' alone aborts, as inline", {
  skip_if_not_installed("BayesFactor")
  reg <- apa_tidy(fixture("bf_regression"))
  expect_error(apa_table(reg, stats = "error"), regexp = "without")
})

test_that("bf_models: bf = 'sci' reaches apa_bf()", {
  out <- apa_tidy(test_bf_models_lm())
  tab <- apa_table(out, bf = "sci")
  expect_identical(
    tab[["*BF*~10~"]], bfm_bf_cell(out$bf, out$log_bf, "10", "sci")
  )
})

# ---- bf_models: note --------------------------------------------------------

test_that("bf_models note: the denominator is named from the attribute", {
  skip_if_not_installed("BayesFactor")
  out <- apa_tidy(fixture("bf_regression"))
  note <- apa_note(apa_table(out))
  expect_match(
    note,
    "*BF*~10~ = Bayes factor of the model over Intercept only (JZS (BayesFactor))", # nolint: line_length_linter.
    fixed = TRUE
  )
})

test_that("bf_models note: the regression fixture's whole note", {
  skip_if_not_installed("BayesFactor")
  expect_identical(
    apa_note(apa_table(apa_tidy(fixture("bf_regression")))),
    paste0(
      "*BF*~10~ = Bayes factor of the model over Intercept only ",
      "(JZS (BayesFactor)); Error (%) = proportional error of the Bayes ",
      "factor estimate."
    )
  )
})

test_that("bf_models note: falls back to the row, then to 'the denominator model'", { # nolint: line_length_linter.
  from_row <- apa_note(apa_table(bf_models_underflow_table(), stats = "bf"))
  expect_match(
    from_row, "Bayes factor of the model over Intercept only",
    fixed = TRUE
  )
  expect_false(grepl("over Intercept only (", from_row, fixed = TRUE))
  fallback <- apa_note(apa_table(bf_models_no_denominator_flag(), stats = "bf"))
  expect_match(
    fallback, "Bayes factor of the model over the denominator model",
    fixed = TRUE
  )
})

test_that("bf_models note: the '01' wording names the denominator first", {
  inverted <- apa_note(
    apa_table(bf_models_underflow_table(), stats = "bf", bf_direction = "01")
  )
  expect_match(
    inverted, "Bayes factor of Intercept only over the model",
    fixed = TRUE
  )
})

test_that("bf_models note: prior-odds phrases, equal, custom and absent", {
  skip_if_not_installed("BayesFactor")
  equal_odds <- apa_note(apa_table(
    fixture("bf_regression") |> apa_tidy(),
    stats = c("bf", "post_prob")
  ))
  expect_match(
    equal_odds,
    "*P*(M | D) = posterior model probability under equal prior odds",
    fixed = TRUE
  )
  custom_odds <- apa_note(
    apa_table(bf_models_underflow_table(), stats = c("bf", "post_prob"))
  )
  expect_match(
    custom_odds,
    "posterior model probability under the given prior odds",
    fixed = TRUE
  )
  none <- apa_note(
    apa_table(bf_models_no_denominator_flag(), stats = c("bf", "post_prob"))
  )
  expect_true(grepl(
    "*P*(M | D) = posterior model probability", none,
    fixed = TRUE
  ))
  expect_false(grepl(
    "*P*(M | D) = posterior model probability under", none,
    fixed = TRUE
  ))
})

test_that("bf_models note: the log Bayes factor is defined when shown", {
  out <- apa_tidy(test_bf_models_lm())
  note <- apa_note(apa_table(out, stats = c("bf", "log_bf")))
  expect_match(
    note, "log(*BF*~10~) = natural logarithm of *BF*~10~",
    fixed = TRUE
  )
  inverted <- apa_note(
    apa_table(out, stats = c("bf", "log_bf"), bf_direction = "01")
  )
  expect_match(
    inverted, "log(*BF*~01~) = natural logarithm of *BF*~01~",
    fixed = TRUE
  )
})

# ---- bf_inclusion: names and cells ------------------------------------------

test_that("bf_inclusion: default names, and *BF*~excl~ under '01'", {
  out <- apa_tidy(fixture("inc_anova"))
  tab <- apa_table(out)
  expect_identical(
    names(tab), c("Term", "*P*(incl)", "*P*(incl | D)", "*BF*~incl~")
  )
  expect_identical(tab[["Term"]], out$term)
  expect_identical(tab[["*P*(incl)"]], prob_cell(out$p_prior))
  expect_identical(tab[["*P*(incl | D)"]], prob_cell(out$p_posterior))
  kind <- inclusion_row_kind(out$bf, out$log_bf)
  expect_identical(tab[["*BF*~incl~"]], bfi_bf_cell(out$bf, kind, "10"))

  reversed <- apa_table(out, stats = c("p_posterior", "bf", "p_prior"))
  expect_identical(names(reversed), names(tab))

  excl <- apa_table(out, bf_direction = "01")
  expect_true("*BF*~excl~" %in% names(excl))
  expect_identical(excl[["*BF*~excl~"]], bfi_bf_cell(out$bf, kind, "01"))
  assert_table_contract(tab)
  assert_table_contract(excl)
})

test_that("bf_inclusion: a missing row empties BF, singular and plural", {
  one <- apa_table(inc_missing_row())
  expect_identical(one[["*BF*~incl~"]][2], "")
  expect_false("log(*BF*~incl~)" %in% names(one))
  expect_match(apa_note(one), missing_sentence("b"), fixed = TRUE)

  many <- apa_table(inc_missing_multi())
  expect_match(apa_note(many), missing_sentence(c("n", "o")), fixed = TRUE)
  assert_table_contract(one)
  assert_table_contract(many)
})

test_that("bf_inclusion: an infinite row empties BF, singular and plural", {
  x <- fixture("inc_inf")
  out <- apa_tidy(x)
  tab <- apa_table(out)
  kind <- inclusion_row_kind(out$bf, out$log_bf)
  expect_identical(tab[["*BF*~incl~"]][kind == "infinite"], "")
  expect_false("log(*BF*~incl~)" %in% names(tab))
  expect_match(apa_note(tab), infinite_sentence("x"), fixed = TRUE)

  many <- apa_table(inc_infinite_multi())
  expect_match(apa_note(many), infinite_sentence(c("p", "q")), fixed = TRUE)
  assert_table_contract(tab)
  assert_table_contract(many)
})

test_that("bf_inclusion: the sentences name the exclusion BF under '01'", {
  excl <- apa_note(apa_table(inc_missing_row(), bf_direction = "01"))
  expect_match(excl, missing_sentence("b", "exclusion"), fixed = TRUE)
  expect_false(grepl("no inclusion Bayes factor", excl, fixed = TRUE))

  infinite <- apa_note(
    apa_table(apa_tidy(fixture("inc_inf")), bf_direction = "01")
  )
  expect_match(infinite, infinite_sentence("x", "exclusion"), fixed = TRUE)
  # The probability the sentence gives as the reason keeps its own name.
  expect_match(
    infinite, "posterior inclusion probability rounds to 1 or 0",
    fixed = TRUE
  )
})

test_that("bf_inclusion: a lost row shows the log column and the shared sentence", { # nolint: line_length_linter.
  x <- inc_lost_row()
  tab <- apa_table(x)
  kind <- inclusion_row_kind(x$bf, x$log_bf)
  expect_true("log(*BF*~incl~)" %in% names(tab))
  expect_identical(tab[["*BF*~incl~"]][kind == "lost"], "")
  expect_identical(tab[["log(*BF*~incl~)"]], bfi_log_cell(x$log_bf, kind, "10"))
  expect_match(apa_note(tab), lost_sentence, fixed = TRUE)
  assert_table_contract(tab)
})

test_that("bf_inclusion note: the log inclusion Bayes factor is defined", {
  note <- apa_note(apa_table(inc_lost_row()))
  expect_match(
    note, "log(*BF*~incl~) = natural logarithm of *BF*~incl~",
    fixed = TRUE
  )
})

test_that("bf_inclusion: '01' names the exclusion Bayes factor and its log", {
  x <- inc_lost_row()
  tab <- apa_table(x, bf_direction = "01")
  kind <- inclusion_row_kind(x$bf, x$log_bf)
  expect_identical(
    names(tab),
    c("Term", "*P*(incl)", "*P*(incl | D)", "*BF*~excl~", "log(*BF*~excl~)")
  )
  expect_identical(tab[["*BF*~excl~"]], bfi_bf_cell(x$bf, kind, "01"))
  expect_identical(
    tab[["log(*BF*~excl~)"]], bfi_log_cell(x$log_bf, kind, "01")
  )
  note <- apa_note(tab)
  expect_match(note, "*BF*~excl~ = exclusion Bayes factor", fixed = TRUE)
  expect_match(
    note, "log(*BF*~excl~) = natural logarithm of *BF*~excl~",
    fixed = TRUE
  )
  assert_table_contract(tab)
})

test_that("bf_inclusion note: the infinite fixture's whole note", {
  expect_identical(
    apa_note(apa_table(apa_tidy(fixture("inc_inf")))),
    paste0(
      "*P*(incl) = prior inclusion probability; *P*(incl | D) = posterior ",
      "inclusion probability; *BF*~incl~ = inclusion Bayes factor, averaged ",
      "across all models. `x` has an infinite inclusion Bayes factor; its ",
      "posterior inclusion probability rounds to 1 or 0."
    )
  )
})

test_that("bf_inclusion: p_prior and p_posterior definitions", {
  note <- apa_note(apa_table(apa_tidy(fixture("inc_anova"))))
  expect_match(note, "*P*(incl) = prior inclusion probability", fixed = TRUE)
  expect_match(
    note, "*P*(incl | D) = posterior inclusion probability",
    fixed = TRUE
  )
})

test_that("bf_inclusion: averaging phrases, and none when the attribute is absent", { # nolint: line_length_linter.
  all_avg <- apa_note(apa_table(apa_tidy(fixture("inc_anova"))))
  expect_true(grepl(
    "*BF*~incl~ = inclusion Bayes factor, averaged across all models",
    all_avg,
    fixed = TRUE
  ))
  matched_avg <- apa_note(apa_table(apa_tidy(fixture("inc_matched"))))
  expect_true(grepl(
    "*BF*~incl~ = inclusion Bayes factor, averaged across matched models",
    matched_avg,
    fixed = TRUE
  ))
  absent <- apa_note(apa_table(inc_missing_row()))
  expect_true(grepl(
    "*BF*~incl~ = inclusion Bayes factor", absent,
    fixed = TRUE
  ))
  expect_false(grepl("averaged", absent, fixed = TRUE))
})

test_that("bf_inclusion: requesting only the probabilities drops BF and sentences", { # nolint: line_length_linter.
  out <- apa_tidy(fixture("inc_inf"))
  tab <- apa_table(out, stats = c("p_prior", "p_posterior"))
  expect_identical(names(tab), c("Term", "*P*(incl)", "*P*(incl | D)"))
  note <- apa_note(tab)
  expect_false(grepl("infinite", note, fixed = TRUE))
  expect_false(grepl("BF", note, fixed = TRUE))
  assert_table_contract(tab)
})

# ---- all-NA stats: the per-type source -------------------------------------

test_that("stats = character() is allowed on all four slice 2 types", {
  skip_if_not_installed("loo")
  hyp <- apa_table(fixture("tidy_hypotheses"), stats = character())
  expect_identical(names(hyp), c("Hypothesis", "*M*", "CrI"))
  loo_tab <- apa_table(
    apa_tidy(fixture("compare_loo_matrix")),
    stats = character()
  )
  expect_identical(names(loo_tab), "Model")
  expect_true(is.na(attr(loo_tab, "note")))
  bfm <- apa_table(bf_models_no_denominator_flag(), stats = character())
  expect_identical(names(bfm), "Model")
  inc <- apa_table(inc_lost_row(), stats = character())
  expect_identical(names(inc), "Term")
  expect_true(is.na(attr(inc, "note")))
  for (tab in list(hyp, loo_tab, bfm, inc)) {
    assert_table_contract(tab)
  }
})

test_that("an all-NA stats column names the per-type source", {
  expect_error(apa_table(hyp_no_bf(), stats = "bf"), regexp = "sample_prior")
  expect_error(apa_table(hyp_no_bf(), stats = "er"), regexp = "sample_prior")
  expect_error(
    apa_table(hyp_no_bf(), stats = "post_prob"),
    regexp = "sample_prior"
  )
  expect_error(
    apa_table(apa_tidy(test_bf_models_lm()), stats = c("bf", "error")),
    regexp = "BayesFactor"
  )
  no_prior <- apabayes_tidy(
    data.frame(
      term = c("a", "b"), p_prior = NA_real_, p_posterior = c(0.6, 0.4),
      bf = c(1.5, 0.7), log_bf = log(c(1.5, 0.7))
    ),
    type = "bf_inclusion", centrality = NA
  )
  expect_error(apa_table(no_prior, stats = "p_prior"), regexp = "not recorded")
  skip_if_not_installed("loo")
  no_weights <- apa_tidy(fixture("compare_loo_matrix"))
  expect_error(apa_table(no_weights, stats = "weight"), regexp = "weights")
})

# ---- group_rows aborts on every slice 2 type -------------------------------

test_that("group_rows aborts naming each of the four slice 2 types", {
  skip_if_not_installed("loo")
  expect_error(
    apa_table(fixture("tidy_hypotheses"), group_rows = TRUE),
    regexp = "group_rows"
  )
  expect_error(
    apa_table(apa_tidy(fixture("compare_loo_matrix")), group_rows = TRUE),
    regexp = "group_rows"
  )
  expect_error(
    apa_table(bf_models_no_denominator_flag(), group_rows = TRUE),
    regexp = "group_rows"
  )
  expect_error(
    apa_table(apa_tidy(fixture("inc_anova")), group_rows = TRUE),
    regexp = "group_rows"
  )
})

# ---- every slice 2 table: the contract check and decision words -----------

test_that("every slice 2 table passes the header contract and decision-word checks", { # nolint: line_length_linter.
  skip_if_not_installed("loo")
  tabs <- list(
    apa_table(fixture("tidy_hypotheses")),
    apa_table(hyp_point_only()),
    apa_table(hyp_directional_only()),
    apa_table(hyp_grouped()),
    apa_table(apa_tidy(fixture("compare_loo_matrix"))),
    apa_table(loo_na_se_table()),
    apa_table(loo_stacking_table()),
    apa_table(bf_models_underflow_table()),
    apa_table(bf_models_no_denominator_flag()),
    apa_table(apa_tidy(fixture("inc_anova"))),
    apa_table(inc_missing_row()),
    apa_table(inc_lost_row())
  )
  if (requireNamespace("BayesFactor", quietly = TRUE)) {
    tabs <- c(tabs, list(
      apa_table(apa_tidy(fixture("bf_regression"))),
      apa_table(apa_tidy(fixture("bf_ttest"))),
      apa_table(apa_tidy(fixture("bf_overflow")))
    ))
  }
  for (tab in tabs) {
    assert_table_contract(tab)
  }
  strings <- unlist(c(
    lapply(tabs, function(x) unlist(x, use.names = FALSE)),
    lapply(tabs, apa_note)
  ))
  strings <- strings[!is.na(strings)]
  for (word in decision_words) {
    expect_false(any(grepl(word, strings, ignore.case = TRUE)), info = word)
  }
})

# ---- the default method -----------------------------------------------------

test_that("default method: apa_table() on a BFBayesFactor equals the tidy method", { # nolint: line_length_linter.
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_ttest")
  direct <- apa_table(b)
  via_tidy <- apa_table(apa_tidy(b))
  expect_identical(direct, via_tidy)
  assert_table_contract(direct)
})

test_that("default method: apa_table(compare_loo_matrix, weights = w) forwards weights", { # nolint: line_length_linter.
  skip_if_not_installed("loo")
  cmp <- fixture("compare_loo_matrix")
  w <- c(good = 0.5, shifted = 0.3, wide = 0.2)
  direct <- apa_table(cmp, weights = w)
  via_tidy <- apa_table(apa_tidy(cmp, weights = w))
  expect_identical(direct, via_tidy)
  assert_table_contract(direct)
})

# ==========================================================================
# Slice 3 (spec-apa_table.md § "Slice 3"): sem_fit, contrasts, correlations
# ==========================================================================

# ---- slice 3 composition helpers ------------------------------------------

# A fit index: three decimals by default, no leading zero, aligned.
index_cell <- function(x, digits = 3, leading_zero = FALSE) {
  apa7::align_chr(apa_num(x, digits, leading_zero, markup = "md"))
}

# The S3-3 cell: the aligned value, a space, then the interval bracket the
# shared rule builds. A row without bounds keeps the value alone; a row
# without a value is empty.
index_interval_cell <- function(value, lo, hi, digits = 3,
                                leading_zero = FALSE, prefix = "") {
  v <- index_cell(value, digits, leading_zero)
  bracket <- ci_cell(lo, hi, digits, leading_zero)
  out <- paste0(v, " ", prefix, bracket)
  out[!nzchar(bracket)] <- v[!nzchar(bracket)]
  out[is.na(value)] <- ""
  out
}

n_cell <- function(x) {
  apa7::align_chr(apa_num(x, 0, big_mark = TRUE, markup = "md"))
}

# ---- slice 3 hand-built tables --------------------------------------------

# nolint next: object_usage_linter. test_lavaan_fit() is defined in setup.R.
lavaan_sem_fit <- function() apa_tidy_sem_fit(test_lavaan_fit("cfa"))

# The lavaan and blavaan rows stacked: each index family is empty in the
# other's row (measured M6).
stacked_sem_fit <- function() {
  lav <- as.data.frame(lavaan_sem_fit())
  # nolint next: object_usage_linter. fixture() is defined in setup.R.
  bla <- as.data.frame(fixture("sem_fit_blavaan"))
  lav$model <- "One factor"
  apabayes_tidy(
    rbind(lav, bla),
    type = "sem_fit", centrality = "median", ci_method = "hdi",
    ci_level = 0.9
  )
}

sem_fit_no_model <- function() {
  x <- as.data.frame(lavaan_sem_fit())
  apabayes_tidy(x,
    type = "sem_fit", centrality = NA, ci_method = NA,
    ci_level = NA
  )
}

sem_fit_fractional_df <- function() {
  x <- as.data.frame(lavaan_sem_fit())
  x$model <- "Scaled"
  x$df <- 23.47
  apabayes_tidy(x,
    type = "sem_fit", centrality = NA, ci_method = NA,
    ci_level = NA
  )
}

sem_fit_mixed_level <- function() {
  a <- as.data.frame(lavaan_sem_fit())
  a$model <- "A"
  b <- a
  b$model <- "B"
  b$rmsea_level <- 0.95
  apabayes_tidy(rbind(a, b),
    type = "sem_fit", centrality = NA,
    ci_method = NA, ci_level = NA
  )
}

# A contrasts table without a ROPE share: the emmGrid route's shape (M7).
contrasts_no_rope <- function() {
  apabayes_tidy(
    data.frame(
      contrast = c("a - b", "a - c"), estimate = c(1.5, -0.4),
      ci_low = c(0.2, -1.1), ci_high = c(2.8, 0.3), pd = c(0.98, 0.87)
    ),
    type = "contrasts", centrality = "median", ci_method = "hdi",
    ci_level = 0.95
  )
}

correlations_two_methods <- function() {
  apabayes_tidy(
    data.frame(
      term = c("x~~y", "x~~z"), var1 = c("x", "x"), var2 = c("y", "z"),
      estimate = c(0.42, -0.31), ci_low = c(0.1, -0.6),
      ci_high = c(0.7, 0.02), pd = c(0.99, 0.94), n = c(30, 28),
      method = c(
        "Bayesian Pearson correlation",
        "Bayesian Spearman correlation"
      )
    ),
    type = "correlations", centrality = "median", ci_method = "hdi",
    ci_level = 0.95
  )
}

correlations_no_method <- function() {
  apabayes_tidy(
    data.frame(
      term = "x~~y", var1 = "x", var2 = "y", estimate = 0.42,
      ci_low = 0.1, ci_high = 0.7, pd = 0.99, n = 30
    ),
    type = "correlations", centrality = "median", ci_method = "hdi",
    ci_level = 0.95
  )
}

# ---- S3-1: the table's interval label names a frequentist method ---------

test_that("a wald interval is headed CI (Wald) and a boot one CI (bootstrap)", {
  lav <- fixture("tidy_lavaan_std")
  expect_true("95% CI (Wald)" %in% names(apa_table(lav)))
  boot <- lav
  boot$ci_method <- "boot"
  tab <- apa_table(boot)
  expect_true("95% CI (bootstrap)" %in% names(tab))
  expect_match(apa_note(tab), "CI (bootstrap) = bootstrap confidence interval",
    fixed = TRUE
  )
  assert_table_contract(tab)
})

test_that("an NA ci_method is headed Interval", {
  x <- fixture("tidy_lavaan_std")
  x$ci_method <- NA_character_
  tab <- apa_table(x)
  expect_true("95% Interval" %in% names(tab))
  assert_table_contract(tab)
})

test_that("a wald table with a missing bound still passes apa7 untouched", {
  # The M2 regression: apa7 reads `95% CI` as its own CI column and
  # rewrote the empty cell to NA. The label now names the method.
  lav <- fixture("tidy_lavaan_std")
  lav$ci_low[1] <- NA_real_
  lav$ci_high[1] <- NA_real_
  tab <- apa_table(lav)
  expect_identical(tab[["95% CI (Wald)"]][1], "")
  assert_table_contract(tab)
})

test_that("apa_inline() still says CI, not CI (Wald)", {
  # The inline label is unchanged: apa7 never sees running text.
  lav <- fixture("tidy_lavaan_std")
  expect_match(apa_inline(lav)$full_result[1], "95% CI [", fixed = TRUE)
  expect_no_match(apa_inline(lav)$full_result[1], "CI (Wald)", fixed = TRUE)
})

test_that("a ci_label that is a format name behind a level aborts", {
  # apa7 strips a leading `<digits>% ` before matching (M1), which
  # check_table_headers() now does too.
  t <- fixture("tidy_brms_full")
  expect_error(apa_table(t, ci_label = "CI"), regexp = "apa7")
  expect_no_error(apa_table(t, ci_label = "CI (Wald)"))
})

# ---- sem_fit: names, order and cells ---------------------------------------

test_that("a lavaan sem_fit table shows the frequentist indices in order", {
  f <- lavaan_sem_fit()
  row <- as.data.frame(f)
  tab <- apa_table(f)
  expect_identical(
    names(tab),
    c("*χ*^2^", "*df*", "*p*", "CFI", "TLI", "RMSEA [90% CI]", "SRMR")
  )
  expect_identical(tab[["*χ*^2^"]], num_cell(row$chisq, 2))
  expect_identical(tab[["*df*"]], num_cell(row$df, 0))
  expect_identical(tab[["*p*"]], p_cell(row$p))
  expect_identical(tab[["CFI"]], index_cell(row$cfi))
  expect_identical(tab[["TLI"]], index_cell(row$tli))
  expect_identical(tab[["SRMR"]], index_cell(row$srmr))
  expect_identical(
    tab[["RMSEA [90% CI]"]],
    index_interval_cell(row$rmsea, row$rmsea_low, row$rmsea_high)
  )
  assert_table_contract(tab)
})

test_that("a blavaan sem_fit table shows the Bayesian indices", {
  b <- fixture("sem_fit_blavaan")
  row <- as.data.frame(b)
  tab <- apa_table(b)
  expect_identical(
    names(tab),
    c("Model", "PPP", "BRMSEA [90% HDI]", "BΓ̂ [90% HDI]")
  )
  expect_identical(tab[["Model"]], row$model)
  expect_identical(tab[["PPP"]], index_cell(row$ppp))
  expect_identical(
    tab[["BRMSEA [90% HDI]"]],
    index_interval_cell(row$brmsea, row$brmsea_low, row$brmsea_high)
  )
  expect_identical(
    tab[["BΓ̂ [90% HDI]"]],
    index_interval_cell(row$bgammahat, row$bgammahat_low, row$bgammahat_high)
  )
  assert_table_contract(tab)
})

test_that("a stacked table shows both index families with empty cells", {
  s <- stacked_sem_fit()
  tab <- apa_table(s)
  expect_identical(
    names(tab),
    c(
      "Model", "*χ*^2^", "*df*", "*p*", "CFI", "TLI", "RMSEA [90% CI]",
      "SRMR", "PPP", "BRMSEA [90% HDI]", "BΓ̂ [90% HDI]"
    )
  )
  expect_identical(tab[["Model"]], c("One factor", "Two factors"))
  expect_identical(tab[["CFI"]][2], "")
  expect_identical(tab[["PPP"]][1], "")
  expect_identical(tab[["RMSEA [90% CI]"]][2], "")
  assert_table_contract(tab)
})

test_that("the Model column appears only when some row records one", {
  expect_false("Model" %in% names(apa_table(sem_fit_no_model())))
  expect_true("Model" %in% names(apa_table(fixture("sem_fit_blavaan"))))
})

test_that("df prints as an integer when whole and with digits when not", {
  whole <- apa_table(lavaan_sem_fit())
  expect_identical(whole[["*df*"]], "24")
  frac <- sem_fit_fractional_df()
  tab <- apa_table(frac)
  expect_identical(tab[["*df*"]], num_cell(as.data.frame(frac)$df, 2))
  expect_match(tab[["*df*"]], "23.47", fixed = TRUE)
  assert_table_contract(tab)
})

test_that("sem_fit digits: NULL gives chi-square 2 and the indices 3", {
  f <- lavaan_sem_fit()
  row <- as.data.frame(f)
  default <- apa_table(f)
  expect_identical(default[["*χ*^2^"]], num_cell(row$chisq, 2))
  expect_identical(default[["CFI"]], index_cell(row$cfi, 3))
  one <- apa_table(f, digits = 1)
  expect_identical(one[["*χ*^2^"]], num_cell(row$chisq, 1))
  expect_identical(one[["CFI"]], index_cell(row$cfi, 1))
  assert_table_contract(one)
})

test_that("sem_fit leading_zero drops it on indices, keeps it on chi-square", {
  f <- lavaan_sem_fit()
  row <- as.data.frame(f)
  auto <- apa_table(f)
  expect_match(auto[["CFI"]], "^\\.", perl = TRUE)
  expect_match(auto[["*χ*^2^"]], "^85", perl = TRUE)
  forced <- apa_table(f, leading_zero = TRUE)
  expect_identical(forced[["CFI"]], index_cell(row$cfi, 3, TRUE))
  assert_table_contract(forced)
})

test_that("sem_fit stats subsets, and character() gives Model alone", {
  s <- stacked_sem_fit()
  sub <- apa_table(s, stats = c("cfi", "chisq"))
  expect_identical(names(sub), c("Model", "*χ*^2^", "*df*", "*p*", "CFI"))
  bare <- apa_table(s, stats = character())
  expect_identical(names(bare), "Model")
  expect_error(
    apa_table(sem_fit_no_model(), stats = character()),
    regexp = "no column|nothing"
  )
  assert_table_contract(sub)
  assert_table_contract(bare)
})

test_that("sem_fit stats naming an all-NA index names the route", {
  expect_error(
    apa_table(lavaan_sem_fit(), stats = "ppp"),
    regexp = "blavaan"
  )
  expect_error(
    apa_table(fixture("sem_fit_blavaan"), stats = "cfi"),
    regexp = "lavaan"
  )
})

# ---- sem_fit: intervals ------------------------------------------------

test_that("interval = FALSE drops the bracket from header and cell", {
  f <- lavaan_sem_fit()
  row <- as.data.frame(f)
  tab <- apa_table(f, interval = FALSE)
  expect_true("RMSEA" %in% names(tab))
  expect_false(any(grepl("[", names(tab), fixed = TRUE)))
  expect_identical(tab[["RMSEA"]], index_cell(row$rmsea))
  assert_table_contract(tab)
})

test_that("RMSEA is labelled CI even when the table's ci_method is hdi", {
  s <- stacked_sem_fit()
  expect_true("RMSEA [90% CI]" %in% names(apa_table(s)))
  expect_true("BRMSEA [90% HDI]" %in% names(apa_table(s)))
})

test_that("mixed rmsea levels move the level into the cells", {
  m <- sem_fit_mixed_level()
  row <- as.data.frame(m)
  tab <- apa_table(m)
  expect_true("RMSEA [CI]" %in% names(tab))
  expect_identical(
    tab[["RMSEA [CI]"]],
    index_interval_cell(
      row$rmsea, row$rmsea_low, row$rmsea_high,
      prefix = c("90% ", "95% ")
    )
  )
  assert_table_contract(tab)
})

test_that("ci_label overrides the bracketed labels", {
  tab <- apa_table(fixture("sem_fit_blavaan"), ci_label = "CrI")
  expect_true("BRMSEA [90% CrI]" %in% names(tab))
  assert_table_contract(tab)
})

# ---- sem_fit: note ---------------------------------------------------------

test_that("the lavaan sem_fit note defines the four indices and the interval", {
  note <- apa_note(apa_table(lavaan_sem_fit()))
  expect_identical(
    note,
    paste0(
      "CFI = comparative fit index; TLI = Tucker–Lewis index; ",
      "RMSEA = root mean square error of approximation, with its 90% ",
      "confidence interval; SRMR = standardized root mean square residual."
    )
  )
  expect_no_match(note, "χ", fixed = TRUE)
})

test_that("the blavaan sem_fit note defines the Bayesian indices", {
  note <- apa_note(apa_table(fixture("sem_fit_blavaan")))
  expect_identical(
    note,
    paste0(
      "PPP = posterior predictive *p*-value; BRMSEA = Bayesian root mean ",
      "square error of approximation, with its 90% highest density interval; ",
      "BΓ̂ = Bayesian gamma-hat, with its 90% highest density interval."
    )
  )
})

test_that("interval = FALSE drops the interval phrase from the note", {
  note <- apa_note(apa_table(lavaan_sem_fit(), interval = FALSE))
  expect_match(note, "RMSEA = root mean square error of approximation;",
    fixed = TRUE
  )
  expect_no_match(note, "with its", fixed = TRUE)
})

test_that("a Bayesian index built with a bare NA ci_method still tabulates", {
  # The constructor canonicalises `ci_method`: a logical NA used as a
  # subscript into a named character vector recycles across the whole
  # vector, which made this table abort before the attribute was stored
  # as NA_character_.
  x <- apabayes_tidy(
    data.frame(
      model = "M", brmsea = 0.09, brmsea_low = 0.07, brmsea_high = 0.12
    ),
    type = "sem_fit", centrality = NA, ci_method = NA, ci_level = NA
  )
  expect_identical(attr(x, "ci_method", exact = TRUE), NA_character_)
  tab <- apa_table(x)
  # No level is recorded, so none is printed: not `NA%`.
  expect_identical(names(tab), c("Model", "BRMSEA [Interval]"))
  expect_no_match(apa_note(tab), "with its", fixed = TRUE)
  assert_table_contract(tab)
})

test_that("a Bayesian index with no ci_method gets no interval phrase", {
  x <- fixture("sem_fit_blavaan")
  attr(x, "ci_method") <- NA_character_
  note <- apa_note(apa_table(x))
  expect_match(
    note, "BRMSEA = Bayesian root mean square error of approximation;",
    fixed = TRUE
  )
  expect_no_match(note, "with its", fixed = TRUE)
  # The bracket still names the level, which the table does record.
  expect_true("BRMSEA [90% Interval]" %in% names(apa_table(x)))
})

test_that("a renamed RMSEA label keeps the note's own description", {
  # The label is the user's; the description follows the interval, as on
  # a parameters table.
  tab <- apa_table(lavaan_sem_fit(), ci_label = "Bounds")
  expect_true("RMSEA [90% Bounds]" %in% names(tab))
  expect_match(apa_note(tab), "with its 90% confidence interval", fixed = TRUE)
  assert_table_contract(tab)
})

# ---- contrasts -------------------------------------------------------------

test_that("a contrasts table shows contrast, estimate, interval, pd and ROPE", {
  x <- apa_tidy(fixture("mb_contrasts"))
  row <- as.data.frame(x)
  tab <- apa_table(x)
  expect_identical(
    names(tab), c("Contrast", "*Mdn*", "95% CrI", "*pd*", "% in ROPE")
  )
  expect_identical(tab[["Contrast"]], row$contrast)
  expect_identical(tab[["*Mdn*"]], num_cell(row$estimate))
  expect_identical(tab[["95% CrI"]], ci_cell(row$ci_low, row$ci_high))
  expect_identical(tab[["*pd*"]], pd_cell(row$pd))
  expect_identical(tab[["% in ROPE"]], rope_cell(row$rope_pct))
  assert_table_contract(tab)
})

test_that("contrasts: Group appears only when some row has one", {
  x <- apa_tidy(fixture("mb_contrasts_by"))
  tab <- apa_table(x)
  expect_identical(names(tab)[1:2], c("Contrast", "Group"))
  expect_identical(tab[["Group"]], as.data.frame(x)$group)
  expect_false("Group" %in% names(apa_table(apa_tidy(fixture("mb_contrasts")))))
  assert_table_contract(tab)
})

test_that("the emmGrid contrasts table is headed 95% HDI and has no ROPE", {
  x <- contrasts_no_rope()
  tab <- apa_table(x)
  expect_identical(names(tab), c("Contrast", "*Mdn*", "95% HDI", "*pd*"))
  expect_false("% in ROPE" %in% names(tab))
  assert_table_contract(tab)
})

test_that("contrasts: NA centrality gives Estimate; stats subsets", {
  x <- contrasts_no_rope()
  attr(x, "centrality") <- NA_character_
  expect_identical(names(apa_table(x))[2], "Estimate")
  only_pd <- apa_table(apa_tidy(fixture("mb_contrasts")), stats = "pd")
  expect_identical(
    names(only_pd), c("Contrast", "*Mdn*", "95% CrI", "*pd*")
  )
  bare <- apa_table(apa_tidy(fixture("mb_contrasts")), stats = character())
  expect_identical(names(bare), c("Contrast", "*Mdn*", "95% CrI"))
  assert_table_contract(only_pd)
  assert_table_contract(bare)
})

test_that("the contrasts note defines the estimate, interval, pd and ROPE", {
  note <- apa_note(apa_table(apa_tidy(fixture("mb_contrasts"))))
  expect_identical(
    note,
    paste0(
      "*Mdn* = posterior median; CrI = equal-tailed credible interval; ",
      "*pd* = probability of direction; % in ROPE = percentage of the 95% ",
      "posterior interval inside the region of practical equivalence ",
      "[−0.10, 0.10]."
    )
  )
  expect_no_match(note, "standardized", fixed = TRUE)
})

test_that("group_rows aborts on a contrasts table", {
  expect_error(
    apa_table(contrasts_no_rope(), group_rows = TRUE),
    regexp = "contrasts"
  )
})

# ---- correlations ----------------------------------------------------------

test_that("a correlations table names its pair in two columns", {
  x <- apa_tidy(fixture("cor_default"))
  row <- as.data.frame(x)
  tab <- apa_table(x)
  expect_identical(
    names(tab),
    c("Variable 1", "Variable 2", "*r*", "95% HDI", "*pd*", "*BF*~10~", "*n*")
  )
  expect_identical(tab[["Variable 1"]], row$var1)
  expect_identical(tab[["Variable 2"]], row$var2)
  expect_identical(tab[["*r*"]], num_cell(row$estimate, leading_zero = FALSE))
  expect_identical(
    tab[["95% HDI"]],
    ci_cell(row$ci_low, row$ci_high, leading_zero = FALSE)
  )
  expect_identical(tab[["*pd*"]], pd_cell(row$pd))
  expect_identical(tab[["*BF*~10~"]], bf_cell(row$bf))
  expect_identical(tab[["*n*"]], n_cell(row$n))
  assert_table_contract(tab)
})

test_that("correlations: the default drops pd and ROPE when they are NA", {
  x <- apa_tidy(fixture("cor_bf"))
  tab <- apa_table(x)
  expect_identical(
    names(tab),
    c("Variable 1", "Variable 2", "*r*", "95% HDI", "*BF*~10~", "*n*")
  )
  expect_false("*pd*" %in% names(tab))
  expect_false("% in ROPE" %in% names(tab))
  assert_table_contract(tab)
})

test_that("correlations: n differs between rows and Group is shown", {
  na <- apa_tidy(fixture("cor_na"))
  tab <- apa_table(na)
  expect_identical(tab[["*n*"]], n_cell(as.data.frame(na)$n))
  expect_false(length(unique(tab[["*n*"]])) == 1)
  grouped <- apa_tidy(fixture("cor_grouped"))
  g <- apa_table(grouped)
  expect_identical(names(g)[1:3], c("Variable 1", "Variable 2", "Group"))
  expect_identical(g[["Group"]], as.data.frame(grouped)$group)
  assert_table_contract(tab)
  assert_table_contract(g)
})

test_that("correlations drop the leading zero and follow bf_direction", {
  x <- apa_tidy(fixture("cor_default"))
  row <- as.data.frame(x)
  tab <- apa_table(x)
  expect_false(any(grepl("^-?0\\.", tab[["*r*"]])))
  forced <- apa_table(x, leading_zero = TRUE)
  expect_identical(forced[["*r*"]], num_cell(row$estimate, leading_zero = TRUE))
  one <- apa_table(x, bf_direction = "01")
  expect_true("*BF*~01~" %in% names(one))
  expect_identical(one[["*BF*~01~"]], bf_cell(row$bf, "01"))
  assert_table_contract(forced)
  assert_table_contract(one)
})

test_that("correlations: stats subsets and rope column", {
  x <- apa_tidy(fixture("cor_default"))
  rope <- apa_table(x, stats = c("pd", "rope"))
  expect_identical(
    names(rope),
    c("Variable 1", "Variable 2", "*r*", "95% HDI", "*pd*", "% in ROPE")
  )
  bare <- apa_table(x, stats = character())
  expect_identical(names(bare), c("Variable 1", "Variable 2", "*r*", "95% HDI"))
  assert_table_contract(rope)
  assert_table_contract(bare)
})

test_that("the correlations note names the method and the n", {
  note <- apa_note(apa_table(apa_tidy(fixture("cor_default"))))
  expect_identical(
    note,
    paste0(
      "*r* = Bayesian Pearson correlation; HDI = highest density interval; ",
      "*pd* = probability of direction; *BF*~10~ = Bayes factor of the ",
      "alternative over the null hypothesis; *n* = number of complete pairs."
    )
  )
})

test_that("two methods join with `or`; no method gives no definition", {
  two <- apa_note(apa_table(correlations_two_methods()))
  expect_match(
    two,
    "*r* = Bayesian Pearson correlation or Bayesian Spearman correlation;",
    fixed = TRUE
  )
  none <- apa_note(apa_table(correlations_no_method()))
  expect_no_match(none, "*r* =", fixed = TRUE)
  # A table that has the column but records nothing in it says no more
  # than one that has no column at all.
  all_na <- correlations_two_methods()
  all_na$method <- NA_character_
  expect_no_match(apa_note(apa_table(all_na)), "*r* =", fixed = TRUE)
})

test_that("the correlations ROPE definition has neither level nor range", {
  # The correlation route records no rope_ci and no rope_range (M8).
  note <- apa_note(apa_table(apa_tidy(fixture("cor_default")),
    stats = c("pd", "rope")
  ))
  expect_match(
    note,
    "% in ROPE = percentage of the posterior interval inside the region of practical equivalence.", # nolint: line_length_linter.
    fixed = TRUE
  )
})

test_that("group_rows aborts on a sem_fit and a correlations table", {
  expect_error(
    apa_table(fixture("sem_fit_blavaan"), group_rows = TRUE),
    regexp = "sem_fit"
  )
  expect_error(
    apa_table(correlations_stub(), group_rows = TRUE),
    regexp = "correlations"
  )
})

# ---- slice 3 default method ------------------------------------------------

test_that("the default method reaches the three new types", {
  cor_direct <- apa_table(fixture("cor_default"))
  expect_identical(cor_direct, apa_table(apa_tidy(fixture("cor_default"))))
  grid <- test_emm_grid("pairs")
  expect_identical(
    apa_table(grid, ci = "eti"), apa_table(apa_tidy(grid, ci = "eti"))
  )
  assert_table_contract(cor_direct)
})

test_that("no slice 3 table shows a verdict word", {
  tabs <- list(
    apa_table(lavaan_sem_fit()),
    apa_table(fixture("sem_fit_blavaan")),
    apa_table(apa_tidy(fixture("mb_contrasts"))),
    apa_table(apa_tidy(fixture("cor_default")))
  )
  for (tab in tabs) {
    note <- attr(tab, "note", exact = TRUE)
    expect_false(any(vapply(decision_words, function(w) {
      any(grepl(w, c(unlist(tab), note), ignore.case = TRUE))
    }, logical(1))))
  }
})
