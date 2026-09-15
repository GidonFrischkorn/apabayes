# Tests for the apa_print() methods registered on papaja
# (local/specs/spec-apa_print.md).
#
# The name rule is papaja's own sanitize_terms(), run on the ids below
# and recorded in local/probes/probe_papaja.log; the expected names are
# that output, not a reimplementation. Stored-table tests run on CRAN;
# the fit tests go through papaja::apa_print(), so they test the
# registration as well as the method.

# ---- row names -----------------------------------------------------------

test_that("row names follow papaja's sanitising rule", {
  ids <- c(
    "(Intercept)", "b_wt", "factor(cyl)6", "wt:hp", "visual=~x1",
    "x1~~x1", "a < b", "sd_Intercept__id", "x1 ~1",
    "visual =~ x1 (Pasteur)", "(Intercept) (cyl_f)", "(wt) < 0",
    "Two factors", "Grant-White", "a.b", "β", "x1~1.g2", "__x",
    "1abc"
  )
  expect_identical(
    sanitize_print_names(ids),
    c(
      "Intercept", "b_wt", "factorcyl6", "wt_hp", "visual_x1", "x1_x1",
      "a_b", "sd_Intercept_id", "x1_1", "visual_x1_Pasteur",
      "Intercept_cyl_f", "wt_0", "Two_factors", "Grant_White", "a_b",
      "β", "x1_1_g2", "x", "1abc"
    )
  )
})

test_that("duplicates get a suffix and empty names the row position", {
  t <- apabayes_tidy(
    data.frame(
      term = c("h1", "h2", "x", "y", "h3"),
      label = c("a < 0", "a > 0", NA, "()", "a = 0"),
      estimate = as.numeric(1:5), component = NA_character_
    ),
    type = "parameters"
  )
  expect_identical(
    apa_print_names(t),
    c("a_0", "a_0_2", "x", "row4", "a_0_3")
  )
})

test_that("a generated suffix never takes a name another row has", {
  t <- apabayes_tidy(
    data.frame(
      hypothesis = c("(wt) < 0", "(wt) > 0", "wt_0_2"),
      estimate = c(1, 2, 3)
    ),
    type = "hypotheses"
  )
  nms <- apa_print_names(t)
  expect_identical(nms, c("wt_0", "wt_0_3", "wt_0_2"))
  expect_false(anyDuplicated(nms) > 0)
})

# ---- stored tables -------------------------------------------------------

test_that("every row gives named lists of apa_inline()'s strings", {
  t <- fixture("tidy_brms_full")
  r <- apa_print.apabayes_tidy(t)
  ref <- apa_inline(t)
  expect_s3_class(
    r, c("apabayes_results", "apa_results", "list"),
    exact = TRUE
  )
  nms <- c("Intercept", "wt", "am", "sigma")
  for (el in c("estimate", "statistic", "full_result")) {
    expect_type(r[[el]], "list")
    expect_named(r[[el]], nms)
    expect_identical(unlist(r[[el]], use.names = FALSE), ref[[el]])
  }
  expect_identical(r$full_result$wt, apa_inline(t, "wt")$full_result)
  expect_identical(r$table, ref$table)
  expect_identical(r$markup, ref$markup)
})

test_that("a term gives exactly what apa_inline() gives", {
  t <- fixture("tidy_brms_full")
  expect_identical(apa_print.apabayes_tidy(t, "wt"), apa_inline(t, "wt"))
  expect_identical(
    apa_print.apabayes_tidy(t, "wt", interval = FALSE, markup = "latex"),
    apa_inline(t, "wt", interval = FALSE, markup = "latex")
  )
})

test_that("group without a term lists that group's rows", {
  t <- fixture("tidy_lavaan_groups")
  r <- apa_print.apabayes_tidy(t, group = "Grant-White")
  expect_length(r$full_result, sum(t$group == "Grant-White"))
  expect_identical(names(r$full_result)[1], "visual_x1_Grant_White")
  expect_identical(
    r$full_result$visual_x1_Grant_White,
    apa_inline(t, "visual", "x1", op = "=~", group = "Grant-White")$full_result
  )
})

test_that("hypotheses and diagnostics tables are named by their ids", {
  h <- apa_print.apabayes_tidy(fixture("tidy_hypotheses"))
  expect_named(h$full_result, c("wt_0", "am_0"))
  expect_identical(
    h$full_result$wt_0,
    apa_inline(fixture("tidy_hypotheses"), "(wt) < 0")$full_result
  )
  d <- apa_print.apabayes_tidy(fixture("diag_brms_full"))
  expect_named(
    d$statistic, c("b_Intercept", "b_wt", "b_am", "sigma", "Intercept")
  )
})

test_that("a sem_fit table keeps its NA estimate as a list element", {
  r <- apa_print.apabayes_tidy(fixture("sem_fit_blavaan"))
  expect_named(r$full_result, "Two_factors")
  expect_identical(r$estimate, list(Two_factors = NA_character_))
  expect_identical(
    r$full_result$Two_factors,
    apa_inline(fixture("sem_fit_blavaan"))$full_result
  )
})

# ---- in_paren ------------------------------------------------------------

test_that("in_paren turns parentheses into brackets", {
  t <- apa_tidy_sem_fit(test_lavaan_fit("cfa"))
  plain <- apa_inline(t)
  r <- apa_print.apabayes_tidy(t, in_paren = TRUE)
  expect_named(r$statistic, "row1")
  expected <- gsub(")", "]", gsub("(", "[", plain$statistic, fixed = TRUE),
    fixed = TRUE
  )
  expect_identical(r$statistic$row1, expected)
  expect_match(
    r$full_result$row1, paste0(symbol("chisq", "md"), "[24] = "),
    fixed = TRUE
  )
  expect_false(grepl("(", r$full_result$row1, fixed = TRUE))
  expect_identical(r$estimate$row1, NA_character_)
})

test_that("in_paren must be TRUE or FALSE", {
  t <- fixture("tidy_brms_full")
  expect_error(apa_print.apabayes_tidy(t, in_paren = NA), "in_paren")
  expect_error(
    apa_print.apabayes_tidy(t, in_paren = c(TRUE, FALSE)), "in_paren"
  )
  expect_error(apa_print.apabayes_tidy(t, in_paren = "yes"), "in_paren")
})

# ---- the list shape on the result methods ---------------------------------

test_that("the list shape prints named lines and formats unnamed", {
  r <- apa_print.apabayes_tidy(fixture("tidy_brms_full"))
  strings <- unlist(r$full_result, use.names = FALSE)
  expect_output(
    print(r),
    paste0(c("Intercept", "wt", "am", "sigma"), ": ", strings,
      collapse = "\n"
    ),
    fixed = TRUE
  )
  vis <- NULL
  utils::capture.output(vis <- withVisible(print(r)))
  expect_false(vis$visible)
  expect_identical(format(r), strings)
  expect_identical(as.character(r), strings)
  skip_if_not_installed("knitr")
  expect_identical(knitr::knit_print(r, inline = TRUE), strings)
})

test_that("papaja::apa_table() accepts the list shape", {
  skip_if_not_installed("papaja")
  expect_s3_class(
    papaja::apa_table(apa_print.apabayes_tidy(fixture("tidy_brms_full"))),
    "knit_asis"
  )
})

# ---- registration on papaja ----------------------------------------------

test_that("the methods are registered on papaja's generic, and only those", {
  skip_if_not_installed("papaja")
  registered <- as.character(utils::methods(papaja::apa_print))
  ours <- paste0("apa_print.", c(
    "brmsfit", "stanreg", "lavaan", "blavaan", "brmshypothesis",
    "apabayes_tidy", "compare.loo", "bayesfactor_models"
  ))
  expect_true(all(ours %in% registered))
  s3 <- get(".__S3MethodsTable__.", envir = asNamespace("papaja"))
  for (cls in c("BFBayesFactor", "emmGrid")) {
    method <- get(paste0("apa_print.", cls), envir = s3)
    expect_identical(environmentName(environment(method)), "papaja")
  }
})

test_that("a lavaan fit dispatches through papaja::apa_print", {
  skip_if_not_installed("papaja")
  fit <- test_lavaan_fit("cfa")
  r <- papaja::apa_print(fit, standardize = TRUE)
  ref <- apa_inline(fit, standardize = TRUE)
  expect_identical(unlist(r$full_result, use.names = FALSE), ref$full_result)
  expect_identical(names(r$full_result)[1:2], c("visual_x1", "visual_x2"))
  expect_identical(
    papaja::apa_print(fit, "visual", "x2", op = "=~"),
    apa_inline(fit, "visual", "x2", op = "=~")
  )
})

test_that("a stored table dispatches through papaja::apa_print", {
  skip_if_not_installed("papaja")
  t <- fixture("tidy_brms_full")
  expect_identical(papaja::apa_print(t), apa_print.apabayes_tidy(t))
})

test_that("a blavaan fit reaches the blavaan route through papaja", {
  skip_if_not_installed("papaja")
  fit <- test_blavaan_fit("one")
  r <- papaja::apa_print(fit)
  expect_identical(attr(r$table, "source_class"), "blavaan")
  expect_identical(
    unlist(r$full_result, use.names = FALSE),
    apa_inline(fit)$full_result
  )
  # blavaan reports free parameters only: the marker loading has no row.
  expect_identical(names(r$full_result)[1], "visual_x2")
})

test_that("brmsfit, stanreg and brmshypothesis dispatch through papaja", {
  skip_if_not_installed("papaja")
  fit <- test_brms_fit("full")
  expect_identical(papaja::apa_print(fit, "wt"), apa_inline(fit, "wt"))
  expect_named(
    papaja::apa_print(fit)$full_result,
    c("Intercept", "wt", "am", "sigma")
  )
  h <- brms::hypothesis(fit, c("wt < 0", "am = 0"))
  rh <- papaja::apa_print(h)
  expect_identical(
    unlist(rh$full_result, use.names = FALSE),
    apa_inline(h)$full_result
  )
  sr <- test_stanreg_fit("full")
  expect_identical(papaja::apa_print(sr, "wt"), apa_inline(sr, "wt"))
})
