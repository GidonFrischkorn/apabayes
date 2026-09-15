# Tests for the emmGrid route (local/specs/spec-apa_tidy_emmGrid.md).
#
# No expected number is typed: every one is read from emmeans's own
# `summary()` of the grid (its HPD interval, which the route reports as
# the HDI, and its point estimate, which the route must reproduce bit for
# bit) or from `model_parameters()` (pd,
# the equal-tailed interval, the ROPE share). The grids are built by
# `emmeans::qdrg()` from simulated draws (`test_emm_grid()`), so they run
# on CRAN; the two live fits are off CRAN.

# The grid's own summary as a plain data frame, its point-estimate column
# renamed `estimate`: emmeans calls it `emmean` on a means grid and
# `estimate` on a contrast grid (its `estName`).
emm_summary <- function(x, ...) {
  s <- summary(x, ...)
  out <- as.data.frame(s)
  names(out)[names(out) == attr(s, "estName")] <- "estimate"
  out
}

# ---- contract ------------------------------------------------------------

test_that("apa_tidy() on a contrast grid returns the contrasts contract", {
  g <- test_emm_grid("pairs")
  out <- apa_tidy(g)
  s <- emm_summary(g)
  mp <- as.data.frame(parameters::model_parameters(g))
  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "contrasts")
  expect_identical(
    names(out),
    c(
      "contrast", "group", "estimate", "ci_low", "ci_high", "ci_method",
      "ci_level", "pd", "rope_pct"
    )
  )
  expect_identical(out$contrast, as.character(g@grid$contrast))
  expect_identical(out$group, rep(NA_character_, 3))
  expect_identical(out$estimate, unname(s$estimate))
  expect_identical(out$ci_low, unname(s$lower.HPD))
  expect_identical(out$ci_high, unname(s$upper.HPD))
  expect_identical(out$ci_method, rep("hdi", 3))
  expect_identical(out$ci_level, rep(0.95, 3))
  expect_identical(out$pd, mp$pd)
  expect_identical(out$rope_pct, rep(NA_real_, 3))
})

test_that("the attributes name the interval, the grid kind and the packages", {
  g <- test_emm_grid("pairs")
  out <- apa_tidy(g)
  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "hdi")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), "emmGrid")
  expect_named(
    attr(out, "package_versions"),
    c("emmeans", "parameters", "bayestestR", "apabayes")
  )
  expect_null(attr(out, "by"))
  expect_identical(attr(out, "est_type"), "pairs")
  expect_identical(attr(out, "contrast_method"), "pairwise differences")
  expect_null(attr(out, "rope_range"))
  expect_null(attr(out, "rope_ci"))
})

test_that("apa_tidy() on an emmGrid returns visibly", {
  expect_true(withVisible(apa_tidy(test_emm_grid("pairs")))$visible)
})

# ---- the reporting arguments ---------------------------------------------

test_that("ci = 'eti' reports the equal-tailed interval and says so", {
  g <- test_emm_grid("pairs")
  out <- apa_tidy(g, ci = "eti")
  mp <- as.data.frame(parameters::model_parameters(g, ci_method = "eti"))
  expect_identical(out$ci_low, mp$CI_low)
  expect_identical(out$ci_high, mp$CI_high)
  expect_identical(out$ci_method, rep("eti", 3))
  expect_identical(attr(out, "ci_method"), "eti")
  expect_false(identical(out$ci_low, apa_tidy(g)$ci_low))
})

test_that("ci_level reaches the interval", {
  g <- test_emm_grid("pairs")
  out <- apa_tidy(g, ci_level = 0.9)
  s <- emm_summary(g, level = 0.9)
  expect_identical(out$ci_low, unname(s$lower.HPD))
  expect_identical(out$ci_high, unname(s$upper.HPD))
  expect_identical(out$ci_level, rep(0.9, 3))
  expect_identical(attr(out, "ci_level"), 0.9)
})

test_that("centrality = 'mean' reports the posterior mean", {
  g <- test_emm_grid("pairs")
  out <- apa_tidy(g, centrality = "mean")
  s <- emm_summary(g, point.est = mean)
  expect_identical(out$estimate, unname(s$estimate))
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("rope is opt-in and its values follow model_parameters()", {
  g <- test_emm_grid("pairs")
  mp <- as.data.frame(parameters::model_parameters(
    g,
    test = c("pd", "rope"), rope_range = c(-1, 1), rope_ci = 1
  ))
  out <- apa_tidy(g, rope = c(-1, 1), rope_ci = 1)
  expect_identical(out$rope_pct, mp$ROPE_Percentage)
  expect_identical(out$pd, mp$pd)
  expect_identical(attr(out, "rope_range"), c(-1, 1))
  expect_identical(attr(out, "rope_ci"), 1)
})

# ---- means grids and their labels ---------------------------------------

test_that("a means grid is labelled as emmeans labels it", {
  for (name in c("means", "at", "means_two")) {
    g <- test_emm_grid(name)
    out <- apa_tidy(g)
    oracle <- emmeans::contrast(g, "identity")@grid$contrast
    expect_identical(out$contrast, as.character(oracle), label = name)
    expect_identical(attr(out, "est_type"), "prediction", label = name)
    expect_identical(attr(out, "contrast_method"), "emmeans", label = name)
    s <- emm_summary(g)
    expect_identical(out$estimate, unname(s$estimate), label = name)
    expect_identical(out$ci_low, unname(s$lower.HPD), label = name)
  }
})

test_that("the grid variables of a means grid are kept as extra columns", {
  out <- apa_tidy(test_emm_grid("means"))
  expect_identical(names(out)[10], "cyl_f")
  expect_identical(out$cyl_f, c("4", "6", "8"))
  expect_identical(out$contrast, c("cyl_f4", "cyl_f6", "cyl_f8"))
  two <- apa_tidy(test_emm_grid("means_two"))
  expect_identical(names(two)[10:11], c("cyl_f", "am_f"))
  expect_identical(two$am_f, rep(c("auto", "manual"), each = 3))
  expect_identical(two$contrast[1:2], c("cyl_f4 auto", "cyl_f6 auto"))
  at <- apa_tidy(test_emm_grid("at"))
  expect_identical(at$wt, c(2.5, 3.5))
  expect_identical(at$contrast, c("wt2.5", "wt3.5"))
  expect_false(".wgt." %in% names(at))
})

# ---- by groups -----------------------------------------------------------

test_that("a by variable becomes the group column and the by attribute", {
  g <- test_emm_grid("by")
  out <- apa_tidy(g)
  s <- emm_summary(g)
  expect_identical(out$group, as.character(g@grid$am_f))
  expect_identical(out$am_f, as.character(g@grid$am_f))
  expect_identical(attr(out, "by"), "am_f")
  expect_identical(out$contrast, as.character(g@grid$contrast))
  expect_identical(out$estimate, unname(s$estimate))
  expect_identical(out$ci_low, unname(s$lower.HPD))
  expect_identical(out$ci_high, unname(s$upper.HPD))
})

test_that("two by variables are pasted into one group value", {
  g <- test_emm_grid("two_by")
  out <- apa_tidy(g)
  expect_identical(attr(out, "by"), c("am_f", "wt"))
  expect_identical(
    out$group,
    paste(as.character(g@grid$am_f), as.character(g@grid$wt))
  )
  expect_identical(out$wt, g@grid$wt)
  expect_identical(out$am_f, as.character(g@grid$am_f))
})

# ---- other kinds of grid -------------------------------------------------

test_that("a custom contrast is reported under its own name", {
  g <- test_emm_grid("custom")
  out <- apa_tidy(g)
  expect_identical(out$contrast, "4 vs rest")
  expect_identical(attr(out, "est_type"), "contrast")
  # A list method carries no description (measured); a named one does.
  expect_identical(attr(out, "contrast_method"), NA_character_)
  trt <- emmeans::contrast(test_emm_grid("means"), "trt.vs.ctrl")
  expect_identical(
    attr(apa_tidy(trt), "contrast_method"),
    "differences from control"
  )
  expect_identical(out$estimate, unname(emm_summary(g)$estimate))
})

test_that("a subset grid reports its rows", {
  g <- test_emm_grid("pairs")
  full <- apa_tidy(g)
  out <- apa_tidy(g[1:2])
  expect_identical(nrow(out), 2L)
  expect_identical(out$contrast, full$contrast[1:2])
  expect_identical(out$estimate, full$estimate[1:2])
  expect_identical(out$ci_low, full$ci_low[1:2])
})

# ---- what the route refuses ----------------------------------------------

test_that("a grid without posterior draws is refused", {
  expect_error(apa_tidy(test_emm_grid("frequentist")), "posterior")
})

test_that("an emm_list is refused naming its parts", {
  el <- test_emm_grid("list")
  msg <- conditionMessage(rlang::catch_cnd(apa_tidy(el), "error"))
  # The rendered text, not a loose pattern: the hint once printed its
  # second part as literal cli markup and a regex let it through.
  expect_match(
    msg,
    "`x` is an <emm_list> holding emmeans and contrasts, tables of",
    fixed = TRUE
  )
  expect_match(
    msg,
    "Pass one of its parts: `x$emmeans` or `x$contrasts`.",
    fixed = TRUE
  )
  expect_no_match(msg, "{.", fixed = TRUE)
  one <- structure(el[1], class = class(el))
  msg_one <- conditionMessage(rlang::catch_cnd(apa_tidy(one), "error"))
  expect_match(msg_one, "Pass one of its parts: `x$emmeans`.", fixed = TRUE)
  expect_no_match(msg_one, "or", fixed = TRUE)
})

test_that("an empty grid is refused", {
  g <- test_emm_grid("pairs")
  expect_error(apa_tidy(g[integer(0)]), "no rows")
})

test_that("rows that cannot be told apart are refused", {
  g <- test_emm_grid("pairs")
  # `[` with a repeated index duplicates a grid row.
  expect_error(apa_tidy(g[c(1, 1, 2)]), "cannot be matched")
  expect_error(apa_tidy(g[c(1, 1, 2)]), "distinct")
})

test_that("a summary that names none of the grid columns is refused", {
  g <- test_emm_grid("pairs")
  local_mocked_bindings(
    call_model_parameters = function(...) {
      structure(
        data.frame(Median = 1, CI_low = 0, CI_high = 2),
        parameter_names = "Parameter"
      )
    }
  )
  expect_error(apa_tidy(g), "names none of its grid columns")
})

test_that("a grid without a recorded kind reports NA for it", {
  g <- test_emm_grid("pairs")
  g@misc$estType <- NULL
  g@misc$methDesc <- NULL
  out <- apa_tidy(g)
  expect_identical(attr(out, "est_type"), NA_character_)
  expect_identical(attr(out, "contrast_method"), NA_character_)
})

test_that("the reporting arguments are checked", {
  g <- test_emm_grid("pairs")
  expect_error(apa_tidy(g, ci = "hpd"), "hdi")
  expect_error(apa_tidy(g, centrality = "map"), "median")
  expect_error(apa_tidy(g, ci_level = 1), "ci_level")
  expect_error(apa_tidy(g, rope = 1), "rope")
  expect_error(apa_tidy(g, rope = c(-1, 1), rope_ci = 2), "rope_ci")
})

test_that("an argument the route does not take is refused", {
  expect_error(
    apa_tidy(test_emm_grid("pairs"), variables = "a"),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("the route needs emmeans installed", {
  g <- test_emm_grid("pairs")
  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      cli::cli_abort("{pkg} is not installed.")
    },
    .package = "rlang"
  )
  expect_error(apa_tidy(g), "emmeans")
})

# ---- live fits -----------------------------------------------------------

test_that("a grid from an rstanarm fit reports emmeans's own summary", {
  fit <- test_stanreg_fit("factor")
  g <- emmeans::contrast(emmeans::emmeans(fit, ~cyl_f), "pairwise")
  out <- apa_tidy(g)
  s <- emm_summary(g)
  expect_identical(out$contrast, as.character(g@grid$contrast))
  expect_identical(out$estimate, unname(s$estimate))
  expect_identical(out$ci_low, unname(s$lower.HPD))
  expect_identical(out$ci_high, unname(s$upper.HPD))
  expect_identical(attr(out, "source_class"), "emmGrid")
})

test_that("a grid from a brms fit reports emmeans's own summary", {
  fit <- test_brms_fit("full")
  g <- emmeans::contrast(
    emmeans::emmeans(fit, ~am, at = list(am = c(0, 1))),
    "pairwise"
  )
  out <- apa_tidy(g)
  s <- emm_summary(g)
  expect_identical(out$contrast, "am0 - am1")
  expect_identical(out$estimate, unname(s$estimate))
  expect_identical(out$ci_low, unname(s$lower.HPD))
  expect_identical(out$ci_high, unname(s$upper.HPD))
})
