# Tests for the correlation route (local/specs/spec-apa_tidy_correlation.md).
#
# No expected number is typed. The fixtures are tables from
# correlation::correlation() and cor_test(), which carry no draws, so the
# numbers the route reports are compared with the table's own columns;
# the live test at the end reproduces those columns through BayesFactor
# and parameters under the same seed. The tables are data frames with
# attributes, so every test but the live one needs neither package.

# ---- contract ------------------------------------------------------------

test_that("a Bayesian correlation table returns the correlations contract", {
  x <- fixture("cor_default")
  out <- apa_tidy(x)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "correlations")
  expect_identical(
    names(out),
    c(
      "term", "var1", "var2", "group", "estimate", "ci_low", "ci_high",
      "ci_method", "ci_level", "pd", "rope_pct", "bf", "n",
      "method", "prior_distribution", "prior_location", "prior_scale"
    )
  )
  expect_identical(out$term, paste0(x$Parameter1, "~~", x$Parameter2))
  expect_identical(out$term[1], "mpg~~wt")
  expect_identical(out$var1, x$Parameter1)
  expect_identical(out$var2, x$Parameter2)
  expect_identical(out$group, rep(NA_character_, 3))
  expect_identical(out$estimate, x$rho)
  expect_identical(out$ci_low, x$CI_low)
  expect_identical(out$ci_high, x$CI_high)
  expect_identical(out$ci_method, rep("hdi", 3))
  expect_identical(out$ci_level, rep(0.95, 3))
  expect_identical(out$pd, x$pd)
  expect_identical(out$rope_pct, x$ROPE_Percentage)
  expect_identical(out$bf, x$BF)
  expect_identical(out$n, as.double(x$n_Obs))
  expect_identical(out$method, x$Method)
  expect_identical(out$prior_distribution, x$Prior_Distribution)
  expect_identical(out$prior_location, x$Prior_Location)
  expect_identical(out$prior_scale, x$Prior_Scale)
  expect_true(withVisible(apa_tidy(x))$visible)
})

test_that("the attributes record the settings, the prior and the packages", {
  x <- fixture("cor_default")
  out <- apa_tidy(x)

  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "hdi")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), class(x))
  expect_identical(attr(out, "prior"), "medium")
  expect_identical(attr(out, "method"), "pearson")
  # The ROPE was fixed upstream and the table carries no bounds.
  expect_null(attr(out, "rope_range"))
  expect_null(attr(out, "rope_ci"))
  versions <- names(attr(out, "package_versions"))
  expect_true(all(c("parameters", "bayestestR", "apabayes") %in% versions))
  expect_identical(
    "correlation" %in% versions,
    rlang::is_installed("correlation")
  )
})

# ---- settings the table does not record -----------------------------------

test_that("ci names the interval, which the table cannot confirm", {
  x <- fixture("cor_eti")
  expect_identical(apa_tidy(x, ci = "eti")$ci_method, rep("eti", 3))
  expect_identical(attr(apa_tidy(x, ci = "eti"), "ci_method"), "eti")
  expect_identical(apa_tidy(x, ci = "eti")$ci_low, x$CI_low)
  # The limitation the help page states: the default labels it hdi.
  expect_identical(apa_tidy(x)$ci_method, rep("hdi", 3))
})

test_that("centrality names the estimate; without rho it picks the column", {
  mean_x <- fixture("cor_mean")
  out <- apa_tidy(mean_x, centrality = "mean")
  expect_identical(attr(out, "centrality"), "mean")
  expect_identical(out$estimate, mean_x$rho)

  all <- fixture("cor_all")
  expect_false("rho" %in% names(all))
  expect_identical(apa_tidy(all)$estimate, all$Median)
  expect_identical(apa_tidy(all, centrality = "mean")$estimate, all$Mean)
  expect_identical(
    attr(apa_tidy(all, centrality = "mean"), "centrality"),
    "mean"
  )

  expect_error(apa_tidy(fixture("cor_map")), "MAP")
  no_mean <- all
  no_mean$Mean <- NULL
  expect_error(apa_tidy(no_mean, centrality = "mean"), "Mean")
})

test_that("the statistics a table was computed without are NA", {
  pd <- apa_tidy(fixture("cor_pd"))
  expect_identical(pd$rope_pct, rep(NA_real_, 3))
  expect_identical(pd$pd, fixture("cor_pd")$pd)

  bf_x <- fixture("cor_bf")
  bf <- apa_tidy(bf_x)
  expect_identical(bf$pd, rep(NA_real_, 3))
  expect_identical(bf$rope_pct, rep(NA_real_, 3))
  expect_identical(bf$bf, bf_x$BF)

  bare <- fixture("cor_default")
  bare$BF <- NULL
  bare$n_Obs <- NULL
  bare$Method <- NULL
  out <- apa_tidy(bare)
  expect_identical(out$bf, rep(NA_real_, 3))
  expect_identical(out$n, rep(NA_real_, 3))
  expect_false("method" %in% names(out))
})

# ---- the interval level ---------------------------------------------------

test_that("a level other than .95 is refused: correlation never applies it", {
  x <- fixture("cor_ci90")
  # The fixture shows the upstream behaviour: a 0.9 label on .95 bounds.
  expect_identical(attr(x, "ci"), 0.9)
  expect_identical(x$CI_low, fixture("cor_default")$CI_low)
  expect_error(apa_tidy(x), "0.9")
  expect_error(apa_tidy(x), "95%", fixed = TRUE)

  no_level <- fixture("cor_default")
  attr(no_level, "ci") <- NULL
  expect_identical(apa_tidy(no_level)$ci_level, rep(0.95, 3))
  no_level$CI <- NULL
  expect_error(apa_tidy(no_level), "no interval level")

  mixed <- fixture("cor_default")
  attr(mixed, "ci") <- NULL
  mixed$CI <- c(0.95, 0.9, 0.95)
  expect_error(apa_tidy(mixed), "0.9")
})

# ---- rows -----------------------------------------------------------------

test_that("a grouped table fills group and n per group", {
  x <- fixture("cor_grouped")
  out <- apa_tidy(x)
  expect_identical(out$group, x$Group)
  expect_identical(out$group[c(1, 4)], c("auto", "manual"))
  expect_identical(out$n, as.double(x$n_Obs))
  expect_identical(nrow(out), 6L)
})

test_that("select2, cor_test, a wide prior and missing data read alike", {
  expect_identical(
    apa_tidy(fixture("cor_select2"))$term,
    c("mpg~~wt", "mpg~~hp")
  )

  single_x <- fixture("cor_test")
  single <- apa_tidy(single_x)
  expect_identical(single$term, "mpg~~wt")
  expect_identical(single$estimate, single_x$rho)
  expect_identical(single$method, "Bayesian Pearson")
  expect_null(attr(single, "prior"))
  expect_null(attr(single, "method"))
  expect_identical(attr(single, "source_class"), class(single_x))

  wide_x <- fixture("cor_wide")
  wide <- apa_tidy(wide_x)
  expect_identical(attr(wide, "prior"), "wide")
  expect_identical(wide$prior_location, wide_x$Prior_Location)

  na_x <- fixture("cor_na")
  expect_identical(apa_tidy(na_x)$n, as.double(na_x$n_Obs))
  expect_identical(apa_tidy(na_x)$n, c(29, 32, 29))
})

# ---- refusals -------------------------------------------------------------

test_that("a frequentist correlation table is refused", {
  expect_error(apa_tidy(fixture("cor_freq")), "not Bayesian")
  # A Spearman table has a rho column too; the marker is not the column.
  spearman <- fixture("cor_freq_spearman")
  expect_true("rho" %in% names(spearman))
  expect_error(apa_tidy(spearman), "not Bayesian")
  # A frequentist cor_test() result has no bayesian attribute.
  expect_error(apa_tidy(fixture("cor_freq_test")), "not Bayesian")

  no_method <- fixture("cor_test")
  no_method$Method <- NULL
  expect_error(apa_tidy(no_method), "Bayesian")
  # A missing Method value is no marker either (review, session 24).
  na_method <- fixture("cor_test")
  na_method$Method <- NA_character_
  expect_error(apa_tidy(na_method), "not Bayesian")
})

test_that("the interval level is checked before the estimate column", {
  x <- fixture("cor_map")
  attr(x, "ci") <- 0.9
  expect_error(apa_tidy(x), "95%", fixed = TRUE)
})

test_that("factor variable columns are read as their labels", {
  x <- fixture("cor_default")
  x$Parameter1 <- factor(x$Parameter1)
  x$Parameter2 <- factor(x$Parameter2)
  out <- apa_tidy(x)
  expect_identical(out$term, apa_tidy(fixture("cor_default"))$term)
  expect_identical(out$var1, c("mpg", "mpg", "wt"))
  diagonal <- fixture("cor_redundant")
  diagonal$Parameter1 <- factor(diagonal$Parameter1)
  expect_error(apa_tidy(diagonal), "redundant")
})

test_that("a table without rows or identifying columns is refused", {
  x <- fixture("cor_default")
  expect_error(apa_tidy(x[0, ]), "no rows")
  no_var <- x
  no_var$Parameter1 <- NULL
  expect_error(apa_tidy(no_var), "Parameter1")
  no_bound <- x
  no_bound$CI_high <- NULL
  expect_error(apa_tidy(no_bound), "CI_high")
})

test_that("a table with diagonal rows is refused", {
  expect_error(apa_tidy(fixture("cor_redundant")), "redundant")
  expect_error(apa_tidy(fixture("cor_redundant")), "mpg")
})

test_that("arguments are checked", {
  x <- fixture("cor_default")
  expect_error(apa_tidy(x, ci = "spi"), "ci")
  expect_error(apa_tidy(x, centrality = "map"), "centrality")
  expect_error(apa_tidy(x, rope = c(-1, 1)), "rope")
  expect_error(apa_tidy(x, ci_level = 0.9), "ci_level")
})

# ---- live -----------------------------------------------------------------

test_that("the columns are parameters on BayesFactor's correlation", {
  skip_if_not_installed("correlation")
  skip_if_not_installed("BayesFactor")
  data <- mtcars[, c("mpg", "wt")]
  x <- withr::with_seed(
    2, correlation::cor_test(data, "mpg", "wt", bayesian = TRUE)
  )
  reference <- withr::with_seed(2, parameters::model_parameters(
    BayesFactor::correlationBF(data$mpg, data$wt, rscale = "medium"),
    dispersion = FALSE, ci_method = "hdi", test = c("pd", "rope", "bf"),
    rope_range = c(-0.1, 0.1), rope_ci = 1
  ))
  out <- apa_tidy(x)
  expect_identical(out$estimate, reference$Median)
  expect_identical(out$ci_low, reference$CI_low)
  expect_identical(out$ci_high, reference$CI_high)
  expect_identical(out$pd, reference$pd)
  expect_identical(out$bf, reference$BF)
  expect_identical(
    out$bf,
    BayesFactor::extractBF(
      BayesFactor::correlationBF(data$mpg, data$wt, rscale = "medium")
    )$bf
  )
})
