# Tests for the modelbased route (local/specs/spec-apa_tidy_modelbased.md).
#
# No expected number is typed. The fixtures are modelbased tables of
# rstanarm fits computed with `keep_iterations = TRUE`, and every number
# the route reports is recomputed here with bayestestR from those
# iterations, which modelbased summarised (measured identical). The
# tables are data frames with attributes, so these tests need neither
# modelbased nor a fit; the live test at the end runs off CRAN on an
# unstripped table.

# The iterations of row `i`, as the numeric vector modelbased summarised.
iterations <- function(x, i) {
  cols <- grep("^iter_", names(x), value = TRUE)
  unlist(x[i, cols], use.names = FALSE)
}

# A table whose call says `...`: the fixture with its call replaced.
with_call <- function(x, call) {
  attr(x, "call") <- call
  x
}

skip_if_no_bayestestr <- function() {
  testthat::skip_if_not_installed("bayestestR")
}

# ---- contract ------------------------------------------------------------

test_that("a contrasts table returns the contrasts contract", {
  skip_if_no_bayestestr()
  x <- fixture("mb_contrasts")
  out <- apa_tidy(x)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "contrasts")
  expect_identical(
    names(out),
    c(
      "contrast", "group", "estimate", "ci_low", "ci_high", "ci_method",
      "ci_level", "pd", "rope_pct", "Level1", "Level2"
    )
  )
  expect_identical(
    out$contrast,
    paste(as.character(x$Level1), "-", as.character(x$Level2))
  )
  expect_identical(out$contrast[1], "6 - 4")
  expect_identical(out$Level1, as.character(x$Level1))
  expect_identical(out$group, rep(NA_character_, 3))
  for (i in 1:3) {
    v <- iterations(x, i)
    eti <- bayestestR::eti(v, ci = 0.95)
    expect_identical(out$estimate[i], stats::median(v))
    expect_identical(out$ci_low[i], eti$CI_low)
    expect_identical(out$ci_high[i], eti$CI_high)
    expect_identical(out$pd[i], as.numeric(bayestestR::p_direction(v)))
    expect_identical(
      out$rope_pct[i],
      as.numeric(
        bayestestR::rope(v, range = c(-0.1, 0.1), ci = 0.95)$ROPE_Percentage
      )
    )
  }
  expect_identical(out$ci_method, rep("eti", 3))
  expect_identical(out$ci_level, rep(0.95, 3))
})

test_that("the attributes record the settings and the packages", {
  x <- fixture("mb_contrasts")
  out <- apa_tidy(x)

  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "eti")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), class(x))
  expect_named(
    attr(out, "package_versions"),
    c("modelbased", "bayestestR", "apabayes")
  )
  expect_identical(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_identical(attr(out, "rope_ci"), 0.95)
  expect_null(attr(out, "by"))
  expect_identical(attr(out, "predict"), "response")
  expect_identical(attr(out, "marginalization"), "typical")
  expect_false(any(grepl("^iter_", names(out))))
  expect_true(withVisible(apa_tidy(x))$visible)
})

test_that("a table without iterations reads the same numbers", {
  plain <- apa_tidy(fixture("mb_contrasts_plain"))
  expect_identical(nrow(plain), 3L)
  expect_identical(plain$contrast, apa_tidy(fixture("mb_contrasts"))$contrast)
})

# ---- centrality, interval, ROPE -------------------------------------------

test_that("an HDI table is read as one from its call", {
  skip_if_no_bayestestr()
  x <- fixture("mb_contrasts_hdi")
  out <- apa_tidy(x)
  for (i in 1:3) {
    hdi <- bayestestR::hdi(iterations(x, i), ci = 0.95)
    expect_identical(out$ci_low[i], hdi$CI_low)
    expect_identical(out$ci_high[i], hdi$CI_high)
  }
  expect_identical(out$ci_method, rep("hdi", 3))
  expect_identical(attr(out, "ci_method"), "hdi")
})

test_that("the centrality is the column the table holds", {
  x <- fixture("mb_contrasts_mean")
  out <- apa_tidy(x)
  expect_identical(attr(out, "centrality"), "mean")
  expect_identical(out$estimate, x$Mean)
  expect_identical(out$estimate[1], mean(iterations(x, 1)))

  all <- fixture("mb_contrasts_all")
  expect_error(apa_tidy(all), "centrality")
  expect_identical(apa_tidy(all, centrality = "median")$estimate, all$Median)
  expect_identical(apa_tidy(all, centrality = "mean")$estimate, all$Mean)
  expect_error(apa_tidy(fixture("mb_contrasts_map")), "MAP")
  expect_error(apa_tidy(x, centrality = "median"), "Median")
})

test_that("the ROPE share is read when the table has one, with its bounds", {
  skip_if_no_bayestestr()
  pd_only <- apa_tidy(fixture("mb_contrasts_pd"))
  expect_identical(pd_only$rope_pct, rep(NA_real_, 3))
  expect_null(attr(pd_only, "rope_range"))

  x <- fixture("mb_contrasts_rope")
  out <- apa_tidy(x)
  expect_identical(attr(out, "rope_range"), c(-1, 1))
  expect_identical(
    out$rope_pct[3],
    as.numeric(bayestestR::rope(
      iterations(x, 3),
      range = c(-1, 1), ci = 0.95
    )$ROPE_Percentage)
  )
})

test_that("the interval level is the table's ci attribute", {
  skip_if_no_bayestestr()
  x <- fixture("mb_contrasts_ci90")
  out <- apa_tidy(x)
  expect_identical(out$ci_level, rep(0.9, 3))
  expect_identical(attr(out, "ci_level"), 0.9)
  expect_identical(
    out$ci_low[2],
    bayestestR::eti(iterations(x, 2), ci = 0.9)$CI_low
  )
  # ROPE_CI is the ROPE's own level, not the interval's.
  expect_identical(attr(out, "rope_ci"), 0.95)

  bad <- x
  attr(bad, "ci") <- c(0.9, 0.95)
  expect_error(apa_tidy(bad), "interval level")
  attr(bad, "ci") <- NULL
  expect_error(apa_tidy(bad), "no interval level")
})

# ---- reading the interval off the call ------------------------------------

test_that("the call names the interval, in any case and by prefix", {
  x <- fixture("mb_contrasts_plain")
  upper <- with_call(x, quote(
    estimate_contrasts(model = fit, contrast = "cyl_f", ci_method = "HDI")
  ))
  expect_identical(apa_tidy(upper)$ci_method[1], "hdi")
  prefix <- with_call(x, quote(
    estimate_contrasts(model = fit, contrast = "cyl_f", ci_m = "hdi")
  ))
  expect_identical(apa_tidy(prefix)$ci_method[1], "hdi")
  agreeing <- with_call(x, quote(
    estimate_contrasts(model = fit, contrast = "cyl_f", ci_method = "eti")
  ))
  expect_identical(apa_tidy(agreeing, ci = "eti")$ci_method[1], "eti")
})

test_that("an unreadable call needs ci, and then takes it", {
  x <- fixture("mb_contrasts_plain")
  symbol <- with_call(x, quote(
    estimate_contrasts(model = fit, contrast = "cyl_f", ci_method = m)
  ))
  expect_error(apa_tidy(symbol), "ci", fixed = TRUE)
  expect_error(apa_tidy(symbol), "ci_method = m", fixed = TRUE)
  expect_identical(apa_tidy(symbol, ci = "hdi")$ci_method[1], "hdi")

  missing_call <- with_call(x, NULL)
  expect_error(apa_tidy(missing_call), "call")
  expect_identical(apa_tidy(missing_call, ci = "eti")$ci_method[1], "eti")

  twice <- with_call(x, quote(
    estimate_contrasts(model = fit, ci_meth = "hdi", ci_method = "eti")
  ))
  expect_error(apa_tidy(twice), "ci_meth")
})

test_that("a ci that contradicts the call is refused", {
  x <- fixture("mb_contrasts_plain")
  expect_error(apa_tidy(x, ci = "hdi"), "eti")
  hdi <- fixture("mb_contrasts_hdi")
  expect_error(apa_tidy(hdi, ci = "eti"), "hdi")
  spi <- with_call(x, quote(
    estimate_contrasts(model = fit, contrast = "cyl_f", ci_method = "spi")
  ))
  expect_error(apa_tidy(spi), "spi")
  expect_error(apa_tidy(x, ci = "spi"), "eti")
})

# ---- rows and labels ------------------------------------------------------

test_that("a by variable fills group and stays as a column", {
  x <- fixture("mb_contrasts_by")
  out <- apa_tidy(x)
  expect_identical(out$group, as.character(x$am_f))
  expect_identical(out$am_f, as.character(x$am_f))
  expect_identical(attr(out, "by"), "am_f")
  expect_identical(
    names(out)[10:12],
    c("Level1", "Level2", "am_f")
  )
})

test_that("several variables and custom comparisons keep modelbased's labels", {
  two <- apa_tidy(fixture("mb_contrasts_two"))
  expect_identical(two$contrast[1], "4, manual - 4, auto")
  expect_identical(two$group, rep(NA_character_, 15))

  custom_x <- fixture("mb_contrasts_custom")
  custom <- apa_tidy(custom_x)
  expect_identical(custom$contrast, as.character(custom_x$Parameter))
  expect_identical(custom$contrast, "b2=b1")
  expect_false("Level1" %in% names(custom))
})

test_that("a means table is labelled by its row variables", {
  skip_if_no_bayestestr()
  x <- fixture("mb_means")
  out <- apa_tidy(x)
  expect_identical(out$contrast, c("4", "6", "8"))
  expect_identical(out$group, rep(NA_character_, 3))
  expect_identical(out$cyl_f, c("4", "6", "8"))
  expect_identical(
    names(out),
    c(
      "contrast", "group", "estimate", "ci_low", "ci_high", "ci_method",
      "ci_level", "pd", "rope_pct", "cyl_f"
    )
  )
  expect_identical(attr(out, "by"), "cyl_f")
  v <- iterations(x, 2)
  expect_identical(out$estimate[2], stats::median(v))
  expect_identical(out$ci_high[2], bayestestR::eti(v, ci = 0.95)$CI_high)

  two <- apa_tidy(fixture("mb_means_two"))
  expect_identical(two$contrast[1:2], c("4, auto", "4, manual"))
})

# ---- refusals ---------------------------------------------------------------

test_that("the emmeans backend is refused, pointing to the grid", {
  expect_error(apa_tidy(fixture("mb_contrasts_emmeans")), "emmeans")
  expect_error(apa_tidy(fixture("mb_contrasts_emmeans")), "backend")
  # Its means table inherits describe_posterior; the route still wins.
  means <- fixture("mb_means_emmeans")
  expect_true(inherits(means, "describe_posterior"))
  expect_error(apa_tidy(means), "backend")
})

test_that("a table of a model that is not Bayesian is refused", {
  expect_error(apa_tidy(fixture("mb_contrasts_lm")), "not Bayesian")
  expect_error(apa_tidy(fixture("mb_means_lm")), "not Bayesian")
  stripped <- fixture("mb_contrasts_plain")
  attr(stripped, "model_info") <- NULL
  expect_error(apa_tidy(stripped), "model information")
})

test_that("a table without rows or identifying columns is refused", {
  x <- fixture("mb_contrasts_plain")
  expect_error(apa_tidy(x[0, ]), "no rows")
  no_levels <- x
  no_levels$Level1 <- NULL
  expect_error(apa_tidy(no_levels), "Level1")

  means <- fixture("mb_means")
  attr(means, "by") <- "gear"
  expect_error(apa_tidy(means), "gear")
  attr(means, "by") <- NULL
  expect_error(apa_tidy(means), "by")
})

test_that("arguments are checked", {
  x <- fixture("mb_contrasts_plain")
  expect_error(apa_tidy(x, ci = "quantile"), "ci")
  expect_error(apa_tidy(x, centrality = "map"), "centrality")
  expect_error(apa_tidy(x, rope = c(-1, 1)), "rope")
  expect_error(apa_tidy(fixture("mb_means"), ci_level = 0.9), "ci_level")
})

test_that("a slopes table is refused by the default method", {
  skip_if_not_installed("posterior")
  expect_error(apa_tidy(fixture("mb_slopes")), "estimate_slopes")
})

# ---- live -------------------------------------------------------------------

test_that("an unstripped table of a live stanreg fit reads the same way", {
  skip_if_not_installed("modelbased")
  # `estimate_contrasts()` does its work through marginaleffects, which is
  # a Suggests of modelbased rather than a dependency: an installed
  # modelbased is not enough (measured on CI, 2026-09-17).
  skip_if_not_installed("marginaleffects", "0.29.0")
  skip_if_no_bayestestr()
  fit <- test_stanreg_fit("factor")
  x <- modelbased::estimate_contrasts(
    fit,
    contrast = "cyl_f", keep_iterations = TRUE
  )
  out <- apa_tidy(x)
  expect_identical(out$contrast[3], "8 - 6")
  v <- iterations(x, 3)
  expect_identical(out$estimate[3], stats::median(v))
  expect_identical(out$ci_low[3], bayestestR::eti(v, ci = 0.95)$CI_low)
  expect_identical(attr(out, "source_class"), class(x))

  means <- apa_tidy(modelbased::estimate_means(fit, by = "cyl_f"))
  expect_identical(means$contrast, c("4", "6", "8"))
})
