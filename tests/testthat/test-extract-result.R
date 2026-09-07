# Tests for the result-object route (dev/specs/spec-apa_tidy_result.md):
# apa_tidy() on what describe_posterior() or model_parameters() already
# computed. The draws fixture needs no fit, so most of these run on CRAN;
# the pretty_names, Rhat, bf and model_class cases need a fit and skip.

# ---- helpers -----------------------------------------------------------

# The one fixture this file uses. Read once: every test below treats it as
# read-only, and the two producers are called with it many times over.
draws_brms <- fixture("draws_brms")

dp_of <- function(...) bayestestR::describe_posterior(draws_brms, ...)
mp_of <- function(...) parameters::model_parameters(draws_brms, ...)

# ---- contract ----------------------------------------------------------

test_that("apa_tidy() on a describe_posterior object returns the contract", {
  dp <- dp_of()
  out <- apa_tidy(dp)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "parameters")
  expect_true(all(
    names(tidy_contracts()$parameters$columns) %in% names(out)
  ))
  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "eti")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), class(dp))
  expect_true(all(
    c("bayestestR", "apabayes") %in% names(attr(out, "package_versions"))
  ))
  expect_false("parameters" %in% names(attr(out, "package_versions")))
  expect_null(attr(out, "model_class"))
})

test_that("a model_parameters() object reaches the same route via the guard", {
  mp <- mp_of()
  out <- apa_tidy(mp)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "source_class"), class(mp))
  expect_true(all(
    c("parameters", "bayestestR", "apabayes") %in%
      names(attr(out, "package_versions"))
  ))
  expect_identical(out$term, mp$Parameter)
  expect_equal(out$estimate, mp$Median)
})

test_that("the result-object methods return their table visibly", {
  expect_true(withVisible(apa_tidy(dp_of()))$visible)
  expect_true(withVisible(apa_tidy(mp_of()))$visible)
})

# ---- the numbers are read, not computed --------------------------------

test_that("term, estimate, interval, pd and rope follow the object", {
  dp <- dp_of()
  out <- apa_tidy(dp)

  expect_identical(out$term, dp$Parameter)
  expect_equal(out$estimate, dp$Median)
  expect_equal(out$ci_low, dp$CI_low)
  expect_equal(out$ci_high, dp$CI_high)
  expect_equal(out$pd, dp$pd)
  expect_equal(out$rope_pct, dp$ROPE_Percentage)
  expect_identical(out$ci_method, rep("eti", nrow(dp)))
  expect_equal(out$ci_level, rep(0.95, nrow(dp)))
})

test_that("rope bounds become attributes only when the object carries them", {
  # describe_posterior() carries ROPE_low/ROPE_high/ROPE_CI per row;
  # model_parameters() carries the percentage alone (measured point 7).
  dp <- dp_of()
  out <- apa_tidy(dp)
  expect_equal(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_equal(attr(out, "rope_ci"), 0.95)

  mp <- mp_of(test = c("pd", "rope"), rope_range = c(-0.1, 0.1))
  out <- apa_tidy(mp)
  expect_equal(out$rope_pct, mp$ROPE_Percentage)
  expect_null(attr(out, "rope_range"))
  expect_null(attr(out, "rope_ci"))

  out <- apa_tidy(mp_of())
  expect_true(all(is.na(out$rope_pct)))
})

test_that("rope attributes need one pair of bounds across every row", {
  # The bounds are per row upstream, and a table note may not state one
  # ROPE for rows that did not share it: a multivariate model gets its
  # default bounds per response (0.1 * sd(y)), and a stripped object may
  # have lost ROPE_CI. The percentage stays, since it is per row anyway.
  dp <- dp_of()
  dp$ROPE_low[1] <- dp$ROPE_low[1] - 1
  out <- apa_tidy(dp)
  expect_null(attr(out, "rope_range"))
  expect_null(attr(out, "rope_ci"))
  expect_equal(out$rope_pct, dp$ROPE_Percentage)

  dp <- dp_of()
  dp$ROPE_CI <- NULL
  out <- apa_tidy(dp)
  expect_equal(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_null(attr(out, "rope_ci"))

  dp <- dp_of()
  dp$ROPE_CI[1] <- 0.89
  out <- apa_tidy(dp)
  expect_equal(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_null(attr(out, "rope_ci"))
})

test_that("the interval level comes from the ci attribute with no CI column", {
  # Measured: model_parameters(draws_df) has no CI column, only attr ci.
  mp <- mp_of(ci = 0.9)
  expect_false("CI" %in% names(mp))
  out <- apa_tidy(mp)

  expect_identical(attr(out, "ci_level"), 0.9)
  expect_equal(out$ci_level, rep(0.9, nrow(mp)))
})

test_that("an object without any interval level is refused", {
  dp <- dp_of()
  dp$CI <- NULL
  expect_error(apa_tidy(dp), "no interval level")
})

test_that("pd is NA when the object was computed without it", {
  expect_true(all(is.na(apa_tidy(dp_of(test = NULL))$pd)))
  expect_true(all(is.na(apa_tidy(mp_of(test = NULL))$pd)))
})

# ---- centrality --------------------------------------------------------

test_that("centrality is inferred from the column the object has", {
  dp <- dp_of(centrality = "mean")
  out <- apa_tidy(dp)

  expect_equal(out$estimate, dp$Mean)
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("an object with both median and mean needs centrality named", {
  dp <- dp_of(centrality = "all")
  expect_error(apa_tidy(dp), "name .*centrality")

  out <- apa_tidy(dp, centrality = "mean")
  expect_equal(out$estimate, dp$Mean)
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("a MAP-only object and a named-but-absent centrality are refused", {
  expect_error(apa_tidy(dp_of(centrality = "MAP")), "neither")
  expect_error(apa_tidy(dp_of(), centrality = "mean"), "no Mean column")
  expect_error(apa_tidy(dp_of(), centrality = "mode"), "must be one of")
})

# ---- ci_method ---------------------------------------------------------

test_that("ci_method is read from the attribute, case-insensitively", {
  out <- apa_tidy(dp_of(ci_method = "hdi"))
  expect_identical(attr(out, "ci_method"), "hdi")
  expect_true(all(out$ci_method == "hdi"))

  dp <- dp_of()
  attr(dp, "ci_method") <- "ETI"
  expect_identical(attr(apa_tidy(dp), "ci_method"), "eti")
})

test_that("an interval type outside the contract is refused by name", {
  expect_error(apa_tidy(dp_of(ci_method = "spi")), "spi")
  dp <- dp_of()
  attr(dp, "ci_method") <- NULL
  expect_error(apa_tidy(dp), "ci_method")
})

# ---- several interval levels -------------------------------------------

test_that("several interval levels are refused in both forms", {
  # Measured: describe_posterior() returns the long form (Parameter
  # duplicated), model_parameters() the wide form and loses the
  # describe_posterior class, so the two cases hit different checks.
  expect_error(apa_tidy(dp_of(ci = c(0.89, 0.95))), "interval levels")
  expect_error(apa_tidy(mp_of(ci = c(0.89, 0.95))), "interval levels")
})

# ---- the parameters_model guard ----------------------------------------

test_that("a frequentist parameters table is refused, not coerced to draws", {
  mp <- parameters::model_parameters(stats::lm(mpg ~ wt + am, mtcars))
  expect_false(inherits(mp, "describe_posterior"))
  expect_error(apa_tidy(mp), "not Bayesian")
})

test_that("the guard forwards every argument to the worker", {
  # The guard's signature is (x, ...), so NextMethod() has to re-match its
  # `...` against the worker's named formals. If it did not, these three
  # would fall through unused and the defaults would answer instead.
  mp <- mp_of(centrality = "all")
  out <- apa_tidy(
    mp,
    variables = "b_wt",
    labels = c(b_wt = "Weight"),
    centrality = "mean"
  )

  expect_identical(out$term, "b_wt")
  expect_identical(out$label, "Weight")
  expect_equal(out$estimate, mp$Mean[mp$Parameter == "b_wt"])
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("a parameters_model that is neither case is refused generically", {
  mp <- mp_of()
  class(mp) <- setdiff(
    class(mp),
    c("describe_posterior", "see_describe_posterior")
  )
  expect_error(apa_tidy(mp), "not a posterior summary")
})

# ---- diagnostics -------------------------------------------------------

test_that("diagnostics are NA on a draws input and ess_bulk always", {
  out <- apa_tidy(dp_of())
  expect_true(all(is.na(out$rhat)))
  expect_true(all(is.na(out$ess_bulk)))
  expect_true(all(is.na(out$ess_tail)))
})

test_that("Rhat and ESS_tail are read from a fit's table; ess_bulk stays NA", {
  fit <- test_brms_fit("full")
  mp <- parameters::model_parameters(fit)
  out <- apa_tidy(mp)

  expect_equal(out$rhat, mp$Rhat)
  expect_equal(out$ess_tail, mp$ESS_tail)
  expect_false(anyNA(out$rhat))
  expect_true(all(is.na(out$ess_bulk)))
})

# ---- bf ----------------------------------------------------------------

test_that("bf is the natural-scale log_BF when the object carries one", {
  fit <- test_brms_fit("full")
  mp <- suppressWarnings(parameters::model_parameters(
    fit,
    test = c("pd", "bf")
  ))
  expect_true("log_BF" %in% names(mp))
  out <- apa_tidy(mp)

  expect_equal(out$bf, exp(mp$log_BF))
  expect_true(all(is.na(apa_tidy(dp_of())$bf)))
})

# ---- labels ------------------------------------------------------------

test_that("label falls back to term without pretty_names, and labels = wins", {
  dp <- dp_of()
  expect_identical(apa_tidy(dp)$label, dp$Parameter)

  out <- apa_tidy(dp, labels = c(b_wt = "Weight"))
  expect_identical(out$label[out$term == "b_wt"], "Weight")
  expect_identical(
    out$label[out$term != "b_wt"],
    dp$Parameter[dp$Parameter != "b_wt"]
  )
})

test_that("label comes from pretty_names on a fit's table, with model_class", {
  fit <- test_brms_fit("full")
  mp <- parameters::model_parameters(fit)
  out <- apa_tidy(mp)

  expect_identical(out$label, unname(attr(mp, "pretty_names")[out$term]))
  expect_identical(attr(out, "model_class"), "brmsfit")
})

# ---- effects, group, component -----------------------------------------

test_that("effects, group and component are read when the object has them", {
  fit <- test_brms_fit("mixed")
  mp <- as.data.frame(parameters::model_parameters(fit, effects = "all"))
  out <- apa_tidy(parameters::model_parameters(fit, effects = "all"))

  expect_identical(out$effects, mp$Effects)
  expect_identical(out$component, mp$Component)
  expect_identical(out$group, ifelse(nzchar(mp$Group), mp$Group, NA_character_))
  expect_true(any(is.na(out$group)))
})

test_that("effects, group and component are NA on a bare describe_posterior", {
  out <- apa_tidy(dp_of())
  expect_true(all(is.na(out$effects)))
  expect_true(all(is.na(out$group)))
  expect_true(all(is.na(out$component)))
})

# ---- selection and typed NA --------------------------------------------

test_that("variables = selects and orders; errors list candidates", {
  dp <- dp_of()
  out <- apa_tidy(dp, variables = c("b_wt", "b_Intercept"))

  expect_identical(out$term, c("b_wt", "b_Intercept"))
  expect_equal(out$estimate, dp$Median[match(out$term, dp$Parameter)])
  expect_error(apa_tidy(dp, variables = "nope"), "must name reported")
  expect_error(apa_tidy(dp, variables = 1), "character vector")
})

test_that("std and p are typed NA on the result-object route", {
  out <- apa_tidy(dp_of())
  expect_true(all(is.na(out$std)))
  expect_true(all(is.na(out$p)))
  expect_type(out$std, "logical")
  expect_type(out$p, "double")
})
