# Tests for the stanreg route (dev/specs/spec-apa_tidy_stanreg.md).
#
# As on the brmsfit route, every expected value is computed from the fit
# inside the test, never typed. The fits come from test_stanreg_fit() in
# setup.R and skip on CRAN (decision 1).

# ---- helpers -----------------------------------------------------------

# `model_parameters()` with the arguments the method always passes
# (`priors = FALSE`, `component = "all"`, measured points 3 and 4), plus
# the arguments under test.
mp_of <- function(fit, ...) {
  as.data.frame(parameters::model_parameters(
    fit,
    priors = FALSE, component = "all", ...
  ))
}

# `summarise_draws()` on the fit's own draws, the diagnostics source of
# this route.
sd_of <- function(fit, terms) {
  draws <- posterior::subset_draws(
    posterior::as_draws_df(fit),
    variable = terms
  )
  as.data.frame(posterior::summarise_draws(
    draws, "rhat", "ess_bulk", "ess_tail"
  ))
}

# ---- contract ----------------------------------------------------------

test_that("apa_tidy() on a stanreg returns the parameters contract", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "parameters")
  expect_true(all(
    names(tidy_contracts()$parameters$columns) %in% names(out)
  ))
  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "eti")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), c("stanreg", "glm", "lm"))
  expect_true(all(
    c("rstanarm", "parameters", "posterior", "apabayes") %in%
      names(attr(out, "package_versions"))
  ))
})

test_that("the stanreg method returns its table visibly", {
  fit <- test_stanreg_fit("full")
  expect_true(withVisible(apa_tidy(fit))$visible)
})

# ---- the numbers come from easystats -----------------------------------

test_that("term, estimate, interval and pd follow model_parameters()", {
  fit <- test_stanreg_fit("full")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)

  expect_identical(out$term, mp$Parameter)
  expect_equal(out$estimate, mp$Median)
  expect_equal(out$ci_low, mp$CI_low)
  expect_equal(out$ci_high, mp$CI_high)
  expect_equal(out$pd, mp$pd)
  expect_identical(out$component, mp$Component)
  expect_identical(out$ci_method, rep("eti", nrow(mp)))
  expect_equal(out$ci_level, rep(0.95, nrow(mp)))
})

test_that("sigma is in the default rows, as on the brmsfit route", {
  # Measured: the bare easystats default omits sigma on stanreg; the
  # shared `component = "all"` default of the parameters routes adds it.
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)

  expect_true("sigma" %in% out$term)
  expect_identical(out$component[out$term == "sigma"], "sigma")
})

test_that("component = 'conditional' drops sigma and the Component column", {
  fit <- test_stanreg_fit("full")
  mp <- as.data.frame(parameters::model_parameters(
    fit,
    priors = FALSE, component = "conditional"
  ))
  out <- apa_tidy(fit, component = "conditional")

  expect_identical(out$term, mp$Parameter)
  expect_false("sigma" %in% out$term)
  expect_true(all(is.na(out$component)))
})

test_that("no prior column reaches the table", {
  # `parameters_rows()` builds a fixed column set, so this passes however
  # `priors` was set: it guards the contract, not the argument. What the
  # `priors = FALSE` decision rests on is the row set, and that is pinned
  # by "effects = 'random' reports only the real group-level rows" below,
  # where `priors = TRUE` appends three rows with NA estimates.
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)

  expect_false(any(startsWith(names(out), "Prior_")))
})

test_that("centrality switches the column model_parameters() is read from", {
  fit <- test_stanreg_fit("full")
  mp <- mp_of(fit, centrality = "mean")
  out <- apa_tidy(fit, centrality = "mean")

  expect_equal(out$estimate, mp$Mean)
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("ci type and level reach the interval", {
  fit <- test_stanreg_fit("full")
  mp <- mp_of(fit, ci_method = "hdi", ci = 0.9)
  out <- apa_tidy(fit, ci = "hdi", ci_level = 0.9)

  expect_equal(out$ci_low, mp$CI_low)
  expect_equal(out$ci_high, mp$CI_high)
  expect_identical(out$ci_method, rep("hdi", nrow(mp)))
  expect_equal(out$ci_level, rep(0.9, nrow(mp)))
})

# ---- the diagnostics join ----------------------------------------------

test_that("diagnostics come from summarise_draws(), matched by name, none NA", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)
  sd <- sd_of(fit, out$term)
  row <- match(out$term, sd$variable)

  expect_equal(out$rhat, sd$rhat[row])
  expect_equal(out$ess_bulk, sd$ess_bulk[row])
  expect_equal(out$ess_tail, sd$ess_tail[row])
  expect_false(anyNA(out$rhat))
  expect_false(is.na(out$ess_bulk[out$term == "sigma"]))
})

test_that("the diagnostics are not diagnostic_posterior()'s on this route", {
  # Measured: on stanreg, bayestestR::diagnostic_posterior() never
  # covers sigma and returns numbers that differ from posterior's on the
  # terms it does cover (rstan's summary against posterior's). The route
  # takes one source, posterior, and this test pins the choice.
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)
  dp <- as.data.frame(bayestestR::diagnostic_posterior(
    fit,
    effects = "all", component = "all"
  ))

  expect_false("sigma" %in% dp$Parameter)
  shared <- intersect(out$term, dp$Parameter)
  expect_true(length(shared) > 0)
  expect_false(isTRUE(all.equal(
    out$rhat[match(shared, out$term)],
    dp$Rhat[match(shared, dp$Parameter)]
  )))
})

test_that("diagnostics = FALSE leaves the three columns NA", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit, diagnostics = FALSE)

  expect_true(all(is.na(out$rhat)))
  expect_true(all(is.na(out$ess_bulk)))
  expect_true(all(is.na(out$ess_tail)))
})

# ---- ROPE --------------------------------------------------------------

test_that("rope is opt-in and its values follow model_parameters()", {
  fit <- test_stanreg_fit("full")
  mp <- mp_of(fit, test = c("pd", "rope"), rope_range = c(-0.1, 0.1))
  out <- apa_tidy(fit, rope = c(-0.1, 0.1))

  expect_equal(out$rope_pct, mp$ROPE_Percentage)
  expect_equal(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_equal(attr(out, "rope_ci"), 0.95)
})

test_that("rope is absent by default", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)

  expect_true(all(is.na(out$rope_pct)))
  expect_null(attr(out, "rope_range"))
  expect_null(attr(out, "rope_ci"))
})

# ---- labels ------------------------------------------------------------

test_that("label comes from the pretty_names of model_parameters()", {
  fit <- test_stanreg_fit("full")
  mp <- parameters::model_parameters(fit, priors = FALSE, component = "all")
  pretty <- attr(mp, "pretty_names")
  out <- apa_tidy(fit)

  expect_identical(out$label, unname(pretty[out$term]))
})

test_that("labels = overrides the derived label and falls back", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit, labels = c(wt = "Weight"))
  plain <- apa_tidy(fit)

  expect_identical(out$label[out$term == "wt"], "Weight")
  other <- out$term != "wt"
  expect_identical(out$label[other], plain$label[other])
})

# ---- the mixed fit: effects, group, random rows, label collision -------

test_that("effects = 'all' fills the effects and group columns", {
  fit <- test_stanreg_fit("mixed")
  mp <- mp_of(fit, effects = "all")
  out <- apa_tidy(fit, effects = "all")

  expect_identical(out$term, mp$Parameter)
  expect_identical(out$effects, mp$Effects)
  expect_identical(out$group, ifelse(nzchar(mp$Group), mp$Group, NA_character_))
  expect_true(any(out$effects == "random"))
  expect_true(any(is.na(out$group)))
  expect_false(anyNA(out$effects))
})

test_that("colliding pretty_names are disambiguated so labels stay unique", {
  # Measured: Sigma[cyl_f:(Intercept),(Intercept)] and (Intercept) share
  # the pretty_name "(Intercept)".
  fit <- test_stanreg_fit("mixed")
  mp <- parameters::model_parameters(
    fit,
    priors = FALSE, effects = "all", component = "all"
  )
  pretty <- attr(mp, "pretty_names")
  out <- apa_tidy(fit, effects = "all")

  expect_true(anyDuplicated(unname(pretty[out$term])) > 0)
  expect_equal(anyDuplicated(out$label), 0)
  expect_true(any(grepl("cyl_f", out$label, fixed = TRUE)))
})

test_that("effects = 'random' reports only the real group-level rows", {
  # Measured: with priors = TRUE easystats appends three rows with NA
  # estimates and a bogus Group; with priors = FALSE they are gone. Every
  # remaining row has an estimate and, through the draws, a diagnostic.
  fit <- test_stanreg_fit("mixed")
  mp <- mp_of(fit, effects = "random")
  out <- apa_tidy(fit, effects = "random")

  expect_identical(out$term, mp$Parameter)
  expect_false(anyNA(out$estimate))
  expect_true(any(startsWith(out$term, "b[")))
  expect_true(all(out$group == "cyl_f"))
  expect_false(anyNA(out$rhat))
})

test_that("effects = 'random' on a fixed-effects fit is refused up front", {
  # Measured: easystats itself would silently return the fixed rows here
  # (unlike on brmsfit, where it aborts). The shared guard gives both
  # routes the same refusal.
  fit <- test_stanreg_fit("full")
  expect_error(apa_tidy(fit, effects = "random"), "no random effects")
})

test_that("a model without random effects leaves effects and group NA", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit, effects = "all")

  expect_true(all(is.na(out$group)))
  expect_true(all(is.na(out$effects)))
})

# ---- selection ---------------------------------------------------------

test_that("variables = selects and orders the reported terms", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit, variables = c("wt", "(Intercept)"))
  plain <- apa_tidy(fit)

  expect_identical(out$term, c("wt", "(Intercept)"))
  expect_identical(nrow(out), 2L)
  expect_equal(out$estimate, plain$estimate[match(out$term, plain$term)])
})

test_that("variables = errors on an unreported term, listing candidates", {
  fit <- test_stanreg_fit("full")
  expect_error(apa_tidy(fit, variables = "nope"), "must name reported")
  expect_error(apa_tidy(fit, variables = 1), "character vector")
})

test_that("rstanarm is required, not assumed", {
  fit <- test_stanreg_fit("full")

  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      cli::cli_abort("{pkg} is not installed.")
    },
    .package = "rlang"
  )
  expect_error(apa_tidy(fit), "rstanarm")
})

# ---- columns not on this route -----------------------------------------

test_that("bf, std and p are typed NA on the stanreg route", {
  fit <- test_stanreg_fit("full")
  out <- apa_tidy(fit)

  expect_true(all(is.na(out$bf)))
  expect_true(all(is.na(out$std)))
  expect_true(all(is.na(out$p)))
  expect_type(out$bf, "double")
  expect_type(out$std, "logical")
  expect_type(out$p, "double")
})

# ---- apa_tidy_diagnostics() needs no stanreg method --------------------

test_that("apa_tidy_diagnostics() reads a stanreg through the default method", {
  fit <- test_stanreg_fit("mixed")
  out <- apa_tidy_diagnostics(fit)

  expect_identical(attr(out, "type"), "diagnostics")
  expect_identical(attr(out, "source_class"), class(fit))
  expect_true("sigma" %in% out$term)
  expect_true(any(startsWith(out$term, "b[")))
  expect_false(anyNA(out$rhat))
})
