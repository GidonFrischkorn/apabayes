# Tests for the brmsfit route (dev/specs/spec-apa_tidy_brmsfit.md).
#
# Every expected value is computed from the fit inside the test, never
# typed: the numbers must equal what easystats returns on the machine the
# test runs on, not what it returned on the machine the test was written
# on. The fits come from test_brms_fit() in setup.R and skip on CRAN
# (decision 1: no fitted object is checked in).

# ---- helpers -----------------------------------------------------------

# `model_parameters()` with the same arguments the method uses, so a test
# compares apabayes against the easystats call it is supposed to wrap.
mp_of <- function(fit, ...) {
  as.data.frame(parameters::model_parameters(fit, ...))
}

dp_of <- function(fit) {
  as.data.frame(bayestestR::diagnostic_posterior(
    fit,
    effects = "all", component = "all"
  ))
}

# ---- contract ----------------------------------------------------------

test_that("apa_tidy() on a brmsfit returns the parameters contract", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "parameters")
  expect_true(all(
    names(tidy_contracts()$parameters$columns) %in% names(out)
  ))
  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "eti")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), "brmsfit")
  expect_true(all(
    c("brms", "parameters", "bayestestR", "apabayes") %in%
      names(attr(out, "package_versions"))
  ))
})

test_that("the brmsfit method returns its table visibly", {
  # The session-5 bug: a constructor ending on the invisible-returning
  # validator made apa_tidy() print nothing. Guarded per route.
  fit <- test_brms_fit("full")
  expect_true(withVisible(apa_tidy(fit))$visible)
})

# ---- the numbers come from easystats -----------------------------------

test_that("term, estimate, interval and pd follow model_parameters()", {
  fit <- test_brms_fit("full")
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

test_that("centrality switches the column model_parameters() is read from", {
  fit <- test_brms_fit("full")
  mp <- mp_of(fit, centrality = "mean")
  out <- apa_tidy(fit, centrality = "mean")

  expect_equal(out$estimate, mp$Mean)
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("ci type and level reach the interval", {
  fit <- test_brms_fit("full")
  mp <- mp_of(fit, ci_method = "hdi", ci = 0.9)
  out <- apa_tidy(fit, ci = "hdi", ci_level = 0.9)

  expect_equal(out$ci_low, mp$CI_low)
  expect_equal(out$ci_high, mp$CI_high)
  expect_identical(out$ci_method, rep("hdi", nrow(mp)))
  expect_equal(out$ci_level, rep(0.9, nrow(mp)))
  expect_identical(attr(out, "ci_method"), "hdi")
})

# ---- the diagnostics join ----------------------------------------------

test_that("diagnostics come from diagnostic_posterior(), matched by name", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit)
  dp <- dp_of(fit)
  row <- match(out$term, dp$Parameter)

  expect_equal(out$rhat, dp$Rhat[row])
  expect_equal(out$ess_bulk, dp$ESS_bulk[row])
  expect_equal(out$ess_tail, dp$ESS_tail[row])
  expect_false(anyNA(out$rhat))
})

test_that("the diagnostics join is by name, not by position", {
  # diagnostic_posterior() returns its rows in a different order from
  # model_parameters() (measured: alphabetical against model order), so a
  # positional join would silently attach the wrong parameter's R-hat.
  # This test fails if the implementation ever uses position.
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit)
  dp <- dp_of(fit)

  expect_false(identical(out$term, dp$Parameter))
  named <- dp$Rhat[match(out$term, dp$Parameter)]
  positional <- dp$Rhat[seq_len(nrow(out))]
  expect_false(isTRUE(all.equal(named, positional)))
  expect_equal(out$rhat, named)
})

test_that("diagnostic_posterior() covers sigma, which its default omits", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit)

  expect_true("sigma" %in% out$term)
  expect_false(is.na(out$ess_bulk[out$term == "sigma"]))
})

test_that("diagnostics = FALSE leaves the three columns NA", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit, diagnostics = FALSE)

  expect_true(all(is.na(out$rhat)))
  expect_true(all(is.na(out$ess_bulk)))
  expect_true(all(is.na(out$ess_tail)))
})

# ---- ROPE --------------------------------------------------------------

test_that("rope is opt-in and its values follow model_parameters()", {
  fit <- test_brms_fit("full")
  mp <- mp_of(fit, test = c("pd", "rope"), rope_range = c(-0.1, 0.1))
  out <- apa_tidy(fit, rope = c(-0.1, 0.1))

  expect_equal(out$rope_pct, mp$ROPE_Percentage)
  expect_equal(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_equal(attr(out, "rope_ci"), 0.95)
})

test_that("rope is absent by default", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit)

  expect_true(all(is.na(out$rope_pct)))
  expect_null(attr(out, "rope_range"))
  expect_null(attr(out, "rope_ci"))
})

# ---- labels ------------------------------------------------------------

test_that("label comes from the pretty_names of model_parameters()", {
  fit <- test_brms_fit("full")
  mp <- parameters::model_parameters(fit)
  pretty <- attr(mp, "pretty_names")
  out <- apa_tidy(fit)

  expect_identical(out$label, unname(pretty[out$term]))
})

test_that("labels = overrides the derived label and falls back", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit, labels = c(b_wt = "Weight"))
  plain <- apa_tidy(fit)

  expect_identical(out$label[out$term == "b_wt"], "Weight")
  other <- out$term != "b_wt"
  expect_identical(out$label[other], plain$label[other])
})

test_that("labels = errors when it names no reported term", {
  fit <- test_brms_fit("full")
  expect_error(apa_tidy(fit, labels = c(nope = "Nope")), "names no reported")
  expect_error(apa_tidy(fit, labels = "unnamed"), "named character vector")
})

# ---- the mixed fit: effects, group and the label collision -------------

test_that("effects = 'all' fills the effects and group columns", {
  fit <- test_brms_fit("mixed")
  mp <- mp_of(fit, effects = "all", component = "all")
  out <- apa_tidy(fit, effects = "all")

  expect_identical(out$term, mp$Parameter)
  expect_identical(out$effects, mp$Effects)
  # `Group` is "" on fixed rows; the contract stores NA there.
  expect_identical(out$group, ifelse(nzchar(mp$Group), mp$Group, NA_character_))
  expect_true(any(out$effects == "random"))
  expect_true(any(is.na(out$group)))
  expect_false(anyNA(out$effects))
})

test_that("colliding pretty_names are disambiguated so labels stay unique", {
  # Measured: both b_Intercept and sd_cyl_f__Intercept have the
  # pretty_name "(Intercept)". Two rows labelled the same would break the
  # inline layer's term-or-label addressing.
  fit <- test_brms_fit("mixed")
  mp <- parameters::model_parameters(fit, effects = "all", component = "all")
  pretty <- attr(mp, "pretty_names")
  out <- apa_tidy(fit, effects = "all")

  expect_true(anyDuplicated(unname(pretty[out$term])) > 0)
  expect_equal(anyDuplicated(out$label), 0)
  expect_true(any(grepl("cyl_f", out$label, fixed = TRUE)))
})

test_that("effects = 'random' on a fixed-effects fit is refused up front", {
  # Measured (2026-09-07): left to parameters::model_parameters(), this
  # call aborts with the opaque merge() error `'by' must specify a
  # uniquely valid column`. The guard asks insight::is_mixed_model()
  # first, so the message names the actual problem and never depends on
  # the wording of the upstream error.
  fit <- test_brms_fit("full")
  expect_error(apa_tidy(fit, effects = "random"), "no random effects")
  expect_error(apa_tidy(fit, effects = "random"), "effects", fixed = TRUE)
})

test_that("effects = 'random' on the mixed fit reports the group-level rows", {
  # Measured: the rows are the r_* deviations and the sd_* term, with a
  # `Group` column but neither `Effects` nor `Component`. The diagnostics
  # join covers sd_* but not r_*: diagnostic_posterior() returns no row
  # for the deviations, so their rhat and ESS are NA by the join rule.
  fit <- test_brms_fit("mixed")
  mp <- mp_of(fit, effects = "random", component = "all")
  out <- apa_tidy(fit, effects = "random")

  expect_identical(out$term, mp$Parameter)
  expect_equal(out$estimate, mp$Median)
  expect_true(all(out$group == "cyl_f"))
  expect_true(all(is.na(out$effects)))
  expect_true(all(is.na(out$component)))
  expect_false(is.na(out$rhat[out$term == "sd_cyl_f__Intercept"]))
})

test_that("a model without random effects leaves effects and group NA", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit, effects = "all")

  expect_true(all(is.na(out$group)))
  expect_true(all(is.na(out$effects)))
})

# ---- selection ---------------------------------------------------------

test_that("variables = selects and orders the reported terms", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit, variables = c("b_wt", "b_Intercept"))

  expect_identical(out$term, c("b_wt", "b_Intercept"))
  expect_identical(nrow(out), 2L)
  expect_equal(out$estimate, apa_tidy(fit)$estimate[c(2, 1)])
})

test_that("variables = errors on an unreported term, listing candidates", {
  fit <- test_brms_fit("full")
  expect_error(apa_tidy(fit, variables = "nope"), "must name reported")
  expect_error(apa_tidy(fit, variables = 1), "character vector")
})

test_that("an empty selection is named as such, not as a missing term", {
  # Folded into the unknown-term branch, this reported `NA` as the term
  # that could not be found.
  message <- tryCatch(
    resolve_brms_variables(character(), c("b_Intercept", "b_wt")),
    error = conditionMessage
  )

  expect_match(message, "selects no parameter")
  expect_false(grepl("NA", message, fixed = TRUE))
})

test_that("brms is required, not assumed", {
  # A brmsfit read back with readRDS() keeps its class without brms being
  # installed, so the method guards rather than trusting the class.
  fit <- test_brms_fit("full")

  local_mocked_bindings(
    check_installed = function(...) cli::cli_abort("brms is not installed."),
    .package = "rlang"
  )
  expect_error(apa_tidy(fit), "brms")
})

# ---- columns not on this route -----------------------------------------

test_that("bf, std and p are typed NA on the brmsfit route", {
  fit <- test_brms_fit("full")
  out <- apa_tidy(fit)

  expect_true(all(is.na(out$bf)))
  expect_true(all(is.na(out$std)))
  expect_true(all(is.na(out$p)))
  expect_type(out$bf, "double")
  expect_type(out$std, "logical")
  expect_type(out$p, "double")
})

# ---- apa_tidy_diagnostics() --------------------------------------------

test_that("apa_tidy_diagnostics() returns the diagnostics contract", {
  fit <- test_brms_fit("full")
  out <- apa_tidy_diagnostics(fit)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "diagnostics")
  expect_identical(
    names(out)[seq_len(4)],
    c("term", "rhat", "ess_bulk", "ess_tail")
  )
  expect_identical(attr(out, "source_class"), "brmsfit")
  expect_true(withVisible(apa_tidy_diagnostics(fit))$visible)
})

test_that("apa_tidy_diagnostics() equals summarise_draws() on the fit", {
  fit <- test_brms_fit("full")
  draws <- posterior::as_draws_df(fit)
  out <- apa_tidy_diagnostics(fit)
  sd <- as.data.frame(posterior::summarise_draws(
    posterior::subset_draws(draws, variable = out$term),
    "rhat", "ess_bulk", "ess_tail"
  ))
  row <- match(out$term, sd$variable)

  expect_equal(out$rhat, sd$rhat[row])
  expect_equal(out$ess_bulk, sd$ess_bulk[row])
  expect_equal(out$ess_tail, sd$ess_tail[row])
})

test_that("apa_tidy_diagnostics() drops internal variables by default", {
  fit <- test_brms_fit("full")
  out <- apa_tidy_diagnostics(fit)

  expect_false(any(grepl("__$", out$term)))
  expect_false("lprior" %in% out$term)
  expect_false(any(startsWith(out$term, "prior_")))
  expect_true(all(c("b_Intercept", "b_wt", "sigma") %in% out$term))
})

test_that("apa_tidy_diagnostics(variables =) overrides the internal rule", {
  fit <- test_brms_fit("full")
  out <- apa_tidy_diagnostics(fit, variables = "lp__")

  expect_identical(out$term, "lp__")
  expect_false(is.na(out$rhat))
})

test_that("apa_tidy_diagnostics() covers the group levels of a mixed fit", {
  fit <- test_brms_fit("mixed")
  out <- apa_tidy_diagnostics(fit)

  expect_true(any(startsWith(out$term, "r_cyl_f")))
  expect_true("sd_cyl_f__Intercept" %in% out$term)
  expect_false(anyNA(out$rhat))
})

test_that("apa_tidy_diagnostics() works on a draws fixture without a fit", {
  # Runs on CRAN: no model is fitted, the fixture is checked in.
  draws <- fixture("draws_brms")
  out <- apa_tidy_diagnostics(draws)
  sd <- as.data.frame(posterior::summarise_draws(
    posterior::subset_draws(draws, variable = out$term),
    "rhat", "ess_bulk", "ess_tail"
  ))

  expect_identical(attr(out, "type"), "diagnostics")
  expect_equal(out$rhat, sd$rhat[match(out$term, sd$variable)])
  expect_identical(attr(out, "source_class"), class(draws))
})

test_that("apa_tidy_diagnostics() aborts on an object posterior cannot take", {
  expect_error(
    apa_tidy_diagnostics(stats::lm(mpg ~ wt, mtcars)),
    "posterior"
  )
})

test_that("the runjags diagnostics method reads $mcmc", {
  # As on the draws route: a fake runjags object built from the checked-in
  # mcmc.list fixture, so the test needs no JAGS binary.
  ml <- fixture("draws_jags")
  fake <- structure(list(mcmc = ml), class = "runjags")

  out <- apa_tidy_diagnostics(fake)
  expect_identical(attr(out, "type"), "diagnostics")
  expect_identical(attr(out, "source_class"), "runjags")
  expect_equal(out$rhat, apa_tidy_diagnostics(ml)$rhat)
})

# ---- helper fallbacks --------------------------------------------------

test_that("labels fall back to the term when pretty_names is absent", {
  # model_parameters() carries pretty_names, but the attribute is not part
  # of any documented contract; if it disappears the label must become the
  # term, never NA.
  mp <- data.frame(Parameter = c("b_Intercept", "b_wt"))
  terms <- mp$Parameter

  expect_identical(
    brms_labels(mp, terms, rep(NA_character_, 2), NULL),
    terms
  )
})

test_that("a selection with no reportable parameter aborts", {
  expect_error(
    resolve_brms_variables(NULL, character()),
    "no parameters to report"
  )
})
