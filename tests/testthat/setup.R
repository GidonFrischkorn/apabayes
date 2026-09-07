# Test-run setup (ARCHITECTURE.md § Tests).
#
# Fixtures are read from tests/testthat/fixtures/ and are built by
# data-raw/fixtures.R; they carry no fitted model, so the extract tests
# run on CRAN. The brms fits are the exception: decision 1 keeps fitted
# objects out of the tarball, so a test that needs a live fit calls
# `test_brms_fit()`, which fits once per test run, off CRAN only, and
# caches the result. Nothing here calls set.seed(); `seed =` goes to
# brm().

fixture <- function(name) {
  readRDS(testthat::test_path("fixtures", paste0(name, ".rds")))
}

.apabayes_fit_cache <- new.env(parent = emptyenv())

# The probe pair of ARCHITECTURE.md § Tests. `name` is "full"
# (mpg ~ wt + am) or "reduced" (mpg ~ wt); both carry proper priors,
# save_pars(all = TRUE) and sample_prior = "yes" so that the hypothesis
# and bayesfactor_models routes of later milestones use the same fits.
#
# "mixed" (mpg ~ wt + (1 | cyl_f)) is the third fit, added in Milestone 2
# for the brmsfit route: `parameters::model_parameters()` returns the
# `Effects` and `Group` columns only for a model that has random effects
# (measured, spec-apa_tidy_brmsfit.md point 3), so the probe pair alone
# cannot exercise the `effects` and `group` columns of the contract. It
# needs neither save_pars nor sample_prior: no Bayes-factor route uses it.
test_brms_fit <- function(name = c("full", "reduced", "mixed")) {
  name <- match.arg(name)
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("brms")
  if (!is.null(.apabayes_fit_cache[[name]])) {
    return(.apabayes_fit_cache[[name]])
  }
  data <- mtcars
  data$cyl_f <- factor(data$cyl)
  formula <- switch(name,
    full = mpg ~ wt + am,
    reduced = mpg ~ wt,
    mixed = mpg ~ wt + (1 | cyl_f)
  )
  args <- list(
    formula,
    data = data,
    prior = brms::set_prior("normal(0, 10)", class = "b"),
    chains = 2, iter = 1000, seed = 1, refresh = 0, silent = 2
  )
  if (name != "mixed") {
    args$save_pars <- brms::save_pars(all = TRUE)
    args$sample_prior <- "yes"
  }
  fit <- suppressMessages(do.call(brms::brm, args))
  .apabayes_fit_cache[[name]] <- fit
  fit
}
