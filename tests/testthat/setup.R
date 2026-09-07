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
test_brms_fit <- function(name = c("full", "reduced")) {
  name <- match.arg(name)
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("brms")
  if (!is.null(.apabayes_fit_cache[[name]])) {
    return(.apabayes_fit_cache[[name]])
  }
  formula <- switch(name,
    full = mpg ~ wt + am,
    reduced = mpg ~ wt
  )
  fit <- suppressMessages(brms::brm(
    formula,
    data = mtcars,
    prior = brms::set_prior("normal(0, 10)", class = "b"),
    chains = 2, iter = 1000, seed = 1, refresh = 0, silent = 2,
    save_pars = brms::save_pars(all = TRUE),
    sample_prior = "yes"
  ))
  .apabayes_fit_cache[[name]] <- fit
  fit
}
