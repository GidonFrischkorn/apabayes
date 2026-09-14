# Test-run setup (ARCHITECTURE.md § Tests).
#
# Fixtures are read from tests/testthat/fixtures/ and are built by the
# fixture builders kept with the design record (local/data-raw/, not
# shipped); they carry no fitted model, so the extract tests
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

# The stanreg fits of the stanreg route (spec-apa_tidy_stanreg.md): the
# same two formulas as "full" and "mixed" above, through rstanarm. They
# need no compilation and take 0.1 s and 0.5 s (measured), but stay
# off CRAN with the brms fits for the same reason: a fit made on the
# test machine, never a checked-in one (decision 1).
test_stanreg_fit <- function(name = c("full", "mixed")) {
  name <- match.arg(name)
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rstanarm")
  key <- paste0("stanreg_", name)
  if (!is.null(.apabayes_fit_cache[[key]])) {
    return(.apabayes_fit_cache[[key]])
  }
  data <- mtcars
  data$cyl_f <- factor(data$cyl)
  fit <- switch(name,
    full = rstanarm::stan_glm(
      mpg ~ wt + am,
      data = data, chains = 2, iter = 1000, seed = 1, refresh = 0
    ),
    mixed = rstanarm::stan_glmer(
      mpg ~ wt + (1 | cyl_f),
      data = data, chains = 2, iter = 1000, seed = 1, refresh = 0
    )
  )
  .apabayes_fit_cache[[key]] <- fit
  fit
}

# The lavaan fits of the lavaan route (spec-apa_tidy_lavaan.md). Unlike
# the Stan fits these run on CRAN: lavaan needs no compiler and every fit
# takes well under a second (measured), which is what ARCHITECTURE.md
# § Tests planned. The bootstrap fit draws random samples, so it runs
# under withr::with_seed(); nothing here calls set.seed().
test_lavaan_fit <- function(name = "cfa") {
  fits <- c(
    "cfa", "sem", "means", "groups", "boot", "mlr", "nose", "nonconverged"
  )
  name <- match.arg(name, fits)
  testthat::skip_if_not_installed("lavaan")
  key <- paste0("lavaan_", name)
  if (!is.null(.apabayes_fit_cache[[key]])) {
    return(.apabayes_fit_cache[[key]])
  }
  hs <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  # PoliticalDemocracy with labels, equality constraints and a defined
  # parameter: the regression, correlation and defined components.
  pd <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + a*y2 + b*y3 + c*y4
    dem65 =~ y5 + a*y6 + b*y7 + c*y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
    y1 ~~ y5
    ab := a*b
  "
  data <- lavaan::HolzingerSwineford1939
  fit <- switch(name,
    cfa = lavaan::cfa(hs, data = data),
    sem = lavaan::sem(pd, data = lavaan::PoliticalDemocracy),
    means = lavaan::cfa(hs, data = data, meanstructure = TRUE),
    groups = lavaan::cfa(hs, data = data, group = "school"),
    # 200 replicates: with fewer, the percentile interval's endpoints
    # are the extreme order statistics and lavaan warns on every row.
    boot = withr::with_seed(
      1,
      lavaan::cfa(hs, data = data, se = "bootstrap", bootstrap = 200)
    ),
    mlr = lavaan::cfa(hs, data = data, estimator = "MLR"),
    nose = lavaan::cfa(hs, data = data, se = "none"),
    nonconverged = suppressWarnings(
      lavaan::cfa(hs, data = data, control = list(iter.max = 1))
    )
  )
  .apabayes_fit_cache[[key]] <- fit
  fit
}

# The blavaan fits of the blavaan route (spec-apa_tidy_blavaan.md), and
# of the guard of the lavaan route (a blavaan object dispatches to
# `apa_tidy.lavaan()` unless refused, measured). Stan sampling, so off
# CRAN like the brms fits. blavaan prints progress through the console
# and warns about ESS at this size; neither is under test here.
# `bcfa()` builds an unqualified `blavaan()` call and evaluates it in
# the caller's frame, so the package is attached for the duration of the
# fit.
#
# `name` is:
#   "one"     the one-factor fit (1.1 s), for the parameters method. Its
#             three indicators make it *saturated*, so its fit indices
#             are degenerate (BRMSEA exactly 0, `adjBGammaHat` upper
#             bound 1.182, and blavaan warns that the effective number
#             of parameters exceeds the sample statistics) — measured,
#             which is why "two" exists.
#   "two"     a two-factor fit (1.8 s), for the fit-index method: 13
#             parameters, ppp .03, BRMSEA .094 [.071, .119].
#   "groups"  a two-group fit (2.2 s), which the route refuses:
#             `model_parameters()` aborts on one (measured).
#   "notest"  `test = "none"` (0.5 s), which has no ppp and no fit
#             indices but a working parameter table.
#   "divergent" the two-factor model with 30 warmup iterations and
#             `adapt_delta = 0.05` (0.9 s), which diverges on every
#             transition (400 of 400, measured), for the divergence count.
test_blavaan_fit <- function(name = "one") {
  fits <- c("one", "two", "groups", "notest", "divergent")
  name <- match.arg(name, fits)
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("blavaan")
  key <- paste0("blavaan_", name)
  if (!is.null(.apabayes_fit_cache[[key]])) {
    return(.apabayes_fit_cache[[key]])
  }
  one <- "visual =~ x1 + x2 + x3"
  two <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  args <- list(
    if (name %in% c("two", "divergent")) two else one,
    data = lavaan::HolzingerSwineford1939,
    n.chains = 2, burnin = 200, sample = 200, seed = 1,
    bcontrol = list(refresh = 0)
  )
  if (name == "groups") args$group <- "school"
  if (name == "notest") args$test <- "none"
  if (name == "divergent") {
    args$burnin <- 30
    args$seed <- 3
    args$bcontrol <- list(refresh = 0, control = list(adapt_delta = 0.05))
  }
  fit <- NULL
  # `do.call("bcfa", ...)` by name, not `do.call(blavaan::bcfa, ...)`:
  # bcfa() reads `as.character(match.call()[[1]])` to build the
  # `blavaan()` call it evaluates, and a function object there is a
  # closure it cannot coerce. The package is attached for the duration,
  # so the name resolves.
  invisible(utils::capture.output(
    fit <- withr::with_package("blavaan", suppressWarnings(suppressMessages(
      do.call("bcfa", args)
    )))
  ))
  .apabayes_fit_cache[[key]] <- fit
  fit
}
