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
#
# "factor" (mpg ~ wt + cyl_f) was added in session 22 for the emmGrid
# route: a live Bayesian grid needs a factor to contrast, and the brms
# probe pair has none (its `am` is numeric, contrasted at 0 and 1).
test_stanreg_fit <- function(name = c("full", "mixed", "factor")) {
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
    ),
    factor = rstanarm::stan_glm(
      mpg ~ wt + cyl_f,
      data = data, chains = 2, iter = 1000, seed = 1, refresh = 0
    )
  )
  .apabayes_fit_cache[[key]] <- fit
  fit
}

# The comparison routes (spec-apa_tidy_compare_loo.md,
# spec-apa_tidy_bayesfactor_models.md).
#
# A pointwise log-likelihood matrix (draws x observations) for a normal
# model whose mean draws sit at `shift`, evaluated at fixed data. It needs
# no Stan, so the LOO tests run on CRAN; the fixture builder
# local/data-raw/fixture-compare-loo-matrix.R copies it verbatim to make
# the matrix-shaped fixture from the same numbers (the two shapes agree
# to 1.8e-15, measured).
simulated_log_lik <- function(shift, sd, n = 40, draws = 400) {
  y <- stats::qnorm(stats::ppoints(n))
  mu <- stats::rnorm(draws, shift, 0.1)
  outer(mu, y, function(m, yy) stats::dnorm(yy, m, sd, log = TRUE))
}

# Three `psis_loo` objects, named good / shifted / wide, whose comparison
# orders them in that order.
test_loo_list <- function() {
  testthat::skip_if_not_installed("loo")
  if (!is.null(.apabayes_fit_cache$loo_list)) {
    return(.apabayes_fit_cache$loo_list)
  }
  ll <- withr::with_seed(1, list(
    good = simulated_log_lik(0, 1),
    shifted = simulated_log_lik(0.3, 1),
    wide = simulated_log_lik(0, 1.6)
  ))
  out <- lapply(ll, function(m) loo::loo(m, r_eff = rep(1, ncol(m))))
  .apabayes_fit_cache$loo_list <- out
  out
}

# Three lm fits compared by the BIC approximation: deterministic, no
# Stan, and bayestestR is in Imports, so it runs everywhere. The
# intercept-only model is the denominator (row 3); row names are the
# deparsed arguments m1, m0, m00 (measured).
test_bf_models_lm <- function() {
  m1 <- stats::lm(mpg ~ wt + am, data = mtcars)
  m0 <- stats::lm(mpg ~ wt, data = mtcars)
  m00 <- stats::lm(mpg ~ 1, data = mtcars)
  bayestestR::bayesfactor_models(m1, m0, m00, denominator = m00)
}

# The brms probe pair through bridge sampling. Stochastic (measured: a
# different seed moves log_BF in the third decimal), so it is computed
# once per run under a fixed RNG state and every expectation reads this
# object.
test_bf_models_brms <- function() {
  full <- test_brms_fit("full")
  reduced <- test_brms_fit("reduced")
  if (!is.null(.apabayes_fit_cache$bf_models_brms)) {
    return(.apabayes_fit_cache$bf_models_brms)
  }
  out <- withr::with_seed(1, suppressMessages(suppressWarnings(
    bayestestR::bayesfactor_models(full, reduced)
  )))
  .apabayes_fit_cache$bf_models_brms <- out
  out
}

# The emmeans grids of the emmGrid route (spec-apa_tidy_emmGrid.md).
# `emmeans::qdrg(mcmc = )` builds a Bayesian reference grid from a matrix
# of coefficient draws (measured: 2 ms, and its HPD interval and median
# are bit-identical to those of a grid from a brms or rstanarm fit), so
# the route's tests run on CRAN without Stan. The draws are simulated
# under `withr::with_seed()` around an lm fit's coefficients and
# covariance; they stand for a posterior only in shape, which is all a
# shape fixture needs. `.wgt.` weights come from the data as they would
# from a fit.
#
# `name` is:
#   "pairs"        pairwise contrasts of cyl_f (mpg ~ wt + cyl_f)
#   "means"        the marginal means of cyl_f, one primary variable
#                  with numeric-looking levels
#   "at"           the means of wt at 2.5 and 3.5: a numeric primary
#                  variable
#   "custom"       one named custom contrast (a list method)
#   "by"           pairwise contrasts of cyl_f within am_f, from the
#                  model with the cyl_f by am_f interaction
#   "means_two"    the means of cyl_f * am_f: two primary variables
#   "two_by"       pairwise contrasts of cyl_f within am_f and wt, from
#                  the model with wt and the interaction
#   "list"         emmeans(g, pairwise ~ cyl_f), an emm_list
#   "frequentist"  pairwise contrasts from the lm itself (no draws)
test_emm_grid <- function(name = "pairs") {
  grids <- c(
    "pairs", "means", "at", "custom", "by", "means_two", "two_by", "list",
    "frequentist"
  )
  name <- match.arg(name, grids)
  testthat::skip_if_not_installed("emmeans")
  key <- paste0("emm_", name)
  if (!is.null(.apabayes_fit_cache[[key]])) {
    return(.apabayes_fit_cache[[key]])
  }
  data <- mtcars
  data$cyl_f <- factor(data$cyl)
  data$am_f <- factor(data$am, labels = c("auto", "manual"))
  formula <- switch(name,
    by = ,
    means_two = mpg ~ cyl_f * am_f,
    two_by = mpg ~ wt + cyl_f * am_f,
    mpg ~ wt + cyl_f
  )
  lm_fit <- stats::lm(formula, data = data)
  if (name == "frequentist") {
    grid <- emmeans::contrast(emmeans::emmeans(lm_fit, ~cyl_f), "pairwise")
  } else {
    beta <- stats::coef(lm_fit)
    draws <- withr::with_seed(1, {
      z <- matrix(stats::rnorm(400 * length(beta)), 400)
      sweep(z %*% chol(stats::vcov(lm_fit)), 2, beta, "+")
    })
    colnames(draws) <- names(beta)
    # `at` must reach `qdrg()`: `emmeans()` on a grid it built ignores
    # it (measured).
    at <- if (name %in% c("at", "two_by")) list(wt = c(2.5, 3.5))
    g <- emmeans::qdrg(formula[-2], data = data, mcmc = draws, at = at)
    grid <- switch(name,
      pairs = emmeans::contrast(emmeans::emmeans(g, ~cyl_f), "pairwise"),
      means = emmeans::emmeans(g, ~cyl_f),
      at = emmeans::emmeans(g, ~wt),
      custom = emmeans::contrast(
        emmeans::emmeans(g, ~cyl_f),
        list(`4 vs rest` = c(1, -0.5, -0.5))
      ),
      by = emmeans::contrast(emmeans::emmeans(g, ~ cyl_f | am_f), "pairwise"),
      means_two = emmeans::emmeans(g, ~ cyl_f * am_f),
      two_by = emmeans::contrast(
        emmeans::emmeans(g, ~ cyl_f | am_f * wt),
        "pairwise"
      ),
      list = emmeans::emmeans(g, pairwise ~ cyl_f)
    )
  }
  .apabayes_fit_cache[[key]] <- grid
  grid
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
