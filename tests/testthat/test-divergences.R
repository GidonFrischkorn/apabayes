# Tests for the divergences attribute of apa_tidy_diagnostics() and the
# internal sampler_divergences() (local/specs/spec-apa_convergence.md).
#
# The oracle for every count is the sampler's own record, read through
# the public accessor of the package that made the fit:
# brms::nuts_params(), rstan::get_sampler_params(), or the CmdStan
# object's own sampler_diagnostics(). The CmdStan path is also tested on
# a mock object, because cmdstanr is not on CRAN.

sum_divergent <- function(stanfit) {
  params <- rstan::get_sampler_params(stanfit, inc_warmup = FALSE)
  sum(vapply(params, function(m) sum(m[, "divergent__"]), numeric(1)))
}

# A stand-in for a CmdStanMCMC: an R6 object is an environment whose
# methods are called with `$`, which a classed list reproduces.
mock_cmdstan <- function(algorithm = "hmc", divergent = c(0, 1, 1, 0)) {
  diagnostics <- data.frame(treedepth__ = seq_along(divergent))
  if (!is.null(divergent)) diagnostics$divergent__ <- divergent
  structure(
    list(
      metadata = function() list(algorithm = algorithm),
      sampler_diagnostics = function(inc_warmup = FALSE, format = "draws_df") {
        stopifnot(!inc_warmup, format == "draws_df")
        posterior::as_draws_df(diagnostics)
      }
    ),
    class = c("CmdStanMCMC", "CmdStanFit", "R6")
  )
}

# ---- no sampler information ------------------------------------------------

test_that("objects without NUTS sampler information count NA", {
  expect_identical(sampler_divergences(fixture("draws_brms")), NA_integer_)
  expect_identical(sampler_divergences(fixture("draws_jags")), NA_integer_)
  expect_identical(sampler_divergences(list()), NA_integer_)
  expect_identical(
    attr(apa_tidy_diagnostics(fixture("draws_brms")), "divergences"),
    NA_integer_
  )
  fake <- structure(list(mcmc = fixture("draws_jags")), class = "runjags")
  expect_identical(
    attr(apa_tidy_diagnostics(fake), "divergences"),
    NA_integer_
  )
})

test_that("a diagnostics table claims no centrality", {
  out <- apa_tidy_diagnostics(fixture("draws_brms"))
  expect_identical(attr(out, "centrality"), NA_character_)
  expect_false(any(grepl("median", utils::capture.output(print(out)))))
})

# ---- CmdStan -----------------------------------------------------------------

test_that("a CmdStanMCMC is counted from its own sampler diagnostics", {
  expect_identical(sampler_divergences(mock_cmdstan()), 2L)
  expect_identical(
    sampler_divergences(mock_cmdstan(divergent = c(0, 0))),
    0L
  )
  expect_identical(
    sampler_divergences(mock_cmdstan(algorithm = "fixed_param")),
    NA_integer_
  )
  expect_identical(
    sampler_divergences(mock_cmdstan(divergent = NULL)),
    NA_integer_
  )
})

# ---- Stan fits ---------------------------------------------------------

test_that("a brms fit is counted as brms::nuts_params() counts it", {
  fit <- test_brms_fit("full")
  np <- brms::nuts_params(fit)
  expected <- sum(np$Value[np$Parameter == "divergent__"])
  expect_identical(sampler_divergences(fit), as.integer(expected))
  expect_identical(
    attr(apa_tidy_diagnostics(fit), "divergences"),
    as.integer(expected)
  )
  expect_identical(sampler_divergences(fit$fit), as.integer(expected))
})

test_that("a stanreg is counted from its stanfit", {
  fit <- test_stanreg_fit("full")
  expect_identical(
    sampler_divergences(fit),
    as.integer(sum_divergent(fit$stanfit))
  )
  expect_identical(
    attr(apa_tidy_diagnostics(fit), "divergences"),
    as.integer(sum_divergent(fit$stanfit))
  )
})

test_that("stanfits without NUTS draws count NA, not zero", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  # Measured: optimizing leaves @mode 2 and no sampler parameters;
  # meanfield has @mode 0 but method "variational", where
  # get_sampler_params() aborts.
  opt <- rstanarm::stan_glm(
    mpg ~ wt,
    data = mtcars, algorithm = "optimizing", seed = 1, refresh = 0
  )
  expect_identical(sampler_divergences(opt), NA_integer_)
  mf <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ wt,
    data = mtcars, algorithm = "meanfield", seed = 1, refresh = 0
  ))
  expect_identical(sampler_divergences(mf), NA_integer_)
})

test_that("a blavaan fit is counted from its sampler object", {
  fit <- test_blavaan_fit("two")
  mcobj <- blavaan::blavInspect(fit, "mcobj")
  expect_identical(sampler_divergences(fit), as.integer(sum_divergent(mcobj)))

  divergent <- test_blavaan_fit("divergent")
  k <- sampler_divergences(divergent)
  expect_gt(k, 0L)
  expect_identical(
    k,
    as.integer(sum_divergent(blavaan::blavInspect(divergent, "mcobj")))
  )
})

test_that("a blavaan fit on the cmdstan target is counted too", {
  # Measured: under target = "cmdstan" the sampler object is a
  # CmdStanMCMC, not a stanfit, and get_sampler_params() aborts on it.
  skip_on_cran()
  skip_if_not_installed("blavaan")
  skip_if_not_installed("cmdstanr")
  fit <- NULL
  invisible(utils::capture.output(
    fit <- withr::with_package("blavaan", suppressWarnings(suppressMessages(
      do.call("bcfa", list(
        "visual =~ x1 + x2 + x3",
        data = lavaan::HolzingerSwineford1939,
        n.chains = 2, burnin = 200, sample = 200, seed = 1,
        target = "cmdstan"
      ))
    )))
  ))
  mcobj <- blavaan::blavInspect(fit, "mcobj")
  expect_s3_class(mcobj, "CmdStanMCMC")
  diagnostics <- mcobj$sampler_diagnostics(format = "draws_df")
  expect_identical(
    sampler_divergences(fit),
    as.integer(sum(diagnostics$divergent__))
  )
})

test_that("a sampled stanfit without divergent__ counts NA", {
  # Measured: Fixed_param records only accept_stat__ and static HMC has no
  # divergent__ column. The live fit's own sampler record is replaced by
  # one of that shape rather than compiling a second model.
  fit <- test_brms_fit("full")
  local_mocked_bindings(
    get_sampler_params = function(object, ...) {
      list(cbind(accept_stat__ = c(0.9, 0.8)), cbind(accept_stat__ = 0.7))
    },
    .package = "rstan"
  )
  expect_identical(sampler_divergences(fit), NA_integer_)
})
