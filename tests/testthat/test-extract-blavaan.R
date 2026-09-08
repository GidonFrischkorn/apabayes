# Tests for the blavaan route (dev/specs/spec-apa_tidy_blavaan.md).
#
# As on every other route, each expected value is computed from the fit
# inside the test with blavaan's, parameters' and posterior's own
# accessors as the oracle, never typed. The fits come from
# test_blavaan_fit() in setup.R and skip on CRAN (decision 1).

# ---- helpers -----------------------------------------------------------

# `model_parameters()` with the arguments the method always passes:
# `component = "all"` and `diagnostic = NULL`, the latter because the
# route takes R-hat and ESS from posterior instead (design question 1).
mp_of <- function(fit, component = "all", ...) {
  as.data.frame(parameters::model_parameters(
    fit,
    component = component, diagnostic = NULL, ...
  ))
}

# `coef()` on a blavaan fit is an S4 method, and S3 dispatch from a
# detached namespace reaches `coef.default()`, which aborts on an S4
# object. The fixtures attach blavaan only for the duration of the fit,
# so the oracle attaches it again for the duration of the call.
coef_names <- function(fit) {
  withr::with_package("blavaan", names(coef(fit)))
}

# The fit's own chains, the diagnostics source of both paths.
dr_of <- function(fit) {
  posterior::as_draws_df(blavaan::blavInspect(fit, "mcmc"))
}

sd_of <- function(draws) {
  as.data.frame(posterior::summarise_draws(
    draws, "rhat", "ess_bulk", "ess_tail"
  ))
}

# The standardized posterior as the route reads it, with the chain
# structure restored from the unstandardized draws.
std_draws_of <- function(fit, type = "std.all") {
  sp <- as.data.frame(blavaan::standardizedPosterior(fit, type = type))
  chains <- dr_of(fit)
  sp$.chain <- chains$.chain
  sp$.iteration <- chains$.iteration
  posterior::as_draws_df(sp)
}

# ---- contract ----------------------------------------------------------

test_that("apa_tidy() on a blavaan fit returns the parameters contract", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "parameters")
  expect_true(all(
    names(tidy_contracts()$parameters$columns) %in% names(out)
  ))
  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "eti")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), "blavaan")
  expect_identical(attr(out, "estimator"), "Bayes")
  expect_false(attr(out, "standardized"))
  expect_true(all(
    c("blavaan", "parameters", "posterior", "apabayes") %in%
      names(attr(out, "package_versions"))
  ))
})

test_that("the blavaan method returns its table visibly", {
  fit <- test_blavaan_fit("one")
  expect_true(withVisible(apa_tidy(fit))$visible)
})

test_that("a blavaan fit dispatches to the blavaan method, not lavaan's", {
  # Before this route existed, S3 dispatch on the S4 class chain reached
  # apa_tidy.lavaan(), which refused it. Now the blavaan method wins.
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit)

  expect_identical(attr(out, "source_class"), "blavaan")
  expect_identical(attr(out, "centrality"), "median")
  expect_true(all(is.na(out$p)))
})

# ---- the numbers come from easystats -----------------------------------

test_that("term is model_parameters()' Parameter, which is coef()'s names", {
  # Measured: model_parameters(blavaan) reports the FREE parameters only,
  # so the fixed marker loading is absent from the default row set.
  fit <- test_blavaan_fit("one")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)

  expect_identical(out$term, mp$Parameter)
  expect_identical(out$term, coef_names(fit))
  expect_false("visual=~x1" %in% out$term)
})

test_that("estimate, interval and pd follow model_parameters()", {
  fit <- test_blavaan_fit("one")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)
  row <- match(out$term, mp$Parameter)

  expect_equal(out$estimate, mp$Median[row])
  expect_equal(out$ci_low, mp$CI_low[row])
  expect_equal(out$ci_high, mp$CI_high[row])
  expect_equal(out$pd, mp$pd[row])
  expect_identical(out$ci_method, rep("eti", nrow(out)))
  expect_equal(out$ci_level, rep(0.95, nrow(out)))
})

test_that("centrality switches the column the estimate is read from", {
  fit <- test_blavaan_fit("one")
  mp <- mp_of(fit, centrality = "mean")
  out <- apa_tidy(fit, centrality = "mean")

  expect_equal(out$estimate, mp$Mean[match(out$term, mp$Parameter)])
  expect_identical(attr(out, "centrality"), "mean")
})

test_that("ci type and level reach the interval", {
  fit <- test_blavaan_fit("one")
  mp <- mp_of(fit, ci_method = "hdi", ci = 0.9)
  out <- apa_tidy(fit, ci = "hdi", ci_level = 0.9)
  row <- match(out$term, mp$Parameter)

  expect_equal(out$ci_low, mp$CI_low[row])
  expect_equal(out$ci_high, mp$CI_high[row])
  expect_identical(out$ci_method, rep("hdi", nrow(out)))
  expect_equal(out$ci_level, rep(0.9, nrow(out)))
})

# ---- the diagnostics join ----------------------------------------------

test_that("diagnostics come from summarise_draws(), matched by name", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit)
  sd <- sd_of(dr_of(fit))
  row <- match(out$term, sd$variable)

  expect_equal(out$rhat, sd$rhat[row])
  expect_equal(out$ess_bulk, sd$ess_bulk[row])
  expect_equal(out$ess_tail, sd$ess_tail[row])
  expect_false(anyNA(out$rhat))
  expect_false(anyNA(out$ess_bulk))
})

test_that("the ESS reported is not blavaan's own, which is a third one", {
  # Measured: mp$ESS equals blavInspect(x, "neff") exactly and is neither
  # ess_bulk nor ess_tail. The contract has two ESS columns and the
  # "> 400" rule apabayes reports against is defined for posterior's
  # estimators, so the route takes both from there and passes
  # diagnostic = NULL so blavaan's are never computed. This pins the
  # choice and the fact behind it.
  fit <- test_blavaan_fit("one")
  mp <- as.data.frame(parameters::model_parameters(fit, component = "all"))
  out <- apa_tidy(fit)
  row <- match(out$term, mp$Parameter)

  neff <- blavaan::blavInspect(fit, "neff")
  rhat <- blavaan::blavInspect(fit, "rhat")
  expect_equal(mp$ESS[row], unname(neff[out$term]))
  expect_equal(mp$Rhat[row], unname(rhat[out$term]))
  expect_false(isTRUE(all.equal(out$ess_bulk, mp$ESS[row])))
  expect_false(isTRUE(all.equal(out$ess_tail, mp$ESS[row])))
  expect_false(isTRUE(all.equal(out$rhat, mp$Rhat[row])))
})

test_that("diagnostics = FALSE leaves the three columns NA", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit, diagnostics = FALSE)

  expect_true(all(is.na(out$rhat)))
  expect_true(all(is.na(out$ess_bulk)))
  expect_true(all(is.na(out$ess_tail)))
})

# ---- ROPE --------------------------------------------------------------

test_that("rope is opt-in and its values follow model_parameters()", {
  fit <- test_blavaan_fit("one")
  mp <- mp_of(fit, test = c("pd", "rope"), rope_range = c(-0.1, 0.1))
  out <- apa_tidy(fit, rope = c(-0.1, 0.1))

  expect_equal(out$rope_pct, mp$ROPE_Percentage[match(out$term, mp$Parameter)])
  # Measured: easystats sets no rope_range attribute on this route,
  # unlike the brms routes; apabayes supplies it so a note can state it.
  expect_null(attr(mp, "rope_range", exact = TRUE))
  expect_equal(attr(out, "rope_range"), c(-0.1, 0.1))
  expect_equal(attr(out, "rope_ci"), 0.95)
})

test_that("rope is absent by default", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit)

  expect_true(all(is.na(out$rope_pct)))
  expect_null(attr(out, "rope_range"))
})

# ---- component ---------------------------------------------------------

test_that("component follows blavaan's own vocabulary", {
  fit <- test_blavaan_fit("one")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)

  expect_identical(out$component, as.character(mp$Component))
  expect_true(all(out$component %in% c("latent", "residual")))
})

test_that("component = 'latent' and 'residual' select their rows", {
  fit <- test_blavaan_fit("one")
  latent <- apa_tidy(fit, component = "latent")
  residual <- apa_tidy(fit, component = "residual")
  all_rows <- apa_tidy(fit)

  expect_identical(latent$term, mp_of(fit, component = "latent")$Parameter)
  expect_true(all(latent$component == "latent"))
  expect_true(all(residual$component == "residual"))
  expect_identical(
    sort(c(latent$term, residual$term)), sort(all_rows$term)
  )
})

test_that("a lavaan component name is refused before easystats aborts", {
  # Measured: the filter in model_parameters.blavaan is
  # `tolower(Component) %in% component`, so each of the lavaan route's six
  # names selects zero rows and easystats then aborts with "replacement
  # has 1 row, data has 0 rows" — a message naming nothing the user can
  # act on.
  fit <- test_blavaan_fit("one")
  expect_error(apa_tidy(fit, component = "loading"), "component")
  expect_error(
    parameters::model_parameters(fit, component = "loading"),
    "data has 0"
  )
})

# ---- standardize -------------------------------------------------------

test_that("model_parameters(standardize =) still aborts upstream", {
  # The reason the standardized path exists (spec point 6): every value,
  # FALSE included, warns and then aborts with "$ operator not defined
  # for this S4 class". A regression test, so that the second path is
  # dropped only once easystats can do this itself.
  fit <- test_blavaan_fit("one")

  for (value in list("std.all", "std.lv", TRUE, FALSE)) {
    expect_error(
      suppressWarnings(
        parameters::model_parameters(fit, standardize = value)
      ),
      "S4 class"
    )
  }
})

test_that("standardize reports the whole partable, a superset of the free", {
  fit <- test_blavaan_fit("one")
  sp <- blavaan::standardizedPosterior(fit)
  out <- apa_tidy(fit, standardize = TRUE)
  plain <- apa_tidy(fit)

  expect_identical(out$term, colnames(sp))
  expect_true(all(plain$term %in% out$term))
  expect_true("visual=~x1" %in% setdiff(out$term, plain$term))
  expect_true(all(out$std))
  expect_identical(attr(out, "standardized"), "std.all")
})

test_that("the standardized numbers are describe_posterior()'s", {
  fit <- test_blavaan_fit("one")
  d <- as.data.frame(bayestestR::describe_posterior(
    as.data.frame(blavaan::standardizedPosterior(fit)),
    centrality = "median", ci = 0.95, ci_method = "eti", test = "pd"
  ))
  out <- apa_tidy(fit, standardize = TRUE)
  row <- match(out$term, d$Parameter)

  expect_equal(out$estimate, d$Median[row])
  expect_equal(out$ci_low, d$CI_low[row])
  expect_equal(out$ci_high, d$CI_high[row])
  expect_equal(out$pd, d$pd[row])
})

test_that("standardized rows carry real diagnostics, not NA", {
  # Measured elementwise to 1.1e-16: row i of standardizedPosterior() is
  # the standardization of draw i of the chains, so the chain structure
  # carries over and R-hat and ESS are defined for every standardized row.
  fit <- test_blavaan_fit("one")
  sd <- sd_of(std_draws_of(fit))
  out <- apa_tidy(fit, standardize = TRUE)
  row <- match(out$term, sd$variable)

  expect_equal(out$rhat, sd$rhat[row])
  expect_equal(out$ess_bulk, sd$ess_bulk[row])
  expect_equal(out$ess_tail, sd$ess_tail[row])
  expect_true(all(is.finite(out$rhat)))
  expect_true(all(is.finite(out$ess_bulk)))
})

test_that("component is NA under standardize, and bayestestR is credited", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit, standardize = TRUE)

  expect_true(all(is.na(out$component)))
  # The standardized path takes its numbers from bayestestR, not from
  # parameters, and the version note says which package produced them.
  expect_true("bayestestR" %in% names(attr(out, "package_versions")))
  expect_false("parameters" %in% names(attr(out, "package_versions")))
})

test_that("standardize takes a lavaan type string", {
  fit <- test_blavaan_fit("one")
  d <- as.data.frame(bayestestR::describe_posterior(
    as.data.frame(
      blavaan::standardizedPosterior(fit, type = "std.lv")
    ),
    centrality = "median", ci = 0.95, ci_method = "eti", test = "pd"
  ))
  out <- apa_tidy(fit, standardize = "std.lv")

  expect_equal(out$estimate, d$Median[match(out$term, d$Parameter)])
  expect_identical(attr(out, "standardized"), "std.lv")
})

test_that("component cannot be combined with standardize", {
  fit <- test_blavaan_fit("one")
  expect_error(
    apa_tidy(fit, standardize = TRUE, component = "latent"),
    "component"
  )
  expect_error(
    apa_tidy(fit, standardize = TRUE, component = "latent"),
    "standardize"
  )
})

test_that("standardize is validated", {
  fit <- test_blavaan_fit("one")
  expect_error(apa_tidy(fit, standardize = "std.nope"), "standardize")
})

# ---- labels and selection ----------------------------------------------

test_that("label is the term with spaces around the operator", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit)

  expect_identical(out$label[out$term == "visual=~x2"], "visual =~ x2")
  expect_identical(out$label[out$term == "x1~~x1"], "x1 ~~ x1")
})

test_that("the label operators are matched longest-first", {
  # A unit test on the helper, because the bcfa() fixtures only ever
  # produce `=~` and `~~`: a bsem() or bgrowth() fit also has `~`
  # regressions, `~1` intercepts, `:=` defined parameters and `~*~`
  # scaling factors, and reading `=~` or `~~` as a bare `~` would split
  # the term in the wrong place without any test noticing.
  terms <- c(
    "visual=~x1", "x1~~x1", "dem60~ind60", "x1~1", "ab:=a*b", "x1~*~x1"
  )

  expect_identical(
    blavaan_labels(terms, NULL),
    c(
      "visual =~ x1", "x1 ~~ x1", "dem60 ~ ind60", "x1 ~1", "ab := a*b",
      "x1 ~*~ x1"
    )
  )
})

test_that("labels = overrides the derived label, term case included", {
  # On this route the derived label is not the term, so asking for the
  # term itself is a real request and must not read as "no label given".
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit, labels = c("visual=~x2" = "Loading of x2"))
  same <- apa_tidy(fit, labels = c("visual=~x2" = "visual=~x2"))
  plain <- apa_tidy(fit)

  expect_identical(out$label[out$term == "visual=~x2"], "Loading of x2")
  expect_identical(same$label[same$term == "visual=~x2"], "visual=~x2")
  other <- out$term != "visual=~x2"
  expect_identical(out$label[other], plain$label[other])
})

test_that("variables = selects and orders the reported terms", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit, variables = c("x1~~x1", "visual=~x2"))
  plain <- apa_tidy(fit)

  expect_identical(out$term, c("x1~~x1", "visual=~x2"))
  expect_equal(out$estimate, plain$estimate[match(out$term, plain$term)])
  expect_equal(out$rhat, plain$rhat[match(out$term, plain$term)])
})

test_that("variables = errors on an unreported term", {
  fit <- test_blavaan_fit("one")
  expect_error(apa_tidy(fit, variables = "nope"), "must name reported")
  # The fixed marker is not in the default row set, but is under
  # standardize; the message names what is available either way.
  expect_error(apa_tidy(fit, variables = "visual=~x1"), "must name reported")
  expect_identical(
    apa_tidy(fit, standardize = TRUE, variables = "visual=~x1")$term,
    "visual=~x1"
  )
})

# ---- columns not on this route -----------------------------------------

test_that("p, bf, group and effects are typed NA on the blavaan route", {
  fit <- test_blavaan_fit("one")
  out <- apa_tidy(fit)

  expect_true(all(is.na(out$p)))
  expect_true(all(is.na(out$bf)))
  expect_true(all(is.na(out$group)))
  expect_true(all(is.na(out$effects)))
  expect_type(out$p, "double")
  expect_type(out$bf, "double")
  expect_type(out$group, "character")
  expect_type(out$effects, "character")
})

# ---- guards ------------------------------------------------------------

test_that("a multi-group fit is refused by both methods", {
  # Measured: model_parameters() itself aborts on one ("differing number
  # of rows: 156, 145") and each fallback names the parameters
  # differently, so there is no row set that keeps `term` meaning what it
  # means on every other fit.
  fit <- test_blavaan_fit("groups")
  expect_equal(lavaan::lavInspect(fit, "ngroups"), 2L)
  expect_error(apa_tidy(fit), "multi-group")
  expect_error(apa_tidy_sem_fit(fit), "multi-group")
  expect_error(suppressWarnings(parameters::model_parameters(fit)))
})

test_that("ci_level and rope are validated", {
  fit <- test_blavaan_fit("one")
  expect_error(apa_tidy(fit, ci_level = 1), "ci_level")
  expect_error(apa_tidy(fit, rope = 0.1), "rope")
  expect_error(apa_tidy(fit, rope = c(-0.1, 0.1), rope_ci = 0), "rope_ci")
  expect_error(apa_tidy(fit, diagnostics = "yes"), "diagnostics")
})

test_that("blavaan is required, not assumed", {
  fit <- test_blavaan_fit("one")

  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      cli::cli_abort("{pkg} is not installed.")
    },
    .package = "rlang"
  )
  expect_error(apa_tidy(fit), "blavaan")
  expect_error(apa_tidy_sem_fit(fit), "blavaan")
})

test_that("the lavaan methods still refuse a blavaan fit when called direct", {
  # apa_tidy(bfit) now dispatches to the blavaan method, so the guard on
  # the lavaan method is reachable only through the method itself — which
  # a user can still call, and which apa_tidy_sem_fit() reaches whenever
  # a blavaan method is missing.
  fit <- test_blavaan_fit("one")
  expect_error(apa_tidy.lavaan(fit), "blavaan")
  expect_error(apa_tidy_sem_fit.lavaan(fit), "blavaan")
})

# ---- the fit-index row -------------------------------------------------

test_that("apa_tidy_sem_fit() on a blavaan fit returns the sem_fit contract", {
  two <- test_blavaan_fit("two")
  out <- suppressWarnings(apa_tidy_sem_fit(two, model = "Two factors"))

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "sem_fit")
  expect_identical(nrow(out), 1L)
  expect_identical(out$model, "Two factors")
  expect_identical(attr(out, "centrality"), "median")
  expect_identical(attr(out, "ci_method"), "hdi")
  expect_identical(attr(out, "ci_level"), 0.90)
  expect_identical(attr(out, "source_class"), "blavaan")
  expect_identical(attr(out, "estimator"), "Bayes")
  expect_identical(attr(out, "pD"), "loo")
  expect_identical(attr(out, "rescale"), "devM")
  expect_equal(attr(out, "n"), lavaan::lavInspect(two, "ntotal"))
  expect_true(withVisible(
    suppressWarnings(apa_tidy_sem_fit(two))
  )$visible)
})

test_that("ppp comes from fitMeasures() and the indices from blavaan", {
  two <- test_blavaan_fit("two")
  fi <- suppressWarnings(blavaan::blavFitIndices(two))
  s <- summary(fi, central.tendency = "median", prob = 0.90)
  out <- suppressWarnings(apa_tidy_sem_fit(two))

  expect_equal(out$ppp, unname(lavaan::fitMeasures(two, "ppp")))
  expect_equal(out$brmsea, s["BRMSEA", "Median"])
  expect_equal(out$brmsea_low, s["BRMSEA", "lower"])
  expect_equal(out$brmsea_high, s["BRMSEA", "upper"])
  expect_equal(out$bgammahat, s["BGammaHat", "Median"])
  expect_equal(out$bgammahat_low, s["BGammaHat", "lower"])
  expect_equal(out$bgammahat_high, s["BGammaHat", "upper"])
  expect_equal(out$brmsea, median(fi@indices$BRMSEA))
})

test_that("the fit-index interval is the HDI, not the ETI", {
  # Measured: blavaan's own summary() prints an HPD interval, and
  # describe_posterior(ci_method = "hdi") reproduces it while "eti"
  # differs. Reporting an ETI under a heading blavaan prints as HPD
  # would misstate it, so the row says "hdi".
  two <- test_blavaan_fit("two")
  fi <- suppressWarnings(blavaan::blavFitIndices(two))
  indices <- as.data.frame(fi@indices)
  hdi <- as.data.frame(bayestestR::describe_posterior(
    indices,
    centrality = "median", ci = 0.90, ci_method = "hdi", test = NULL
  ))
  eti <- as.data.frame(bayestestR::describe_posterior(
    indices,
    centrality = "median", ci = 0.90, ci_method = "eti", test = NULL
  ))
  out <- suppressWarnings(apa_tidy_sem_fit(two))

  expect_equal(
    out$brmsea_low, hdi$CI_low[hdi$Parameter == "BRMSEA"],
    tolerance = 1e-6
  )
  expect_false(isTRUE(all.equal(
    out$brmsea_low, eti$CI_low[eti$Parameter == "BRMSEA"],
    tolerance = 1e-6
  )))
})

test_that("fit_ci_level widens the interval and reaches the attribute", {
  two <- test_blavaan_fit("two")
  narrow <- suppressWarnings(apa_tidy_sem_fit(two))
  wide <- suppressWarnings(apa_tidy_sem_fit(two, fit_ci_level = 0.95))

  expect_lt(wide$brmsea_low, narrow$brmsea_low)
  expect_gt(wide$brmsea_high, narrow$brmsea_high)
  expect_equal(wide$brmsea, narrow$brmsea)
  expect_identical(attr(wide, "ci_level"), 0.95)
})

test_that("pd and rescale reach blavFitIndices()", {
  two <- test_blavaan_fit("two")
  out <- suppressWarnings(apa_tidy_sem_fit(two, pd = "dic"))
  plain <- suppressWarnings(apa_tidy_sem_fit(two))
  expected <- suppressWarnings(
    blavaan::blavFitIndices(two, pD = "dic", rescale = "devM")
  )

  expect_equal(out$brmsea, median(expected@indices$BRMSEA))
  expect_false(isTRUE(all.equal(out$brmsea, plain$brmsea)))
  expect_identical(attr(out, "pD"), "dic")
})

test_that("every lavaan fit index is NA on a blavaan fit", {
  # Measured: fitMeasures(blavaan) carries no chisq, df, pvalue, cfi,
  # tli, rmsea or srmr at all — fitMeasures(x, "rmsea") is numeric(0),
  # silently. The two sem_fit rows are complementary.
  two <- test_blavaan_fit("two")
  out <- suppressWarnings(apa_tidy_sem_fit(two))

  lavaan_only <- c(
    "chisq", "df", "p", "cfi", "tli", "rmsea", "rmsea_low",
    "rmsea_high", "rmsea_level", "srmr"
  )
  for (col in lavaan_only) {
    expect_true(is.na(out[[col]]), info = col)
    expect_type(out[[col]], "double")
  }
  expect_length(lavaan::fitMeasures(two, "rmsea"), 0L)
})

test_that("the fit-index arguments are validated", {
  two <- test_blavaan_fit("two")
  expect_error(apa_tidy_sem_fit(two, pd = "nope"), "pd")
  expect_error(apa_tidy_sem_fit(two, rescale = "nope"), "rescale")
  expect_error(apa_tidy_sem_fit(two, fit_ci_level = 1), "fit_ci_level")
  expect_error(apa_tidy_sem_fit(two, model = 1), "model")
})

test_that("test = 'none' is refused by the fit-index method only", {
  # Measured: with test = "none" blavaan drops ppp from fitMeasures()
  # and blavFitIndices() aborts, while model_parameters() is unaffected.
  # Exactly parallel to the lavaan route's se = "none" guard.
  notest <- test_blavaan_fit("notest")

  expect_identical(lavaan::lavInspect(notest, "options")$test, "none")
  # blavaan's own NOTE and its loo warnings, neither under test here.
  expect_false("ppp" %in% names(suppressWarnings(suppressMessages(
    lavaan::fitMeasures(notest)
  ))))
  expect_error(apa_tidy_sem_fit(notest), "test = \"none\"")

  out <- apa_tidy(notest)
  expect_identical(nrow(out), 6L)
  expect_identical(out$term, coef_names(notest))
})
