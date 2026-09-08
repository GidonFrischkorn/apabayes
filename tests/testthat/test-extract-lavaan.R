# Tests for the lavaan route (dev/specs/spec-apa_tidy_lavaan.md).
#
# Every expected value is computed from the fit inside the test, with
# lavaan's own accessors as the oracle: parameterEstimates(),
# standardizedSolution() and fitMeasures(). The fits come from
# test_lavaan_fit() in setup.R and run on CRAN (no Stan involved).

# ---- helpers -----------------------------------------------------------

key_of <- function(d, lhs = "lhs", op = "op", rhs = "rhs") {
  paste0(d[[lhs]], d[[op]], d[[rhs]])
}

mp_of <- function(fit, ...) {
  as.data.frame(parameters::model_parameters(fit, component = "all", ...))
}

# The lavaan table rows in `terms` order, matched by lhs/op/rhs.
rows_of <- function(tab, terms) {
  match(terms, key_of(tab))
}

# ---- contract ----------------------------------------------------------

test_that("apa_tidy() on a lavaan fit returns the parameters contract", {
  fit <- test_lavaan_fit("cfa")
  out <- apa_tidy(fit)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "parameters")
  expect_true(all(
    names(tidy_contracts()$parameters$columns) %in% names(out)
  ))
  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(attr(out, "ci_method"), "wald")
  expect_identical(attr(out, "ci_level"), 0.95)
  expect_identical(attr(out, "source_class"), "lavaan")
  expect_null(attr(attr(out, "source_class"), "package"))
  expect_true(all(
    c("lavaan", "parameters", "apabayes") %in%
      names(attr(out, "package_versions"))
  ))
  expect_identical(attr(out, "standardized"), FALSE)
  expect_identical(attr(out, "estimator"), "ML")
  expect_identical(attr(out, "se"), "standard")
})

test_that("the lavaan method returns its table visibly", {
  fit <- test_lavaan_fit("cfa")
  expect_true(withVisible(apa_tidy(fit))$visible)
})

# ---- the numbers are lavaan's ------------------------------------------

test_that("term is lavaan's own parameter naming", {
  fit <- test_lavaan_fit("cfa")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)

  expect_identical(out$term, paste0(mp$To, mp$Operator, mp$From))
  expect_identical(
    out$term,
    lavaan:::lav_partable_labels(lavaan::parTable(fit))
  )
  expect_true(all(names(lavaan::coef(fit)) %in% out$term))
  expect_equal(anyDuplicated(out$term), 0)
})

test_that("estimate, interval and p follow parameterEstimates()", {
  fit <- test_lavaan_fit("cfa")
  pe <- lavaan::parameterEstimates(fit)
  out <- apa_tidy(fit)
  row <- rows_of(pe, out$term)

  expect_false(anyNA(row))
  expect_equal(out$estimate, pe$est[row])
  expect_equal(out$ci_low, pe$ci.lower[row])
  expect_equal(out$ci_high, pe$ci.upper[row])
  expect_equal(out$p, pe$pvalue[row])
  expect_identical(out$component, mp_of(fit)$Component)
  expect_identical(out$ci_method, rep("wald", nrow(out)))
  expect_equal(out$ci_level, rep(0.95, nrow(out)))
})

test_that("a fixed parameter has no p value, whatever parameters says", {
  # Measured: parameters replaces lavaan's NA with 0 on the marker
  # loadings (`params$p[is.na(params$p)] <- 0`). The route restores NA.
  fit <- test_lavaan_fit("cfa")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)
  fixed <- is.na(mp$z)

  expect_true(any(fixed))
  expect_true(all(mp$p[fixed] == 0))
  expect_true(all(is.na(out$p[fixed])))
  expect_false(anyNA(out$p[!fixed]))
  # The estimate and its degenerate interval stay as lavaan reports them.
  expect_equal(out$estimate[fixed], out$ci_low[fixed])
})

test_that("ci_level reaches the interval", {
  fit <- test_lavaan_fit("cfa")
  pe <- lavaan::parameterEstimates(fit, level = 0.9)
  out <- apa_tidy(fit, ci_level = 0.9)
  row <- rows_of(pe, out$term)

  expect_equal(out$ci_low, pe$ci.lower[row])
  expect_equal(out$ci_high, pe$ci.upper[row])
  expect_equal(out$ci_level, rep(0.9, nrow(out)))
  expect_identical(attr(out, "ci_level"), 0.9)
})

# ---- the interval type -------------------------------------------------

test_that("a bootstrap fit reports a percentile interval, named as such", {
  fit <- test_lavaan_fit("boot")
  pe <- lavaan::parameterEstimates(fit, boot.ci.type = "perc")
  out <- apa_tidy(fit)
  row <- rows_of(pe, out$term)

  expect_identical(attr(out, "se"), "bootstrap")
  expect_identical(out$ci_method, rep("boot", nrow(out)))
  expect_identical(attr(out, "ci_method"), "boot")
  expect_equal(out$ci_low, pe$ci.lower[row])
  # Not a Wald interval on the bootstrap SE.
  free <- !is.na(out$p)
  expect_false(isTRUE(all.equal(
    out$ci_low[free],
    out$estimate[free] - stats::qnorm(0.975) * pe$se[row][free]
  )))
})

test_that("the standardized solution of a bootstrap fit is Wald again", {
  # Measured: standardizedSolution() uses the delta method on the
  # bootstrap covariance matrix, so its interval is est +/- z * se.
  fit <- test_lavaan_fit("boot")
  ss <- lavaan::standardizedSolution(fit)
  out <- apa_tidy(fit, standardize = TRUE)
  row <- rows_of(ss, out$term)

  expect_identical(out$ci_method, rep("wald", nrow(out)))
  expect_equal(out$ci_low, ss$ci.lower[row])
})

# ---- standardization ---------------------------------------------------

test_that("standardize = TRUE is the std.all solution with its own interval", {
  fit <- test_lavaan_fit("cfa")
  ss <- lavaan::standardizedSolution(fit, type = "std.all")
  out <- apa_tidy(fit, standardize = TRUE)
  row <- rows_of(ss, out$term)

  expect_false(anyNA(row))
  expect_equal(out$estimate, ss$est.std[row])
  expect_equal(out$ci_low, ss$ci.lower[row])
  expect_equal(out$ci_high, ss$ci.upper[row])
  expect_equal(out$p, ss$pvalue[row])
  expect_true(all(out$std))
  expect_identical(attr(out, "standardized"), "std.all")
  # The fixed latent variances are the rows without a test statistic now.
  expect_true(any(is.na(out$p)))
  expect_true(all(out$component[is.na(out$p)] == "Variance"))
})

test_that("standardize takes a lavaan type string", {
  fit <- test_lavaan_fit("cfa")
  ss <- lavaan::standardizedSolution(fit, type = "std.lv")
  out <- apa_tidy(fit, standardize = "std.lv")
  row <- rows_of(ss, out$term)

  expect_equal(out$estimate, ss$est.std[row])
  expect_identical(attr(out, "standardized"), "std.lv")
})

test_that("the default is the unstandardized solution", {
  fit <- test_lavaan_fit("cfa")
  out <- apa_tidy(fit)

  expect_true(all(!out$std))
  expect_type(out$std, "logical")
})

test_that("standardize is validated", {
  fit <- test_lavaan_fit("cfa")
  expect_error(apa_tidy(fit, standardize = "yes"), "standardize")
  expect_error(apa_tidy(fit, standardize = NA), "standardize")
  expect_error(apa_tidy(fit, standardize = c(TRUE, FALSE)), "standardize")
})

# ---- component ---------------------------------------------------------

test_that("component = 'all' is the default and includes variances", {
  fit <- test_lavaan_fit("cfa")
  mp <- mp_of(fit)
  out <- apa_tidy(fit)

  expect_identical(nrow(out), nrow(mp))
  expect_true("Variance" %in% out$component)
})

test_that("component selects one or several easystats components", {
  fit <- test_lavaan_fit("cfa")
  loadings <- apa_tidy(fit, component = "loading")
  two <- apa_tidy(fit, component = c("loading", "correlation"))

  expect_identical(nrow(loadings), 9L)
  expect_true(all(loadings$component == "Loading"))
  expect_true(all(grepl("=~", loadings$term, fixed = TRUE)))
  expect_identical(nrow(two), 12L)
  expect_setequal(unique(two$component), c("Loading", "Correlation"))
})

test_that("a component the model does not have is an error, not a table", {
  # Measured: easystats silently returns zero rows here.
  fit <- test_lavaan_fit("cfa")
  expect_error(
    apa_tidy(fit, component = "regression"),
    "no parameters to report"
  )
})

test_that("component is validated before easystats sees it", {
  # Measured: the capitalised name silently returns zero rows upstream.
  fit <- test_lavaan_fit("cfa")
  expect_error(apa_tidy(fit, component = "Loading"), "component")
  expect_error(apa_tidy(fit, component = "loadings"), "component")
})

# ---- other model shapes ------------------------------------------------

test_that("regressions keep lavaan's orientation and defined rows appear", {
  fit <- test_lavaan_fit("sem")
  pe <- lavaan::parameterEstimates(fit)
  out <- apa_tidy(fit)

  expect_true(all(c("Regression", "Defined") %in% out$component))
  reg <- out[out$component == "Regression", ]
  expect_true("dem60~ind60" %in% reg$term)
  expect_equal(
    reg$estimate[reg$term == "dem60~ind60"],
    pe$est[pe$lhs == "dem60" & pe$op == "~" & pe$rhs == "ind60"]
  )
  expect_identical(reg$label[reg$term == "dem60~ind60"], "dem60 ~ ind60")
  defined <- out[out$component == "Defined", ]
  expect_identical(defined$term, "ab:=a*b")
  expect_identical(defined$label, "ab := a*b")
  expect_equal(defined$estimate, pe$est[pe$op == ":="])
  expect_equal(anyDuplicated(out$term), 0)
})

test_that("intercepts are named as lavaan names them", {
  fit <- test_lavaan_fit("means")
  out <- apa_tidy(fit)

  expect_true("x1~1" %in% out$term)
  expect_identical(out$label[out$term == "x1~1"], "x1 ~1")
  expect_identical(out$component[out$term == "x1~1"], "Mean")
  expect_true("x1~1" %in% names(lavaan::coef(fit)))
})

test_that("a multi-group fit keeps terms and labels unique, groups named", {
  fit <- test_lavaan_fit("groups")
  pe <- lavaan::parameterEstimates(fit)
  labels <- lavaan::lavInspect(fit, "group.label")
  out <- apa_tidy(fit)

  expect_identical(nrow(out), nrow(pe))
  expect_equal(anyDuplicated(out$term), 0)
  expect_equal(anyDuplicated(out$label), 0)
  expect_setequal(unique(out$group), labels)
  second <- out$group == labels[2]
  expect_true(all(endsWith(out$term[second], ".g2")))
  expect_false(any(endsWith(out$term[!second], ".g2")))
  expect_true(all(endsWith(out$label[second], paste0("(", labels[2], ")"))))
  # The suffix is lavaan's own.
  expect_true(all(names(lavaan::coef(fit)) %in% out$term))
  expect_equal(out$estimate, pe$est)
})

# ---- selection and labels ----------------------------------------------

test_that("variables = selects and orders the reported terms", {
  fit <- test_lavaan_fit("cfa")
  out <- apa_tidy(fit, variables = c("speed=~x9", "visual=~x2"))
  plain <- apa_tidy(fit)

  expect_identical(out$term, c("speed=~x9", "visual=~x2"))
  expect_equal(out$estimate, plain$estimate[match(out$term, plain$term)])
  expect_error(apa_tidy(fit, variables = "visual =~ x2"), "must name reported")
})

test_that("labels = overrides the derived label by term", {
  fit <- test_lavaan_fit("cfa")
  out <- apa_tidy(fit, labels = c("visual=~x2" = "Visual: x2"))
  plain <- apa_tidy(fit)

  expect_identical(out$label[out$term == "visual=~x2"], "Visual: x2")
  other <- out$term != "visual=~x2"
  expect_identical(out$label[other], plain$label[other])
  expect_identical(plain$label[plain$term == "visual=~x2"], "visual =~ x2")
})

test_that("labels = wins even where the label asked for is the term", {
  fit <- test_lavaan_fit("cfa")
  # On this route the derived label is not the term (`visual =~ x2` against
  # `visual=~x2`), so asking for the term itself is a real request, not a
  # no-op, and it must not be read as "no label supplied".
  out <- apa_tidy(fit, labels = c("visual=~x2" = "visual=~x2"))

  expect_identical(out$label[out$term == "visual=~x2"], "visual=~x2")
})

# ---- columns not on this route -----------------------------------------

test_that("the Bayesian columns are typed NA on the lavaan route", {
  fit <- test_lavaan_fit("cfa")
  out <- apa_tidy(fit)

  for (col in c("pd", "rope_pct", "rhat", "ess_bulk", "ess_tail", "bf")) {
    expect_true(all(is.na(out[[col]])), info = col)
    expect_type(out[[col]], "double")
  }
  expect_true(all(is.na(out$group)))
  expect_true(all(is.na(out$effects)))
})

# ---- guards ------------------------------------------------------------

test_that("a blavaan fit is refused by the lavaan method", {
  # Measured: S3 dispatch on the S4 object reaches apa_tidy.lavaan() from
  # a blavaan fit, which would then be reported with a Wald CI and a p.
  # Since the blavaan route landed, `apa_tidy(fit)` dispatches to that
  # method and succeeds, so the guard is reached only by naming the
  # lavaan method — which is still a thing a user can do.
  fit <- test_blavaan_fit("one")
  expect_error(apa_tidy.lavaan(fit), "blavaan")
  expect_error(apa_tidy_sem_fit.lavaan(fit), "blavaan")
  expect_identical(attr(apa_tidy(fit), "source_class"), "blavaan")
})

test_that("a non-converged fit is refused before easystats or fitMeasures", {
  fit <- test_lavaan_fit("nonconverged")
  expect_false(lavaan::lavInspect(fit, "converged"))
  expect_error(apa_tidy(fit), "did not converge")
  expect_error(apa_tidy_sem_fit(fit), "did not converge")
})

test_that("se = 'none' is refused with its own message", {
  # Measured: model_parameters() aborts inside data.frame() here.
  fit <- test_lavaan_fit("nose")
  expect_error(apa_tidy(fit), "se = \"none\"")
})

test_that("ci_level is validated", {
  fit <- test_lavaan_fit("cfa")
  expect_error(apa_tidy(fit, ci_level = 1), "ci_level")
  expect_error(apa_tidy(fit, ci_level = "95%"), "ci_level")
})

test_that("lavaan is required, not assumed", {
  fit <- test_lavaan_fit("cfa")

  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      cli::cli_abort("{pkg} is not installed.")
    },
    .package = "rlang"
  )
  expect_error(apa_tidy(fit), "lavaan")
  expect_error(apa_tidy_sem_fit(fit), "lavaan")
})

# ---- apa_tidy_sem_fit() ------------------------------------------------

test_that("apa_tidy_sem_fit() returns the sem_fit contract of fitMeasures()", {
  fit <- test_lavaan_fit("cfa")
  fm <- lavaan::fitMeasures(fit)
  out <- apa_tidy_sem_fit(fit)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "sem_fit")
  expect_identical(nrow(out), 1L)
  expect_identical(out$model, NA_character_)
  expect_equal(out$chisq, unname(fm["chisq"]))
  expect_equal(out$df, unname(fm["df"]))
  expect_equal(out$p, unname(fm["pvalue"]))
  expect_equal(out$cfi, unname(fm["cfi"]))
  expect_equal(out$tli, unname(fm["tli"]))
  expect_equal(out$rmsea, unname(fm["rmsea"]))
  expect_equal(out$rmsea_low, unname(fm["rmsea.ci.lower"]))
  expect_equal(out$rmsea_high, unname(fm["rmsea.ci.upper"]))
  expect_equal(out$rmsea_level, 0.9)
  expect_equal(out$srmr, unname(fm["srmr"]))
  for (col in c(
    "ppp", "brmsea", "brmsea_low", "brmsea_high", "bgammahat",
    "bgammahat_low", "bgammahat_high"
  )) {
    expect_identical(out[[col]], NA_real_, info = col)
  }
})

test_that("the sem_fit attributes say where the row came from", {
  fit <- test_lavaan_fit("cfa")
  out <- apa_tidy_sem_fit(fit)

  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(attr(out, "ci_method"), NA_character_)
  expect_identical(attr(out, "ci_level"), NA_real_)
  expect_identical(attr(out, "source_class"), "lavaan")
  expect_identical(attr(out, "estimator"), "ML")
  expect_identical(attr(out, "test"), "standard")
  expect_equal(attr(out, "n"), lavaan::lavInspect(fit, "ntotal"))
  expect_true(all(
    c("lavaan", "apabayes") %in% names(attr(out, "package_versions"))
  ))
  expect_true(withVisible(apa_tidy_sem_fit(fit))$visible)
})

test_that("rmsea_level changes the RMSEA interval and is recorded", {
  fit <- test_lavaan_fit("cfa")
  fm <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = 0.95))
  out <- apa_tidy_sem_fit(fit, rmsea_level = 0.95)
  default <- apa_tidy_sem_fit(fit)

  expect_equal(out$rmsea_low, unname(fm["rmsea.ci.lower"]))
  expect_equal(out$rmsea_high, unname(fm["rmsea.ci.upper"]))
  expect_equal(out$rmsea_level, 0.95)
  expect_false(isTRUE(all.equal(out$rmsea_high, default$rmsea_high)))
  expect_equal(out$rmsea, default$rmsea)
  expect_error(apa_tidy_sem_fit(fit, rmsea_level = 1), "rmsea_level")
})

test_that("model names the row", {
  fit <- test_lavaan_fit("cfa")
  expect_identical(apa_tidy_sem_fit(fit, model = "CFA")$model, "CFA")
  expect_error(apa_tidy_sem_fit(fit, model = c("a", "b")), "model")
  expect_error(apa_tidy_sem_fit(fit, model = 1), "model")
})

test_that("test = 'scaled' and 'robust' pick lavaan's variants", {
  fit <- test_lavaan_fit("mlr")
  fm <- lavaan::fitMeasures(fit)
  scaled <- apa_tidy_sem_fit(fit, test = "scaled")
  robust <- apa_tidy_sem_fit(fit, test = "robust")
  standard <- apa_tidy_sem_fit(fit)

  expect_equal(scaled$chisq, unname(fm["chisq.scaled"]))
  expect_equal(scaled$df, unname(fm["df.scaled"]))
  expect_equal(scaled$p, unname(fm["pvalue.scaled"]))
  expect_equal(scaled$cfi, unname(fm["cfi.scaled"]))
  expect_equal(scaled$tli, unname(fm["tli.scaled"]))
  expect_equal(scaled$rmsea, unname(fm["rmsea.scaled"]))
  expect_equal(scaled$rmsea_low, unname(fm["rmsea.ci.lower.scaled"]))
  expect_equal(scaled$srmr, unname(fm["srmr"]))
  expect_identical(attr(scaled, "test"), "scaled")

  expect_equal(robust$chisq, unname(fm["chisq.scaled"]))
  expect_equal(robust$cfi, unname(fm["cfi.robust"]))
  expect_equal(robust$tli, unname(fm["tli.robust"]))
  expect_equal(robust$rmsea, unname(fm["rmsea.robust"]))
  expect_equal(robust$rmsea_low, unname(fm["rmsea.ci.lower.robust"]))
  expect_equal(robust$rmsea_high, unname(fm["rmsea.ci.upper.robust"]))
  expect_identical(attr(robust, "test"), "robust")

  expect_equal(standard$chisq, unname(fm["chisq"]))
  expect_false(isTRUE(all.equal(standard$chisq, scaled$chisq)))
  expect_identical(attr(standard, "estimator"), "ML")
})

test_that("a scaled variant is refused on a fit that has none", {
  fit <- test_lavaan_fit("cfa")
  expect_error(apa_tidy_sem_fit(fit, test = "scaled"), "scaled test")
  expect_error(apa_tidy_sem_fit(fit, test = "robust"), "scaled test")
  expect_error(apa_tidy_sem_fit(fit, test = "naive"), "test")
})

test_that("a measure fitMeasures() does not report is named, not left NA", {
  fit <- test_lavaan_fit("cfa")
  # Every lavaan fit met so far reports all eleven measures, so the gap is
  # provoked rather than found. It has to be named: indexing a measure that
  # is not there yields NA, and an NA SRMR in a fit table reads as a fit
  # index of unknown value rather than as one lavaan never computed.
  real <- lavaan::fitMeasures
  local_mocked_bindings(
    fitMeasures = function(object, ...) {
      fm <- real(object, ...)
      fm[names(fm) != "srmr"]
    },
    .package = "lavaan"
  )
  expect_error(apa_tidy_sem_fit(fit), "srmr")
})
