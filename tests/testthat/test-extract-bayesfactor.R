# Tests for the BayesFactor routes and the inclusion route
# (local/specs/spec-apa_tidy_BFBayesFactor.md).
#
# No expected number is typed: every one is computed from the fixture
# object under test (`@shortName`, `@longName`, `@bayesFactor`,
# `bayesfactor_models()`, `rownames()`, `apa_bf()`, `apa_prob()`). The
# BFBayesFactor, BFBayesFactorList and BFmcmc fixtures are S4 and need
# BayesFactor to load; the inclusion and model_parameters fixtures are
# plain data frames and load anywhere.

# ---- BFBayesFactor: the bf_models contract --------------------------------

test_that("a BFBayesFactor anova returns the bf_models contract", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_anova")
  out <- apa_tidy(b)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "bf_models")
  expect_identical(nrow(out), length(b@numerator) + 1L)

  short_names <- vapply(b@numerator, function(m) m@shortName, character(1),
    USE.NAMES = FALSE
  )
  long_names <- vapply(b@numerator, function(m) m@longName, character(1),
    USE.NAMES = FALSE
  )
  expect_identical(out$model, c(b@denominator@shortName, short_names))
  expect_identical(out$name, c(b@denominator@longName, long_names))

  log_bf <- c(0, BayesFactor::extractBF(b, logbf = TRUE)$bf)
  expect_identical(out$log_bf, log_bf)
  expect_identical(out$bf, exp(log_bf))
  expect_identical(out$error, c(NA_real_, as.double(b@bayesFactor$error)))
  expect_identical(out$denominator, seq_len(nrow(out)) == 1L)
  expect_identical(
    out$method,
    rep(paste0(b@denominator@type, " (BayesFactor)"), nrow(out))
  )
  expect_equal(sum(out$post_prob), 1)
})

test_that("the attributes name the method, prior odds and denominator", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_anova")
  out <- apa_tidy(b)

  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(attr(out, "ci_method"), NA_character_)
  expect_identical(attr(out, "ci_level"), NA_real_)
  expect_identical(attr(out, "source_class"), as.character(class(b)))
  versions <- attr(out, "package_versions")
  expect_true(all(c("BayesFactor", "apabayes") %in% names(versions)))
  expect_identical(attr(out, "bf_method"), out$method[1])
  expect_identical(attr(out, "prior_odds"), "equal")
  expect_identical(attr(out, "denominator_model"), out$model[1])
})

test_that("log_bf on numerator rows matches bayesfactor_models() for JZS", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_regression")
  out <- apa_tidy(b)
  bm <- bayestestR::bayesfactor_models(b)
  expect_equal(out$log_bf[-1], bm$log_BF[-1])
})

test_that("a t-test's rows are the null and the alternative", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_ttest")
  out <- apa_tidy(b)
  expect_identical(
    out$model,
    c(b@denominator@shortName, b@numerator[[1]]@shortName)
  )
  expect_identical(out$model, c("Null, mu1-mu2=0", "Alt., r=0.707"))
})

test_that("an interval t-test keeps every restricted alternative", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_ttest_interval")
  out <- apa_tidy(b)
  expect_identical(nrow(out), 3L)
  expect_identical(
    out$model,
    c(
      b@denominator@shortName,
      vapply(b@numerator, function(m) m@shortName, character(1),
        USE.NAMES = FALSE
      )
    )
  )
  expect_true(any(grepl("-Inf<d<0", out$model, fixed = TRUE)))
})

test_that("bf_top's denominator is the full model", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_top")
  out <- apa_tidy(b)
  expect_identical(out$model[1], b@denominator@shortName)
  expect_identical(out$model[1], "wt + hp + qsec")
  expect_true(out$denominator[1])
})

test_that("a numerator identical to the denominator is dropped", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_denom_num")
  out <- apa_tidy(b)
  expect_identical(nrow(out), 7L)
  expect_identical(sum(out$model == "wt"), 1L)
  expect_true(out$denominator[out$model == "wt"])
})

test_that("duplicate models are both kept, named apart by BayesFactor", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_dup")
  out <- apa_tidy(b)
  expect_identical(nrow(out), 3L)
  expect_identical(out$model[!out$denominator], c("wt", "wt #1"))
})

test_that("correlation and contingency methods and errors are read off x", {
  skip_if_not_installed("BayesFactor")
  cor_b <- fixture("bf_cor_interval")
  cor_out <- apa_tidy(cor_b)
  expect_true(all(is.na(cor_out$error)))
  expect_identical(
    cor_out$method,
    rep(paste0(cor_b@denominator@type, " (BayesFactor)"), nrow(cor_out))
  )
  expect_identical(cor_out$method[1], "Jeffreys-beta* (BayesFactor)")

  cont_b <- fixture("bf_contingency")
  cont_out <- apa_tidy(cont_b)
  expect_identical(
    cont_out$method,
    rep(paste0(cont_b@denominator@type, " (BayesFactor)"), nrow(cont_out))
  )
  numerator_row <- !cont_out$denominator
  expect_identical(
    cont_out$method[numerator_row], "independent multinomial (BayesFactor)"
  )
  expect_identical(cont_out$model[numerator_row], "Non-indep. (a=1)")
  expect_identical(cont_out$error[numerator_row], 0)
})

test_that("an overflowing Bayes factor stays finite on the log scale", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_overflow")
  out <- apa_tidy(b)
  numerator_row <- !out$denominator
  expect_true(is.infinite(out$bf[numerator_row]))
  expect_true(is.finite(out$log_bf[numerator_row]))
  expect_true(all(is.finite(out$post_prob)))
})

test_that("non-empty dots and a BFBayesFactorList are refused", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_ttest")
  expect_error(
    apa_tidy(b, foo = 1),
    class = "rlib_error_dots_nonempty"
  )
  expect_true(withVisible(apa_tidy(b))$visible)

  bl <- fixture("bf_list")
  expect_error(apa_tidy(bl), "x[, j]", fixed = TRUE)
})

# ---- BFmcmc: draws through posterior() -------------------------------------

test_that("an edited bayesFactor slot is refused", {
  skip_if_not_installed("BayesFactor")
  b <- fixture("bf_ttest")
  empty <- b
  empty@bayesFactor <- empty@bayesFactor[0, ]
  expect_error(apa_tidy(empty), "no Bayes factors")
  text <- b
  text@bayesFactor$bf <- as.character(text@bayesFactor$bf)
  expect_error(apa_tidy(text), "numeric")
})

test_that("a BFmcmc object reaches the draws route without a warning", {
  skip_if_not_installed("BayesFactor")
  skip_if_not_installed("posterior")
  b <- fixture("bf_post")
  m <- matrix(as.numeric(b), nrow(b), dimnames = list(NULL, colnames(b)))
  reference <- apa_tidy(posterior::as_draws_matrix(m))

  out <- expect_no_warning(apa_tidy(b))
  expect_identical(attr(out, "source_class"), "BFmcmc")

  out_matched <- out
  attr(out_matched, "source_class") <- attr(reference, "source_class")
  expect_identical(out_matched, reference)
})

test_that("BFmcmc route arguments pass through to the draws route", {
  skip_if_not_installed("BayesFactor")
  skip_if_not_installed("posterior")
  b <- fixture("bf_post")
  m <- matrix(as.numeric(b), nrow(b), dimnames = list(NULL, colnames(b)))
  reference <- apa_tidy(posterior::as_draws_matrix(m), ci = "hdi")
  out <- apa_tidy(b, ci = "hdi")
  expect_identical(out$ci_method, reference$ci_method)
  expect_identical(attr(out, "ci_method"), attr(reference, "ci_method"))
})

# ---- refusing model_parameters(<BFBayesFactor>) ----------------------------

test_that("a model_parameters() table from a BFBayesFactor is refused", {
  for (nm in c("mp_bf_anova", "mp_bf_ttest", "mp_bf_contingency")) {
    x <- fixture(nm)
    msg <- conditionMessage(rlang::catch_cnd(apa_tidy(x), "error"))
    expect_match(msg, "numerator", label = nm)
    expect_no_match(msg, "not Bayesian", label = nm)
  }
})

# ---- bf_inclusion -----------------------------------------------------------

test_that("a bayesfactor_inclusion table returns the bf_inclusion contract", {
  x <- fixture("inc_anova")
  out <- apa_tidy(x)

  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "bf_inclusion")
  expect_identical(out$term, rownames(x))
  expect_identical(out$p_prior, x$p_prior)
  expect_identical(out$p_posterior, x$p_posterior)
  log_bf <- x$log_BF
  log_bf[is.nan(log_bf)] <- NA_real_
  expect_identical(out$log_bf, log_bf)
  expect_identical(out$bf, exp(log_bf))

  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(attr(out, "ci_method"), NA_character_)
  expect_identical(attr(out, "ci_level"), NA_real_)
  expect_identical(attr(out, "source_class"), class(x))
  expect_identical(attr(out, "averaging"), "all")
  expect_identical(attr(out, "prior_odds"), "equal")
  expect_null(attr(out, "prior_odds_values"))
  expect_identical(attr(out, "bf_method"), NA_character_)
  expect_named(attr(out, "package_versions"), c("bayestestR", "apabayes"))
  expect_true(withVisible(apa_tidy(x))$visible)
})

test_that("matched averaging and custom prior odds are read off attrs", {
  matched_x <- fixture("inc_matched")
  matched <- apa_tidy(matched_x)
  expect_identical(attr(matched, "averaging"), "matched")

  odds_x <- fixture("inc_odds")
  odds <- apa_tidy(odds_x)
  expect_identical(attr(odds, "prior_odds"), "custom")
  expect_identical(
    attr(odds, "prior_odds_values"),
    as.numeric(attr(odds_x, "priorOdds", exact = TRUE))
  )
})

test_that("a NaN log Bayes factor becomes NA, not NaN", {
  x <- fixture("inc_random")
  out <- apa_tidy(x)
  expect_true("id" %in% out$term)
  id_row <- out[out$term == "id", ]
  expect_identical(id_row$log_bf, NA_real_)
  expect_false(is.nan(id_row$log_bf))
  expect_identical(id_row$bf, NA_real_)
})

test_that("an infinite log Bayes factor is kept as Inf", {
  x <- fixture("inc_inf")
  out <- apa_tidy(x)
  x_row <- out[out$term == "x", ]
  expect_true(is.infinite(x_row$log_bf))
  expect_identical(x_row$log_bf, Inf)
  expect_identical(x_row$bf, Inf)
})

test_that("the bic-based inclusion table works without BayesFactor", {
  x <- fixture("inc_bic")
  out <- apa_tidy(x)
  expect_identical(out$term, rownames(x))
  log_bf <- x$log_BF
  log_bf[is.nan(log_bf)] <- NA_real_
  expect_identical(out$bf, exp(log_bf))
})

test_that("apa_tidy.bayesfactor_inclusion() checks its columns and rows", {
  x <- fixture("inc_anova")

  no_prior <- x
  no_prior$p_prior <- NULL
  expect_error(apa_tidy(no_prior), "p_prior")

  text_bf <- x
  text_bf$log_BF <- as.character(text_bf$log_BF)
  expect_error(apa_tidy(text_bf), "numeric")

  expect_error(apa_tidy(x[0, ]), "no")

  expect_error(
    apa_tidy(x, foo = 1),
    class = "rlib_error_dots_nonempty"
  )
  expect_true(withVisible(apa_tidy(x))$visible)
})
