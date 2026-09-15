# Tests for the compare.loo route (local/specs/spec-apa_tidy_compare_loo.md).
#
# No expected number is typed: every one is read from the `compare.loo`
# object under test, or from the `loo_model_weights()` result passed in.
# The data-frame shape is built from simulated log-likelihoods
# (`test_loo_list()`, no Stan); the matrix shape is the checked-in object
# loo 2.9.0 made from the same simulation.

loo_columns <- c(
  elpd_diff = "elpd_diff", se_diff = "se_diff", elpd = "elpd_loo",
  se_elpd = "se_elpd_loo", p_loo = "p_loo", looic = "looic",
  se_p_loo = "se_p_loo", se_looic = "se_looic"
)

# ---- the data-frame shape ------------------------------------------------

test_that("apa_tidy() on a compare.loo returns the loo contract", {
  cmp <- loo::loo_compare(test_loo_list())
  out <- apa_tidy(cmp)
  expect_true(is_apabayes_tidy(out))
  expect_identical(attr(out, "type"), "loo")
  expect_identical(nrow(out), nrow(cmp))
  expect_identical(
    names(out)[1:8],
    c(
      "model", "elpd_diff", "se_diff", "elpd", "se_elpd", "p_loo", "looic",
      "weight"
    )
  )
  expect_identical(out$model, cmp$model)
  for (col in names(loo_columns)) {
    expect_identical(out[[col]], cmp[[loo_columns[[col]]]], label = col)
  }
  expect_identical(out$p_worse, cmp$p_worse)
  expect_identical(out$diag_diff, cmp$diag_diff)
  expect_identical(out$diag_elpd, cmp$diag_elpd)
  expect_identical(out$weight, rep(NA_real_, nrow(cmp)))
})

test_that("the attributes say what the table is and which row is best", {
  cmp <- loo::loo_compare(test_loo_list())
  out <- apa_tidy(cmp)
  expect_identical(attr(out, "reference"), cmp$model[1])
  expect_identical(attr(out, "weight_method"), NA_character_)
  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(attr(out, "ci_method"), NA_character_)
  expect_identical(attr(out, "ci_level"), NA_real_)
  expect_identical(attr(out, "source_class"), class(cmp))
  expect_named(attr(out, "package_versions"), c("loo", "apabayes"))
})

test_that("apa_tidy() on a compare.loo returns visibly", {
  cmp <- loo::loo_compare(test_loo_list())
  expect_true(withVisible(apa_tidy(cmp))$visible)
})

test_that("the object's row order is kept whatever order the input had", {
  loos <- test_loo_list()
  cmp <- loo::loo_compare(loos[c("wide", "good", "shifted")])
  out <- apa_tidy(cmp)
  expect_identical(out$model, cmp$model)
  expect_identical(attr(out, "reference"), cmp$model[1])
})

# ---- the matrix shape (loo < 2.10.0) -------------------------------------

test_that("the matrix shape reads the model from the row names", {
  skip_if_not_installed("loo")
  cmp <- fixture("compare_loo_matrix")
  expect_true(is.matrix(cmp))
  out <- apa_tidy(cmp)
  expect_identical(out$model, rownames(cmp))
  for (col in names(loo_columns)) {
    expect_identical(out[[col]], unname(cmp[, loo_columns[[col]]]), label = col)
  }
  expect_identical(out$p_worse, rep(NA_real_, nrow(cmp)))
  expect_identical(out$diag_diff, rep(NA_character_, nrow(cmp)))
  expect_identical(out$diag_elpd, rep(NA_character_, nrow(cmp)))
  expect_identical(attr(out, "reference"), rownames(cmp)[1])
})

test_that("both shapes of the same comparison give the same table", {
  from_matrix <- apa_tidy(fixture("compare_loo_matrix"))
  from_frame <- apa_tidy(loo::loo_compare(test_loo_list()))
  keep <- c("model", names(loo_columns))
  expect_equal(
    as.data.frame(from_matrix)[keep],
    as.data.frame(from_frame)[keep],
    tolerance = 1e-12
  )
})

# ---- weights -------------------------------------------------------------

test_that("weights from loo_model_weights() are matched by model name", {
  loos <- test_loo_list()
  cmp <- loo::loo_compare(loos)
  kinds <- list(
    stacking = loo::loo_model_weights(loos, method = "stacking"),
    `pseudo-BMA+` = withr::with_seed(
      1, loo::loo_model_weights(loos, method = "pseudobma")
    ),
    `pseudo-BMA` = loo::loo_model_weights(
      loos,
      method = "pseudobma", BB = FALSE
    )
  )
  for (kind in names(kinds)) {
    w <- kinds[[kind]]
    out <- apa_tidy(cmp, weights = w)
    expect_identical(out$weight, as.numeric(w[out$model]), label = kind)
    expect_identical(attr(out, "weight_method"), kind)
  }
  # Weights computed on a list in another order: matched by name, they
  # give the same column; matched by position they would not.
  w <- loo::loo_model_weights(
    loos[c("wide", "good", "shifted")],
    method = "pseudobma", BB = FALSE
  )
  expect_false(identical(names(w), cmp$model))
  expect_identical(
    apa_tidy(cmp, weights = w)$weight,
    apa_tidy(cmp, weights = kinds[["pseudo-BMA"]])$weight
  )
})

test_that("a named numeric vector is matched by name and has no method", {
  cmp <- loo::loo_compare(test_loo_list())
  w <- c(wide = 0.2, good = 0.5, shifted = 0.3)
  out <- apa_tidy(cmp, weights = w)
  expect_identical(out$weight, unname(w[out$model]))
  expect_identical(attr(out, "weight_method"), NA_character_)
})

test_that("weights that cannot be matched or are not weights are refused", {
  cmp <- loo::loo_compare(test_loo_list())
  expect_error(apa_tidy(cmp, weights = "a"), "numeric")
  expect_error(apa_tidy(cmp, weights = c(0.5, 0.3, 0.2)), "named")
  expect_error(
    apa_tidy(cmp, weights = c(good = 0.5, 0.3, wide = 0.2)),
    "named"
  )
  expect_error(
    apa_tidy(cmp, weights = c(good = 0.5, shifted = 0.3, other = 0.2)),
    "wide.*other|other.*wide"
  )
  expect_error(
    apa_tidy(cmp, weights = c(good = 0.5, shifted = 0.5)),
    "wide"
  )
  expect_error(
    apa_tidy(cmp, weights = c(good = NA, shifted = 0.5, wide = 0.5)),
    "missing"
  )
  expect_error(
    apa_tidy(cmp, weights = c(good = 1.2, shifted = -0.1, wide = -0.1)),
    "between 0 and 1"
  )
  expect_error(
    apa_tidy(cmp, weights = c(good = 0.5, shifted = 0.3, wide = 0.3)),
    "sum to 1"
  )
  expect_error(
    apa_tidy(cmp, weights = c(good = 0.5, good = 0.3, wide = 0.2)),
    "more than once"
  )
})

# ---- what the route refuses ----------------------------------------------

test_that("a WAIC comparison is refused by name", {
  cmp <- loo::loo_compare(test_loo_list())
  waic <- cmp
  names(waic) <- sub("loo", "waic", names(waic))
  expect_error(apa_tidy(waic), "WAIC")
})

test_that("a comparison missing a column is refused by name", {
  cmp <- loo::loo_compare(test_loo_list())
  bad <- cmp
  bad$looic <- NULL
  expect_error(apa_tidy(bad), "looic")
  no_model <- cmp
  no_model$model <- NULL
  expect_error(apa_tidy(no_model), "model")
  m <- fixture("compare_loo_matrix")
  rownames(m) <- NULL
  expect_error(apa_tidy(m), "row names")
})

test_that("an object that is neither shape is refused", {
  skip_if_not_installed("loo")
  fake <- structure(list(elpd_diff = 0), class = "compare.loo")
  expect_error(apa_tidy(fake), "data frame or matrix")
})

test_that("duplicated model names are refused", {
  loos <- test_loo_list()
  cmp <- loo::loo_compare(stats::setNames(loos, c("a", "a", "b")))
  expect_error(apa_tidy(cmp), "more than once")
})

test_that("an empty comparison is refused", {
  cmp <- loo::loo_compare(test_loo_list())
  expect_error(apa_tidy(cmp[0, ]), "no models")
})

test_that("the route needs loo installed, to record its version", {
  cmp <- loo::loo_compare(test_loo_list())
  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      cli::cli_abort("{pkg} is not installed.")
    },
    .package = "rlang"
  )
  expect_error(apa_tidy(cmp), "loo")
})

test_that("an argument the route does not take is refused", {
  cmp <- loo::loo_compare(test_loo_list())
  # Not `weight =`: R's partial matching would bind it to `weights`.
  expect_error(apa_tidy(cmp, ci = "hdi"), class = "rlib_error_dots_nonempty")
})

# ---- live fits -----------------------------------------------------------

test_that("brms's two producers give the table of the object", {
  # The thin fit has one Pareto k above 0.7 (measured); not under test.
  full <- suppressWarnings(brms::add_criterion(test_brms_fit("full"), "loo"))
  reduced <- suppressWarnings(
    brms::add_criterion(test_brms_fit("reduced"), "loo")
  )
  cmp <- suppressWarnings(brms::loo_compare(full, reduced, criterion = "loo"))
  out <- apa_tidy(cmp)
  expect_identical(out$model, cmp$model)
  for (col in names(loo_columns)) {
    expect_identical(out[[col]], cmp[[loo_columns[[col]]]], label = col)
  }
  diffs <- suppressWarnings(brms::loo(full, reduced))$diffs
  expect_identical(
    as.data.frame(apa_tidy(diffs)),
    as.data.frame(out)
  )
})
