# Tests for the apabayes_tidy contract (dev/specs/spec-apabayes_tidy.md,
# ARCHITECTURE.md decisions 1 and 2). Written before R/tidy-class.R.

param_cols <- c(
  "term", "label", "estimate", "ci_low", "ci_high", "ci_method",
  "ci_level", "pd", "rope_pct", "rhat", "ess_bulk", "ess_tail", "bf",
  "component", "group", "effects", "std", "p"
)

minimal <- function(...) {
  data.frame(
    term = c("b_wt", "b_am"),
    estimate = c(-5.21, 0.16),
    stringsAsFactors = FALSE,
    ...
  )
}

test_that("the constructor returns a classed tibble with the contract", {
  out <- apabayes_tidy(minimal())

  expect_s3_class(out, "apabayes_tidy")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), param_cols)
  expect_identical(nrow(out), 2L)
  expect_true(is_apabayes_tidy(out))
})

test_that("the constructor returns its table visibly", {
  # `validate_apabayes_tidy()` returns invisibly, so the constructor must
  # not end on it: an invisible return would mean `apabayes_tidy(x)` at the
  # console prints nothing and `print.apabayes_tidy()` never fires.
  expect_true(withVisible(apabayes_tidy(minimal()))$visible)
})

test_that("absent columns are filled with the typed NA", {
  out <- apabayes_tidy(minimal())

  expect_identical(out$ci_low, c(NA_real_, NA_real_))
  expect_identical(out$pd, c(NA_real_, NA_real_))
  expect_identical(out$component, c(NA_character_, NA_character_))
  expect_identical(out$std, c(NA, NA))
  expect_type(out$estimate, "double")
  expect_type(out$term, "character")
})

test_that("label falls back to term element-wise", {
  expect_identical(apabayes_tidy(minimal())$label, c("b_wt", "b_am"))

  partial <- minimal(label = c("Weight", NA_character_))
  expect_identical(apabayes_tidy(partial)$label, c("Weight", "b_am"))
})

test_that("the seeding arguments fill ci_method and ci_level", {
  out <- apabayes_tidy(minimal(), ci_method = "hdi", ci_level = 0.9)
  expect_identical(out$ci_method, c("hdi", "hdi"))
  expect_identical(out$ci_level, c(0.9, 0.9))

  supplied <- apabayes_tidy(minimal(ci_method = c("hpd", "hpd")))
  expect_identical(supplied$ci_method, c("hpd", "hpd"))
})

test_that("integer columns are coerced to double and bad types abort", {
  out <- apabayes_tidy(minimal(ess_bulk = c(400L, 512L)))
  expect_type(out$ess_bulk, "double")
  expect_identical(out$ess_bulk, c(400, 512))

  expect_error(
    apabayes_tidy(data.frame(term = "a", estimate = "x")),
    "estimate"
  )
  expect_error(
    apabayes_tidy(data.frame(term = "a", estimate = "x")),
    "character"
  )
})

test_that("required columns are checked per type and named in the message", {
  expect_error(apabayes_tidy(data.frame(estimate = 1)), "term")
  expect_error(apabayes_tidy(data.frame(term = "a")), "estimate")
  expect_error(
    apabayes_tidy(minimal(), type = "loo"),
    "elpd_diff"
  )
  expect_error(apabayes_tidy(list(term = "a")), "data frame")
})

test_that("a zero-row table is valid", {
  out <- apabayes_tidy(minimal()[0, ])

  expect_identical(nrow(out), 0L)
  expect_identical(names(out), param_cols)
  expect_type(out$estimate, "double")
  expect_true(is_apabayes_tidy(out))
})

test_that("extra columns are kept after the contract columns", {
  out <- apabayes_tidy(minimal(mcse = c(0.1, 0.2)))

  expect_identical(names(out), c(param_cols, "mcse"))
  expect_identical(out$mcse, c(0.1, 0.2))
})

test_that("attributes come from the arguments and from ...", {
  out <- apabayes_tidy(
    minimal(),
    centrality = "mean",
    ci_method = "hdi",
    ci_level = 0.89,
    source_class = c("stanfit"),
    package_versions = c(posterior = "1.7.1"),
    rope_range = c(-0.1, 0.1)
  )

  expect_identical(attr(out, "type"), "parameters")
  expect_identical(attr(out, "centrality"), "mean")
  expect_identical(attr(out, "ci_method"), "hdi")
  expect_identical(attr(out, "ci_level"), 0.89)
  expect_identical(attr(out, "source_class"), "stanfit")
  expect_identical(attr(out, "package_versions"), c(posterior = "1.7.1"))
  expect_identical(attr(out, "rope_range"), c(-0.1, 0.1))
})

test_that("unnamed ... aborts", {
  expect_error(apabayes_tidy(minimal(), rope_range = c(-1, 1), "note"), "named")
})

test_that("the arguments are validated", {
  expect_error(apabayes_tidy(minimal(), type = "nope"), "type")
  expect_error(apabayes_tidy(minimal(), centrality = "mode"), "centrality")
  expect_error(apabayes_tidy(minimal(), ci_method = "quantile"), "ci_method")
  expect_error(apabayes_tidy(minimal(), ci_level = 1.2), "ci_level")
  expect_error(apabayes_tidy(minimal(), ci_level = c(0.9, 0.95)), "ci_level")
  expect_error(
    apabayes_tidy(minimal(), package_versions = "1.7.1"),
    "named"
  )
  expect_error(apabayes_tidy(minimal(pd = c(1.4, 0.5))), "pd")
})

test_that("NA is allowed for ci_method and ci_level", {
  out <- apabayes_tidy(minimal(), ci_method = NA, ci_level = NA)

  expect_true(is.na(attr(out, "ci_method")))
  expect_true(all(is.na(out$ci_method)))
  expect_true(all(is.na(out$ci_level)))
})

test_that("centrality = NA marks an estimate that is no posterior summary", {
  # Added with the lavaan route: a maximum-likelihood estimate is neither
  # a median nor a mean, and the header must not claim one.
  out <- apabayes_tidy(minimal(), centrality = NA)

  expect_identical(attr(out, "centrality"), NA_character_)
  expect_identical(validate_apabayes_tidy(out), out)
  header <- utils::capture.output(print(out))[2]
  expect_no_match(header, "median")
  expect_match(header, "95% CrI", fixed = TRUE)

  edited <- out
  attr(edited, "centrality") <- "mode"
  expect_error(validate_apabayes_tidy(edited), "centrality")
})

test_that("wald and boot are confidence intervals the contract knows", {
  wald <- apabayes_tidy(minimal(), ci_method = "wald", centrality = NA)
  boot <- apabayes_tidy(minimal(), ci_method = "boot", centrality = NA)

  expect_identical(wald$ci_method, c("wald", "wald"))
  expect_identical(attr(boot, "ci_method"), "boot")
  expect_match(
    utils::capture.output(print(wald))[2], "95% CI (Wald)",
    fixed = TRUE
  )
  expect_match(
    utils::capture.output(print(boot))[2], "95% CI (percentile bootstrap)",
    fixed = TRUE
  )
  # The open question for Gidon (ARCHITECTURE.md): not added.
  expect_error(apabayes_tidy(minimal(), ci_method = "spi"), "ci_method")
  expect_error(apabayes_tidy(minimal(), ci_method = "bci"), "ci_method")
})

test_that("the validator is the entry point for a classed object", {
  out <- apabayes_tidy(minimal())

  expect_identical(validate_apabayes_tidy(out), out)
  expect_error(validate_apabayes_tidy(tibble::tibble()), "apabayes_tidy")
  expect_false(is_apabayes_tidy(tibble::tibble()))
  expect_false(is_apabayes_tidy(1))

  broken <- out
  broken$estimate <- NULL
  expect_error(validate_apabayes_tidy(broken), "estimate")
})

test_that("every type constructs from its required columns", {
  expect_no_error(apabayes_tidy(
    data.frame(term = "b_wt"),
    type = "diagnostics"
  ))
  expect_no_error(apabayes_tidy(
    data.frame(hypothesis = "wt = 0", estimate = -5.2),
    type = "hypotheses"
  ))
  expect_no_error(apabayes_tidy(
    data.frame(model = "m1", elpd_diff = 0),
    type = "loo"
  ))
  expect_no_error(apabayes_tidy(
    data.frame(model = "m1", bf = 3.2),
    type = "bf_models"
  ))
  expect_no_error(apabayes_tidy(
    data.frame(model = "m1"),
    type = "sem_fit"
  ))
  expect_no_error(apabayes_tidy(
    data.frame(contrast = "a - b", estimate = 0.4),
    type = "contrasts"
  ))

  loo <- apabayes_tidy(data.frame(model = "m1", elpd_diff = 0), type = "loo")
  expect_identical(
    names(loo),
    c(
      "model", "elpd_diff", "se_diff", "elpd", "se_elpd", "p_loo",
      "looic", "weight"
    )
  )
  expect_identical(attr(loo, "type"), "loo")
})

test_that("print writes the two header lines and returns invisibly", {
  out <- apabayes_tidy(minimal(), source_class = c("stanfit"))

  lines <- utils::capture.output(res <- withVisible(print(out)))
  expect_false(res$visible)
  expect_identical(res$value, out)
  expect_match(
    lines[1], "apabayes tidy table: parameters (2 rows)",
    fixed = TRUE
  )
  expect_match(lines[2], "median", fixed = TRUE)
  expect_match(lines[2], "95% CrI (equal-tailed)", fixed = TRUE)
  expect_match(lines[2], "source: stanfit", fixed = TRUE)

  hdi <- apabayes_tidy(minimal(), ci_method = "hdi", ci_level = 0.89)
  expect_match(utils::capture.output(print(hdi))[2], "89% HDI", fixed = TRUE)

  bare <- apabayes_tidy(
    data.frame(term = "b_wt"),
    type = "diagnostics", ci_method = NA, ci_level = NA
  )
  bare_lines <- utils::capture.output(print(bare))
  expect_match(bare_lines[1], "diagnostics (1 row)", fixed = TRUE)
})

test_that("tibble keeps the class and the metadata through subsetting", {
  out <- apabayes_tidy(minimal())

  expect_true(is_apabayes_tidy(out[1, ]))
  expect_identical(attr(out[1, ], "centrality"), "median")
  expect_false(is_apabayes_tidy(as.data.frame(out)))
})

test_that("a logical column is kept as supplied", {
  out <- apabayes_tidy(minimal(std = c(TRUE, FALSE)))

  expect_identical(out$std, c(TRUE, FALSE))
  expect_type(out$std, "logical")
})

test_that("the validator catches a table edited into an invalid state", {
  out <- apabayes_tidy(minimal())

  bad_type <- out
  attr(bad_type, "type") <- "nope"
  expect_error(validate_apabayes_tidy(bad_type), "type")

  bad_centrality <- out
  attr(bad_centrality, "centrality") <- "mode"
  expect_error(validate_apabayes_tidy(bad_centrality), "centrality")

  bad_level <- out
  bad_level$ci_level <- c(0.95, 1.4)
  expect_error(validate_apabayes_tidy(bad_level), "ci_level")
})
