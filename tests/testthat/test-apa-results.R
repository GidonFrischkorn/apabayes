# Tests for the apabayes_results object (spec-apa_results.md).
#
# The object under test is built through apa_inline() on a checked-in
# tidy fixture, so these tests run on CRAN. The knitr tests measure the
# thing the probe found: only a knit_print method makes bare inline use
# print the string.

test_that("the object has papaja's shape under an apabayes class", {
  r <- apa_inline(fixture("tidy_brms_full"), "wt")
  expect_s3_class(
    r, c("apabayes_results", "apa_results", "list"),
    exact = TRUE
  )
  expect_named(r, c("estimate", "statistic", "full_result", "table", "markup"))
  expect_true(is_apa_results(r))
  expect_false(is_apa_results(list()))
  expect_false(is_apa_results(r$table))
  expect_true(is_apabayes_tidy(r$table))
  expect_identical(r$markup, "md")
})

test_that("print writes full_result and returns invisibly", {
  r <- apa_inline(fixture("tidy_brms_full"), "wt")
  expect_output(print(r), r$full_result, fixed = TRUE)
  vis <- NULL
  utils::capture.output(vis <- withVisible(print(r)))
  expect_false(vis$visible)
  expect_identical(vis$value, r)
  many <- apa_inline(fixture("tidy_brms_full"))
  expect_output(
    print(many), paste(many$full_result, collapse = "\n"),
    fixed = TRUE
  )
})

test_that("format() and as.character() return full_result", {
  r <- apa_inline(fixture("tidy_brms_full"), "wt")
  expect_identical(format(r), r$full_result)
  expect_identical(as.character(r), r$full_result)
})

test_that("knit_print returns the string inline and prints in a chunk", {
  skip_if_not_installed("knitr")
  r <- apa_inline(fixture("tidy_brms_full"), "wt")
  expect_identical(knitr::knit_print(r, inline = TRUE), r$full_result)
  expect_output(knitr::knit_print(r), r$full_result, fixed = TRUE)
})

test_that("bare inline code renders the string through knitr", {
  skip_if_not_installed("knitr")
  env <- new.env()
  env$r <- apa_inline(fixture("tidy_brms_full"), "wt")
  out <- knitr::knit(text = "Result: `r r`.", quiet = TRUE, envir = env)
  expect_identical(out, paste0("Result: ", env$r$full_result, "."))
})

test_that("papaja::apa_table() accepts the object", {
  skip_if_not_installed("papaja")
  r <- apa_inline(fixture("tidy_brms_full"), "wt")
  expect_s3_class(papaja::apa_table(r), "knit_asis")
})

test_that("the constructor checks its table and its strings", {
  t <- fixture("tidy_brms_full")[1, ]
  expect_error(
    new_apa_results("a", "b", "c", data.frame(x = 1), "md"),
    "must be an <apabayes_tidy>"
  )
  expect_error(
    new_apa_results(c("a", "b"), "b", "c", t, "md"),
    "`estimate` must be a character vector of length 1"
  )
  expect_error(
    new_apa_results("a", 1, "c", t, "md"),
    "`statistic` must be a character vector"
  )
  expect_error(
    new_apa_results("a", "b", "c", t, "md", "unnamed"),
    "must be named"
  )
  expect_error(new_apa_results("a", "b", "c", t, "html"), "`markup`")
})

test_that("the strings may be one per row or one for the whole table", {
  t <- fixture("tidy_brms_full")
  r <- new_apa_results(NA_character_, "all", "all", t, "plain")
  expect_identical(r$full_result, "all")
  expect_identical(r$table, t)
  three <- t[1:3, ]
  expect_error(
    new_apa_results(c("a", "b"), c("a", "b"), c("a", "b"), three, "md"),
    "length 3 or 1"
  )
})

test_that("extra named elements travel on the object", {
  t <- fixture("tidy_brms_full")[1, ]
  r <- new_apa_results("a", NA_character_, "a", t, "plain", passed = TRUE)
  expect_true(r$passed)
  expect_identical(r$markup, "plain")
})
