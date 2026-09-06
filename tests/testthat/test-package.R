test_that("package skeleton loads and declares the testthat edition", {
  expect_true("apabayes" %in% loadedNamespaces())
  expect_identical(testthat::edition_get(), 3L)
})
