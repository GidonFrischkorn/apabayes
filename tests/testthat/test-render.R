# Milestone 5, D4. The gate that renders a document.
#
# Every other gate this project runs — the suite, coverage, R CMD check —
# was green on a table layer that could not render to PDF at all: apa7 put
# a bracket at the head of a LaTeX run and fontspec ate the interval,
# silently wherever the brackets balanced. Nothing that does not render a
# document can see that class of defect. This is the test that can.
#
# It is opt-in. Set APABAYES_RENDER_TEST to run it, and optionally
# APABAYES_APAQUARTO to an `_extensions` directory to run it offline.

test_that("the render test reports an unavailable toolchain as skipped", {
  reason <- render_skip_reason()
  expect_true(is.na(reason) || nzchar(reason))
})

test_that("every contract type survives every format apabayes promises", {
  skip_on_cran()
  reason <- render_skip_reason()
  skip_if(!is.na(reason), reason)

  lib <- render_install(withr::local_tempdir())
  skip_if(is.null(lib), "the package could not be installed for rendering")

  project <- withr::local_tempdir()
  skip_if(
    !render_add_apaquarto(project),
    "apaquarto is not available (no network and APABAYES_APAQUARTO unset)"
  )
  file.copy(testthat::test_path("render", "all-types.qmd"), project)

  # The tables the document will print, built here by the same code, so
  # the assertions are not a second transcription of the expected numbers.
  types <- c(
    "parameters", "diagnostics", "hypotheses", "sem_fit", "loo",
    "bf_inclusion", "bf_models", "contrasts", "correlations"
  )
  tables <- lapply(types, function(ty) {
    apa_table(readRDS(testthat::test_path(
      "..", "..", "inst", "extdata", paste0(ty, ".rds")
    )))
  })
  names(tables) <- types

  for (format in render_formats()) {
    status <- render_one(project, "all-types.qmd", format, lib)
    expect_identical(status, 0L, label = paste(format, "render exit status"))

    output <- list.files(project,
      pattern = "^all-types\\.(pdf|docx|html)$", full.names = TRUE
    )
    expect_gt(length(output), 0)
    if (length(output) == 0) {
      next
    }
    text <- render_squeeze(render_text(output[[which.max(
      file.mtime(output)
    )]]))
    expect_false(is.na(text))

    for (ty in types) {
      # The marker proves the type's region of the document was rendered
      # at all, so a missing interval below is a lost interval and not a
      # chunk that never ran.
      expect_true(
        grepl(render_squeeze(paste0("MARKER-TABLE-", toupper(ty))), text,
          fixed = TRUE
        ),
        label = paste(format, ty, "table marker")
      )

      interval <- render_first_interval(tables[[ty]])
      if (is.na(interval)) {
        next
      }
      # This is the assertion the word-joiner defect failed: the render
      # exited 0, the PDF was well formed, and the interval column was
      # blank.
      expect_true(
        grepl(render_squeeze(interval), text, fixed = TRUE),
        label = paste(format, ty, "interval", interval)
      )
    }
  }
})

test_that("the shipped apaquarto example renders in every promised format", {
  skip_on_cran()
  reason <- render_skip_reason()
  skip_if(!is.na(reason), reason)

  lib <- render_install(withr::local_tempdir())
  skip_if(is.null(lib), "the package could not be installed for rendering")

  project <- withr::local_tempdir()
  skip_if(
    !render_add_apaquarto(project),
    "apaquarto is not available (no network and APABAYES_APAQUARTO unset)"
  )
  example <- testthat::test_path(
    "..", "..", "inst", "apaquarto-example", "example.qmd"
  )
  file.copy(example, project)

  for (format in render_formats()) {
    status <- render_one(project, "example.qmd", format, lib)
    expect_identical(status, 0L, label = paste(format, "example exit status"))
  }

  pdf <- file.path(project, "example.pdf")
  expect_true(file.exists(pdf))
  text <- render_squeeze(render_text(pdf))
  # Derived, not transcribed: the expected interval is built by the same
  # code the document runs, so the assertion cannot drift from it, and no
  # minus sign or word joiner has to be spelled out as an escape here.
  posterior <- apa_table(readRDS(testthat::test_path(
    "..", "..", "inst", "extdata", "parameters.rds"
  )))
  interval <- render_first_interval(posterior)
  expect_true(grepl(render_squeeze(interval), text, fixed = TRUE))
  expect_true(grepl("BGammaHat", text, fixed = TRUE))
})
