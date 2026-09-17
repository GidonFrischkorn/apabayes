# Helpers for test-render.R (Milestone 5, D4). Everything here is inert
# unless that test runs, which it does not by default.

# The formats 0.1.0 promises (M5-5). typst needs apaquarto >= 6.0.0.
render_formats <- function() {
  c("apaquarto-pdf", "apaquarto-docx", "apaquarto-html", "apaquarto-typst")
}

# Why the test is opt-in: it renders four documents through Quarto and a
# TeX engine, which is minutes and a toolchain no CRAN machine has. A
# check that cannot run is reported as skipped, never as passed.
render_skip_reason <- function() {
  if (!nzchar(Sys.getenv("APABAYES_RENDER_TEST"))) {
    return("APABAYES_RENDER_TEST is not set")
  }
  if (!nzchar(Sys.which("quarto"))) {
    return("quarto is not on the PATH")
  }
  if (!nzchar(Sys.which("pdftotext"))) {
    return("pdftotext is not on the PATH")
  }
  NA_character_
}

# The package source this test is running against.
render_pkg_root <- function() {
  normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
}

# Install the source under test into `lib` and return it. Measured at
# 0.84 s (M5k), which is what makes this affordable on every run. Without
# it the rendered document would `library(apabayes)` out of the user
# library and could pass against a copy that predates the change under
# test.
render_install <- function(lib) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  status <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "INSTALL", "--no-docs", "--no-byte-compile",
      paste0("--library=", shQuote(lib)), shQuote(render_pkg_root())
    ),
    stdout = FALSE, stderr = FALSE
  ))
  if (!identical(status, 0L)) {
    return(NULL)
  }
  lib
}

# apaquarto is a Quarto extension, not an R dependency, so it has to be
# fetched. APABAYES_APAQUARTO points at an `_extensions` directory for an
# offline run; otherwise `quarto add` fetches it (1.7 s, M5k). Returns
# FALSE rather than erroring, so the caller can skip.
render_add_apaquarto <- function(dir) {
  local <- Sys.getenv("APABAYES_APAQUARTO")
  if (nzchar(local) && dir.exists(local)) {
    file.copy(local, dir, recursive = TRUE)
    return(dir.exists(file.path(dir, basename(local))))
  }
  # `quarto add` writes `_extensions/` relative to the working directory,
  # so it has to *run* there. Passing the target as an environment
  # variable does not move it, and it writes into the test directory
  # instead.
  withr::with_dir(dir, {
    suppressWarnings(system2(
      "quarto", c("add", "wjschne/apaquarto", "--no-prompt"),
      stdout = FALSE, stderr = FALSE
    ))
  })
  dir.exists(file.path(dir, "_extensions"))
}

render_one <- function(dir, qmd, format, lib) {
  withr::with_dir(dir, {
    withr::with_envvar(c(R_LIBS = lib), {
      suppressWarnings(system2(
        "quarto", c("render", qmd, "--to", format),
        stdout = FALSE, stderr = FALSE
      ))
    })
  })
}

# What the reader of the rendered document sees, as text. This is the
# whole reach of the render test (M5-7): it catches a lost interval, a
# blank column and an aborted render, and it does not catch a glyph that
# composes wrongly, because the characters are correct in that case
# (M5i). test-table.R's composability test covers that class instead.
render_text <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "pdf")) {
    out <- tempfile(fileext = ".txt")
    system2("pdftotext", c("-layout", shQuote(path), shQuote(out)),
      stdout = FALSE, stderr = FALSE
    )
    if (!file.exists(out)) {
      return(NA_character_)
    }
    return(paste(readLines(out, warn = FALSE), collapse = "\n"))
  }
  if (identical(ext, "docx")) {
    where <- tempfile()
    utils::unzip(path, files = "word/document.xml", exdir = where)
    xml <- file.path(where, "word", "document.xml")
    if (!file.exists(xml)) {
      return(NA_character_)
    }
    body <- paste(readLines(xml, warn = FALSE), collapse = "")
    return(gsub("<[^>]*>", "", body))
  }
  gsub("<[^>]*>", "", paste(readLines(path, warn = FALSE), collapse = "\n"))
}

# Both sides lose every kind of space before comparing: the cells carry
# figure spaces from align_chr() and a word joiner on each bracket, and a
# renderer is free to lay any of them out as it likes. What must survive
# is the characters that carry meaning.
render_squeeze <- function(x) {
  # Built from code points rather than written out: these are a no-break
  # space, a figure space, a thin space and a word joiner, and as literal
  # characters in a regex they are invisible in the source and impossible
  # to edit safely. styler also rewrites \\uXXXX escapes into literals,
  # so an escape would not survive here either.
  invisible_spaces <- intToUtf8(c(0x00a0, 0x2007, 0x2009, 0x2060))
  gsub(paste0("[[:space:]", invisible_spaces, "]"), "", x)
}

# The first interval cell a table prints, or NA when it prints none.
render_first_interval <- function(tab) {
  for (column in names(tab)) {
    cells <- tab[[column]]
    if (!is.character(cells)) {
      next
    }
    hit <- grep("\\[.*,.*\\]", cells, value = TRUE)
    if (length(hit) > 0) {
      return(hit[[1]])
    }
  }
  NA_character_
}
