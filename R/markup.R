# Markup targets and the symbol table (ARCHITECTURE.md decision 4).
#
# Every inline string apabayes produces is written for one of four
# targets: "md" (pandoc markdown, the default and what apaquarto's DOCX
# and HTML routes read), "latex" (markdown with math for the symbols the
# PDF font may drop), "typst" (identical to md until a render test says
# otherwise) and "plain" (ASCII for the console and tests). Tables always
# use "md" because flextable renders markdown in every output format.

markup_targets <- c("md", "latex", "typst", "plain")

#' Resolve the markup target
#'
#' Internal. Order: the `markup` argument, then
#' `getOption("apabayes.markup")`, then knitr detection
#' (`knitr::is_latex_output()` gives `"latex"`, `knitr::pandoc_to("typst")`
#' gives `"typst"`), then `"md"`.
#'
#' @param markup `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`.
#' @return A string, one of the four targets.
#' @noRd
markup_target <- function(markup = NULL) {
  if (is.null(markup)) {
    markup <- getOption("apabayes.markup")
  }
  if (is.null(markup)) {
    return(detect_markup_target())
  }
  rlang::arg_match0(markup, markup_targets, arg_nm = "markup")
}

detect_markup_target <- function() {
  if (!rlang::is_installed("knitr")) {
    return("md")
  }
  if (isTRUE(knitr::is_latex_output())) {
    return("latex")
  }
  if (isTRUE(knitr::pandoc_to("typst"))) {
    return("typst")
  }
  "md"
}

# One row per glyph, one column per target. Non-ASCII glyphs are written
# as escapes so that R CMD check sees ASCII source. U+2212 is the minus
# sign apa7::align_chr() emits in tables (measured 2026-09-06).
symbol_table <- rbind(
  minus        = c("\u2212", "\u2212", "\u2212", "-"),
  times        = c("\u00d7", "$\\times$", "\u00d7", "x"),
  infinity     = c("\u221e", "$\\infty$", "\u221e", "Inf"),
  neg_infinity = c("\u2212\u221e", "$-\\infty$", "\u2212\u221e", "-Inf"),
  geq          = c("\u2265", "$\\geq$", "\u2265", ">="),
  leq          = c("\u2264", "$\\leq$", "\u2264", "<="),
  chisq        = c("\u03c7\u00b2", "$\\chi^2$", "\u03c7\u00b2", "chi2"),
  rhat         = c("*R\u0302*", "$\\hat{R}$", "*R\u0302*", "Rhat"),
  delta        = c("\u0394", "$\\Delta$", "\u0394", "Delta")
)
colnames(symbol_table) <- markup_targets

#' Look up a symbol for a markup target
#'
#' Internal. Vectorised over `name`.
#'
#' @param name Character vector of symbol names, rows of `symbol_table`.
#' @param markup Target, resolved by `markup_target()`.
#' @return Character vector of glyphs.
#' @noRd
symbol <- function(name, markup = NULL) {
  target <- markup_target(markup)
  unknown <- setdiff(name, rownames(symbol_table))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown symbol{?s} {.val {unknown}}.",
      "i" = "Known symbols: {.val {rownames(symbol_table)}}."
    ))
  }
  unname(symbol_table[name, target])
}

#' Wrap text in italics, a subscript or a superscript
#'
#' Internal. Italics are applied first, so `markup("BF", italic = TRUE,
#' subscript = "10")` gives `*BF*~10~`. The plain target drops the italic
#' and subscript markers and keeps a caret for superscripts (`10^5`).
#'
#' @param x Character vector.
#' @param markup Target, resolved by `markup_target()`.
#' @param italic Logical scalar.
#' @param subscript,superscript `NULL` or a character vector recycled
#'   against `x`.
#' @return Character vector of `length(x)`; `NA` stays `NA`.
#' @noRd
markup <- function(x, markup = NULL, italic = FALSE, subscript = NULL,
                   superscript = NULL) {
  target <- markup_target(markup)
  plain <- target == "plain"
  out <- as.character(x)
  if (italic && !plain) {
    out <- paste0("*", out, "*", recycle0 = TRUE)
  }
  if (!is.null(subscript)) {
    out <- if (plain) {
      paste0(out, subscript, recycle0 = TRUE)
    } else {
      paste0(out, "~", subscript, "~", recycle0 = TRUE)
    }
  }
  if (!is.null(superscript)) {
    out <- if (plain) {
      paste0(out, "^", superscript, recycle0 = TRUE)
    } else {
      paste0(out, "^", superscript, "^", recycle0 = TRUE)
    }
  }
  out[is.na(x)] <- NA_character_
  out
}

#' Join a statistic symbol and a formatted value
#'
#' Internal. `"*p* = .023"`, but `"*p* < .001"` when the value begins with
#' a comparison sign, so that floors, caps and bounds read naturally.
#'
#' @param name Character scalar (or vector recycled against `value`).
#' @param value Character vector of formatted values.
#' @return Character vector of `length(value)`; `NA` stays `NA`.
#' @noRd
stat_string <- function(name, value) {
  comparison <- "^(<|>|\u2265|\u2264|\\$\\\\geq\\$|\\$\\\\leq\\$)"
  joiner <- rep(" = ", length(value))
  joiner[!is.na(value) & grepl(comparison, value)] <- " "
  out <- paste0(name, joiner, value, recycle0 = TRUE)
  out[is.na(value)] <- NA_character_
  out
}
