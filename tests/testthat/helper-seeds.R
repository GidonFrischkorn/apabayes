# Seed helpers copied verbatim from the manuscripts apabayes replaces
# (ARCHITECTURE.md § Tests: "one test reproduces its output character for
# character"). Sources, read 2026-09-06:
#   SDVWM: SensoryDiscriminationVWM/scripts/shared/sem_helpers.R, lines
#          147-151 (format_bf), 171 (drop0), 175 (fmt_r), 188 (fmt_p),
#          191-195 (fmt_bf)
#   miniQ: miniQmetrics/reports/miniQ_psychometrics.qmd, lines 115
#          (fmt_r), 126-128 (fmt_pd), 134-139 (fmt_bf)
#   m3:    tutorial-m3-bmm/manuscript/tutorial-m3-bmm.qmd, lines 95-104
#          (fmt_bf), 120-126 (fmt_er)
# They are scalar functions; tests call them element-wise. Whitespace was
# reflowed for line length; the logic is unchanged.

seed_sdvwm <- list(
  drop0 = function(s) sub("^(-?)0[.]", "\\1.", s),
  format_bf = function(bf, digits = 2) {
    if (bf > 100) {
      return("> 100")
    }
    if (bf < 0.01) {
      return("< 0.01")
    }
    formatC(bf, digits = digits, format = "f")
  }
)
seed_sdvwm$fmt_r <- function(x, d = 2) {
  seed_sdvwm$drop0(formatC(x, digits = d, format = "f"))
}
seed_sdvwm$fmt_p <- function(p) {
  if (p < .001) {
    "< .001"
  } else {
    seed_sdvwm$drop0(formatC(p, digits = 3, format = "f"))
  }
}
seed_sdvwm$fmt_bf <- function(bf, which = "10") {
  val <- seed_sdvwm$format_bf(bf)
  sep <- if (grepl("^[<>]", val)) " " else " = "
  paste0("*BF*~", which, "~", sep, val)
}

seed_miniq <- list(
  fmt_r = function(x) {
    sub("^-0\\.", "-.", sub("^0\\.", ".", sprintf("%.2f", x)))
  },
  fmt_pd = function(pd) {
    if (pd > .999) {
      "> .999"
    } else {
      paste0("= ", sub("^0\\.", ".", sprintf("%.3f", pd)))
    }
  },
  fmt_bf = function(bf) {
    if (!is.finite(bf)) {
      return("$\\infty$")
    }
    e <- floor(log10(bf))
    m <- bf / 10^e
    paste0(sprintf("%.2f", m), " × 10^", e, "^")
  }
)

seed_m3 <- list(
  fmt_bf = function(x) {
    if (x >= 10000) {
      ev <- floor(log10(x))
      cv <- x / 10^ev
      sprintf("$%.1f \\times 10^{%d}$", cv, ev)
    } else if (x >= 10) {
      sprintf("%.1f", x)
    } else {
      sprintf("%.2f", x)
    }
  },
  fmt_er = function(x) {
    if (is.na(x)) {
      return("NA")
    }
    if (is.infinite(x) || x >= 10000) {
      return("$\\geq$ 10,000")
    }
    if (x >= 1000) {
      return(format(round(x), big.mark = ","))
    }
    if (x >= 10) {
      return(sprintf("%.1f", x))
    }
    sprintf("%.2f", x)
  }
)

# Words no apabayes formatter may emit next to a number (decision 15).
decision_words <- c(
  "anecdotal", "weak", "moderate", "substantial", "positive", "strong",
  "very strong", "extreme", "decisive", "significant", "evidence",
  "support", "bare mention"
)
