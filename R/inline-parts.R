# The strings of the inline layer, one builder per tidy type. Each takes
# the selected rows and the resolved options and returns a data frame
# with an `estimate` and a `statistic` string per row (`NA` where a row
# has none). Every number passes through the format layer; nothing here
# rounds, and nothing here attaches a word to a number.

inline_strings <- function(x, opts) {
  switch(opts$type,
    parameters = inline_parameters(x, opts),
    hypotheses = inline_hypotheses(x, opts),
    diagnostics = inline_diagnostics(x, opts),
    sem_fit = inline_sem_fit(x, opts),
    loo = inline_loo(x, opts),
    bf_models = inline_bf_models(x, opts),
    contrasts = inline_contrasts(x, opts)
  )
}

# ---- shared pieces -------------------------------------------------------

# The interval label a `ci_method` value earns in text. `wald` and `boot`
# are confidence intervals; an unknown or missing method prints the
# neutral `CI` rather than claiming a credible interval.
ci_label_of <- function(method) {
  labels <- c(
    eti = "CrI", hdi = "HDI", hpd = "HPD", spi = "SPI", bci = "BCI",
    wald = "CI", boot = "CI"
  )
  out <- unname(labels[method])
  out[is.na(out)] <- "CI"
  out
}

# `estimate[, interval]` for one row, with an optional italic symbol in
# front. `level` may be NA (no level recorded): the brackets then print
# without a label whatever `ci_label` asked for — a label without its
# level would claim more than the table records — and the level handed
# to apa_ci() is unused.
inline_estimate_row <- function(estimate, low, high, method, level, sym,
                                leading_zero, opts) {
  m <- opts$markup
  est <- apa_num(estimate, opts$digits, leading_zero, markup = m)
  if (!is.na(sym)) {
    est <- stat_string(markup(sym, m, italic = TRUE), est)
  }
  if (!opts$interval || is.na(low) || is.na(high)) {
    return(est)
  }
  label <- opts$ci_label
  if (identical(label, "auto")) {
    label <- ci_label_of(method)
  }
  if (is.na(level)) {
    label <- NULL
    level <- 0.95
  }
  paste0(est, ", ", apa_ci(
    low, high,
    level = level, label = label, digits = opts$digits,
    leading_zero = leading_zero, markup = m
  ))
}

# Row-wise over a table, so that per-row levels, methods, symbols and
# leading-zero rules are honoured.
inline_estimates <- function(x, sym, leading_zero, opts) {
  vapply(seq_len(nrow(x)), function(i) {
    inline_estimate_row(
      x$estimate[i], x$ci_low[i], x$ci_high[i], x$ci_method[i],
      x$ci_level[i], sym[i], leading_zero[i], opts
    )
  }, character(1))
}

# Join the statistic parts of each row with ", ", dropping NA parts; a
# row with no part at all is NA.
join_columns <- function(parts, n) {
  parts <- Filter(Negate(is.null), parts)
  vapply(seq_len(n), function(i) {
    v <- vapply(parts, function(p) p[i], character(1))
    v <- v[!is.na(v)]
    if (length(v) == 0) NA_character_ else paste(v, collapse = ", ")
  }, character(1))
}

# `estimate, statistic`, or whichever of the two exists.
join_parts <- function(estimate, statistic) {
  out <- paste(estimate, statistic, sep = ", ")
  out[is.na(statistic)] <- estimate[is.na(statistic)]
  out[is.na(estimate)] <- statistic[is.na(estimate)]
  out
}

# The symbol of a row under `symbol = NULL`: `b` for a population-level
# regression coefficient — `component` "conditional" on a row that is
# not a random-effect term (measured: brms puts `sd_*` rows under
# "conditional" too, with `effects` "random") — and none elsewhere.
resolve_symbols <- function(x, symbol) {
  n <- nrow(x)
  if (isFALSE(symbol)) {
    return(rep(NA_character_, n))
  }
  if (!is.null(symbol)) {
    return(rep(symbol, n))
  }
  coefficient <- !is.na(x$component) & x$component == "conditional" &
    (is.na(x$effects) | x$effects == "fixed")
  ifelse(coefficient, "b", NA_character_)
}

# `leading_zero` per row: "auto" keeps it except on a standardized row.
resolve_leading_zero <- function(x, leading_zero) {
  if (identical(leading_zero, "auto")) {
    std <- if ("std" %in% names(x)) x$std else rep(NA, nrow(x))
    return(!(std %in% TRUE))
  }
  rep(leading_zero, nrow(x))
}

# ---- parameters ----------------------------------------------------------

inline_parameters <- function(x, opts) {
  m <- opts$markup
  sym <- resolve_symbols(x, opts$symbol)
  lz <- resolve_leading_zero(x, opts$leading_zero)
  s <- opts$stats
  parts <- list(
    pd = if ("pd" %in% s) {
      apa_pd(x$pd, opts$digits_prob, markup = m, symbol = TRUE)
    },
    rope = if ("rope" %in% s) rope_string(x$rope_pct, m),
    bf = if ("bf" %in% s) {
      apa_bf(x$bf, opts$bf_direction, opts$bf, markup = m, symbol = TRUE)
    },
    p = if ("p" %in% s) {
      apa_p(x$p, opts$digits_prob, markup = m, symbol = TRUE)
    }
  )
  data.frame(
    estimate = inline_estimates(x, sym, lz, opts),
    statistic = join_columns(parts, nrow(x)),
    stringsAsFactors = FALSE
  )
}

# `12.3% in ROPE`; NA stays NA.
rope_string <- function(rope_pct, markup) {
  out <- paste0(apa_prob(rope_pct, percent = TRUE, markup = markup), " in ROPE")
  out[is.na(rope_pct)] <- NA_character_
  out
}

# ---- hypotheses ----------------------------------------------------------

inline_hypotheses <- function(x, opts) {
  m <- opts$markup
  sym <- if (rlang::is_string(opts$symbol)) {
    rep(opts$symbol, nrow(x))
  } else {
    rep(NA_character_, nrow(x))
  }
  lz <- resolve_leading_zero(x, opts$leading_zero)
  s <- opts$stats
  parts <- list(
    bf = if ("bf" %in% s) {
      apa_bf(x$bf10, opts$bf_direction, opts$bf, markup = m, symbol = TRUE)
    },
    er = if ("er" %in% s) apa_er(x$evid_ratio, markup = m, symbol = TRUE),
    post_prob = if ("post_prob" %in% s) {
      stat_string(
        paste0(markup("P", m, italic = TRUE), "(H)"),
        apa_prob(x$post_prob, opts$digits_prob, markup = m)
      )
    }
  )
  data.frame(
    estimate = inline_estimates(x, sym, lz, opts),
    statistic = join_columns(parts, nrow(x)),
    stringsAsFactors = FALSE
  )
}

# ---- diagnostics ---------------------------------------------------------

inline_diagnostics <- function(x, opts) {
  data.frame(
    estimate = rep(NA_character_, nrow(x)),
    statistic = apa_rhat_ess(
      x$rhat, x$ess_bulk, x$ess_tail,
      digits = opts$digits, markup = opts$markup
    ),
    stringsAsFactors = FALSE
  )
}

# ---- sem_fit -------------------------------------------------------------

# One sentence of fit indices per row, the parts in a fixed order and
# each only when its value is present. The seeds fix the defaults: three
# decimals without a leading zero for the bounded indices, two for the
# chi-square (SDVWM `sem_fit_row()`); miniQ's `fmt_bfit()` is the same
# row at `digits = 2, interval = FALSE`. Abbreviations are roman, as APA
# sets CFI and RMSEA and as both seeds print them.
inline_sem_fit <- function(x, opts) {
  statistic <- vapply(seq_len(nrow(x)), function(i) {
    parts <- sem_fit_parts(x, i, opts)
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
  }, character(1))
  data.frame(
    estimate = rep(NA_character_, nrow(x)),
    statistic = statistic,
    stringsAsFactors = FALSE
  )
}

sem_fit_parts <- function(x, i, opts) {
  m <- opts$markup
  auto <- identical(opts$leading_zero, "auto")
  fmt <- list(
    markup = m,
    index_digits = opts$digits %||% 3,
    chisq_digits = opts$digits %||% 2,
    index_zero = if (auto) FALSE else opts$leading_zero,
    chisq_zero = if (auto) TRUE else opts$leading_zero
  )
  # The RMSEA interval is a confidence interval at the row's own level;
  # the Bayesian ones are labelled and levelled by the table.
  bayes_method <- attr(x, "ci_method", exact = TRUE)
  bayes_level <- attr(x, "ci_level", exact = TRUE)
  value <- function(col) x[[col]][i]
  wanted <- function(stat, col = stat) {
    stat %in% opts$stats && !is.na(value(col))
  }
  parts <- list(
    if (wanted("chisq")) {
      sem_chisq_part(value("chisq"), value("df"), value("p"), fmt, opts)
    },
    if (wanted("cfi")) sem_index_part("CFI", value("cfi"), fmt),
    if (wanted("tli")) sem_index_part("TLI", value("tli"), fmt),
    if (wanted("rmsea")) {
      sem_index_part(
        "RMSEA", value("rmsea"), fmt, value("rmsea_low"),
        value("rmsea_high"), NA_character_, value("rmsea_level"), opts
      )
    },
    if (wanted("srmr")) sem_index_part("SRMR", value("srmr"), fmt),
    if (wanted("ppp")) sem_index_part("PPP", value("ppp"), fmt),
    if (wanted("brmsea")) {
      sem_index_part(
        "BRMSEA", value("brmsea"), fmt, value("brmsea_low"),
        value("brmsea_high"), bayes_method, bayes_level, opts
      )
    },
    if (wanted("bgammahat")) {
      sem_index_part(
        symbol("bgammahat", m), value("bgammahat"), fmt,
        value("bgammahat_low"), value("bgammahat_high"), bayes_method,
        bayes_level, opts
      )
    }
  )
  unlist(parts)
}

# `χ²(24) = 85.31, *p* < .001`. A whole df prints as an integer; a
# fractional one (a mean-and-variance-adjusted test) with the chi-square's
# decimals.
sem_chisq_part <- function(chisq, df, p, fmt, opts) {
  m <- fmt$markup
  df_string <- if (is.na(df)) {
    ""
  } else {
    df_digits <- if (df == round(df)) 0 else fmt$chisq_digits
    paste0("(", apa_num(df, df_digits, markup = m), ")")
  }
  out <- paste0(
    symbol("chisq", m), df_string, " = ",
    apa_num(chisq, fmt$chisq_digits, fmt$chisq_zero, markup = m)
  )
  if (!is.na(p)) {
    out <- paste0(
      out, ", ", apa_p(p, opts$digits_prob, markup = m, symbol = TRUE)
    )
  }
  out
}

# `NAME = .931`, followed by `, 90% CI [.071, .114]` when bounds are
# given and intervals are wanted. The label and the missing-level rule
# are the parameters rows' (inline_estimate_row()).
sem_index_part <- function(name, estimate, fmt, low = NA, high = NA,
                           method = NA_character_, level = NA_real_,
                           opts = NULL) {
  m <- fmt$markup
  out <- paste(
    name, "=", apa_num(estimate, fmt$index_digits, fmt$index_zero, markup = m)
  )
  if (is.null(opts) || !opts$interval || is.na(low) || is.na(high)) {
    return(out)
  }
  label <- opts$ci_label
  if (identical(label, "auto")) {
    label <- ci_label_of(method)
  }
  if (is.na(level)) {
    label <- NULL
    level <- 0.95
  }
  paste0(out, ", ", apa_ci(
    low, high,
    level = level, label = label, digits = fmt$index_digits,
    leading_zero = fmt$index_zero, markup = m
  ))
}

# ---- loo -----------------------------------------------------------------

# `ΔELPD = −0.97, *SE* = 0.35` and the opt-in parts after it. Every row
# prints the same way, the reference row's zero difference included
# (Gidon, 2026-09-14). The numbers are unbounded, so "auto" keeps the
# leading zero; the weight is a proportion and goes through apa_prob().
inline_loo <- function(x, opts) {
  m <- opts$markup
  lz <- if (identical(opts$leading_zero, "auto")) TRUE else opts$leading_zero
  num <- function(v) apa_num(v, opts$digits, lz, markup = m)
  se <- markup("SE", m, italic = TRUE)
  # A value with its standard error; an unknown SE drops that part alone.
  with_se <- function(name, value, se_value) {
    out <- stat_string(name, num(value))
    has_se <- !is.na(out) & !is.na(se_value)
    out[has_se] <- paste0(
      out[has_se], ", ", stat_string(se, num(se_value[has_se]))
    )
    out
  }
  s <- opts$stats
  parts <- list(
    elpd_diff = if ("elpd_diff" %in% s) {
      with_se(paste0(symbol("delta", m), "ELPD"), x$elpd_diff, x$se_diff)
    },
    elpd = if ("elpd" %in% s) with_se("ELPD", x$elpd, x$se_elpd),
    p_loo = if ("p_loo" %in% s) {
      stat_string(
        markup("p", m, italic = TRUE, subscript = "loo"), num(x$p_loo)
      )
    },
    looic = if ("looic" %in% s) stat_string("LOOIC", num(x$looic)),
    weight = if ("weight" %in% s) {
      stat_string(
        markup("w", m, italic = TRUE),
        apa_prob(x$weight, opts$digits_prob, markup = m)
      )
    }
  )
  data.frame(
    estimate = rep(NA_character_, nrow(x)),
    statistic = join_columns(parts, nrow(x)),
    stringsAsFactors = FALSE
  )
}

# ---- contrasts -----------------------------------------------------------

# `4.28, 95% HPD [1.38, 7.01], *pd* = .998`: a parameters row without the
# coefficient rule. A contrast has no `component`, so no symbol is
# invented for it; `symbol =` prints one. The interval label comes from
# the row's `ci_method` as everywhere (`HPD` on the emmGrid route's
# default), and the ROPE share prints when the table carries one.
inline_contrasts <- function(x, opts) {
  m <- opts$markup
  sym <- if (rlang::is_string(opts$symbol)) {
    rep(opts$symbol, nrow(x))
  } else {
    rep(NA_character_, nrow(x))
  }
  lz <- resolve_leading_zero(x, opts$leading_zero)
  s <- opts$stats
  parts <- list(
    pd = if ("pd" %in% s) {
      apa_pd(x$pd, opts$digits_prob, markup = m, symbol = TRUE)
    },
    rope = if ("rope" %in% s) rope_string(x$rope_pct, m)
  )
  data.frame(
    estimate = inline_estimates(x, sym, lz, opts),
    statistic = join_columns(parts, nrow(x)),
    stringsAsFactors = FALSE
  )
}

# ---- bf_models -----------------------------------------------------------

# `*BF*~10~ = 6.38`: the row's model over the denominator, a number and
# never a word (decision 15). `bf` is exp(log_bf), which overflows to Inf
# above ~709 and underflows to 0 below ~-745; a Bayes factor printed as
# `∞` or `0.00` would misreport a finite one, so such a row prints its
# log instead, once.
inline_bf_models <- function(x, opts) {
  m <- opts$markup
  lz <- if (identical(opts$leading_zero, "auto")) TRUE else opts$leading_zero
  direction <- opts$bf_direction
  bf_name <- markup("BF", m, italic = TRUE, subscript = direction)
  log_bf <- if (direction == "01") -x$log_bf else x$log_bf
  log_part <- stat_string(
    paste0("log(", bf_name, ")"),
    apa_num(log_bf, opts$digits, lz, markup = m)
  )
  lost <- !is.na(x$bf) & (is.infinite(x$bf) | x$bf == 0) &
    is.finite(x$log_bf)
  s <- opts$stats
  bf_part <- apa_bf(x$bf, direction, opts$bf, markup = m, symbol = TRUE)
  bf_part[lost] <- log_part[lost]
  if ("bf" %in% s) {
    log_part[lost] <- NA_character_
  }
  parts <- list(
    bf = if ("bf" %in% s) bf_part,
    log_bf = if ("log_bf" %in% s) log_part,
    post_prob = if ("post_prob" %in% s) {
      stat_string(
        paste0(markup("P", m, italic = TRUE), "(M | D)"),
        apa_prob(x$post_prob, opts$digits_prob, markup = m)
      )
    }
  )
  data.frame(
    estimate = rep(NA_character_, nrow(x)),
    statistic = join_columns(parts, nrow(x)),
    stringsAsFactors = FALSE
  )
}
