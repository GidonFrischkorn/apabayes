# The column and note builders of the three types slice 3 adds:
# `sem_fit`, `contrasts` and `correlations`
# (local/specs/spec-apa_table.md section "Slice 3"). A contrasts table is a
# parameters table without the coefficient rule and a correlations table
# adds the pair, the Bayes factor and the n, so both lean on the shared
# pieces in table-parts.R; sem_fit is the one type whose columns are
# statistics all the way across.

# ---- sem_fit ---------------------------------------------------------------

# An index and its interval in one cell (S3-3): the aligned value, then
# the bracket the shared interval rule builds. A row with no bounds keeps
# the value alone, a row with no value is empty, and a level the rows
# disagree on moves out of the header into each cell.
#
# A user's `ci_label` needs no check_ci_label_header() here, unlike the
# shared interval column: the label never stands alone as the header,
# which always reads `<index> [<level> <label>]`, and apa7 matches a
# format name only on a whole header or one ending in `<digits>% CI`
# (slice-3 measured M1) — neither of which a compound header can be.
# `_` in a label is still caught by check_table_headers().
index_interval <- function(value, lo, hi, level, label, digits, opts) {
  bounded <- !is.na(lo) & !is.na(hi)
  cells <- align_cells(apa_num(value, digits, FALSE, markup = "md"))
  if (!opts$interval || !any(bounded)) {
    return(list(suffix = "", cells = cells, level = NA_real_))
  }
  bounds <- align_cells(
    paste0(
      align_cells(apa_num(lo, digits, FALSE, markup = "md")), ", ",
      align_cells(apa_num(hi, digits, FALSE, markup = "md"))
    ),
    center = ", "
  )
  shared <- unique(level[bounded])
  mixed <- length(shared) > 1
  # A level no row records is left out, as on every other interval.
  prefix <- if (mixed) level_prefix(level) else ""
  brackets <- paste0(prefix, "[", bounds, "]")
  out <- paste0(cells, " ", brackets)
  out[!bounded] <- cells[!bounded]
  out[is.na(value)] <- ""
  suffix <- paste0(" [", if (mixed) "" else level_prefix(shared), label, "]")
  list(suffix = suffix, cells = out, level = if (mixed) NA_real_ else shared)
}

# `90% `, and nothing at all for a level the table does not record.
level_prefix <- function(level) {
  out <- paste0(format_level(level), "% ")
  out[is.na(level)] <- ""
  out
}

sem_fit_headers <- function() {
  c(
    cfi = "CFI", tli = "TLI", rmsea = "RMSEA", srmr = "SRMR", ppp = "PPP",
    brmsea = "BRMSEA", bgammahat = "B\u0393\u0302"
  )
}

sem_fit_meanings <- function() {
  c(
    cfi = "comparative fit index",
    tli = "Tucker\u2013Lewis index",
    rmsea = "root mean square error of approximation",
    srmr = "standardized root mean square residual",
    ppp = "posterior predictive *p*-value",
    brmsea = "Bayesian root mean square error of approximation",
    bgammahat = "Bayesian gamma-hat"
  )
}

# The interval a bracketed index carries: which label heads it, which
# level it is at, and what the note calls it. The RMSEA interval is a
# confidence interval whatever the table's `ci_method` (the inline rule)
# and the row records none for it (slice-3 measured M6), so the note
# says only "confidence interval": lavaan's is a noncentral chi-square
# interval, not the Wald interval `ci_meanings()` would name. The
# Bayesian indices take the table's own method.
sem_fit_interval_of <- function(x, stat, opts) {
  method <- attr(x, "ci_method", exact = TRUE)
  if (!rlang::is_string(method)) {
    method <- NA_character_
  }
  rmsea <- stat == "rmsea"
  list(
    label = if (!identical(opts$ci_label, "auto")) {
      opts$ci_label
    } else if (rmsea) {
      "CI"
    } else {
      table_ci_label_of(method)
    },
    level = if (rmsea) {
      x$rmsea_level
    } else {
      rep(attr(x, "ci_level", exact = TRUE) %||% NA_real_, nrow(x))
    },
    meaning = if (rmsea) {
      "confidence interval"
    } else {
      unname(ci_meanings()[method])
    }
  )
}

# One index column: its header, cells and note definition. Three of the
# indices carry an interval; the others are a plain number.
sem_fit_column <- function(x, stat, opts) {
  digits <- opts$digits_given %||% 3
  header <- unname(sem_fit_headers()[stat])
  meaning <- sem_fit_meanings()[[stat]]
  bounds <- sem_fit_bounds(stat)
  if (is.null(bounds)) {
    return(list(
      header = header,
      cells = align_cells(
        table_num(x[[stat]], digits, sem_fit_leading_zero(x, opts, stat))
      ),
      definition = paste(header, "=", meaning)
    ))
  }
  interval <- sem_fit_interval_of(x, stat, opts)
  part <- index_interval(
    x[[stat]], x[[bounds[1]]], x[[bounds[2]]],
    interval$level, interval$label, digits, opts
  )
  if (nzchar(part$suffix)) {
    meaning <- paste0(
      meaning, sem_fit_interval_phrase(part$level, interval$meaning)
    )
  }
  list(
    header = paste0(header, part$suffix),
    cells = part$cells,
    definition = paste(header, "=", meaning)
  )
}

# `, with its 90% confidence interval`, dropped when the table does not
# say which interval it is.
sem_fit_interval_phrase <- function(level, meaning) {
  if (length(meaning) == 0 || is.na(meaning)) {
    return("")
  }
  level_part <- if (is.na(level)) "" else paste0(format_level(level), "% ")
  paste0(", with its ", level_part, meaning)
}

sem_fit_bounds <- function(stat) {
  switch(stat,
    rmsea = c("rmsea_low", "rmsea_high"),
    brmsea = c("brmsea_low", "brmsea_high"),
    bgammahat = c("bgammahat_low", "bgammahat_high"),
    NULL
  )
}

# "auto" drops the zero on the indices, which are bounded, and keeps it
# on chi-square and df, which are not (the inline rule).
sem_fit_leading_zero <- function(x, opts, stat) {
  auto <- identical(opts$leading_zero, "auto")
  keep <- if (auto) stat %in% c("chisq", "df") else opts$leading_zero
  rep(keep, nrow(x))
}

table_sem_fit <- function(x, opts) {
  s <- opts$stats
  digits_chisq <- opts$digits_given %||% 2
  columns <- list()
  if (any(!is.na(x$model))) {
    columns[["Model"]] <- table_label(x$model)
  } else if (length(s) == 0) {
    cli::cli_abort(
      c(
        "This table has no column to show.",
        i = "{.arg stats} is empty and no row records a {.field model}."
      ),
      call = NULL
    )
  }
  definitions <- character()
  if ("chisq" %in% s) {
    columns[["*\u03c7*^2^"]] <- align_cells(
      table_num(x$chisq, digits_chisq, sem_fit_leading_zero(x, opts, "chisq"))
    )
    if (any(!is.na(x$df))) {
      # A mean-and-variance-adjusted test has fractional df.
      whole <- all(x$df[!is.na(x$df)] %% 1 == 0)
      columns[["*df*"]] <- align_cells(
        table_num(
          x$df, if (whole) 0 else digits_chisq,
          sem_fit_leading_zero(x, opts, "df")
        )
      )
    }
    if (any(!is.na(x$p))) {
      columns[["*p*"]] <- align_cells(
        apa_p(x$p, opts$digits_prob, markup = "md")
      )
    }
  }
  for (stat in setdiff(s, "chisq")) {
    column <- sem_fit_column(x, stat, opts)
    columns[[column$header]] <- column$cells
    definitions <- c(definitions, column$definition)
  }
  list(columns = columns, note = table_note(unname(definitions)))
}

# ---- contrasts -------------------------------------------------------------

# A parameters row without the coefficient rule: no symbol is invented
# for a contrast, and it carries no `std` column, so no standardization
# sentence follows the definitions.
table_contrasts <- function(x, opts) {
  s <- opts$stats
  lz <- resolve_leading_zero(x, opts$leading_zero)
  columns <- list(Contrast = table_label(x$contrast))
  if (any(!is.na(x$group))) {
    columns[["Group"]] <- table_label(x$group)
  }
  estimate <- table_estimate(x, lz, opts)
  columns[[estimate$header]] <- estimate$cells
  interval <- table_interval(x, lz, opts)
  if (!is.null(interval)) {
    columns[[interval$header]] <- interval$cells
  }
  shares <- share_columns(x, s, opts)
  columns <- c(columns, shares$columns)
  list(
    columns = columns,
    note = table_note(unname(c(
      estimate$definitions, interval$definitions, shares$definitions
    )))
  )
}

# The `*pd*` and `% in ROPE` columns, shared by the contrasts and
# correlations types with the parameters one.
share_columns <- function(x, s, opts) {
  columns <- list()
  definitions <- character()
  if ("pd" %in% s) {
    columns[["*pd*"]] <- align_cells(
      apa_pd(x$pd, opts$digits_prob, markup = "md")
    )
    definitions <- c(definitions, "*pd* = probability of direction")
  }
  if ("rope" %in% s) {
    share <- apa_prob(x$rope_pct, percent = TRUE, markup = "md")
    columns[["% in ROPE"]] <- align_cells(sub("%$", "", share))
    definitions <- c(definitions, rope_definition(
      attr(x, "rope_ci", exact = TRUE),
      attr(x, "rope_range", exact = TRUE),
      opts$digits
    ))
  }
  list(columns = columns, definitions = definitions)
}

# ---- correlations ----------------------------------------------------------

# The pair in two columns (S3-4): `term` records it as `var1~~var2`,
# which is lavaan syntax, and the contract keeps both names anyway.
table_correlations <- function(x, opts) {
  s <- opts$stats
  # A correlation is bounded, so "auto" drops the leading zero on the
  # coefficient and its bounds (decision 8), as the inline layer does.
  lz <- if (identical(opts$leading_zero, "auto")) {
    rep(FALSE, nrow(x))
  } else {
    rep(opts$leading_zero, nrow(x))
  }
  columns <- list(
    `Variable 1` = table_label(x$var1),
    `Variable 2` = table_label(x$var2)
  )
  if (any(!is.na(x$group))) {
    columns[["Group"]] <- table_label(x$group)
  }
  # `*r*` whatever the method, following the inline symbol; the note says
  # which coefficient it is.
  columns[["*r*"]] <- align_cells(table_num(x$estimate, opts$digits, lz))
  interval <- table_interval(x, lz, opts)
  if (!is.null(interval)) {
    columns[[interval$header]] <- interval$cells
  }
  shares <- share_columns(x, s, opts)
  columns <- c(columns, shares$columns)
  bf_header <- markup("BF", "md", italic = TRUE, subscript = opts$bf_direction)
  if ("bf" %in% s) {
    bf <- apa_bf(x$bf, opts$bf_direction, opts$bf, markup = "md")
    bf[is.na(bf)] <- ""
    columns[[bf_header]] <- bf
  }
  if ("n" %in% s) {
    columns[["*n*"]] <- align_cells(
      apa_num(x$n, 0, big_mark = TRUE, markup = "md")
    )
  }
  list(
    columns = columns,
    note = table_note(unname(c(
      correlation_definition(x),
      interval$definitions,
      shares$definitions,
      if ("bf" %in% s) bf_definition(bf_header, opts$bf_direction),
      if ("n" %in% s) "*n* = number of complete pairs"
    )))
  )
}

# `*r* = Bayesian Pearson correlation`, the method column verbatim; two
# methods in one table join with ` or `, and a table that records none
# defines nothing.
correlation_definition <- function(x) {
  # `method` is not a contract column: the correlation route adds it, a
  # hand-built table need not, and then nothing is defined.
  if (!"method" %in% names(x)) {
    return(NULL)
  }
  methods <- unique(x$method[!is.na(x$method)])
  if (length(methods) == 0) {
    return(NULL)
  }
  paste("*r* =", paste(methods, collapse = " or "))
}
