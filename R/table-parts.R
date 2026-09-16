# The columns and notes of the table layer, one builder per tidy type.
# Each takes the tidy table and the resolved options and returns a list:
# `columns`, a named list of character vectors whose names are the
# markdown headers, in column order, and `note`, a string or NA. Every
# number passes through the format layer and then `apa7::align_chr()`;
# nothing here rounds, and nothing here attaches a word to a number. The
# hypotheses and model-comparison types live in table-parts-models.R.

table_parts <- function(x, opts) {
  switch(opts$type,
    parameters = table_parameters(x, opts),
    diagnostics = table_diagnostics(x, opts),
    hypotheses = table_hypotheses(x, opts),
    loo = table_loo(x, opts),
    bf_models = table_bf_models(x, opts),
    bf_inclusion = table_bf_inclusion(x, opts),
    sem_fit = table_sem_fit(x, opts),
    contrasts = table_contrasts(x, opts),
    correlations = table_correlations(x, opts)
  )
}

# ---- shared pieces -------------------------------------------------------

# apa7::align_chr() for decimal alignment; a column without rows stays
# empty, where align_chr() would warn from `max()` of nothing.
align_cells <- function(x, ...) {
  if (length(x) == 0) {
    return(character(0))
  }
  apa7::align_chr(x, ...)
}

# apa_num() with `leading_zero` given per row (apa_num() takes one flag).
table_num <- function(x, digits, leading_zero) {
  out <- rep(NA_character_, length(x))
  out[leading_zero] <- apa_num(x[leading_zero], digits, TRUE, markup = "md")
  out[!leading_zero] <- apa_num(
    x[!leading_zero], digits, FALSE,
    markup = "md"
  )
  out
}

# The estimate column of the parameters and hypotheses types. Its header
# and definition come from the `centrality` attribute, never from the
# rows: `*Mdn*`, `*M*`, or `Estimate`, which needs no definition.
table_estimate <- function(x, leading_zero, opts) {
  centrality <- attr(x, "centrality", exact = TRUE)
  headers <- c(median = "*Mdn*", mean = "*M*")
  header <- unname(headers[centrality])
  header[is.na(header)] <- "Estimate"
  definitions <- c(
    median = "*Mdn* = posterior median", mean = "*M* = posterior mean"
  )[centrality[!is.na(centrality)]]
  list(
    header = header,
    cells = align_cells(table_num(x$estimate, opts$digits, leading_zero)),
    definitions = unname(definitions)
  )
}

# `<definitions joined by "; ">.` and any sentences after it; NA when the
# table shows nothing that needs defining, which apa_note() refuses.
table_note <- function(definitions, sentences = NULL) {
  if (length(definitions) == 0) {
    return(NA_character_)
  }
  paste(
    c(paste0(paste(definitions, collapse = "; "), "."), sentences),
    collapse = " "
  )
}

# The header label, which is the inline layer's except for the three
# labels that would read `CI` (S3-1). apa7 matches its own format names
# exactly, and matches an interval column by shape as well — anything
# ending in `<digits>% CI` — so `95% CI` is a column it re-formats
# (slice-3 measured M1); naming the method keeps the header safe and
# tells the reader which interval it is. The inline layer keeps `CI`:
# apa7 never sees running text.
table_ci_label_of <- function(method) {
  label <- ci_label_of(method)
  frequentist <- c(wald = "CI (Wald)", boot = "CI (bootstrap)")
  named <- unname(frequentist[method])
  label[!is.na(named)] <- named[!is.na(named)]
  label[is.na(method)] <- "Interval"
  label
}

# What each interval method is, for the note; the header label comes
# from table_ci_label_of().
ci_meanings <- function() {
  c(
    eti = "equal-tailed credible interval",
    hdi = "highest density interval",
    hpd = "highest posterior density interval",
    spi = "shortest probability interval",
    bci = "bias-corrected and accelerated interval",
    wald = "Wald confidence interval",
    boot = "bootstrap confidence interval"
  )
}

# The interval column: `NULL` when `interval` is FALSE or no row has both
# bounds, else its header, cells and note definitions. The bounds are
# aligned the way apa7's own interval formatter aligns them (spec
# measured 7). The header carries what the rows with bounds share: a
# level they disagree on moves into each cell (`90% [...]`), a method
# they disagree on puts each cell's label there (`HDI [...]`) under
# `<level>% Interval`, and a level no row records is left out.
#
# Every bracket opens with U+2060 WORD JOINER. apa7 writes each run of a
# cell as `\fontspec{Times New Roman} <text>`, and fontspec takes
# `\fontspec{font}[options]`, so a run whose text begins with `[` has the
# interval eaten as a font option list: silently when the brackets balance,
# and as `! Argument of \fontspec has an extra }.` when a split leaves them
# unbalanced. An invisible character before the `[` ends the scan. The
# joiner goes on the bracket rather than the cell, because a level or
# label prefix would otherwise leave the bracket leading its own run again
# (Milestone 5 amendment, measured in local/probes/probe_quarto_m5b.log).
# Notes keep their plain brackets: a note is prose, not a cell.
table_interval <- function(x, leading_zero, opts) {
  bounded <- !is.na(x$ci_low) & !is.na(x$ci_high)
  if (!opts$interval || !any(bounded)) {
    return(NULL)
  }
  lo <- align_cells(table_num(x$ci_low, opts$digits, leading_zero))
  hi <- align_cells(table_num(x$ci_high, opts$digits, leading_zero))
  cells <- paste0(
    "\u2060[", align_cells(paste0(lo, ", ", hi), center = ", "), "]"
  )
  auto <- identical(opts$ci_label, "auto")
  label <- if (auto) {
    table_ci_label_of(x$ci_method)
  } else {
    rep(opts$ci_label, nrow(x))
  }
  level <- paste0(format_level(x$ci_level), "% ")
  level[is.na(x$ci_level)] <- ""
  mixed_level <- length(unique(x$ci_level[bounded])) > 1
  mixed_method <- auto && length(unique(x$ci_method[bounded])) > 1
  first <- which(bounded)[1]
  header <- paste0(
    if (mixed_level) "" else level[first],
    if (mixed_method) "Interval" else label[first]
  )
  if (!auto) {
    check_ci_label_header(header)
  }
  prefix <- paste0(
    if (mixed_level) level else "",
    if (mixed_method) paste0(label, " ") else ""
  )
  cells <- paste0(prefix, cells)
  cells[!bounded] <- ""
  # One definition per method among the rows with bounds, under the label
  # the header or the cells show; the description follows the method
  # even when `ci_label` renames it.
  methods <- unique(x$ci_method[bounded])
  methods <- methods[methods %in% names(ci_meanings())]
  definitions <- paste(
    label[match(methods, x$ci_method)], "=", ci_meanings()[methods],
    recycle0 = TRUE
  )
  list(
    header = header,
    cells = cells,
    definitions = definitions
  )
}

# The R-hat and ESS columns, shared by the parameters and diagnostics
# types. R-hat takes `digits`; ESS prints as an integer with a thousands
# separator.
diagnostic_columns <- function(x, stats, digits) {
  columns <- list()
  if ("rhat" %in% stats) {
    columns[[symbol("rhat", "md")]] <- align_cells(
      apa_num(x$rhat, digits, markup = "md")
    )
  }
  for (stat in intersect(c("ess_bulk", "ess_tail"), stats)) {
    columns[[ess_header(stat)]] <- align_cells(
      apa_num(x[[stat]], 0, big_mark = TRUE, markup = "md")
    )
  }
  columns
}

# `ESS~bulk~`, `ESS~tail~`: a subscript, never `_`, which apa7 would read
# as a spanner (spec measured 4).
ess_header <- function(stat) {
  markup("ESS", "md", subscript = sub("^ess_", "", stat))
}

# Not "rank-normalized": the blavaan route's R-hat is blavaan's own.
diagnostic_definitions <- function(stats) {
  ess <- intersect(c("ess_bulk", "ess_tail"), stats)
  ess_definition <- if (length(ess) == 2) {
    paste(
      ess_header("ess_bulk"), "and", ess_header("ess_tail"),
      "= bulk and tail effective sample size"
    )
  } else {
    paste(
      ess_header(ess), "=", sub("^ess_", "", ess), "effective sample size",
      recycle0 = TRUE
    )
  }
  c(
    if ("rhat" %in% stats) {
      paste(symbol("rhat", "md"), "= potential scale reduction factor")
    },
    ess_definition
  )
}

# ---- parameters ----------------------------------------------------------

table_parameters <- function(x, opts) {
  rope_ci <- attr(x, "rope_ci", exact = TRUE)
  rope_range <- attr(x, "rope_range", exact = TRUE)
  columns <- list()
  if (opts$group_rows) {
    titles <- group_titles(x)
    rows <- order(match(titles, unique(titles)))
    x <- x[rows, ]
    columns[["Component"]] <- titles[rows]
  }
  lz <- resolve_leading_zero(x, opts$leading_zero)
  s <- opts$stats

  # `all()` of nothing is TRUE: a table without rows is not a path table.
  sem <- nrow(x) > 0 && all(!is.na(sem_term_parts(x$term)$op))
  label <- x$label
  label[is.na(label)] <- x$term[is.na(label)]
  label[is.na(label)] <- ""
  columns[[if (sem) "Path" else "Predictor"]] <- label

  estimate <- table_estimate(x, lz, opts)
  columns[[estimate$header]] <- estimate$cells

  interval <- table_interval(x, lz, opts)
  if (!is.null(interval)) {
    columns[[interval$header]] <- interval$cells
  }
  if ("pd" %in% s) {
    columns[["*pd*"]] <- align_cells(
      apa_pd(x$pd, opts$digits_prob, markup = "md")
    )
  }
  if ("rope" %in% s) {
    # The unit lives in the header; the digits are apa_prob()'s own.
    share <- apa_prob(x$rope_pct, percent = TRUE, markup = "md")
    columns[["% in ROPE"]] <- align_cells(sub("%$", "", share))
  }
  bf_header <- markup("BF", "md", italic = TRUE, subscript = opts$bf_direction)
  if ("bf" %in% s) {
    # Not aligned: align_chr() counts the markup of `10^n^` as digits.
    bf <- apa_bf(x$bf, opts$bf_direction, opts$bf, markup = "md")
    bf[is.na(bf)] <- ""
    columns[[bf_header]] <- bf
  }
  if ("p" %in% s) {
    columns[["*p*"]] <- align_cells(
      apa_p(x$p, opts$digits_prob, markup = "md")
    )
  }
  columns <- c(columns, diagnostic_columns(x, s, opts$digits))

  definitions <- c(
    estimate$definitions,
    interval$definitions,
    if ("pd" %in% s) "*pd* = probability of direction",
    if ("rope" %in% s) rope_definition(rope_ci, rope_range, opts$digits),
    if ("bf" %in% s) bf_definition(bf_header, opts$bf_direction),
    diagnostic_definitions(s)
  )
  standardized <- if (nrow(x) > 0 && all(x$std %in% TRUE)) {
    "Estimates are standardized."
  }
  list(
    columns = columns,
    note = table_note(unname(definitions), standardized)
  )
}

# The title of each row under `group_rows`: a random effect by its group,
# the conditional component as the population level, any other component
# as recorded, and `Other` where neither says.
group_titles <- function(x) {
  title <- x$component
  title[x$component %in% "conditional"] <- "Population-level"
  group <- paste0(" (", x$group, ")")
  group[is.na(x$group)] <- ""
  random <- x$effects %in% "random"
  title[random] <- paste0("Group-level", group[random])
  title[is.na(title)] <- "Other"
  title
}

# The percentage phrase only when the table records the interval the
# share was computed on, the range only when it records the range (the
# result-object route records neither).
rope_definition <- function(rope_ci, rope_range, digits) {
  interval <- if (is.null(rope_ci)) "" else paste0(format_level(rope_ci), "% ")
  range <- if (is.null(rope_range)) {
    ""
  } else {
    # The routes record two bounds; a hand-built table may not.
    if (!is.numeric(rope_range) || length(rope_range) != 2) {
      cli::cli_abort(
        "The {.field rope_range} attribute must hold two numbers, not
         {.val {rope_range}}.",
        call = NULL
      )
    }
    bounds <- apa_num(rope_range, digits, markup = "md")
    paste0(" [", bounds[1], ", ", bounds[2], "]")
  }
  paste0(
    "% in ROPE = percentage of the ", interval,
    "posterior interval inside the region of practical equivalence", range
  )
}

bf_definition <- function(header, direction) {
  over <- c(
    "10" = "the alternative over the null hypothesis",
    "01" = "the null over the alternative hypothesis"
  )
  paste(header, "= Bayes factor of", over[[direction]])
}

# ---- diagnostics ---------------------------------------------------------

table_diagnostics <- function(x, opts) {
  term <- x$term
  term[is.na(term)] <- ""
  columns <- c(
    list(Term = term),
    diagnostic_columns(x, opts$stats, opts$digits)
  )
  sentence <- divergence_sentence(attr(x, "divergences", exact = TRUE))
  list(
    columns = columns,
    note = table_note(diagnostic_definitions(opts$stats), sentence)
  )
}

# `0 divergent transitions.`: a count, never a verdict (decision 15). No
# sentence when the table records none.
divergence_sentence <- function(n) {
  if (is.null(n)) {
    return(NULL)
  }
  if (length(n) != 1 || !(is.numeric(n) || identical(n, NA))) {
    cli::cli_abort(
      "The {.field divergences} attribute must be one count, not
       {.val {n}}.",
      call = NULL
    )
  }
  if (is.na(n)) {
    return(NULL)
  }
  paste0(
    apa_num(n, 0, markup = "md"), " divergent transition",
    if (n == 1) "" else "s", "."
  )
}
