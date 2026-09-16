# The columns and notes of the hypotheses and model-comparison tables
# (slice 2 of spec-apa_table.md): `hypotheses`, `loo`, `bf_models` and
# `bf_inclusion`. Each builder returns `columns` and `note` as the
# builders in table-parts.R do. Headers are the names the inline layer
# prints (stat_names()), and a Bayes factor that cannot print as a number
# follows the inline rules (lost_bf_rows(), inclusion_bf_kinds()) but
# never aborts a table (decision S2-3): its cell is empty and the note
# says why.

# ---- shared pieces -------------------------------------------------------

# A label column prints as recorded; a missing label is an empty cell.
table_label <- function(label) {
  label[is.na(label)] <- ""
  label
}

# Cells align_chr() would mis-pad keep their text: Bayes factors, `∞` and
# `≥ 10,000` (spec M4). A missing value is an empty cell.
unaligned_cells <- function(x) {
  x[is.na(x)] <- ""
  x
}

# A probability column; apa_prob() never prints a leading zero.
prob_cells <- function(x, digits_prob) {
  align_cells(apa_prob(x, digits_prob, markup = "md"))
}

# `−1.83 (1.85)` (decision S2-1). Value and standard error are aligned
# each on its own and then pasted, because align_chr(center =) pastes its
# pattern into the cell (spec M3). A row without a standard error prints
# its value alone.
value_se_cells <- function(value, se, digits, leading_zero) {
  cells <- align_cells(table_num(value, digits, leading_zero))
  se_cells <- align_cells(table_num(se, digits, leading_zero))
  with_se <- !is.na(value) & !is.na(se)
  cells[with_se] <- paste0(cells[with_se], " (", se_cells[with_se], ")")
  cells
}

# The log Bayes factor in the direction of the Bayes factor it stands in
# for.
log_bf_cells <- function(log_bf, direction, digits, leading_zero) {
  value <- if (direction == "01") -log_bf else log_bf
  align_cells(table_num(value, digits, leading_zero))
}

log_bf_definition <- function(log_header, bf_header) {
  paste(log_header, "= natural logarithm of", bf_header)
}

lost_bf_sentence <- function() {
  "A Bayes factor too large or too small to print is given as its log."
}

# The phrase an attribute's value earns; "" for an absent, NA or unknown
# one.
attribute_phrase <- function(value, phrases) {
  if (rlang::is_string(value) && value %in% names(phrases)) {
    phrases[[value]]
  } else {
    ""
  }
}

# ---- hypotheses ----------------------------------------------------------

table_hypotheses <- function(x, opts) {
  s <- opts$stats
  name <- stat_names("md", opts$bf_direction)
  lz <- resolve_leading_zero(x, opts$leading_zero)
  columns <- list(Hypothesis = table_label(x$hypothesis))
  if (any(!is.na(x$group))) {
    columns[["Group"]] <- table_label(x$group)
  }
  estimate <- table_estimate(x, lz, opts)
  columns[[estimate$header]] <- estimate$cells
  interval <- table_interval(x, lz, opts)
  if (!is.null(interval)) {
    columns[[interval$header]] <- interval$cells
  }
  if ("bf" %in% s) {
    columns[[name[["bf"]]]] <- unaligned_cells(
      apa_bf(x$bf10, opts$bf_direction, opts$bf, markup = "md")
    )
  }
  if ("er" %in% s) {
    columns[["ER"]] <- unaligned_cells(apa_er(x$evid_ratio, markup = "md"))
  }
  if ("post_prob" %in% s) {
    columns[[name[["post_prob_h"]]]] <- prob_cells(
      x$post_prob, opts$digits_prob
    )
  }
  definitions <- c(
    estimate$definitions,
    interval$definitions,
    if ("bf" %in% s) {
      hypothesis_bf_definition(x, name[["bf"]], opts$bf_direction)
    },
    if ("er" %in% s) "ER = evidence ratio for the hypothesis",
    if ("post_prob" %in% s) {
      paste(name[["post_prob_h"]], "= posterior probability of the hypothesis")
    }
  )
  list(columns = columns, note = table_note(definitions))
}

# brms computes `bf10` in two ways (brmshypothesis decision 3): a
# Savage-Dickey ratio for a point hypothesis, the posterior odds for a
# directional one. The definition names the kinds among the rows with a
# Bayes factor, and both where the table records no kind at all.
hypothesis_bf_definition <- function(x, header, direction) {
  kinds <- x$directional[!is.na(x$bf10)]
  point <- any(kinds %in% FALSE)
  directional <- any(kinds %in% TRUE)
  inverse <- direction == "01"
  point_meaning <- function(article) {
    paste(
      "Bayes factor", if (inverse) "in favour of" else "against", article,
      "point hypothesis (Savage\u2013Dickey density ratio)"
    )
  }
  odds <- if (inverse) {
    c(
      one = "the complement over the hypothesis",
      any = "the complement over a directional hypothesis"
    )
  } else {
    c(
      one = "the hypothesis over its complement",
      any = "a directional hypothesis over its complement"
    )
  }
  meaning <- if (point && !directional) {
    point_meaning("the")
  } else if (directional && !point) {
    paste("posterior odds of", odds[["one"]])
  } else {
    paste0(point_meaning("a"), ", or posterior odds of ", odds[["any"]])
  }
  paste(header, "=", meaning)
}

# ---- loo -----------------------------------------------------------------

table_loo <- function(x, opts) {
  s <- opts$stats
  name <- stat_names("md")
  lz <- resolve_leading_zero(x, opts$leading_zero)
  columns <- list(Model = table_label(x$model))
  reference <- attr(x, "reference", exact = TRUE)
  if (!rlang::is_string(reference)) {
    reference <- "the model in the first row"
  }
  meanings <- c(
    elpd_diff = paste(
      "difference in expected log predictive density from", reference
    ),
    elpd = "expected log predictive density"
  )
  standard_errors <- list(elpd_diff = x$se_diff, elpd = x$se_elpd)
  # `*SE*` is defined once, after the first column that shows one.
  definitions <- character()
  se_defined <- FALSE
  for (stat in intersect(names(meanings), s)) {
    se <- standard_errors[[stat]]
    has_se <- any(!is.na(se))
    header <- name[[stat]]
    if (has_se) {
      header <- paste0(header, " (", name[["se"]], ")")
    }
    columns[[header]] <- value_se_cells(x[[stat]], se, opts$digits, lz)
    definitions <- c(
      definitions,
      paste(name[[stat]], "=", meanings[[stat]]),
      if (has_se && !se_defined) paste(name[["se"]], "= standard error")
    )
    se_defined <- se_defined || has_se
  }
  for (stat in intersect(c("p_loo", "looic"), s)) {
    columns[[name[[stat]]]] <- align_cells(
      table_num(x[[stat]], opts$digits, lz)
    )
  }
  if ("weight" %in% s) {
    columns[[name[["weight"]]]] <- prob_cells(x$weight, opts$digits_prob)
  }
  weight_method <- attr(x, "weight_method", exact = TRUE)
  definitions <- c(
    definitions,
    if ("p_loo" %in% s) {
      paste(name[["p_loo"]], "= effective number of parameters")
    },
    if ("looic" %in% s) "LOOIC = leave-one-out information criterion",
    if ("weight" %in% s) {
      paste0(
        name[["weight"]], " = ",
        if (rlang::is_string(weight_method)) paste0(weight_method, " "),
        "model weight"
      )
    }
  )
  list(columns = columns, note = table_note(definitions))
}

# ---- bf_models -----------------------------------------------------------

# A lost Bayes factor (lost_bf_rows()) leaves its cell empty and brings
# the log column in whether or not `stats` names it (decision S2-3).
table_bf_models <- function(x, opts) {
  s <- opts$stats
  direction <- opts$bf_direction
  name <- stat_names("md", direction)
  lz <- resolve_leading_zero(x, opts$leading_zero)
  lost <- "bf" %in% s & lost_bf_rows(x$bf, x$log_bf)
  show_log <- "log_bf" %in% s || any(lost)
  columns <- list(Model = table_label(x$model))
  if ("bf" %in% s) {
    bf <- apa_bf(x$bf, direction, opts$bf, markup = "md")
    bf[lost] <- NA_character_
    columns[[name[["bf"]]]] <- unaligned_cells(bf)
  }
  if ("error" %in% s) {
    # The unit lives in the header.
    error <- sub("%$", "", bf_error_strings(x$error, "md"))
    columns[["Error (%)"]] <- align_cells(error)
  }
  if (show_log) {
    columns[[name[["log_bf"]]]] <- log_bf_cells(
      x$log_bf, direction, opts$digits, lz
    )
  }
  if ("post_prob" %in% s) {
    columns[[name[["post_prob_m"]]]] <- prob_cells(
      x$post_prob, opts$digits_prob
    )
  }
  prior_odds <- attribute_phrase(
    attr(x, "prior_odds", exact = TRUE),
    c(equal = " under equal prior odds", custom = " under the given prior odds")
  )
  definitions <- c(
    if ("bf" %in% s) bf_models_definition(x, name[["bf"]], direction),
    if ("error" %in% s) {
      "Error (%) = proportional error of the Bayes factor estimate"
    },
    if (show_log) log_bf_definition(name[["log_bf"]], name[["bf"]]),
    if ("post_prob" %in% s) {
      paste0(
        name[["post_prob_m"]], " = posterior model probability", prior_odds
      )
    }
  )
  list(
    columns = columns,
    note = table_note(definitions, if (any(lost)) lost_bf_sentence())
  )
}

# The denominator by name, verbatim: the attribute the routes record, else
# the model of the row flagged as the denominator, else a description.
# The method follows in parentheses when the table records one.
bf_models_definition <- function(x, header, direction) {
  denominator <- attr(x, "denominator_model", exact = TRUE)
  if (!rlang::is_string(denominator)) {
    denominator <- x$model[x$denominator %in% TRUE][1]
  }
  if (!rlang::is_string(denominator)) {
    denominator <- "the denominator model"
  }
  over <- if (direction == "01") {
    paste(denominator, "over the model")
  } else {
    paste("the model over", denominator)
  }
  method <- attr(x, "bf_method", exact = TRUE)
  paste0(
    header, " = Bayes factor of ", over,
    if (rlang::is_string(method)) paste0(" (", method, ")")
  )
}

# ---- bf_inclusion --------------------------------------------------------

# A missing, infinite or lost inclusion Bayes factor leaves its cell
# empty; only a lost one has a log to show, in a column of its own that
# is empty on every other row (decision S2-3).
table_bf_inclusion <- function(x, opts) {
  s <- opts$stats
  direction <- opts$bf_direction
  name <- stat_names("md", direction)
  lz <- resolve_leading_zero(x, opts$leading_zero)
  kinds <- inclusion_bf_kinds(x$bf, x$log_bf)
  lost <- "bf" %in% s & kinds == "lost"
  columns <- list(Term = table_label(x$term))
  for (stat in intersect(c("p_prior", "p_posterior"), s)) {
    columns[[name[[stat]]]] <- prob_cells(x[[stat]], opts$digits_prob)
  }
  if ("bf" %in% s) {
    bf <- apa_bf(x$bf, direction, opts$bf, markup = "md")
    bf[kinds != "normal"] <- NA_character_
    columns[[name[["bf_incl"]]]] <- unaligned_cells(bf)
  }
  if (any(lost)) {
    # Aligned among the logs that print, not the rows left empty.
    log_cells <- rep("", nrow(x))
    log_cells[lost] <- log_bf_cells(
      x$log_bf[lost], direction, opts$digits, lz[lost]
    )
    columns[[name[["log_bf_incl"]]]] <- log_cells
  }
  averaging <- attribute_phrase(
    attr(x, "averaging", exact = TRUE),
    c(
      all = ", averaged across all models",
      matched = ", averaged across matched models"
    )
  )
  kind <- if (direction == "01") "exclusion" else "inclusion"
  definitions <- c(
    if ("p_prior" %in% s) {
      paste(name[["p_prior"]], "= prior inclusion probability")
    },
    if ("p_posterior" %in% s) {
      paste(name[["p_posterior"]], "= posterior inclusion probability")
    },
    if ("bf" %in% s) {
      paste0(name[["bf_incl"]], " = ", kind, " Bayes factor", averaging)
    },
    if (any(lost)) log_bf_definition(name[["log_bf_incl"]], name[["bf_incl"]])
  )
  sentences <- if ("bf" %in% s) inclusion_sentences(x$term, kinds, kind)
  list(columns = columns, note = table_note(definitions, sentences))
}

# Why a term's Bayes factor cell is empty, one sentence per kind, naming
# the terms. `noun` is the Bayes factor's own name, which follows
# `bf_direction` (Gidon, session 29); the posterior inclusion probability
# keeps its name either way.
inclusion_sentences <- function(terms, kinds, noun) {
  about <- function(kind, one, many) {
    named <- terms[kinds == kind]
    if (length(named) == 0) {
      return(NULL)
    }
    paste(backtick_list(named), if (length(named) == 1) one else many)
  }
  no_bf <- paste(noun, "Bayes factor; a term in every model has none.")
  infinite <- function(its) {
    paste0(
      "an infinite ", noun, " Bayes factor; ", its,
      " posterior inclusion probability rounds to 1 or 0."
    )
  }
  c(
    about(
      "missing",
      paste("has no", no_bf), paste("have no", no_bf)
    ),
    about(
      "infinite",
      paste("has", infinite("its")), paste("have", infinite("their"))
    ),
    if (any(kinds == "lost")) lost_bf_sentence()
  )
}

# `a`, `b` and `c`: terms quoted verbatim as code.
backtick_list <- function(terms) {
  quoted <- paste0("`", terms, "`")
  n <- length(quoted)
  if (n == 1) {
    return(quoted)
  }
  paste(paste(quoted[-n], collapse = ", "), "and", quoted[n])
}
