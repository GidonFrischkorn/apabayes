# Report a result as an APA table

`apa_table()` turns a tidy table (or a fitted model) into the tibble
that
[`apa7::apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.html)
renders in an apaquarto manuscript: every value already formatted as
text, every header already its APA markdown, and the text of the table
note attached for `apa_note()`. It is the table counterpart of
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
and takes the same formatting options.

## Usage

``` r
apa_table(x, ...)

# S3 method for class 'apabayes_tidy'
apa_table(
  x,
  ...,
  stats = NULL,
  interval = TRUE,
  ci_label = "auto",
  digits = NULL,
  digits_prob = 3,
  leading_zero = "auto",
  bf = c("auto", "sci", "plain"),
  bf_direction = c("10", "01"),
  group_rows = FALSE
)

# Default S3 method
apa_table(
  x,
  ...,
  stats = NULL,
  interval = TRUE,
  ci_label = "auto",
  digits = NULL,
  digits_prob = 3,
  leading_zero = "auto",
  bf = c("auto", "sci", "plain"),
  bf_direction = c("10", "01"),
  group_rows = FALSE
)

apa_note(x)
```

## Arguments

- x:

  An
  [apabayes_tidy](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  table, or an object
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
  accepts. For `apa_note()`, a table `apa_table()` returned.

- ...:

  Tidy method: must be empty. Default method: passed to
  [`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md).

- stats:

  `NULL` for the default, or a character vector naming the statistic
  columns to show: `"pd"`, `"rope"`, `"bf"`, `"p"`, `"rhat"`,
  `"ess_bulk"`, `"ess_tail"` for a parameters table (the default shows
  each of the first four that has a value in some row), `"rhat"`,
  `"ess_bulk"`, `"ess_tail"` for a diagnostics table (the default shows
  each that has a value in some row); `"bf"`, `"er"`, `"post_prob"` for
  a hypotheses table (default `"bf"`); `"elpd_diff"`, `"elpd"`,
  `"p_loo"`, `"looic"`, `"weight"` for a loo table (default all but
  `"looic"`); `"bf"`, `"error"`, `"log_bf"`, `"post_prob"` for a
  bf_models table (default `"bf"` and `"error"`; `"error"` needs
  `"bf"`); `"p_prior"`, `"p_posterior"`, `"bf"` for a bf_inclusion table
  (default all three). A default shows only the statistics that have a
  value in some row. The column order is fixed, whatever the order of
  `stats`; [`character()`](https://rdrr.io/r/base/character.html) shows
  the label, and on a parameters or hypotheses table the estimate and
  the interval, alone. Naming a statistic the table has no value of is
  an error.

- interval:

  `FALSE` drops the interval column.

- ci_label:

  `"auto"` labels the interval from the rows' `ci_method` (`CrI`, `HDI`,
  `HPD`, `SPI`, `BCI`, `CI`); a string replaces the label in the header
  and in the note, where the definition still follows `ci_method`.
  `NULL` is an error: an interval column needs a header that names it.

- digits:

  Decimals for estimates, interval bounds, R-hat, ELPD and its standard
  error, `p_loo`, LOOIC and log Bayes factors; `NULL` is 2.

- digits_prob:

  Decimals for pd, p, model weights and posterior and inclusion
  probabilities. The ROPE share and the error of a Bayes factor keep
  [`apa_prob()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md)'s
  own percentage digits, as in
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md).

- leading_zero:

  `"auto"` drops the leading zero of the estimate and its bounds on
  standardized rows and keeps it elsewhere; `TRUE` or `FALSE` force one
  rule.

- bf, bf_direction:

  Passed to
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  as `style` and `direction`; the direction also sets the subscript of
  the header.

- group_rows:

  `TRUE` adds a leading `Component` column (parameters tables only) for
  `apa7::apa_flextable(row_title_column = Component)`:
  `Population-level`, `Group-level (<group>)`, the component as recorded
  (`sigma`, `Loading`), or `Other`. Rows are reordered stably so that
  each title is one run.

## Value

`apa_table()`: a tibble with one character column per shown column,
named by its markdown header, one row per row of the tidy table, and the
attributes `note` (a markdown string, or `NA` when the table shows
nothing that needs defining) and `table_type`. `apa_note()`: the note, a
single string.

## Scope

`apa_table()` targets a model-parameter table: one row per parameter (or
per hypothesis, model or pair, depending on the contract type), one
column per statistic. It has no pivot and no second estimate column, so
a table shaped by a design factor — one row per item-feature cell, one
column per measurement occasion — or one that lays two samples'
estimates side by side is outside it. Carry its tibble on into your own
reshaping
([`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
and the like) and then into
[`apa7::apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.html);
the vignette "Reporting brms models in apaquarto" has a worked example
of exactly that, `apa_table()` output pivoted into a cross-tabulated
design table.

## What is printed

A `parameters` table has one row per parameter: the label column (`Path`
when every term is a structural-equation path, else `Predictor`), the
estimate (`*Mdn*` for a posterior median, `*M*` for a posterior mean,
`Estimate` otherwise), the interval, and then, in this order, whichever
of `*pd*`, `% in ROPE`, `*BF*~10~` and `*p*` the table carries. `*R̂*`,
`ESS~bulk~` and `ESS~tail~` are added with `stats`. The interval header
reads `95% CrI`, `95% HDI` or `95% CI`, from the rows' own `ci_method`
and `ci_level`; rows that disagree on the level carry it in their cells
(`90% [0.12, 0.48]`), and rows that disagree on the method carry their
label (`HDI [0.12, 0.48]`) under the header `95% Interval` (`Interval`
when the levels differ too). A bare `CI` label, which apa7 would format
as a column of its own, moves into the cells the same way, and a
`ci_label` that apa7 would format is refused. A `diagnostics` table from
[`apa_tidy_diagnostics()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy_diagnostics.md)
has a `Term` column and `*R̂*`, `ESS~bulk~` and `ESS~tail~`.

A `hypotheses` table from
[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
has one row per hypothesis: `Hypothesis`, `Group` when some row has one,
the estimate and its interval as on a parameters table (rows tested at
different levels carry the level in their cells), and `*BF*~10~`;
`stats` adds the evidence ratio `ER` and the posterior probability
`*P*(H)`. A `loo` table from
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
has `Model`, the difference to the first model with its standard error
in the cell, `ΔELPD (*SE*)`, the model's own `ELPD (*SE*)`, `*p*~loo~`,
and `*w*` when the table was extracted with `weights =`; `LOOIC` is
added with `stats`. A `bf_models` table has `Model`, `*BF*~10~` against
the denominator model (which prints `1.00`) and, from the BayesFactor
route, the proportional error `Error (%)`; `stats` adds `log(*BF*~10~)`
and the posterior model probability `*P*(M | D)`. A `bf_inclusion` table
from
[`bayestestR::bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.html)
has `Term`, `*P*(incl)`, `*P*(incl | D)` and `*BF*~incl~` (`*BF*~excl~`
under `bf_direction = "01"`). A Bayes factor too large or too small to
print leaves its cell empty and is given in a log column instead; an
inclusion Bayes factor that is missing or infinite leaves its cell
empty. The note says which, and why.

Every number goes through the format layer
([`apa_num()`](https://www.gfrischkorn.org/apabayes/reference/apa_num.md),
[`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
[`apa_prob()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md),
[`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md),
[`apa_er()`](https://www.gfrischkorn.org/apabayes/reference/apa_er.md),
[`apa_p()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md))
and is decimal-aligned with
[`apa7::align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.html),
except Bayes factors and evidence ratios, whose markup and bounds the
alignment would count as digits. A missing value is an empty cell. The
ROPE share and the error drop their `%`, which the header carries. No
header equals a column name apa7 formats itself, so `apa_flextable()`
renders the table as it is.

The note defines the abbreviations the table shows
(`*Mdn* = posterior median; CrI = equal-tailed credible interval; *pd* = probability of direction.`),
names the reference model of a comparison and the denominator of a Bayes
factor, states that the estimates are standardized when every row is,
and on a diagnostics table counts the divergent transitions. Nothing
printed judges the result.

A `sem_fit` table has one row per model: `Model` when any row names one,
then `*χ*^2^`, `*df*`, `*p*` and the fit indices the table records, each
index whose interval it records carrying that interval in the cell
(`RMSEA [90% CI]`, `.092 [.071, .114]`). A `contrasts` table has
`Contrast`, `Group` when the contrasts were computed within one, the
estimate and its interval, `*pd*` and `% in ROPE`. A `correlations`
table names each pair in `Variable 1` and `Variable 2`, then `*r*`, its
interval, `*pd*`, `*BF*~10~` and the pairwise `*n*`, which can differ
between rows.

A frequentist interval is headed `95% CI (Wald)` or `95% CI (bootstrap)`
rather than `95% CI`, which apa7 reads as a column of its own to format;
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
still writes `95% CI` in running text.

## Rendering in apaquarto

Make the table in an earlier chunk, then name the note in the chunk
option `apa-note` of the chunk that renders it:

    ```{r}
    tab <- apa_table(fit)
    ```

    ```{r}
    #| label: tbl-fit
    #| tbl-cap: Posterior summary of the regression
    #| apa-note: !expr apa_note(tab)
    apa7::apa_flextable(tab)
    ```

The two chunks are needed because knitr evaluates an `!expr` chunk
option before it runs the chunk, so a table made in the same chunk does
not exist yet when the note is read. `apa_note()` refuses a table with
nothing to define rather than return `NULL` or `""`, because apaquarto
renders either as an empty note, `Note. {}`; remove the `apa-note`
option for such a table. `apa_flextable()` drops the attributes of the
tibble, so the note is read from the tibble, never from the flextable.

## papaja

papaja also exports a function called `apa_table()`. Whichever of the
two packages is attached last masks the other's. With both attached,
call `apabayes::apa_table()`.

## The default method

On a fitted model or any other object
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
accepts, the default method calls `apa_tidy(x, ...)` and tabulates the
result, so `...` takes that route's arguments: `standardize = TRUE` on a
lavaan or blavaan fit, `effects = "all"` on a brms fit, `ci = "hdi"` or
`rope =` on any posterior.

## See also

[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
for the running text,
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
for the tables,
[`apa7::apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.html)
for rendering.

## Examples

``` r
t <- apabayes_tidy(
  data.frame(
    term = c("b_Intercept", "b_wt", "sigma"),
    label = c("(Intercept)", "wt", "sigma"),
    estimate = c(37.3, -5.34, 2.71), ci_low = c(31.2, -6.85, 2.09),
    ci_high = c(43.1, -3.82, 3.60), pd = c(1, 0.9995, 1),
    rhat = c(1.001, 1.002, 1.004), ess_bulk = c(1520, 1633, 2401),
    component = c("conditional", "conditional", "sigma")
  ),
  type = "parameters", centrality = "median", ci_method = "eti",
  ci_level = 0.95
)
tab <- apa_table(t)
tab
#> # A tibble: 3 × 4
#>   Predictor   `*Mdn*` `95% CrI`      `*pd*`
#>   <chr>       <chr>   <chr>          <chr> 
#> 1 (Intercept) 37.30   ⁠[31.20, 43.10] > .999
#> 2 wt          −5.34   ⁠[−6.85, −3.82] > .999
#> 3 sigma        2.71   ⁠[ 2.09,  3.60] > .999
apa_note(tab)
#> [1] "*Mdn* = posterior median; CrI = equal-tailed credible interval; *pd* = probability of direction."
apa_table(t, stats = c("pd", "rhat", "ess_bulk"), group_rows = TRUE)
#> # A tibble: 3 × 7
#>   Component        Predictor   `*Mdn*` `95% CrI`      `*pd*` `*R̂*` `ESS~bulk~`
#>   <chr>            <chr>       <chr>   <chr>          <chr>  <chr> <chr>      
#> 1 Population-level (Intercept) 37.30   ⁠[31.20, 43.10] > .999 1.00  1,520      
#> 2 Population-level wt          −5.34   ⁠[−6.85, −3.82] > .999 1.00  1,633      
#> 3 sigma            sigma        2.71   ⁠[ 2.09,  3.60] > .999 1.00  2,401      

# A model comparison: the standard error travels in the cell, and the
# note names the model the differences are taken from.
cmp <- apabayes_tidy(
  data.frame(
    model = c("full", "additive", "null"),
    elpd_diff = c(0, -1.83, -24.6), se_diff = c(0, 1.85, 6.44),
    elpd = c(-56.51, -58.34, -81.11), se_elpd = c(4.07, 4.48, 6.12),
    p_loo = c(3.51, 2.50, 1.24)
  ),
  type = "loo", reference = "full"
)
apa_table(cmp)
#> # A tibble: 3 × 4
#>   Model    `ΔELPD (*SE*)` `ELPD (*SE*)` `*p*~loo~`
#>   <chr>    <chr>          <chr>         <chr>     
#> 1 full       0.00 (0.00)  −56.51 (4.07) 3.51      
#> 2 additive  −1.83 (1.85)  −58.34 (4.48) 2.50      
#> 3 null     −24.60 (6.44)  −81.11 (6.12) 1.24      
apa_note(apa_table(cmp))
#> [1] "ΔELPD = difference in expected log predictive density from full; *SE* = standard error; ELPD = expected log predictive density; *p*~loo~ = effective number of parameters."

# A Bayes factor too large to print keeps its row: the cell is empty,
# the log column comes in unasked, and the note says why.
bfm <- apabayes_tidy(
  data.frame(
    model = c("intercept only", "wt", "wt + hp"),
    bf = c(1, 4.6e7, Inf), log_bf = c(0, 17.64, 779.19),
    denominator = c(TRUE, FALSE, FALSE)
  ),
  type = "bf_models", denominator_model = "intercept only"
)
apa_table(bfm)
#> # A tibble: 3 × 3
#>   Model          `*BF*~10~`     `log(*BF*~10~)`
#>   <chr>          <chr>          <chr>          
#> 1 intercept only "1.00"           0.00         
#> 2 wt             "4.60 × 10^7^"  17.64         
#> 3 wt + hp        ""             779.19         
apa_note(apa_table(bfm))
#> [1] "*BF*~10~ = Bayes factor of the model over intercept only; log(*BF*~10~) = natural logarithm of *BF*~10~. A Bayes factor too large or too small to print is given as its log."

# Fit indices: one row per model, each index carrying its interval in
# the cell.
fits <- apabayes_tidy(
  data.frame(
    model = c("One factor", "Two factors"),
    chisq = c(85.31, 24.02), df = c(24, 19), p = c(8.5e-09, 0.196),
    cfi = c(0.931, 0.994), tli = c(0.896, 0.991),
    rmsea = c(0.092, 0.030), rmsea_low = c(0.071, 0.000),
    rmsea_high = c(0.114, 0.061), rmsea_level = c(0.90, 0.90),
    srmr = c(0.065, 0.031)
  ),
  type = "sem_fit", centrality = NA, ci_method = NA, ci_level = NA
)
apa_table(fits)
#> # A tibble: 2 × 8
#>   Model       `*χ*^2^` `*df*` `*p*`  CFI   TLI   `RMSEA ⁠[90% CI]`  SRMR 
#>   <chr>       <chr>    <chr>  <chr>  <chr> <chr> <chr>             <chr>
#> 1 One factor  85.31    24     < .001 .931  .896  .092 ⁠[.071, .114] .065 
#> 2 Two factors 24.02    19       .196 .994  .991  .030 ⁠[.000, .061] .031 
apa_note(apa_table(fits))
#> [1] "CFI = comparative fit index; TLI = Tucker–Lewis index; RMSEA = root mean square error of approximation, with its 90% confidence interval; SRMR = standardized root mean square residual."

# Correlations: each pair named in two columns, with the pairwise n.
cors <- apabayes_tidy(
  data.frame(
    term = c("mpg~~wt", "mpg~~hp"), var1 = c("mpg", "mpg"),
    var2 = c("wt", "hp"), estimate = c(-0.82, -0.70),
    ci_low = c(-0.92, -0.84), ci_high = c(-0.66, -0.51),
    pd = c(1, 1), bf = c(1.3e7, 3.9e4), n = c(32, 32),
    method = "Bayesian Pearson correlation"
  ),
  type = "correlations", centrality = "median", ci_method = "hdi",
  ci_level = 0.95
)
apa_table(cors)
#> # A tibble: 2 × 7
#>   `Variable 1` `Variable 2` `*r*` `95% HDI`    `*pd*` `*BF*~10~`   `*n*`
#>   <chr>        <chr>        <chr> <chr>        <chr>  <chr>        <chr>
#> 1 mpg          wt           −.82  ⁠[−.92, −.66] > .999 1.30 × 10^7^ 32   
#> 2 mpg          hp           −.70  ⁠[−.84, −.51] > .999 3.90 × 10^4^ 32   
apa_note(apa_table(cors))
#> [1] "*r* = Bayesian Pearson correlation; HDI = highest density interval; *pd* = probability of direction; *BF*~10~ = Bayes factor of the alternative over the null hypothesis; *n* = number of complete pairs."
hs <- lavaan::HolzingerSwineford1939
fit <- lavaan::cfa("visual =~ x1 + x2 + x3", hs)
apa_table(fit, standardize = TRUE)
#> # A tibble: 7 × 4
#>   Path             Estimate `95% CI (Wald)` `*p*`   
#>   <chr>            <chr>    <chr>           <chr>   
#> 1 visual =~ x1      .62     ⁠[ .49,  .75]    "< .001"
#> 2 visual =~ x2      .48     ⁠[ .36,  .60]    "< .001"
#> 3 visual =~ x3      .71     ⁠[ .57,  .85]    "< .001"
#> 4 x1 ~~ x1          .61     ⁠[ .45,  .78]    "< .001"
#> 5 x2 ~~ x2          .77     ⁠[ .65,  .89]    "< .001"
#> 6 x3 ~~ x3          .50     ⁠[ .30,  .70]    "< .001"
#> 7 visual ~~ visual 1.00     ⁠[1.00, 1.00]    ""      
```
