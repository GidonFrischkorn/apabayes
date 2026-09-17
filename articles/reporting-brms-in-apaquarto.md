# Reporting brms models in apaquarto

apabayes turns a fitted Bayesian model into the two things an APA
manuscript needs from it: a number in a sentence, and a table with a
note that says what its columns mean. It does not fit anything, and it
does not compute anything — every value comes from the easystats
packages or from the model’s own package. apabayes selects, arranges and
formats.

This vignette reports a stored posterior summary rather than fitting a
model, so that it builds in a second and without a compiler. Everything
shown works identically on a live fit.

## One object in the middle

[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
is the only function that knows about model classes. It takes a
`brmsfit`, a `lavaan` or `blavaan` fit, a `brms` hypothesis test, a
`loo` comparison, a Bayes factor object, posterior contrasts, a Bayesian
correlation — fourteen object families in all, listed on
[`?apa_tidy`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
— and returns one shape:

``` r

fit <- brms::brm(mpg ~ wt + am, data = mtcars)
posterior <- apa_tidy(fit)
```

``` r

posterior <- readRDS(
  system.file("extdata", "parameters.rds", package = "apabayes")
)
posterior
#> # apabayes tidy table: parameters (4 rows)
#> # median, 95% CrI (equal-tailed); source: brmsfit
#>          term       label    estimate    ci_low   ci_high ci_method ci_level
#> 1 b_Intercept (Intercept) 37.46964399 31.023175 43.834509       eti     0.95
#> 2        b_wt          wt -5.38646977 -6.953618 -3.775363       eti     0.95
#> 3        b_am          am  0.01441709 -3.378270  3.031017       eti     0.95
#> 4       sigma       sigma  3.17189827  2.507811  4.177748       eti     0.95
#>      pd rope_pct     rhat ess_bulk ess_tail bf   component group effects std  p
#> 1 1.000       NA 1.003599      517      508 NA conditional  <NA>    <NA>  NA NA
#> 2 1.000       NA 1.000076      544      585 NA conditional  <NA>    <NA>  NA NA
#> 3 0.502       NA 1.006669      599      454 NA conditional  <NA>    <NA>  NA NA
#> 4 1.000       NA 1.009196      958      683 NA       sigma  <NA>    <NA>  NA NA
```

The class is `apabayes_tidy`, a tibble with the interval method,
interval level and centrality recorded as attributes, so nothing
downstream has to guess whether a bound is a credible interval or a
highest-density one.

Everything after this point takes that object, not the model. That is
the whole architecture: one extract layer per model class, and a single
reporting layer that never sees a fit.

## A number in a sentence

[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
addresses one term by name and returns the APA string:

``` r

apa_inline(posterior, "wt")
#> *b* = −5.39, 95% CrI [−6.95, −3.78], *pd* > .999
```

In a Quarto or R Markdown document you write that inline, and the string
lands in the prose:

    Weight predicted fuel economy, `r apa_inline(posterior, "wt")`.

The object it returns follows papaja’s `apa_results` shape, so
`$estimate`, `$statistic` and `$full_result` are all there if you want a
part rather than the whole.

`ci_label` controls the interval’s label, and `NULL` prints the brackets
bare, for a sentence that lists several estimates:

``` r

apa_inline(posterior, "wt", stats = character(), ci_label = NULL)
#> *b* = −5.39 [−6.95, −3.78]
```

## The number behind the string

Some sentences need the value rather than the string: a number that goes
into arithmetic, into a comparison, or into wording you format yourself.
[`apa_value()`](https://www.gfrischkorn.org/apabayes/reference/apa_value.md)
addresses a row exactly as
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
does and returns what is in the column:

``` r

apa_value(posterior, "wt")
#> [1] -5.38647
apa_value(posterior, "wt", column = "ci_high")
#> [1] -3.775363
```

It is the alternative to a hand-rolled lookup helper, which indexes by
position and reports the wrong row the day the table changes. The column
defaults to the value the table’s type is about — `estimate` here,
`elpd_diff` on a `loo` table, `bf` on a Bayes-factor one — and nothing
is rounded or marked up, so a value that lands in prose goes through the
format layer on the way:

``` r

apa_num(apa_value(posterior, "wt") * 2)
#> [1] "−10.77"
```

## A table and its note

[`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
returns a data frame whose columns are already formatted and whose
headers are already APA.
[`apa_note()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
builds the note that defines them — and it defines whatever the table
actually has, not a fixed list:

``` r

tab <- apa_table(posterior)
names(tab)
#> [1] "Predictor" "*Mdn*"     "95% CrI"   "*pd*"
apa_note(tab)
#> [1] "*Mdn* = posterior median; CrI = equal-tailed credible interval; *pd* = probability of direction."
```

In an apaquarto document the table is printed by apa7, and the note goes
in the chunk option:


    ``` r
    apa7::apa_flextable(tab)
    ```


    ```{=html}
    <div class="tabwid tabwid_left"><style>.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-86359788{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-5b771c80{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-e16b4d3c{width:1.762in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-448214b6{width:1.075in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3a9fde05{width:2.611in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1b02c2c6{width:1.052in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-27cb9a71{width:1.762in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4071165d{width:1.075in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-72519da5{width:2.611in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-96c62bd3{width:1.052in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-012f5b0b{width:1.762in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8d52ddd5{width:1.075in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-522d2489{width:2.611in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f4fb48d3{width:1.052in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-d8bad469'><thead><tr style="overflow-wrap:break-word;"><th class="cl-e16b4d3c"><p class="cl-86359788"><span class="cl-fd7caf76">Predictor</span></p></th><th class="cl-448214b6"><p class="cl-86359788"><span class="cl-f148d26d">Mdn</span></p></th><th class="cl-3a9fde05"><p class="cl-86359788"><span class="cl-fd7caf76">95%</span><span class="cl-fd7caf76"> </span><span class="cl-fd7caf76">CrI</span></p></th><th class="cl-1b02c2c6"><p class="cl-86359788"><span class="cl-f148d26d">pd</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-27cb9a71"><p class="cl-5b771c80"><span class="cl-fd7caf76">(Intercept)</span></p></td><td class="cl-4071165d"><p class="cl-86359788"><span class="cl-fd7caf76">37.47</span></p></td><td class="cl-72519da5"><p class="cl-86359788"><span class="cl-fd7caf76">⁠[31.02,</span><span class="cl-fd7caf76"> </span><span class="cl-fd7caf76">43.83]</span></p></td><td class="cl-96c62bd3"><p class="cl-86359788"><span class="cl-fd7caf76">.999</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-27cb9a71"><p class="cl-5b771c80"><span class="cl-fd7caf76">wt</span></p></td><td class="cl-4071165d"><p class="cl-86359788"><span class="cl-fd7caf76">−5.39</span></p></td><td class="cl-72519da5"><p class="cl-86359788"><span class="cl-fd7caf76">⁠[−6.95,</span><span class="cl-fd7caf76"> </span><span class="cl-fd7caf76">−3.78]</span></p></td><td class="cl-96c62bd3"><p class="cl-86359788"><span class="cl-fd7caf76">.999</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-27cb9a71"><p class="cl-5b771c80"><span class="cl-fd7caf76">am</span></p></td><td class="cl-4071165d"><p class="cl-86359788"><span class="cl-fd7caf76"> 0.01</span></p></td><td class="cl-72519da5"><p class="cl-86359788"><span class="cl-fd7caf76">⁠[−3.38,</span><span class="cl-fd7caf76"> </span><span class="cl-fd7caf76"> 3.03]</span></p></td><td class="cl-96c62bd3"><p class="cl-86359788"><span class="cl-fd7caf76"> .502</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-012f5b0b"><p class="cl-5b771c80"><span class="cl-fd7caf76">sigma</span></p></td><td class="cl-8d52ddd5"><p class="cl-86359788"><span class="cl-fd7caf76"> 3.17</span></p></td><td class="cl-522d2489"><p class="cl-86359788"><span class="cl-fd7caf76">⁠[ 2.51,</span><span class="cl-fd7caf76"> </span><span class="cl-fd7caf76"> 4.18]</span></p></td><td class="cl-f4fb48d3"><p class="cl-86359788"><span class="cl-fd7caf76">.999</span></p></td></tr></tbody></table></div>
    ```

One thing to know before you copy that: a chunk option is evaluated
*before* its own chunk runs, so `tab` has to be built in an earlier
chunk. Building it inside the chunk that prints it will fail.

The same two calls work on all nine kinds of table apabayes produces.
Model comparison, for instance:

``` r

comparison <- readRDS(
  system.file("extdata", "loo.rds", package = "apabayes")
)
apa_table(comparison)
#> # A tibble: 3 × 4
#>   Model   `ΔELPD (*SE*)` `ELPD (*SE*)` `*p*~loo~`
#>   <chr>   <chr>          <chr>         <chr>     
#> 1 good     0.00 (0.00)   −56.51 (4.07) 0.37      
#> 2 shifted −1.83 (1.85)   −58.34 (4.48) 0.50      
#> 3 wide    −6.73 (2.49)   −63.24 (1.58) 0.06
```

[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
signs every difference against the model with the highest ELPD, so a
sentence that reports how far a baseline trails would have to flip the
sign by hand. `apa_tidy(x, reference = "baseline")` takes the
differences from that model instead, and the model loo referenced then
prints a positive ΔELPD. The standard errors that survive are the ones
loo measured: a `compare.loo` object does not carry the pointwise ELPDs
any other pair would need, so those are `NA` and the pair is compared
with
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
directly.

## Beyond one row per parameter

[`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
targets a model-parameter table: one row per parameter, one column per
statistic. A table shaped by a design factor — one row per condition,
one column per measurement occasion — is not something it produces on
its own, because the tidy contract underneath it is one row per
parameter and nothing in
[`apa_table()`](https://www.gfrischkorn.org/apabayes/reference/apa_table.md)
pivots that. Reshape its output instead.

`group_rows = TRUE` adds a `Component` column, normally used for random
effects; naming your own design factor in `component` repurposes it as
the column `pivot_wider()` spreads on. Two waves of the same two
parameters, illustrated with made-up numbers:

``` r

by_wave <- apabayes_tidy(
  data.frame(
    term = c("drift", "drift", "boundary", "boundary"),
    label = c(
      "Drift rate", "Drift rate",
      "Boundary separation", "Boundary separation"
    ),
    component = c("Wave 1", "Wave 2", "Wave 1", "Wave 2"),
    effects = "fixed",
    estimate = c(0.42, 0.51, 1.18, 1.24),
    ci_low = c(0.31, 0.39, 0.98, 1.05),
    ci_high = c(0.53, 0.63, 1.38, 1.43),
    pd = c(1, 1, 1, 1)
  ),
  type = "parameters", centrality = "median",
  ci_method = "eti", ci_level = 0.95
)
wide <- apa_table(by_wave, stats = character(), group_rows = TRUE) |>
  tidyr::pivot_wider(
    id_cols = Predictor, names_from = Component,
    values_from = 3
  )
wide
#> # A tibble: 2 × 3
#>   Predictor           `Wave 1` `Wave 2`
#>   <chr>               <chr>    <chr>   
#> 1 Drift rate          0.42     0.51    
#> 2 Boundary separation 1.18     1.24
```

``` r

apa7::apa_flextable(wide)
```

[`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
is the public constructor behind this: it takes a plain data frame with
the contract’s column names, so a design table built from a stored
summary — not only from a fresh
[`apa_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apa_tidy.md)
call — starts from the same place.

## Four output formats

An APA symbol is not one string. A combining circumflex over an R
composes in Word, HTML and Typst, and the PDF font drops it; LaTeX maths
sets it correctly in PDF and is meaningless everywhere else. So apabayes
asks knitr what is being rendered and spells its symbols to suit:

| apaquarto format  | what apabayes emits                   |
|-------------------|---------------------------------------|
| `apaquarto-pdf`   | LaTeX maths — `$\hat{R}$`, `$\chi^2$` |
| `apaquarto-docx`  | Unicode and pandoc markdown           |
| `apaquarto-html`  | Unicode and pandoc markdown           |
| `apaquarto-typst` | Unicode and pandoc markdown           |

You do not have to do anything for this; it is what
[`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
does when you do not pass `markup`. Pass `markup = "plain"` to get an
unformatted string for a console or a test.

`apaquarto-typst` needs **apaquarto 6.0.0 or newer**, where it compiles
for the first time.

## A document to start from

A complete, minimal apaquarto manuscript ships with the package:

``` r

system.file("apaquarto-example", package = "apabayes")
```

It holds `example.qmd` and a README with the two commands that render
it. Copy the directory somewhere of your own, run
`quarto add wjschne/apaquarto` beside it, and render.

## What apa_inline() does not do yet

A real manuscript’s prose does more with one estimate than apabayes
currently reaches in one call. Measured against a real manuscript
conversion rather than invented, and scoped for a future release rather
than silently absent:

- **A single statistic without the estimate** — quoting only a Bayes
  factor or a *pd* where the sentence has already named the quantity.
  [`apa_inline()`](https://www.gfrischkorn.org/apabayes/reference/apa_inline.md)
  always includes the estimate; take the value with
  `apa_value(t, term, column = "bf10")` and format it with
  [`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md)
  or
  [`apa_pd()`](https://www.gfrischkorn.org/apabayes/reference/apa_p.md).
- **`*r*` for every correlation method.** A Bayesian Spearman or Kendall
  correlation prints `*r*`, where APA sets `*r*~s~` or `*ρ*`; the
  table’s note names the method the coefficient came from.
- **No route reads a named numeric vector** — the natural shape of
  [`blavaan::blavFitIndices()`](https://blavaan.org/reference/blavFitIndices.html)
  saved on its own. Build a one-row data frame with the contract’s
  column names (`ppp`, `brmsea`, `bgammahat`, …) and pass it to
  [`apabayes_tidy()`](https://www.gfrischkorn.org/apabayes/reference/apabayes_tidy.md)
  instead.

## What apabayes will not do

Nothing apabayes prints judges a model. There is no rule that turns a
Bayes factor into “strong evidence” unless you ask for one, no threshold
on R-hat that produces a verdict, and no fit index that comes with a
pass or a fail. Those are the author’s to write, and the author’s to
defend.
