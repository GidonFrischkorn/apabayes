# Format an interval

`95% CrI [0.20, 0.74]`, with the same digits and leading-zero rule as
the estimate it accompanies and a label that names the interval type.

## Usage

``` r
apa_ci(
  low,
  high,
  level = 0.95,
  label = "CrI",
  digits = 2,
  leading_zero = TRUE,
  big_mark = TRUE,
  markup = NULL
)
```

## Arguments

- low, high:

  Numeric vectors of the same length (one of them may have length 1);
  `NA` and infinite bounds allowed.

- level:

  Single number in (0, 1), printed as a percentage before the label:
  `0.95` gives `95%`, `0.9` gives `90%`.

- label:

  Interval name after the level: `"CrI"` (equal-tailed credible
  interval, the default), `"HDI"`, `"HPD"`, `"CI"`. `NULL` drops the
  prefix and returns the bracket only, for table cells whose header
  carries the level.

- digits:

  Single whole number of decimals to print (fixed, never significant
  digits).

- leading_zero:

  `FALSE` drops the zero before the decimal point (`.47`, `-.47`), the
  APA rule for statistics that cannot exceed 1 in absolute value
  (correlations, standardized paths, proportions). Values of 1 or more
  keep their digits.

- big_mark:

  `TRUE` separates groups of three digits with a comma (`1,240`), the
  APA rule for numbers of 1,000 or more.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

## Value

A character vector; `NA` in either bound gives `NA_character_` for that
element.

## Details

The label is part of the report: a reader must be able to tell an
equal-tailed interval from a highest-density interval, because the two
differ for skewed posteriors. apabayes never relabels; the caller says
which interval was computed.

## Examples

``` r
apa_ci(0.2, 0.74)
#> [1] "95% CrI [0.20, 0.74]"
apa_ci(0.2, 0.74, leading_zero = FALSE)
#> [1] "95% CrI [.20, .74]"
apa_ci(0.2, 0.74, label = "HDI", level = 0.9)
#> [1] "90% HDI [0.20, 0.74]"
apa_ci(-0.5, 0.74, label = NULL)
#> [1] "[−0.50, 0.74]"
```
