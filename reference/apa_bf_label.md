# Verbal category for a Bayes factor (opt-in)

Returns the words a published labelling scheme assigns to a Bayes
factor, for authors whose venue asks for them. This is the only function
in apabayes that turns evidence into words. No other function calls it,
and nothing attaches its output to a number unless the user does.

## Usage

``` r
apa_bf_label(x, scheme, markup = NULL)
```

## Arguments

- x:

  Numeric vector of Bayes factors as BF10; values of 0 or more, `NA` and
  `Inf` allowed.

- scheme:

  The scheme, chosen explicitly: `"jeffreys"` or `"raftery"`. There is
  no default; see Details.

- markup:

  `NULL` or one of `"md"`, `"latex"`, `"typst"`, `"plain"`. `NULL` uses
  `getOption("apabayes.markup")`, then the knitr output format, then
  `"md"`. Decides the minus sign (U+2212 outside `"plain"`) and the
  infinity symbol.

## Value

A character vector of `length(x)`; `NA` in gives `NA_character_` out.

## Details

Verbal categories are interpretation aids, not part of the statistic.
The Bayes factor is a continuous measure of relative evidence, and the
guidelines this package follows recommend reporting the number itself
with its direction made explicit (Tendeiro et al., 2024; van Doorn et
al., 2021; Heck et al., 2023). Use a label only where a venue requires
one, name the scheme in the text, and never let the label replace the
number.

The category is looked up on the evidence in favour of whichever
hypothesis the value supports (`x` for H1, `1 / x` for H0), and the
result names that hypothesis: `"moderate evidence for H~1~"`. A value of
exactly 1 gives `"no evidence for either hypothesis"`. Bounds are
inclusive on the upper side: a Bayes factor of 3 is the last value in
the lowest category, as
[`effectsize::interpret_bf()`](https://easystats.github.io/effectsize/reference/interpret_bf.html)
reads them.

Thresholds and words (BF in favour, upper bound inclusive):

- `"jeffreys"`: up to 3 anecdotal, 10 moderate, 30 strong, 100 very
  strong, above 100 extreme.

- `"raftery"`: up to 3 weak, 20 positive, 150 strong, above 150 very
  strong.

Both tables are taken from
[`effectsize::interpret_bf()`](https://easystats.github.io/effectsize/reference/interpret_bf.html)
(version 1.0.3, rules `"jeffreys1961"` and `"raftery1995"`), whose help
page cites Jeffreys (1961) and Raftery (1995). The primary sources were
not consulted when this function was written; check the thresholds
against them before relying on the attribution in a manuscript. The
scheme of Lee and Wagenmakers (2013), which uses the same five words as
the `"jeffreys"` table above, is not offered until its table has been
verified against the book.

## References

Heck, D. W., Boehm, U., Böing-Messing, F., Bürkner, P.-C., Derks, K.,
Dienes, Z., Fu, Q., Gu, X., Karimova, D., Kiers, H. A. L., Klugkist, I.,
Kuiper, R. M., Lee, M. D., Leenders, R., Leplaa, H. J., Linde, M., Ly,
A., Meijerink-Bosman, M., Moerbeek, M., ... Hoijtink, H. (2023). A
review of applications of the Bayes factor in psychological research.
*Psychological Methods, 28*(3), 558–579.
[doi:10.1037/met0000454](https://doi.org/10.1037/met0000454)

Jeffreys, H. (1961). *Theory of probability* (3rd ed.). Oxford
University Press.

Raftery, A. E. (1995). Bayesian model selection in social research.
*Sociological Methodology, 25*, 111–163.

Tendeiro, J. N., Kiers, H. A. L., Hoekstra, R., Wong, T. K., & Morey, R.
D. (2024). Diagnosing the misuse of the Bayes factor in applied
research. *Advances in Methods and Practices in Psychological Science,
7*(1).
[doi:10.1177/25152459231213371](https://doi.org/10.1177/25152459231213371)

van Doorn, J., et al. (2021). The JASP guidelines for conducting and
reporting a Bayesian analysis. *Psychonomic Bulletin & Review, 28*(3),
813–826.
[doi:10.3758/s13423-020-01798-5](https://doi.org/10.3758/s13423-020-01798-5)

## See also

[`apa_bf()`](https://www.gfrischkorn.org/apabayes/reference/apa_bf.md),
which prints the number.

## Examples

``` r
apa_bf_label(c(0.2, 1, 5, 50, Inf), scheme = "jeffreys")
#> [1] "moderate evidence for H~0~"        "no evidence for either hypothesis"
#> [3] "moderate evidence for H~1~"        "very strong evidence for H~1~"    
#> [5] "extreme evidence for H~1~"        
apa_bf_label(5, scheme = "raftery", markup = "plain")
#> [1] "positive evidence for H1"
```
