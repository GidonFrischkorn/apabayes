# cran-comments

## Status of this file

**Not submitted yet.** 0.1.0 goes out as a tagged GitHub release first;
CRAN comes after a round of use. This file records the check state a
submission would rest on, and is rewritten on the day one is made.

## Test environments

* local: macOS 26.6.2, aarch64-apple-darwin23, R 4.6.1 (2026-06-24)
* GitHub Actions, `.github/workflows/R-CMD-check.yaml`: macOS, Windows and
  Ubuntu at R release. **Written but never executed** — there is no remote
  yet, so nothing in this package has been checked on a second machine.
  An R-devel and an oldrel-1 job are still missing from that matrix and
  belong there before a submission.

## R CMD check results

`devtools::check(cran = TRUE, incoming = TRUE, remote = TRUE)`,
2026-09-17: **0 errors | 0 warnings | 1 note**.

The note is CRAN incoming feasibility:

```text
Maintainer: 'Gidon T. Frischkorn <gfrischkorn@icloud.com>'

New submission

Found the following (possibly) invalid URLs:
  URL: https://github.com/GidonFrischkorn/apabayes
  URL: https://github.com/GidonFrischkorn/apabayes/issues
  URL: https://www.gfrischkorn.org/apabayes/
    Status: 404
```

* **New submission** — correct, this is the first release.
* **All three 404s are one fact:** the repository is not public yet, so
  neither it, nor its issue tracker, nor the pkgdown site it will serve
  exists. All three resolve on the day it does, and a submission waits
  for that. Nothing else in the note.

Two further items in that note were real and are fixed rather than
explained: the `Title` field now quotes `'apaquarto'`, which is the
package's own convention in `Description` and makes
`tools::toTitleCase()` idempotent; and the documentation URL is
`https://www.gfrischkorn.org/apabayes/`, the address the maintainer's
GitHub Pages custom domain actually serves, rather than the `github.io`
one, which 301-redirects to it.

## What the local check covers, and what it does not

* Tests run with `NOT_CRAN=true`, so the live `brms`, `rstan`, `lavaan`
  and `blavaan` fits are fitted rather than skipped: 3462 passing, 0
  failures, 0 warnings, **2 skipped** — the two render tests below, which
  need `APABAYES_RENDER_TEST` — in 209 s (2026-09-17).
* Coverage is 100 % of every file in `R/`, measured with `NOT_CRAN=true`.
* `tests/testthat/test-render.R` renders the package's output to all four
  apaquarto formats and reads it back. It is **opt-in behind
  `APABAYES_RENDER_TEST` and therefore skipped on CRAN**, because it needs
  Quarto, the apaquarto extension, a LaTeX toolchain and `pdftotext`. It
  skips with its reason wherever those are absent. Run separately on
  2026-09-17 against this tree: 76 expectations, 0 skips, 0 failures.
* Every heavy dependency is in `Suggests` and every test that needs one is
  guarded by `skip_if_not_installed()`, so the suite runs on a machine with
  only the `Imports` installed.

## Downstream dependencies

None — this is a new package.
