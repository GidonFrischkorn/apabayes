# apabayes in an apaquarto document

`example.qmd` is a minimal apaquarto manuscript that uses apabayes for one
sentence and two tables. It ships with no fitted models: both objects come
from `inst/extdata`, so it renders without brms, blavaan or a compiler.

## Rendering it

The document needs the apaquarto Quarto extension, which is not part of
this package. Copy `example.qmd` somewhere of your own and run:

```sh
quarto add wjschne/apaquarto
quarto render example.qmd
```

`quarto add` writes an `_extensions/` directory next to the document.

## Versions

Rendering was verified on Quarto 1.10.18 with apaquarto 6.0.0.

`apaquarto-typst` needs **apaquarto 6.0.0 or newer**. On the 5.x line it
does not compile at all — apaquarto writes a pandoc-escaped underscore into
a Typst path and Typst rejects it. That is an apaquarto matter and nothing
apabayes does is involved; the other three formats are unaffected.

## If every chunk fails with a graphics device error

knitr's default device is svg, which needs cairo or X11. `example.qmd`
sets

```yaml
knitr:
  opts_chunk:
    dev: png
```

for that reason. Keep it, or use an R built with cairo support.
