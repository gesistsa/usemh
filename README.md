
<!-- README.md is generated from README.Rmd. Please edit that file -->

# usemh

<!-- badges: start -->

<!-- badges: end -->

The goal of usemh is to …

## Installation

You can install the development version of `usemh` like so:

``` r
## Don't install this if you don't know how to do so.
```

## Example

Hopefully, it will generate all the boilerplate files for the MH
submission.

``` r
library(usemh)
use_mh()
```

By default, it will open up the file `methodshub.qmd` automatically (use
`use_mh(open = FALSE)` otherwise). Under the hood, `use_mh()` generates
all the boilerplate files, namely:

1.  `CITATION.cff` - [Citation File
    Format](https://citation-file-format.github.io/)
2.  All the files for (my)Binder integration, e.g. `postBuild`,
    `install.R`, `apt.txt`, and `runtime.txt`, `install.R`, see the
    [tutorial preparation guide on
    this](https://github.com/gesiscss/mh_tutorial?tab=readme-ov-file#binder-compatibility).
3.  Initialize a Quarto project (`_quarto.yml`) and `methodshub.qmd` to
    act as the “README” for the submission

In general, if the R package is already on CRAN, editing
`methodshub.qmd` suffices. However, if your package has additional
system dependencies, you might need to edit `apt.txt` to add the
additional ubuntu packages. Similarly, if the rendering of
`methodshub.qmd` needs further R packages, add them in `install.R`.

When submitting the R package to MH, please put `methodshub.qmd` in the
“File” field of the submission form.

## Additional information

  - For the initial `methodshub.qmd`, information is collected from
    `DESCRIPTION`. Don’t ask me / us the why question about those
    headings in `methodshub.qmd` for an existing CRAN package.
  - For `CITATION.cff`, information is collected from `inst/CITATION`
    (If available, otherwise generated from `DESCRIPTION`).
  - This package is designed to make the boilerplate files affecting
    neither the existing `README.md` nor the usual `R CMD check`. All of
    boilerplate files added are tracked in `.RBuildignore`.
  - Dig into the source code to study how to undo `usemh::use_mh()`.
