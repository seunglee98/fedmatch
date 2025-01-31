
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fedmatch

# <!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/fedmatch)](https://CRAN.R-project.org/package=fedmatch)
<!-- badges: end -->

*Any views expressed here do not reflect those of the Federal Reserve
Board or Federal Reserve System.*

The goal of fedmatch is to match un-linked datasets. It provides a
variety of tools that allow a user to build a custom matching algorithm
for their specific application. To get started, see the [“Introduction
to fedmatch”
vignette](https://seunglee98.github.io/fedmatch/articles/Intro-to-fedmatch.html).

You can view all the vignettes, and the rest of the documentation, on
the [fedmatch website](https://seunglee98.github.io/fedmatch/).

## Features

- String cleaning tools
- Fuzzy matching with standard string distance metrics from the package
  `stringdist`
- A new fuzzy matching method which we call a *Weighted Jaccard* metric
- Numeric matching using a trained logit model
- A system to sequentially execute many different types of match
  algorithms
- A system for evaluating matches post-hoc

# Installation

You can install ‘fedmatch’ from [CRAN](https://cran.r-project.org/) with

    install.packages("fedmatch")

You can install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("seunglee98/fedmatch", build_vignettes = TRUE)
```

Or, you can download the folder from github, either by cloning it or
downloading it manually and unzipping it, then running:

``` r
devtools::install("path_to_fedmatch", build_vignettes = TRUE)
```

# Citation

This package is licensed under the terms of the MIT license. See the
LICENSE.md file for details.

If you use this package for your research, please cite the technical
paper:

Gregory J. Cohen, Jacob Dice, Melanie Friedrichs, Kamran Gupta, William
Hayes, Isabel Kitschelt, Seung Jung Lee, W. Blake Marsh, Nathan Mislang,
Maya Shaton, Martin Sicilian, Chris Webster. “The U.S. Syndicated Loan
Market: Matching Data.” Journal of Financial Research, 2021.
