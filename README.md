
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fedmatch

<!-- badges: start -->

<!-- badges: end -->

The goal of fedmatch is to …

## Installation

You can install the released version of fedmatch from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fedmatch")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fedmatch)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

This package is a byproduct of an effort to merge various datasets with
information about the US corporate lending market.

See the fedmatch.pdf and examples/match\_template.R for more information
on the functionality of the package.

In order to install the package use the devtools package.

``` 
 library(devtools)
 install_github("seunglee98/fedmatch")
```

If you use this package for your research, please cite the technical
paper:

Cohen, Gregory J., Melanie Friedrichs, Kamran Gupta, William Hayes,
Seung Jung Lee, W. Blake Marsh, Nathan Mislang, Maya Shaton, and Martin
Sicilian (2018). “The U.S. Syndicated Loan Market: Matching Data,”
Finance and Economics Discussion Series 2018-085. Washington: Board of
Governors of the Federal Reserve System,
<https://doi.org/10.17016/FEDS.2018.085>.

Contacts: <melanie.r.friedrichs@gmail.com> <blake.marsh@kc.frb.org>
<seung.j.lee@frb.gov> <chris.webster@frb.gov>
