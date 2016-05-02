<!-- README.md is generated from README.Rmd. Please edit that file -->
simulateR
=========

The `simulateR` R package includes a set of functions used to generate and test simulated data. It allows you to run simulations while easily varying parameters. One common use case would be to estimate statistical power for complex models.

Please be aware that this package is still in development, and as such, bugs may still exist, and functions and function parameters may still be subject to change.

To install the most recent stable release, use the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/simulateR@v0.1.1", build_vignettes=TRUE)
```

### Installation Issues

Networked computers can sometimes result in installation issues, as the `install_github` function sometimes has difficulty with networked directories. If this happens to you, use the `.libPaths()` function to find the path to your R libraries. That will likely give you a path starting with two backslashes, but you will need to convert that to a path starting with a drive letter (e.g., 'C:', 'D:'). From there, use the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/simulateR@v0.1.1", build_vignettes=TRUE,
    args=c('--library="N:/path/to/libraries/"'))
```

Obviously, change the path to the path where your R libraries are stored.
