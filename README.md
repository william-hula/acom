PNT-CAT (beta)
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Beta-version of computer adaptive philadelphia naming test

To run, make sure these packages are installed on your local machine:

``` r
# not run
install.packages(c("here", "shiny", "tibble", "dplyr", "tidyr", "ggplot2",
                   "shinyWidgets", "htmltools", "keys", "DT", "shinyjs", "catR",
                   "bslib", "bayestestR"))
```

Then run the following:

``` r
# not run
shiny::runGitHub("aphasia-apps/pnt")
```

Or clone the repository and run locally.

Update 5-24: Now preliminary version is on shinyapps.io:
<https://rb-cavanaugh.shinyapps.io/pnt-cat/>

Update 7-10 Bugs/issues/feature requests have been moved to
[issues](https://github.com/rbcavanaugh/pnt/issues).

-   Anyone is welcome to submit an issue with a bug/feedback/feature
    requests.

Update 8-12-21: Now can be downloaded as a package! (which will address
dependencies)

*Note: It’s likely that installing the package will prompt you to update
packages on your local machine. This may be necessary if you have much
older versions of some packages installed (e.g. the {bslib} package).
The number of packages to update is large, as the current app uses quite
a few {tidyverse} apps which have a number of dependencies. Please raise
an issue in github if there are any issues downloading. *

``` r
install.packages("remotes")
remotes::install_github("aphasia-apps/pnt")
```
