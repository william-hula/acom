PNT-CAT: A free, open-source web-app for administering the computer
adative Philadelphia Naming Test
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Beta-version of computer adaptive philadelphia naming test

There are a number of ways to try out the current version of the app.

1.  The app is now online at
    <https://aphasia-apps.shinyapps.io/PNT-CAT/>

2.  The app can be installed locally via `remotes::install_github()`

*Note: It’s likely that installing the package will prompt you to update
packages on your local machine. This may be necessary if you have much
older versions of some packages installed (e.g. the {bslib} package).
The number of packages to update is large, as the current app uses quite
a few {tidyverse} apps which have a number of dependencies. Please raise
an issue in github if there are any issues downloading.*

First, download the package:

``` r
install.packages("remotes")
remotes::install_github("aphasia-apps/pnt")
```

Then, run the app using the built in function

``` r
library(pnt)
pnt::run_app()
```

3.  via `shiny::runGitHub()`

Necessary packages must be installed first:

``` r
install.packages(c("here", "shiny", "tibble", "dplyr", "tidyr", "ggplot2",
                   "shinyWidgets", "htmltools", "keys", "DT", "shinyjs", "catR",
                   "bslib", "bayestestR"))
```

Then the app can be run straight from Github

``` r
shiny::runGitHub("aphasia-apps/pnt")
```

*Note: If you get any errors about missing a package, install it,
restart your session, and try again*

4.  Clone the repository and run locally.

<!-- -->

    git clone https://github.com/aphasia-apps/pnt.git
