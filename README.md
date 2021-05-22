
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pnt

<!-- badges: start -->
<!-- badges: end -->

Barebones structure for implementing a CAT naming test using shiny.

To run, make sure these packages are installed on your local machine:

``` r
# not run
install.packages(c("here", "shiny", "tibble", "dplyr", "tidyr",
                   "shinyWidgets", "keys", "DT", "shinyjs", "catR"))
```

Then run the following:

``` r
# not run
shiny::runGitHub("rbcavanaugh/pnt")
```

Or clone the repository and run locally.

## To do list

### Validation

-   Check if app is choosing the same items as catpuccino
-   validate final results in app use (bonus if automated)
-   ensure no bias in slight differences between catR and catpuccino

### For short-term use

-   Add slides with practice trials
-   Add instructions, background info, and other important info
-   Make it pretty
-   produce a results plot, norms, and any other information
-   make a “report” that is downloadable

### Longer-term

-   randomly select one of 2-3 best next options
-   create ability to upload previous file to prevent reuse of
    previously tested words
