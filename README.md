
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
-   ~~validate final results in app use (bonus if automated)~~
    -   QUESTION: How does catpuccino handle ties? Testing is showing
        occasional inconsistencies in which items are being chosen, but
        its only in cases where difficulty is the same (e.g. kite:
        -0.475 and bridge: -0.475). Shouldn’t affect final ability
        estimate right?
-   ~~ensure no bias in slight differences between catR and catpuccino~~

### For short-term use

-   ~~Add slides with practice trials~~
    -   NOTE: Responses not currently logged. ok?
-   Add instructions, background info, and other important info
-   ~~click sound~~
-   Make it pretty
-   produce a results plot, norms, and any other information
-   make a “report” that is downloadable
-   ~~indicator that response has been logged~~

### Longer-term

-   randomly select one of 2-3 best next options
-   create ability to upload previous file to prevent reuse of
    previously tested words
-   Convert to an R package. I started this, but its tricky and things
    go wrong so I reverted back. right now, there’s an r-package branch
    to be worked on…
