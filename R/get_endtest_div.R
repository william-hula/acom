


#' text for end test modal
#'
#' @return html tag
#' @export
get_endtest_div <- function(){
  txt = div(
    h5("Are you sure you want to end the test?"),br(),
    p("Make sure you have pressed 1 or 2 for the current item if you would like this response to be saved. If you have inadvertantly entered a response, click cancel and then press 0 to clear it."),
    p("Note that at least 30 items must be administered to obtain reliable naming ability estimates."),
    p("Taking a break and want to download results in the mean time? You will be able to download data from the current test on the results page.")
  )
  return(txt)
}