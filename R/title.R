#' Page title
#' @export
pagetitle <- function(){
  
    if (isTRUE(getOption("shiny.testmode"))) {
     title= pagetitle = div(#
        radioButtons("keys", "for testing inputs",
                     choices = c(NA, incorrect_key_response, correct_key_response),
                     inline = T, selected = NULL),
        actionButton("enter_key", "enter")
      )
    } else {
     title =  "PNT-CAT"
    }
  return(title)
}