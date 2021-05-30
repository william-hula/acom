### Langauge data
# If testing, insert ability for test to simulate key presses...
pagetitle = if (isTRUE(getOption("shiny.testmode"))) {
  pagetitle = div(#shinyjs::hidden(
    radioButtons("keys", "for testing inputs",
                 choices = c(NA, incorrect_key_response, correct_key_response),
                 inline = T, selected = NULL),
    actionButton("enter_key", "enter")
  )#)
} else {
  "CAT-PNT"
}


tabtitle0 = "Home"
tabtitle_practice = "Practice"
tabtitle1 = "Assessment"
tabtitle2 = "Results"

backbutton = "Back"
nextbutton = "Next"
resultstext = "This page has the results"
