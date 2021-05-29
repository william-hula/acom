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
  "Computer-adaptive Philadelphia Naming Test"
}

nameinput = "Enter a Name"

otherinput = "Enter any notes"

dateinput = "Select date"

instruction1 = "Enter information below"

instruction2 =  "Press '1' for an incorrect response and '2' for a correct response. Press 'enter' to progress to the next item."

instruction3 = " for more details on administration."

instruction4 = "Any other instructions can go on this page, or in the more information modal in the footer."


inputstart = "Start Assessment"

tabtitle0 = "Home"
tabtitle_practice = "Practice"
tabtitle1 = "Assessment"
tabtitle2 = "Results"

backbutton = "Back"

nextbutton = "Next"

resultstext = "This page has the results"
