
#' Returns the page title for the navbar
#' 
#' Reveals keys for testing inputs during tests (not permitted by keys).
#' Code for download buttons, start-over, help, and github link
#'
#' @export
pagetitle <- function(){
  
    # if (isTRUE(getOption("shiny.testmode"))) {
    #  title= pagetitle = div(#
    #     radioButtons("keys", "for testing inputs",
    #                  choices = c(NA, incorrect_key_response, correct_key_response),
    #                  inline = T, selected = NULL),
    #     actionButton("enter_key", "enter")
    #   )
    # } else {
     title = div(
       #div("PNT-CAT"),
       div(#id = "navbar-right",
           shinyjs::hidden(
             downloadReportUI(id = "download_report")
           ),
           shinyjs::hidden(
             # downloadButton("downloadData",
             #                "Download results",
             #                style = "background-color:#f8f9fa; border:0px;")
             downloadResultsUI(id = "download_results")
           ),
          shinyjs::hidden(
             downloadReportUI(id = "download_report_rescore")
         ),
           shinyjs::hidden(
             # downloadButton("rescore_downloadData",
             #                "Download results",
             #                style = "background-color:#f8f9fa; border:0px;")
             downloadResultsUI(id = "download_results_rescore")
           ),
           shinyjs::hidden(
             actionButton("start_over",
                          "Start Over",
                          icon = icon("undo-alt"),
                          style = "background-color:#f8f9fa; border:0px;")
           ),
           
             actionButton(
             inputId = "feedback",
             label = "Leave Feedback!",
             icon = icon("comment"),
             style = "background-color:#f8f9fa; border:0px;"
             )
           #)#,
           # actionButton(
           #   inputId='source',
           #   label="",#Source Code",
           #   icon = icon("github"),
           #   onclick ="window.open('https://github.com/rbcavanaugh/pnt', '_blank')",
           #   style = "background:transparent; border:none;"
           #   
           # ),
           #style = "position: absolute; right: 5px; top: 8px;")
     )
     )
   # }
  return(title)
}