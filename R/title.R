
#' Returns the page title for the navbar
#' 
#' Reveals keys for testing inputs during tests (not permitted by keys).
#' Code for download buttons, start-over, help, and github link
#'
#' @export
pagetitle <- function(){
  
     title = div(
       #div("PNT-CAT"),
       div(#id = "navbar-right",
           shinyjs::hidden(
             downloadReportUI(id = "download_report")
           ),
           shinyjs::hidden(
             downloadResultsUI(id = "download_results")
           ),
         #  shinyjs::hidden(
         #     downloadReportUI(id = "download_report_rescore")
         # ),
           # shinyjs::hidden(
           #   downloadResultsUI(id = "download_results_rescore")
           # ),
         # shinyjs::hidden(
         #   actionButton("end_test", "End Test",
         #                style = "background-color:#f8f9fa; border:0px;")
         # ),
           shinyjs::hidden(
             actionButton("start_over",
                          "Start Over",
                          style = "background-color:#f8f9fa; border:0px;")
           )
     )
     )
  return(title)
}