
#' download results button module UI
#'
#' @param id 
#'
#' @export
downloadResultsUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("results_download"),
                 label = "Download Data",
                 style = "background-color:#f8f9fa; border:0px;")
}

#' Download results module
#'
#' @param id 
#' @param values 
#' @param in_progress 
#'
#' @export
downloadResultsServer <- function(id, values, in_progress) {
  
  moduleServer(id, function(input, output, session){
    output$results_download <- downloadHandler(
      filename = function() {
        paste(as.character(Sys.Date()),
              "pnt.csv", sep = "_"
        )
      },
      content = function(file) {
        write.csv(values, file, row.names = FALSE)
      }
    )
  })
}

