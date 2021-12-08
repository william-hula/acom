
#' download report button module UI
#'
#' @param id 
#'
#' @export
downloadReportUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("report_download"),
                 label = "Download Report",
                 style = "background-color:#f8f9fa; border:0px;")
}

#' Download report module
#'
#' @param id 
#' @param values 
#' @param name 
#' @param notes 
#'
#' @export
downloadReportServer <- function(id, values, name, notes) {
  
  moduleServer(id, function(input, output, session){
      output$report_download <- downloadHandler(
        
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
          withProgress(message = 'Rendering, please wait!', {
            tempReport <- system.file("report.Rmd", package = "pnt")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
              values = values,
              irt_final = values$irt_final,
              text = get_text_summary(ability = values$irt_final$ability,
                                      sem = values$irt_final$sem,
                                      last_ability = values$irt_final$last_ability,
                                      last_sem = values$irt_final$last_sem,
                                      num_previous = values$num_previous,
                                      n_items = values$i,
                                      html_p = F),
              caption = get_caption(repeat_admin = !values$new_test),
              download_time = Sys.time()
            )
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          })
        }
      )
  })
}

