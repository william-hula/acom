#' User interface for the assessment tab
#' 
#' Shows PNT images during testing (not practice). Also
#' shows the 1 and 2 in the top right of the screen. 
#'
#' @param values values
#' @param progbar progress bar (not used)
#' @export
slides_tab_div <- function(values){
tmp_div =  column(width = 12,
       fluidRow(style = "min-height:36px;",
         uiOutput("key_feedback_slides")
       ),
       fluidRow(
         column(width = 12, align = "center",
                #imageOutput("practice_image")
                tags$img(src = paste0("slides/Slide", values$n, ".jpeg"),
                         style = "height:80vh;")
                
         )
       )
)
  return(tmp_div)
}
