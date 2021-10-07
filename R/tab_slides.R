#' slides tab
#'
#' @param values values
#' @param progbar progress bar (not used)
#' @export
slides_tab_div <- function(values){
tmp_div =  column(width = 12,
       fluidRow(
         div(textOutput("key_feedback_slides"),
             style = "position: absolute; right: 1px; color:grey;")
       ),
       fluidRow(
         column(width = 12, align = "center",
                #imageOutput("practice_image")
                tags$img(src = paste0("slides/Slide", values$n, ".jpeg"),
                           #system.file(paste0("app/www/slides/Slide", values$n, ".jpeg"), package = "PNT.CAT"),
                         style = "height:80vh;")
                
         )
       )
)
  return(tmp_div)
}
