#' slides tab
#'
#' @param values values
#' @param progbar progress bar (not used)
#' @export
slides_tab_div <- function(values){
tmp_div =  column(width = 12,
       fluidRow(
         
         if (isTruthy(values$key_val == incorrect_key_response)){
           icon("check-circle", style = "position: absolute; right: 1px; color:grey;")
         } else if (isTruthy(values$key_val == correct_key_response)){
           icon("check-square", style = "position: absolute; right: 1px; color:grey;")
         } else {
           #icon("circle", style = "color: grey; position: absolute; right: 5px;")
         }
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
