#' User interface for the assessment tab
#' 
#' Shows PNT images during testing (not practice). Also
#' shows the 1 and 2 in the top right of the screen. 
#'
#' @param values values
#' @param progbar progress bar (not used)
#' @export
slides_tab_div <- function(values){
tmp_div =  div(
       fluidRow(class="justify-content-center",
         column(width=1,class="ipad",onclick="Mousetrap.trigger('9')",
                uiOutput("item_number_slides")
         ),
         column(width = 10, align = "center",
                
                tags$img(src = paste0("slides/Slide", values$n, ".jpeg"),
                         style = "max-height:80vh;touch-action: manipulation;", onclick="Mousetrap.trigger('enter');")
         ),
         column(width=1,class="ipad",onclick="Mousetrap.trigger('9')"
                ,uiOutput("key_feedback_slides")
                )
       )
  )
  return(tmp_div)
}
