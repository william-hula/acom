
#' Shows the user interface for the practice slides. 
#'
#' @param values values
#' @export
practice_tab_div <- function(values){
       div(
         
       fluidRow(class="justify-content-center",
             column(width=1,class="ipad",onclick="Mousetrap.trigger('9')"
                    ),
             column(width = 10, align = "center",
                    tags$img(src = paste0("slides/Slide", values$i, ".jpeg"),
                      style = "max-height:80vh;touch-action: manipulation;", onclick="Mousetrap.trigger('enter');"),
                    if(values$i == 13){
                      div(br(),
                          actionButton("start", "Start Assessment")
                      )
                    }
             ),
             column(width=1,class="ipad",onclick="Mousetrap.trigger('9')",
                    if(values$i %in% c(3:12)){
                      uiOutput("key_feedback_practice")
                    }
                    )
           )
  )
   
}