
#' Shows the user interface for the practice slides. 
#'
#' @param values values
#' @export
practice_tab_div <- function(values){
    column(width = 12,
           fluidRow(style = "min-height:36px;",
             if(values$i %in% c(3:12)){
               uiOutput("key_feedback_practice")
               }
           ),
           fluidRow(
             column(width = 12, align = "center",
                    # system.file add to filepath. 
                    tags$img(src = paste0("slides/Slide", values$i, ".jpeg"),
                      style = "height:80vh;", onclick="Mousetrap.trigger('9');"),
                    # start button, at the end of the practice slides
                    if(values$i == 13){
                      div(br(),
                          actionButton("start", "Start Assessment")
                      )
                    }
             )
           )
    )
}