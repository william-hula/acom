
#' practice
#'
#' @param values values
#' @export
practice_tab_div <- function(values){
    column(width = 12,
           fluidRow(
             if(values$i %in% c(3:12)){
              div(textOutput("key_feedback_practice"),
                  style = "position: absolute; right: 1px; color:grey;")
             }
           ),
           fluidRow(
             column(width = 12, align = "center",
                    # system.file add to filepath. 
                    tags$img(src = paste0("slides/Slide", values$i, ".jpeg"),
                      style = "height:80vh;"),
                    # start button, at the end of the practice slides
                    if(values$i == 13){
                      div(br(),
                          actionButton("start", "Start Assessment")
                      )
                    }
             ),
             column(width = 2)
           )
    )
}