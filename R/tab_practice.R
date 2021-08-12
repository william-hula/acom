
#' practice
#'
#' @param values values
#' @export
practice_tab_div <- function(values){
    column(width = 12,
           fluidRow(
             if(values$i %in% c(3:12)){
               if (isTruthy(values$key_val == incorrect_key_response | values$key_val == correct_key_response)){
                 icon("dot-circle", style = "color: grey; position: absolute; right: 5px;")
               } else {
                 icon("circle", style = "color: grey; position: absolute; right: 5px;")
               }
             }
           ),
           fluidRow(
             column(width = 12, align = "center",
                    tags$img(src = paste0("PNT/Slide", values$i, ".jpeg"), style = "height:80vh;"),
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