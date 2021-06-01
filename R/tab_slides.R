slides_tab_div <- function(values, progbar){
tmp_div =  column(width = 12,
       fluidRow(
         
         if (isTruthy(values$key_val == incorrect_key_response | values$key_val == correct_key_response)){
           icon("dot-circle", style = "color: grey; position: absolute; right: 10px;")
         } else {
           icon("circle", style = "color: grey; position: absolute; right: 10px;")
         }
       ),
       fluidRow(
         column(width = 8, offset = 2, align = "center",
                tags$img(src = paste0("PNT/Slide", values$n, ".jpeg"))
         )
       )
)
  return(tmp_div)
}
