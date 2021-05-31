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
                tags$img(src = paste0("PNT/Slide", values$n, ".jpeg")),
                # note the progress bar and next/back buttons are not in the slide image. They
                # are their  own static area below the slides. 
                fluidRow(
                  div(align = "center", style = "width: 50%;",
                      
                      if (progbar){
                        progressBar(id = "progress_bar",
                                    value = values$i, display_pct = F,
                                    size = "xs",
                                    range_value = c(1,values$test_length+1))
                      },br()
                      
                  )
                )
         )
       )
)
  return(tmp_div)
}
