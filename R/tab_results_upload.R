#' results tab
#' @export
results_tab_div2 <- function(){
  fluidRow(
  column(width = 8,offset = 2,
         tabsetPanel(
                     tabPanel("Summary",br(),
                              uiOutput("results_summary2"), 
                              plotOutput("plot2"),
                              tags$em("Red line reflects current estimate. Shaded area reflects uncertainty in current estiate.\n The average ability for individuals with aphasia is 50, with a standard deviation of 10. The density plot represents the distribution of ability scores in the calibration sample, which includes 296 cases from the Moss Aphasia Psycholinguistic Project Database and XX cases from NIH/NIDCD Award R03DC014556 (PI: Fergadiotis)")
                     ),
                     tabPanel("Data", 
                              DT::DTOutput("results_table2"),
                     )
         )
      )
    )
}