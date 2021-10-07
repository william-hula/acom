#' results tab
#' @export
results_tab_div <- function(){
  fluidRow(
  column(width = 8,offset = 2,
         tabsetPanel(type = "pills",
                     tabPanel("Summary",br(),
                              uiOutput("results_summary"), 
                              plotOutput("plot"),
                              tags$em("Red line reflects current estimate. Shaded area reflects uncertainty in current estiate.\n The average ability for individuals with aphasia is 50, with a standard deviation of 10.")
                     ),
                     tabPanel("Data", 
                              DT::DTOutput("results_table"),
                     )
         )
      )
    )
}