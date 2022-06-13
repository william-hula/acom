#' Shows the user interface for the results page. 
#' 
#' @export
results_tab_div <- function(){
  fluidRow(
  column(width = 8,offset = 2, br(),
         tabsetPanel(type = "pills",
                     tabPanel("Summary",br(),
                              uiOutput("results_summary"), 
                              plotOutput("plot"), br(),
                              uiOutput("plot_caption")
                     ),
                     tabPanel("Data", 
                              DT::DTOutput("results_table"),
                     )
         )
      )
    )
}