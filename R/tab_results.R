#' Shows the user interface for the results page. 
#' 
#' @export
results_tab_div <- function(){
  fluidRow(
  column(width = 8,offset = 2,
         tabsetPanel(type = "pills",
                     tabPanel("Summary",br(),
                              uiOutput("results_summary"), 
                              plotOutput("plot"),
                              uiOutput("plot_caption")
                     ),
                     tabPanel("Data", 
                              DT::DTOutput("results_table"),
                     )
         )
      )
    )
}