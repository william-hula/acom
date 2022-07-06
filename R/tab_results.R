#' Shows the user interface for the results page. 
#' 
#' @export
results_tab_div <- function(){
  fluidRow(
  column(width = 12,offset = 0, br(),
         #tabsetPanel(type = "pills",
                     #tabPanel("Summary",br(),
                              # uiOutput("results_summary"), 
                              # plotOutput("plot"), br(),
                              # uiOutput("plot_caption")
                     #),
                     tabPanel("Data", 
                              DT::DTOutput("responses")#,
                     #)
         )
      )
    )
}