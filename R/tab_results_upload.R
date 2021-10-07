#' results tab
#' @export
results_tab_div2 <- function(){
  fluidRow(
  column(width = 8,offset = 2,
         tabsetPanel(
                     tabPanel("Summary",br(),
                              uiOutput("results_summary2"), 
                              plotOutput("plot2"),
                              tags$em("Red line reflects current estimate. Shaded area reflects uncertainty in current estiate.\n The average ability for individuals with aphasia is 50, with a standard deviation of 10.")
                     ),
                     tabPanel("Data", 
                              DT::DTOutput("results_table2"),
                     )
         ), 
         tags$div(align = "center"#,
                  # downloadButton("downloadData",
                  #                "Download results"),
                  # actionButton("start_over",
                  #              "Start Over",
                  #              icon = icon("undo-alt")
                  # )
             )
      )
    )
}