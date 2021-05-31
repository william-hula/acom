results_tab_div <- fluidRow(
  column(width = 8,offset = 2,
         tabsetPanel(type = "pills",
                     tabPanel("Summary",br(),
                              uiOutput("results_summary"), 
                              plotOutput("plot")
                     ),
                     tabPanel("Data", 
                              DTOutput("results_table"),
                     )
         ), 
         tags$div(align = "center",
                  downloadButton("downloadData",
                                 "Download results"),
                  actionButton("start_over",
                               "Start Over",
                               icon = icon("undo-alt")
                  )
         )
  )
)
