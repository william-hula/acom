#' This is the user interface for the intro pages bfore testing
#'
#' It looks a little crazy, but is relatively straightforward.
#'
#'
#' @export
intro_tab_div <- function() {
         fluidRow(column(style = "padding: 1%;",
           width = 12,
           tabsetPanel(id = "intropage",type = "hidden",
             tabPanelBody(
               value = "intro1",
               column(width = 8, offset = 2,
                 div(align = "center",
                     h3("Aphasia Communication Outcome Measure")
                 ),
                 div(align = "center",
                     div(style = "display: inline-block; text-align: left;margin-top:40px;",
                         # selectInput("test", "Select a Test Version:",
                         #             choices = c("Adaptive/Full Length ACOM"=59,
                         #                         "Adaptive/Short Form ACOM" = 12,
                         #                         "Adaptive/Custom Length ACOM" = "custom"
                         #                         #,"Short ACOM" = "12"
                         #             )),
                         # shinyjs::hidden(
                           sliderInput("test", label = "Select a Test Length",
                                       min = 4, max = 59, step = 1, value = 4),
                         #),
                         uiOutput("test_text"),
                         
                         textInput("participant", "Enter Participant ID"),
                         textInput("examiner", "Enter Examiner ID"),
                     )),br(),
                 div(align = "center",
                     actionButton("next1", "Next")
                 )
               )
             ),
             tabPanelBody(value = "intro2",
                          column(width = 8, offset = 2,
                            div(style = "font-size:1.2rem;margin-top:30px;margin-bottom:30px;",
                                includeMarkdown(system.file("app/www/instructions1.md", package = "acom"))),
                            div(align = "center",
                                actionButton("back2", "Back"), actionButton("next2", "Next"))
                          )),
             tabPanelBody(
               value = "intro3",
               column(width = 8, offset = 2, align = "center",
                 tags$h3("You will rate how effectively you perform tasks using the following categories:"),
                 tags$img(src="assets/acom_scale_radio_q.png", style="width:90%;"),
                 div(align = "center",actionButton("back3", "Back"), actionButton("next3", "Next"))
               )
             ),
             tabPanelBody(
               value = "intro4",
               column(width = 8, offset = 2,
                 div(style = "font-size:1.2rem;padding:50px;", includeMarkdown(system.file("app/www/instructions3.md",
                                                                                         package = "acom"))),
                 br(), br(), br(), br(),
                 div(align = "center",actionButton("back4", "Back"), actionButton("start", "Start"))
               )
             )
           )
         ))
}
