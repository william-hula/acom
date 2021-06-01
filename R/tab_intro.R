
intro_tab_div <-
  column(width = 12,
         fluidRow(
           column(align = "center", width = 12,
                  div(
                    style = "width:50%;",
                    div(
                      h5(
                        "Welcome to the computer adaptive version of the",
                        tags$a(href = "https://mrri.org/philadelphia-naming-test/",
                               "Philadelphia Naming Test.",
                               target = "_blank", style = "text-decoration: underline;cursor: pointer;")
                      )
                    )
                  )
           )
         ),br(),
         fluidRow(
           column(width = 12,
                    tabsetPanel(type="hidden", id = "glide",
                                tabPanelBody(value = "glide1",
                                         div(align = "center",
                                             div(style="display: inline-block; text-align: left;",
                                                 h5("Input participant information"), br(),
                                                 textInput("name", "Enter a Name"),
                                                 textInput("notes", "Enter any notes"),
                                                 fileInput("file1", "Upload previous results", accept = ".csv"),
                                                 div(align="center",
                                                     actionButton("glide_next1", "Next"))
                                             )
                                         )
                                ),
                                tabPanelBody(value = "glide2",
                                         div(align = "center",
                                             div(style="display: inline-block; text-align: left;",
                                                 h5("Choose test options"), br(),
                                                 ### Use this to set how many items to run.
                                                 radioButtons(inputId = "numitems",
                                                              label = "Number of items (10 is for testing)",
                                                              choices = c("10", "30", "60", "100", "175", "SEM"),
                                                              selected = "10",
                                                              inline = T),
                                                 # sets SEM precision. disabled if SEM not selected in numitems radio buttons
                                                 sliderInput("sem", "Minimum acceptable SEM",
                                                             min = 0.1,
                                                             max = 0.5,
                                                             step = 0.01,
                                                             value = 0.3),
                                                 # randomize PNT order if doing the full 175 item test?
                                                 checkboxInput("random",
                                                               "Random Order (175 only)",
                                                               value = F),
                                                 checkboxInput("sound",
                                                               "Mute sound",
                                                               value = F),
                                                 div(align = "center",
                                                     actionButton("glide_back1", "Back"),
                                                     actionButton("glide_next2", "Next")
                                                 )
                                             )
                                         )
                                ),
                                tabPanelBody(value = "glide3",
                                         div(align = "center",
                                             div(style="display: inline-block; text-align: left;",
                                                 tags$ul(
                                                   tags$li("Click Start Practice to get started"),
                                                   tags$li("Press 1 for incorrect and 2 for correct"),
                                                   tags$li("Remember to score the first complete response"),
                                                   tags$li("Press Enter to advance the screen"),
                                                 ),br(),
                                                 # start!
                                                 div(align = "center",
                                                     actionButton("glide_back2", "Back"),
                                                     actionButton("start_practice",
                                                                  "Start Practice")
                                                 )
                                             )
                                         )
                                )
                    )

           )
         )
  )



