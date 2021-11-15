#' Intro doc
#' @export
intro_tab_div <- function(){
  
  column(width = 12,
         fluidRow(
           column(width = 12,
              tabsetPanel(type="hidden", id = "glide",
                # PAGE 1 #########################################################
                tabPanelBody(value = "welcome_page", # glide0
                             fluidRow(
                               column(align = "center", width = 12,
                                      div(
                                        style = "width:50%;",
                                        div(
                                          h5(
                                            "Welcome to the computer adaptive version of the",
                                            tags$a(href = "https://mrri.org/philadelphia-naming-test/",
                                                   HTML("Philadelphia&nbsp;Naming&nbsp;Test."),
                                                   target = "_blank", style = "text-decoration: underline;cursor: pointer;")
                                          )
                                        )
                                      )
                               )
                             ),br(),
                             fluidRow(
                               column(width = 10, offset = 1,
                                      includeMarkdown(
                                        system.file("app/www/intro.md",
                                                    package = "pnt")
                                      ),
                                      div(align="center",
                                          actionButton("welcome_next", "Get Started"),
                                      ))
                             )
                ),
                  tabPanelBody(value = "intro_page", # glide0
                               fluidRow(
                                 column(align = "center", width = 12,
                                        div(
                                          style = "width:50%;",
                                          div(
                                            h5(
                                              "Welcome to the computer adaptive version of the",
                                              tags$a(href = "https://mrri.org/philadelphia-naming-test/",
                                                     HTML("Philadelphia&nbsp;Naming&nbsp;Test."),
                                                     target = "_blank", style = "text-decoration: underline;cursor: pointer;")
                                            )
                                          )
                                        )
                                 )
                               ),br(),
                               fluidRow(
                                   column(width = 10, offset = 1,
                                          includeMarkdown(
                                                system.file("app/www/intro2.md",
                                                 package = "pnt")
                                   ),
                                   div(align="center",
                                       actionButton('back_intro', "Back"),
                                       actionButton("administer_test", "Administer new PNT"),
                                       actionButton("administer_retest", "Re-administer PNT"),
                                       actionButton("score_test", "Score offline test")
                                   ))
                               )
                  ),
                # PAGE 2 ############################################################################
                  tabPanelBody(value = "new_pnt_page",#glide 1
                        fluidRow(
                          column(width = 6, offset = 3, align = "center",
                                      div(style="display: inline-block; text-align: left;",
                                           h5("Administer new PNT"), br(),
                                           textInput("name", "Enter a Name"),
                                           textInput("notes", "Enter any notes"),
                                           ### Use this to set how many items to run.
                                           radioButtons(inputId = "numitems",
                                                        label = "Select PNT Test Administration",
                                                        choices = c(#"10-item PNT-CAT (testing only)" = "10",
                                                                    "30-item Computer Adaptive PNT" = "30",
                                                                    "175-item Computer Adaptive PNT" = "175_cat",
                                                                    "175-item Standard PNT" = "175_standard"), 
                                                        selected = "30",
                                                        inline = F),
                                           shinyjs::hidden(
                                             checkboxInput("eskimo",
                                                           'Exclude item "Eskimo"',
                                                           value = T)
                                           ),
                                           div(align = "center",
                                               actionButton("back_test", "Back"),
                                               actionButton("next_test", "Next")
                                           )
                                         )
                                   # )
                                 )
                        )
                           
                  ),
                  tabPanelBody(value = "retest_pnt_page",
                               fluidRow(
                                 column(width = 6, offset = 3, align = "center",
                                        # div(align = "center",
                                        div(style="display: inline-block; text-align: left;",
                                            h5("Readminister PNT"), br(),
                                            
                                            fileInput("file1", "Upload previous results", accept = ".csv"),
                                            textInput("notes_retest", "Enter any notes"),
                                            ### Use this to set how many items to run.
                                            radioButtons(inputId = "numitems_retest",
                                                         label = "Select PNT Re-Administration",
                                                         choices = c(#"10-item PNT-CAT (testing only)" = "10",
                                                                     "30-item Computer Adaptive PNT" = "30",
                                                                     "Variable length Computer Adaptive PNT" = "SEM"),
                                                         selected = "30",
                                                         inline = F),
                                            # should only be available for the 30 item
                                            shinyjs::hidden(
                                              checkboxInput("exclude_previous",
                                                            "Exclude items from the previous test?",
                                                            value = T)
                                            ),
                                            div(align = "center",
                                                actionButton("back_retest", "Back"),
                                                shinyjs::disabled(actionButton("next_retest", "Next"))
                                            )
                                        )
                                        # )
                                 )
                               )
                  ),
                tabPanelBody(value = "score_offline_page",
                   fluidRow(
                     column(width = 6, offset = 3, align = "center",
                            # div(align = "center",
                            div(style="display: inline-block; text-align: left;",
                                h3("Scoring an offline or completed test"),
                                p("To score an offline or previously completed test, download the blank spreadsheet below."),
                                p("Enter 1 for error/incorrect and 2 for correct in the response column."),
                                p("If other responses are entered or additional changes made to the spreadsheet, 
                                rescoring may not work."),
                                p("You have to upload a .csv file before you can hit ok."),
                                downloadButton("downloadEmpty", "Download Blank Spreadsheet"),
                                fileInput("file2", "Upload offline or re-scored data", accept = ".csv"),
                                shinyjs::hidden(
                                  div(id="input_file_warning", uiOutput("upload_error"))
                                ),
                                div(align = "center",
                                    actionButton("back_offline", "Back"),
                                    shinyjs::disabled(actionButton("score_uploaded_data", "OK"))
                                )
                                
                             )
                     )
                   )
                ),
                  tabPanelBody(value = "instructions_page",
                           div(align = "center",
                               div(style="display: inline-block; text-align: left;",
                                   h5("Instructions:"),
                                   tags$ul(
                                     tags$li("Click Start Practice to get started"),
                                     tags$li("Press 1 for incorrect and 2 for correct"),
                                     tags$li("A 1 or 2 will appear in the top-right of the screen to show the key entered."),
                                     tags$li("Remember to score the first complete response"),
                                     tags$li("Press Enter to advance the screen"),
                                   ),br(),
                                   # start!
                                   div(align = "center",
                                       actionButton("back_to_test_or_retest", "Back"),
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
}


