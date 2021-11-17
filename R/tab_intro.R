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
                                          h4(
                                            "Welcome to the computer adaptive version of the Philadelphia Naming Test"#,
                                            # tags$a(href = "https://mrri.org/philadelphia-naming-test/",
                                            #        HTML("Philadelphia&nbsp;Naming&nbsp;Test."),
                                            #        target = "_blank", style = "text-decoration: underline;cursor: pointer;")
                                          )
                                        )
                                      )
                               )
                             ),br(),
                             fluidRow(
                               column(width = 10, offset = 1,
                                      includeMarkdown(
                                        system.file("app/www/new_intro.md",
                                                    package = "pnt")
                                      ), br(),
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
                                            h4(
                                              "Administration Options:"#,
                                              # tags$a(href = "https://mrri.org/philadelphia-naming-test/",
                                              #        HTML("Philadelphia&nbsp;Naming&nbsp;Test."),
                                              #        target = "_blank", style = "text-decoration: underline;cursor: pointer;")
                                            )
                                          )
                                        )
                                 )
                               ),br(),
                               fluidRow(
                                   column(width = 10, offset = 1,
                                          includeMarkdown(
                                                system.file("app/www/new_intro2.md",
                                                 package = "pnt")
                                   ),br(),
                                   div(align="center",
                                       actionButton('back_intro', "Back"),
                                       actionButton("administer_test", "Administer new PNT"),
                                       actionButton("administer_retest", "Re-administer PNT"),
                                       actionButton("score_test", "Rescore PNT / Score offline test")
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
                                                        choices = c(
                                                                    "30-item Computer Adaptive PNT" = "30_cat",
                                                                    "175-item Computer Adaptive PNT" = "175_cat",
                                                                    "30-item PNT Short form (Walker)" = "30_walker",
                                                                    "175-item Standard PNT" = "175_standard"), 
                                                        selected = "30_cat",
                                                        inline = F),
                                           shinyjs::hidden(
                                             checkboxInput("eskimo",
                                                           'Exclude item "Eskimo"',
                                                           value = T)
                                           ),
                                          shinyjs::hidden(
                                            radioButtons("walker",
                                                         "Choose 30-item short form",
                                                         choices = c("A", "B"),
                                                         selected = "A"
                                            )
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
                                                         choices = c("30-item Computer Adaptive PNT" = "30_cat",
                                                                     "30-item PNT Short form (Walker)" = "30_walker",
                                                                     "Variable length Computer Adaptive PNT" = "SEM"),
                                                         selected = "30_cat",
                                                         inline = F),
                                            # should only be available for the 30 item
                                            shinyjs::hidden(
                                              checkboxInput("exclude_previous",
                                                            "Exclude items from the previous test?",
                                                            value = T)
                                            ),
                                            shinyjs::hidden(
                                              radioButtons("walker_retest",
                                                           "Choose 30-item short form",
                                                           choices = c("A", "B"),
                                                           selected = "A"
                                              )
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

                           fluidRow(
                             column(width = 10, offset = 1,
                                    h3("Administration Instructions"),
                                    includeMarkdown(
                                      system.file("app/www/instructions.md",
                                                  package = "pnt")
                                    ),
                                    fileInput("incomplete_test", "Upload incomplete test csv"),
                                   div(align = "center",
                                       actionButton("back_to_test_or_retest", "Back"),
                                       shinyjs::disabled(actionButton("continue_test", "Resume incomplete test")),
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


