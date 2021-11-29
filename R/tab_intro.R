#' This is the user interface for the intro pages bfore testing
#'
#' It looks a little crazy, but is relatively straightforward.
#'
#'
#' @export
intro_tab_div <- function() {
         fluidRow(column(style = "padding: 1%;",
           width = 12,
           tabsetPanel(
             type = "hidden",
             id = "glide",
             # PAGE 1 #########################################################
             tabPanelBody(value = "welcome_page", 
                          fluidRow(
                            column(align = "center",width = 12,
                              div(style = "width:50%;",
                                  div(
                                    HTML("<h4>Welcome to the computer adaptive version of the <br> Philadelphia Naming Test</h4>")
                                    )
                                  )
                          )), br(),
                          fluidRow(
                            column(width = 8,offset = 2,
                              div(
                                includeMarkdown(system.file("app/www/new_intro.md",
                                                          package = "pnt"))
                                ),
                              br(),
                              div(align = "center",
                                  actionButton("welcome_next", "Get Started")), br(), br(),
                              div(id = "funding",
                                includeMarkdown(system.file("app/www/funding.md",
                                                            package = "pnt"))
                              )
                            )
                          )),
             # PAGE 2 #########################################################
             tabPanelBody(value = "intro_page", 
                    column(width = 10, offset = 1,
                          fluidRow(column(
                            align = "center",
                            width = 12,
                            div(style = "width:50%;",
                                div(h4(
                                  "Administration Options:"
                                )))
                          )), br(),
                          fluidRow(
                            column(width = 4, style = "padding: 1%;",
                                  div(class = "page2",
                                      includeMarkdown(system.file("app/www/new_test.md", package = "pnt")),br(),
                                     div(class = "page2buttons",
                                       actionButton("administer_test", "Administer new PNT")
                                       ))),
                            column(width = 4, style = "padding: 1%;",
                                   div(class = "page2",
                                       includeMarkdown(system.file("app/www/retest.md", package = "pnt")),br(),
                                       div(class = "page2buttons",
                                   actionButton("administer_retest", "Re-administer PNT")
                                   ))),
                            column(width = 4, style = "padding: 1%;",
                                   div(class = "page2",
                                       includeMarkdown(system.file("app/www/upload_test.md", package = "pnt")),br(),
                                       div(class = "page2buttons",
                                   actionButton("score_test", "Rescore PNT / Score offline test")
                                   )))
                          ),br(),
                          fluidRow(
                            column(width = 12,
                              div(
                                align = "center",
                                actionButton('back_intro', "Back"),
                              )
                            )
                          )
                      )
                    ),
             # PAGE 3 ############################################################################
             tabPanelBody(value = "new_pnt_page", #glide 1
                          fluidRow(
                            column(
                              width = 4,
                              #offset = 1,
                              align = "center",
                              div(
                                style = "display: inline-block; text-align: left;",
                                h5("Administer PNT"),
                                br(),
                                ### Use this to set how many items to run.
                                radioButtons(
                                  inputId = "numitems",
                                  label = NULL, #"Select PNT Test Administration",
                                  choices = c(
                                    "30-item Computer Adaptive PNT" = "30_cat",
                                    "175-item Computer Adaptive PNT" = "175_cat",
                                    "30-item PNT Short form (Walker)" = "30_walker",
                                    "175-item Standard PNT" = "175_standard"
                                  ),
                                  selected = "30_cat",
                                  inline = F
                                ),
                                shinyjs::hidden(checkboxInput(
                                  "eskimo",
                                  'Exclude item "Eskimo"',
                                  value = T
                                )),
                                shinyjs::hidden(
                                  radioButtons(
                                    "walker",
                                    "Choose 30-item short form",
                                    choices = c("A", "B"),
                                    selected = "A"
                                  )
                                ),
                                #textInput("name", "Enter a Name (optional)"),
                                textInput("notes", "Enter any notes (optional)"),
                                div(
                                  align = "center",
                                  actionButton("back_test", "Back"),
                                  actionButton("next_test", "Next")
                                )
                              )
                              # )
                            ),
                            column(width = 7,
                                   #offset = 1,
                                   class = "testinfo",
                                   h5("About the PNT test Versions", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   accordion_test(),br(), br(),
                                   includeMarkdown(system.file("app/www/esk_footnote.md", package = "pnt"))
                            )
                          )),
             # PAGE 4 #########################################################
             tabPanelBody(value = "retest_pnt_page",
                          fluidRow(
                            column(
                              width = 4,
                              #offset = 1,
                              align = "center",
                              # div(align = "center",
                              div(
                                style = "display: inline-block; text-align: left;",
                                h5("Readminister PNT"),
                                br(),
                                
                                fileInput("file1", "Upload previous results", accept = ".csv"),
                                ### Use this to set how many items to run.
                                radioButtons(
                                  inputId = "numitems_retest",
                                  label = NULL,#"Select PNT Re-Administration",
                                  choices = c(
                                    "30-item Computer Adaptive PNT" = "30_cat",
                                    "Variable length Computer Adaptive PNT" = "SEM",
                                    #"175-item Computer Adaptive PNT" = "175_cat",
                                    "30-item PNT Short form (Walker)" = "30_walker",
                                    "175-item Standard PNT" = "175_standard"
                                  ),
                                  selected = "30_cat",
                                  inline = F
                                ),
                                # should only be available for the 30 item
                                shinyjs::disabled(
                                  checkboxInput(
                                    "exclude_previous",
                                    "Exclude items from first administration",
                                    value = T
                                  )
                                ),
                                shinyjs::hidden(checkboxInput(
                                  "eskimo_retest",
                                  'Exclude item "Eskimo"',
                                  value = T
                                )),
                                shinyjs::hidden(
                                  radioButtons(
                                    "walker_retest",
                                    "Choose 30-item short form",
                                    choices = c("A", "B"),
                                    selected = "A"
                                  )
                                ),
                                textInput("notes_retest", "Enter any notes (optional)"),
                                div(
                                  align = "center",
                                  actionButton("back_retest", "Back"),
                                  shinyjs::disabled(actionButton("next_retest", "Next"))
                                )
                              )
                              # )
                            ),
                            column(width = 7,
                                   #offset = 1, 
                                   class = "testinfo",
                                   h5("About the PNT test Versions", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   accordion_retest(),br(), br(),
                                   includeMarkdown(system.file("app/www/esk_footnote.md", package = "pnt"))
                            )
                            
                          )),
             # PAGE 5 #########################################################
             tabPanelBody(value = "score_offline_page",
                          fluidRow(
                            column(
                              width = 4,
                              #offset = 1,
                              align = "center",
                              # div(align = "center",
                              div(
                                style = "display: inline-block; text-align: left;",
                                h5("Scoring an offline or completed test"),
                                
                                downloadButton("downloadEmpty", "Download Blank Spreadsheet"),
                                fileInput("file2", "Upload offline or re-scored data", accept = ".csv"),
                                shinyjs::hidden(div(id = "input_file_warning", uiOutput("upload_error"))),
                                div(
                                  align = "center",
                                  actionButton("back_offline", "Back"),
                                  shinyjs::disabled(actionButton("score_uploaded_data", "OK"))
                                )
                                
                              )
                            ),
                            column(width = 7,
                                   #offset = 1,
                                   class = "testinfo",
                                   h5("How to upload a file", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   includeMarkdown(system.file("app/www/rescore_pnt_notes.md", package = "pnt")),
                            )
                            
                          )),
             # PAGE 6 #########################################################
             tabPanelBody(value = "instructions_page",
                          fluidRow(
                            column(style = "padding-right:2%;",
                              width = 5,
                              offset = 1,
                              h3("Administration Instructions"),
                              includeMarkdown(system.file("app/www/instructions.md",
                                                          package = "pnt")),
                              div(
                                align = "center",
                                actionButton("back_to_test_or_retest", "Back"),
                                #actionButton("continue_test", "Resume incomplete test"),
                                actionButton("start_practice","Start Practice")
                              ),br(),
                              div(
                                fileInput("file_incomplete", h5("Continue incomplete test")),
                                  div( align = "center",
                                    shinyjs::disabled(actionButton("resume", "Continue Test"))
                                  )
                                )
                            ),
                            column(width = 5,
                                   #offset = 1,
                                   class = "testinfo",
                                   h5("Notes on test administration", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   accordion_faq()
                                   
                            )
                          ))
           )
           
         ))
}
