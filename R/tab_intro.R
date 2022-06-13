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
                                  div(
                                    HTML("<h4>Welcome to the computer adaptive version of the <br> Philadelphia Naming Test</h4>")
                                    )
                          )), br(),
                          fluidRow(class = "justify-content-center",
                            column(width = 9,
                              div(
                                includeMarkdown(system.file("app/www/new_intro.md",
                                                          package = "pnt"))
                                ),
                              br(),
                              div(align = "center",
                                  actionButton("administer_test", "Administer PNT"),
                                  #actionButton("administer_retest", "Re-administer PNT"),
                                  actionButton("score_test", "Rescore PNT / Score offline test")),
                                  #actionButton("welcome_next", "Get Started")),
                              br(), #br(),
                              div(id = "funding",
                                includeMarkdown(system.file("app/www/funding.md",
                                                            package = "pnt"))
                              )
                            )
                          )),
             # PAGE 3 ############################################################################
             tabPanelBody(value = "new_pnt_page", #glide 1
                          fluidRow(class = "justify-content-around",
                            column(class="col-sm-4 col-md-4 col-lg-3",
                              width = 3,
                                h4("Administer PNT", align = "center"),hr(),
                                #br(),
                              tags$b("Administration:"),
                              radioButtons("retest", label = NULL, inline = TRUE,
                                           choices = c("Initial test" = "1",
                                                                 "Retest" = "2")),
                            
                              tags$b("Select Test Version:", style="margin-bottom:50px;"),
                                ### Use this to set how many items to run.
                                radioButtons(
                                  inputId = "numitems", 
                                  label = NULL,
                                  choices = c(
                                    "30-item Computer Adaptive" = "30_cat",
                                    "175-item Computer Adaptive" = "175_cat",
                                    "30-item Short form (Walker)" = "30_walker",
                                    "175-item Standard" = "175_standard"
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
                              shinyjs::hidden(
                                div(id="retest_div", class = "testinfo",
                                    fileInput("file1", "Upload previous results", accept = ".csv"),
                                    checkboxInput("exclude_previous",value = T,
                                                  "Exclude items from first administration"
                                    )
                                )
                              ), br(),
                                #textInput("name", "Enter a Name (optional)"),
                                textInput("notes", "Enter any notes (optional)"),
                                tags$em("If continuing an incomplete test, reselect your original choices and proceed to the next page."), br(), br(),
                                div(
                                  align = "center",
                                  actionButton("back_test", "Back"),
                                  actionButton("next_test", "Next")
                                )
                             # )
                              # )
                            ),
                            column(class="col-sm-7, col-md-8, col-lg-8",
                                   width = 7, 
                                   class = "testinfo",
                                   h5("About the PNT test Versions", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   accordion_test(),br(), br(),
                                   includeMarkdown(system.file("app/www/esk_footnote.md", package = "pnt"))
                            )
                          )),
             # PAGE 4 #########################################################
             # Deleted!
             # PAGE 5 #########################################################
             tabPanelBody(value = "score_offline_page",
                          fluidRow(class = "justify-content-around",
                            column(class="col-sm-4 col-md-4 col-lg-4",
                              width = 4,
                              div(
                                h5("Scoring an offline or completed test"),
                                fileInput("file2", "Upload offline or re-scored data", accept = ".csv"),
                                shinyjs::hidden(div(id = "input_file_warning", uiOutput("upload_error"))),
                                div(
                                  align = "center",
                                  actionButton("back_offline", "Back"),
                                  shinyjs::disabled(actionButton("score_uploaded_data", "OK"))
                                )
                                
                              )
                            ),
                            column(class="col-sm-7, col-md-7, col-lg-7",
                                   width = 7,
                                  # offset = 1,
                                   class = "testinfo",
                                   h5("How to upload a file", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   includeMarkdown(system.file("app/www/rescore_pnt_notes1.md", package = "pnt")),
                                   downloadButton("downloadEmpty", "Download Blank Spreadsheet"),
                                   br(), br(),
                                   includeMarkdown(system.file("app/www/rescore_pnt_notes2.md", package = "pnt"))
                            )
                          )),
             # PAGE 6 #########################################################
             tabPanelBody(value = "instructions_page",
                          fluidRow(class = "justify-content-center",
                            column(style = "padding-right:2%;",
                              width = 4,
                              h4("Administration Instructions", align = "center"),hr(),
                              includeMarkdown(system.file("app/www/instructions.md",
                                                          package = "pnt")),
                              div(
                                align = "center",
                                actionButton("back_to_test_or_retest", "Back"),
                                actionButton("start_practice","Start Practice")
                              ),br(),
                              div(
                                h5("Resume incomplete test"),
                                p("To resume an incomplete test, upload file with in-progress
                                  data then select continue test below."),
                                fileInput("file_incomplete", ""),
                                  div( align = "center",
                                    shinyjs::disabled(actionButton("resume", "Continue Test"))
                                  )
                                )
                            ),
                            column(width = 7,
                                   class = "testinfo",
                                   h5("Test Administration Frequently Asked Questions", style = "margin-top:0;margin-bottom:1.25rem;"),
                                   accordion_faq()
                            )
                          ))
           )
           
         ))
}
