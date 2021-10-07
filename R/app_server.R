
`%!in%` <- Negate(`%in%`)

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  ########################## Initialize reactive values ##########################
  # ------------------------------------------------------------------------------
  ################################################################################
  # reactiveValues is a list where elements of the list can change
  values = reactiveValues()
  values$item_difficulty <- items #items...see observe #dataframe of potential values
  values$i = 0 # this is the counter to track the slide number
  values$test_length <- NULL # number of items to test
  values$irt_out <- list(0, 0, 11) # will be overwritten if IRT 
  values$min_sem <- NULL # sem precision
  values$exclude_previous <- NULL
  values$previous <- NULL # previous data if uploaded
  values$num_previous <- 0 # number of previous tests
  values$datetime <- Sys.time() # establishes datetime when app opens for saving
  values$downloadableData = F # will the download data button appear? starts with no. yes after first response. 
  
  ################################## PREVIOUS DATA ###############################
  # ------------------------------------------------------------------------------
  ################################################################################ 
  
  # observer for uploading data
  observeEvent(input$file1,{
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    # check upload
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    # save upload
    values$previous <- read.csv(file$datapath) %>%
      tidyr::drop_na(response)
    # assign number of previous values
    values$num_previous <- length(unique(values$previous$date))
    
  })
  
  ################################## OBSERVERS ###################################
  # ------------------------------------------------------------------------------
  ################################################################################
  
  ###########################Intro tab next and back############################
  
  # includes a few other actions when moving to the last page. 
  observeEvent(input$administer_test,{
    updateTabsetPanel(session, "glide", "glide1")
  })
  
  observeEvent(input$glide_back0,{
    updateTabsetPanel(session, "glide", "glide0")
  })
  
  observeEvent(input$glide_next1,{
    updateTabsetPanel(session, "glide", "glide2")
  })
  
  observeEvent(input$glide_back1,{
    updateTabsetPanel(session, "glide", "glide1")
  })
  
  observeEvent(input$glide_next2,{
    updateTabsetPanel(session, "glide", "glide3")
  })
  
  observeEvent(input$glide_back2,{
    updateTabsetPanel(session, "glide", "glide2")
  })
  ################################ SOUND ON ##### ##############################
  
  observeEvent(input$sound,{
    if(!isTruthy(input$sound)){
      values$sound = "document.getElementById('audio').play();"
    } else {
      values$sound = ""
    }
  })
  
  ##########################NUM ITEMS AND PRECISION#############################
  # enables or disables precision option if SEM is or isn't selected. 
  # also converts the numeric option to a number
  # saves either to values$test_length
  observe({
    if(input$numitems == "SEM"){
      # precision condition
      values$test_length <- "95_ci"
      shinyjs::show("ci_95")
      shinyjs::hide("random")
      shinyjs::hide("walker")
      shinyjs::hide("eskimo")
      if(values$num_previous>0){
        shinyjs::show("exclude_previous")
      }
    } else if(input$numitems == "walker") {
      # walker short form
      values$test_length <- 30
      shinyjs::hide("ci_95")
      shinyjs::show("walker")
      shinyjs::hide("random")
      shinyjs::hide("exclude_previous")
      shinyjs::hide("eskimo")
    } else if(input$numitems == "175"){
      # full pnt
      values$test_length <- ifelse(input$eskimo, 174, 175)
      shinyjs::show("random")
      shinyjs::hide("ci_95")
      shinyjs::hide("walker")
      shinyjs::hide("exclude_previous")
      shinyjs::show("eskimo")
    } else {
      # fixed length IRT
      values$test_length <- as.numeric(input$numitems)
      shinyjs::hide("ci_95")
      shinyjs::hide("walker")
      shinyjs::hide("random")
      shinyjs::hide("eskimo")
      if(values$num_previous>0){
        shinyjs::show("exclude_previous")
      }
    }
  })
  
  #############################START OVER#########################################
  # if start over is hit, go to home page
  # start assessment button then resets everything
  observeEvent(input$start_over,{
    session$reload()
  })
  
  ################################ END TEST ##################################
  
  observeEvent(input$end_test,{
    shinyWidgets::confirmSweetAlert(
      inputId = "confirm_end_test",
      session = session,
      title = "Are you sure you want to stop?",
      text = "Only items with confirmed responses will be saved.",
      type = "warning",
    )
  })
  
  observeEvent(input$confirm_end_test,{
    if(isTruthy(input$confirm_end_test)){
      updateNavbarPage(session, "mainpage",
                       selected = "Results")
    }
  })
  ################################ Displaying key inputs #######################
  
  output$key_feedback_practice <- renderText({
    req(values$key_val)
    values$key_val
  })
  
  output$key_feedback_slides <- renderText({
    req(values$key_val)
    values$key_val
  })
  
  ################################ START PRACTICE ##############################
  observeEvent(input$start_practice,{
    
    # runjs("document.getElementById('audio').play();") # play click
    shinyjs::runjs(values$sound)
    values$i = 1 # reset values$i
    values$n = 130 # reset 
    values$key_val = NULL # keeps track of button press 1 (error), 2 (correct)
    values$exclude_previous <- input$exclude_previous
    # only use IRT function if NOT 175 items
    # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
    values$IRT = ifelse(input$numitems == "175", FALSE,
                        ifelse(input$numitems == "walker", FALSE,
                               TRUE)
    )
    shinyjs::show("start_over")
    shinyjs::show("help")
    # go to practice slides
    updateNavbarPage(session, "mainpage",
                     selected = "Practice")
  })
  
  ################################## START ASSESSMENT ############################
  # start button. sets the i value to 1 corresponding to the first slide
  # switches to the assessment tab
  # initialize values in here so that they reset whever someone hits start. 
  observeEvent(input$start, {
    
    # keeps track of button press 1 (error), 2 (correct)
    values$i = 1
    
    
    if(isTruthy(input$numitems == "walker")){
      values$item_difficulty <- 
        values$item_difficulty %>%
        dplyr::filter(walker == input$walker)
    }
    
    # randomly orders stuff if the random order box is checked. only affects 175
    if(isTruthy(input$random)){
      values$item_difficulty <-
        values$item_difficulty %>%
        dplyr::mutate(pnt_order = sample(pnt_order)) %>%
        dplyr::arrange(pnt_order)
    }
    
    values$n = 
      # regular old CAT 
      if(isTruthy(values$IRT)){
      # samples one of four first possible items, unless used previously...
      get_first_item(all_items = values$item_difficulty,
                     previous = values$previous,
                     exclude_previous = values$exclude_previous)
      # walker first item
    } else if (input$numitems == "walker"){
      values$item_difficulty[values$item_difficulty$walker_order == 1,]$slide_num 
      # random 175 first item
    } else if (isTruthy(input$random)) {
      # if random, grab first row in values$item_difficulty,
      # which is already randomized in code above
      values$item_difficulty[values$item_difficulty$pnt_order == 1,]$slide_num 
    } else {
      14 #otherwise candle
    }
    # for testing:
    if (isTRUE(getOption("shiny.testmode"))) {
      shinyjs::reset("keys")
    }
    values$irt_out <- list(0, 0, 11) # reset saved data just in case. 
    #play a sound...not working right now :(
    shinyjs::runjs("document.getElementById('audio').play();")
    # got to slides
    # reset keyval
    values$key_val = NULL # keeps track of button press 1 (error) or 2 (correct)
    updateNavbarPage(session, "mainpage", selected = "Assessment")
  })
  
  
  # records the inputted 95% CI width to SEM
  observeEvent(input$ci_95,{
    values$min_sem <- input$ci_95/1.96
  })
  
  
  #############################KEY PRESS##########################################
  # tracks the key inputs
  observeEvent(input$keys, {
    values$key_val = input$keys
  })
  
  #no key presses on home or results page
  observe({
    if(input$mainpage=="Results" || input$mainpage=="Home"){
      keys::pauseKey()
      shinyjs::show("footer_id")
    } else {
      keys::unpauseKey()
      shinyjs::hide("footer_id")
      
    }
  })
  
  ################ THIS IS WHRERE CAT STUFF GETS INCORPORATED ####################
  
  # observe event will take an action if an input changes.
  # here the next button or the enter key
  # This is where the app will interact with the -CAT-IRT algorithm
  observeEvent(input$enter_key, {
    # should the app show another item?
    # if the stopping choice is SEM,
    # check if the current sem is less than the desired precision
    # if its just a static number of items,
    # then check if this number has already been shown
    # returns TRUE or FALSE
    if(input$mainpage=="Practice"){
      # if slide 13, don't iterate, just show a message that says hit start...
      if(values$i == 13){
        showNotification("Press start to start testing", type = "message")
      }
      # essentially, if we're on the first two instruction slides,
      # don't require a 1 or 2..
      else if(values$i %in% c(1, 2)){
        shinyjs::runjs("document.getElementById('audio').play();")
        values$i = ifelse(values$i<13, values$i + 1, values$i)
        # otherwise, (i.e. not a practice slide)
      } else if(is.null(values$key_val)){ 
        # require a key press
        showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
      } else {
        shinyjs::runjs("document.getElementById('audio').play();")
        #js$click_sound()
        values$i = ifelse(values$i<13, values$i + 1, values$i)
      }
      values$key_val = NULL
    } else {
      # can you download data? yes - will calculate the data to go out. 
      values$downloadableData = T
      shinyjs::show("downloadData")
      
      another_item <- if(input$numitems == "SEM"){
        values$min_sem<values$irt_out[[3]]
      } else {
        values$i<=values$test_length
      }
      # require a key input response
      if(is.null(values$key_val)){ 
        showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
      } else if (another_item) {
        shinyjs::runjs("document.getElementById('audio').play();")
        # If a key press was detected, store it in our dataframe of items,
        # difficulty, discrimination etc...
        # 1 is incorrect (1) and 2 is correct (0).
        # IRT model reverses 1 and 0...
        values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <-
          ifelse(values$key_val == incorrect_key_response, 1,
                 ifelse(values$key_val == correct_key_response, 0, "NR"))
        # irt_function: takes in the current data, values$item_difficulty
        # which also includes the most recent response 
        # returns a list of 3 elements
        # element[[1]] is the new ability estimate
        # element[[2]] is a list of info returned by catR::nextSlide(), 
        # including $name, the name of the next item
        # element[[3]] returns the sem after re-estimating the model
        values$irt_out = irt_function(all_items = values$item_difficulty,
                                      IRT = values$IRT,
                                      exclude_previous = values$exclude_previous,
                                      previous = values$previous,
                                      test = input$numitems,
                                      exclude_eskimo = input$eskimo
        )
        # save info to the item_difficulty data_frame
        values$item_difficulty[values$item_difficulty$slide_num == values$n,][9:13] <-
          tibble::tibble(
            # what trial was the item presented
            order = values$i,
            # what was the key press
            key = values$key_val,
            # 1 is incorrect (1) and 2 is correct (0).
            # IRT model reverses 1 and 0...
            resp = ifelse(values$key_val == incorrect_key_response,
                          "incorrect",
                          ifelse(values$key_val == correct_key_response,
                                 "correct", "NR")
            ),
            # NEW ability estimate after model restimation
            ability = round(values$irt_out[[1]],3),
            # NEW sem 
            sem = round(values$irt_out[[3]], 3)
          )
        # pick the next slide using the output of the irt
        # conditional fixes a bug for the last item
        # if the test goes all the way to 175
        values$n = 
          if(values$IRT){
            if(!is.na(values$irt_out[[2]][[1]])){
              values$item_difficulty[values$item_difficulty$target == values$irt_out[[2]]$name,]$slide_num
            } else {
              190
            }
          } else {
            values$irt_out[[2]][[2]]
          } 
        # iterate the order
        values$i = values$i + 1
      } 
      # prints to the console the last 5 items. DELETE FOR RELEASE
      print(tail(values$item_difficulty %>% tidyr::drop_na(response) %>%
                   dplyr::arrange(order), 5))
      # decides whether to cut to the results page or not!
      # returns TRUE or FALSE
      go_to_results <- if(is.na(values$n)){
        TRUE
      } else if(input$numitems == "SEM"){
        values$min_sem>values$irt_out[[3]]
      } else {
        values$i>values$test_length
      }
      # go to results if indicated
      if (go_to_results){
        updateNavbarPage(session, "mainpage",
                         selected = "Results")
        shinyjs::show("report")
      }
      values$key_val = NULL
      #for testing::
      if (isTRUE(getOption("shiny.testmode"))) {
        reset("keys")
      }
    }
    # don't run this on start up. 
  }, ignoreInit = T)
  ################################## REACTIVE DATA ############################### 
  # ------------------------------------------------------------------------------
  ################################################################################
  # holds the item-level responses. 
  results_data_long <- reactive({
    req(isTruthy(values$downloadableData))
    precision = if(input$numitems == "SEM"){
      paste0("95% CI: ", input$ci_95)
    } else {
      paste0(input$numitems, " items")
    }
    
    tmp = dplyr::bind_rows(values$item_difficulty) %>%
      dplyr::mutate(ci_95 = sem*1.96,
                    precision = precision,
                    name = input$name,
                    date = values$datetime,
                    notes = NA
      ) %>%
      dplyr::arrange(order)
    
    tmp$notes[[1]] = input$notes
    return(tmp)
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
    req(input$mainpage=="Results")
    dplyr::bind_rows(values$item_difficulty) %>%
      # have to switch 0s and 1s because IRT is dumb. 
      tidyr::drop_na() %>%
      dplyr::mutate(response = as.numeric(ifelse(response == 0, 1, 0)),
                    ci_95 = sem*1.96) %>%
      dplyr::summarize(accuracy = mean(response)) %>%
      dplyr::pull(accuracy)
  })
  
  # tracks final irt data.
  irt_final <- reactive({
    req(input$mainpage=="Results")
    get_final_numbers(out = values$irt_out,
                      previous = values$previous,
                      num_previous = values$num_previous)
    
  })
  
  
  ################################## EXPORT TEST DATA ############################
  # ------------------------------------------------------------------------------
  ################################################################################
  # get data into strings for exporting...test only
  observeEvent(input$mainpage=="Results",{
    values$out_words <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                                dplyr::pull(target), collapse = "_")
    values$out_nums <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                               dplyr::pull(response), collapse = "_")
    values$out_ability <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                                  dplyr::pull(ability), collapse = "_")
    values$out_sem <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                              dplyr::pull(sem), collapse = "_")
    values$item_dif <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                               dplyr::pull(itemDifficulty), collapse = "_")
    values$disc <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                           dplyr::pull(discrimination), collapse = "_")
    values$key <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                          dplyr::pull(key), collapse = "_")
    values$order <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                            dplyr::pull(order), collapse = "_")
    values$item_number <- paste(results_data_long() %>% tidyr::drop_na(response) %>%
                                  dplyr::pull(item_number), collapse = "_")
  })
  # This makes the above data available after running unit test.
  exportTestValues(abil = values$out_ability,
                   sem = values$out_sem,
                   words = values$out_words,
                   responses = values$out_nums,
                   itemDifficulty = values$item_dif,
                   discrimination = values$disc,
                   key_press = values$key,
                   order = values$order,
                   item_number = values$item_number
  )
  ################################## SUMMARY TEXT ################################
  # ------------------------------------------------------------------------------
  ################################################################################
  #  outputs a summary sentence
  output$results_summary <- renderUI({
    summary = p(
      get_text_summary(acc = results_data_summary(),
                               ability = irt_final()$ability,
                               ci_95 = irt_final()$ci_95,
                               last_ability = irt_final()$last_ability,
                               last_ci_95 = irt_final()$last_ci_95,
                               first_ability = irt_final()$first_ability,
                               first_ci_95 = irt_final()$first_ci_95,
                               num_previous = values$num_previous)
    )
  })
  ################################## DOWNLOAD ####################################  
  # ------------------------------------------------------------------------------
  ################################################################################

  # downloading output
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" ", "-", input$name),
            as.character(Sys.Date()),
            "pnt.csv", sep = "_"
      )
    },
    content = function(file) {
      write.csv(get_data_for_download(dat = results_data_long()), file, row.names = FALSE)
    }
  )
  
########### DOWNLOAD REPORT #############
  
  output$report <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- system.file("report.Rmd", package = "pnt")#file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          name = ifelse(nchar(input$name)>0, input$name, "X"),
          notes = input$notes,
          values = values,
          irt_final = irt_final(),
          text = get_text_summary(
                           acc = results_data_summary(),
                           ability = irt_final()$ability,
                           ci_95 = irt_final()$ci_95,
                           last_ability = irt_final()$last_ability,
                           last_ci_95 = irt_final()$last_ci_95,
                           first_ability = irt_final()$first_ability,
                           first_ci_95 = irt_final()$first_ci_95,
                           num_previous = values$num_previous))
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  
  ################################## FOOTER MODAL ################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # More information modal
  observeEvent(input$info, {
    showModal(modalDialog(
      tags$iframe(src="www/about.html", width = "100%",
                  height = "650px", frameBorder = "0"),
      easyClose = TRUE,
      size = "l"
    ))
  })
# Help modal
  observeEvent(input$help, {
    showModal(modalDialog(
      div(
        h5("Instructions:"),
        tags$ul(
          tags$li("Click Start Practice to get started"),
          tags$li("Press 1 for incorrect and 2 for correct"),
          tags$li("A 1 or 2 will appear in the top-right of the screen to show the key entered."),
          tags$li("Remember to score the first complete response"),
          tags$li("Press Enter to advance the screen"),
        )
      ),
      size = "m",
      easyClose = TRUE,
    ))
  })
  
  ################################## PLOT ######################################## 
  # ------------------------------------------------------------------------------
  ################################################################################
  # plot
  output$plot <- renderPlot({# Fergadiotis, 2019
    req(irt_final())
    get_plot(values = values, irt_final = irt_final())
  })
  
  ################################## TABLE #######################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # outputs a table of the item level responses
  output$results_table <- DT::renderDT({
    results_data_long() %>%
      tidyr::drop_na(response) %>%
      dplyr::select(order, target, resp, key, itemDifficulty, ability, sem)
  }, rownames = F,
  options = list(dom = "tp"))
  
  ################################## TAB UI ######################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # this UI is on the server side so that it can be dynamic based. 
  # see scripts named tab_*.R 
  
  # this shows the practice slides
  output$practice_tab <- renderUI({
    practice_tab_div(values = values)
  })
  
  # UI for assessment slides
  output$slides_tab <- renderUI({
    slides_tab_div(values = values)
  })
  outputOptions(output, "slides_tab", suspendWhenHidden = FALSE)
  outputOptions(output, "results_table", suspendWhenHidden = FALSE)
  
  ################################## SCORE EXISTING TEST MODAL #################
  # ----------------------------------------------------------------------------
  ##############################################################################
  # More information modal
  observeEvent(input$score_test, {
    showModal(modalDialog(
      div(
        h3("Scoring an offline or completed test"),
        p("To score an offline or previously completed test, download the blank spreadsheet
          below."),
        p("Enter 1 for error/incorrect and 2 for correct in the response column."),
        p("If other responses are entered or additional changes made to the spreadsheet, 
          rescoring may not work."),
        p("You have to upload a .csv file before you can hit ok."),
        downloadButton("downloadEmpty", "Download Blank Spreadsheet"),
        fileInput("file2", "Upload offline or re-scored data", accept = ".csv"),
        shinyjs::hidden(
          div(id="input_file_warning", uiOutput("upload_error"))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        shinyjs::disabled(
          actionButton("score_uploaded_data", "OK")
        )
      ),
      easyClose = F,
      size = "m"
    ))
  })
  
  # downloading empty file
  output$downloadEmpty <- downloadHandler(
    filename = function() {
      "pnt-cat-blank.csv"
    },
    content = function(file) {
      write.csv(items %>% 
                  dplyr::select(item_number, target, response = key),
                file, row.names = FALSE)
    }
  )
  
  # observer for uploading data - including error messages
  observeEvent(input$file2,{
    file <- input$file2
    ext <- tools::file_ext(file$datapath)
    # check upload
    req(file)
    # save upload
    values$rescore <- read.csv(file$datapath) %>%
      tidyr::drop_na()
    # saves teh error messages to be put back into the modal. 
    values$error <- if(nrow(values$rescore)==0){
      "Error: Please include at least one scored response"
    } else if (!all(c("item_number", "target", "response") %in% colnames(values$rescore))){
      "Error: Column names have been changed"
    } else if (!all(unique(values$rescore$response) == 1 | unique(values$rescore$response) == 2)){
      "Error: please only enter 1 for correct and 2 for incorrect in the response column"
    } else {
      "no_error"
    }
    
    # depending on the error message, allow progressing or show the error
    if(values$error == "no_error"){
      shinyjs::enable("score_uploaded_data")
      shinyjs::hide("input_file_warning")
    } else {
      shinyjs::show("input_file_warning")
      shinyjs::disable("score_uploaded_data")
    }
  })
  # output of the dynamic error message. 
  output$upload_error <- renderUI({
    p(values$error, style="color:red;")
  })
  # scores the uploaded data, moves to the results page and shows the start over and 
  # download report buttons
  observeEvent(input$score_uploaded_data,{
    values$rescore_list <- score_uploaded_data(values$rescore)
    removeModal()
    updateNavbarPage(session, "mainpage",
                     selected = "Results2")
    shinyjs::show("start_over")
    #shinyjs::show("rescore_downloadData")
    shinyjs::show("rescore_report")
  })
  
  #outputs the rescored plot
  output$plot2 <-
    renderPlot({
      req(values$rescore_list)
      values$rescore_list$plot
    })
  
  # outputs the rescored data
  output$results_table2 <-
    DT::renderDT({
      req(values$rescore_list)
      values$rescore_list$data
    }, rownames = F,
    options = list(dom = "tp"))
  
  #  outputs a summary sentence
  output$results_summary2 <-
    renderUI({
      req(values$rescore_list)
      p(values$rescore_list$text)
    })
  
  # downloading output....not currently used. 
  # output$rescore_downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("rescored-PNT",
  #           as.character(Sys.Date()),
  #           ".csv", sep = "_"
  #     )
  #   },
  #   content = function(file) {
  #     write.csv(values$rescore_list$data, file, row.names = FALSE)
  #   }
  # )
  # download a report for the resscored data. 
  output$rescore_report <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    filename = "rescore_report.pdf",
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- system.file("rescore_report.Rmd", package = "pnt")#file.path(tempdir(), "report.Rmd")
        file.copy("rescore_report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          values = values$rescore_list,
          download_time = Sys.time()
          )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
  ################################## END RESCORE MODULE ########################
  # ----------------------------------------------------------------------------
  ##############################################################################
  # end of app
}
