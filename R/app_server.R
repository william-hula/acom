
#`%!in%` <- Negate(`%in%`)

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  ################################################################################
  ########################## Initialize reactive values ##########################
  # ------------------------------------------------------------------------------
  ################################################################################
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

  ################################################################################
  ################################## UPLOADS #####################################
  # ------------------------------------------------------------------------------
  ################################################################################ 
  ################################################################################
  
  # observer for uploading prior administration data
  observeEvent(input$file1,{
    
    uploadedData <- uploadData(file_input = input$file1)
    values$previous <- uploadedData$dat
    
    if(nrow(values$previous)>1){
        values$num_previous <- 1
        values$min_sem <- min(values$previous$sem, na.rm = T)
        shinyjs::enable("next_retest")
    } else {
        showNotification(uploadedData$error, type = "error")
        values$previous <- NULL
        shinyjs::reset("file1")
    }
    
  })
  
  ################################################################################
  ################################## OBSERVERS ###################################
  # ------------------------------------------------------------------------------
  ################################################################################
  ################################################################################
  
  ###########################Intro tab next and back############################
  
  changeIntroPage <- function(go_to_page){
    updateTabsetPanel(session, "glide", go_to_page)
  }
  
  # INTRO FLOW
  observeEvent(input$welcome_next,{
    values$new_test = T
    changeIntroPage("intro_page")
  })
  
  observeEvent(input$back_intro,{
    changeIntroPage("welcome_page")
  })
  
  # TEST FLOW
  observeEvent(input$administer_test,{
    values$new_test = T
    changeIntroPage("new_pnt_page")
  })
  
  observeEvent(input$back_test,{
    changeIntroPage("intro_page")
  })
  
  # RETEST FLOW
  observeEvent(input$administer_retest,{
    values$new_test = F
    changeIntroPage("retest_pnt_page")
  })
  
  observeEvent(input$back_retest,{
    changeIntroPage("intro_page")
  })
  
  # OFFLINE FLOW
  observeEvent(input$score_test,{
    values$new_test = F
    changeIntroPage("score_offline_page")
  })
  
  observeEvent(input$back_offline,{
    changeIntroPage("intro_page")
  })
  
  observeEvent(input$next_test,{
    changeIntroPage("instructions_page")
  })
  
  observeEvent(input$next_retest,{
    changeIntroPage("instructions_page")
  })
  
  observeEvent(input$back_to_test_or_retest,{
    if(isTruthy(values$new_test)){
      changeIntroPage("new_pnt_page")
    } else {
      changeIntroPage("retest_pnt_page")
    }
  })
  
  
  ##########################NUM ITEMS AND PRECISION#############################
  # enables or disables precision option if SEM is or isn't selected. 
  # also converts the numeric option to a number
  # saves either to values$test_length
  observe({
    if(isTruthy(values$new_test)){
       if(input$numitems == "175_standard"){
          # full pnt
          values$test_length = ifelse(input$eskimo, 174, 175)
          shinyjs::show("eskimo")
          shinyjs::hide("walker")
        } else if(input$numitems == "175_cat"){
          # full pnt
          values$test_length = 174
          shinyjs::hide("eskimo")
          shinyjs::hide("walker")
        } else if(input$numitems == "30_cat"){
          # fixed length IRT
          values$test_length = 30
          shinyjs::hide("eskimo")
          shinyjs::hide("walker")
        } else { #if(input$num_items == "30_walker")
          
          values$test_length = 30
          shinyjs::show("walker")
          shinyjs::hide("eskimo")
        }
    } else { # retest conditions
      
      if(input$numitems_retest == "SEM"){
        
        values$test_length <- "SEM"
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(getDefaultReactiveDomain(), "exclude_previous", value = T)
        shinyjs::hide("walker_retest")
        shinyjs::hide("eskimo_retest")
        
      } else if(input$numitems_retest == "30_cat"){
        
        # fixed length IRT
        values$test_length = 30
        shinyjs::enable("exclude_previous")
        updateCheckboxInput(getDefaultReactiveDomain(), "exclude_previous", value = T)
        shinyjs::hide("walker_retest")
        shinyjs::hide("eskimo_retest")
        
      } else if(input$numitems_retest == "175_cat"){
        
        values$test_length = 174
        
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(getDefaultReactiveDomain(), "exclude_previous", value = F)
        
        shinyjs::hide("walker_retest")
        shinyjs::hide("eskimo_retest")
        
      } else if(input$numitems_retest == "175_standard"){
        # full pnt
        values$test_length = ifelse(input$eskimo_retest, 174, 175)
        
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(getDefaultReactiveDomain(), "exclude_previous", value = F)
        
        shinyjs::hide("walker_retest")
        shinyjs::show("eskimo_retest")
        
      } else { #if(input$numitems_retest == "30_walker")
        
        values$test_length = 30
        
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(getDefaultReactiveDomain(), "exclude_previous", value = F)
        
        shinyjs::show("walker_retest")
        shinyjs::hide("eskimo_retest")
        
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
    if(values$i > 1){
      showModal(modalDialog(
        get_endtest_div(),
        easyClose = TRUE,
        size = "m",
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("downloadIncompleteData","Download current results"),
          actionButton("confirm_end_test", "End test/Go to results")
        )
      ))
    }
  })
  
  output$downloadIncompleteData <- downloadHandler(
    filename = function() {
      paste(gsub(" ", "-", input$name),
            as.character(Sys.Date()),
            "pnt.csv", sep = "_"
      )
    },
    content = function(file) {
      write.csv(get_data_for_download(values = values,
                                      in_progress = input$mainpage
      ), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$confirm_end_test,{
    removeModal()
    if(isTruthy(input$confirm_end_test)){
      
      values$irt_final <- 
        get_final_numbers(out = values$irt_out,
                          previous = values$previous,
                          num_previous = values$num_previous)
      shinyjs::show("download_report-report_download")
      shinyjs::show("download_results-results_download")
      #shinyjs::hide("help")
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
    values$exclude_previous <- ifelse(values$new_test, F, input$exclude_previous) # only informs second tests
    # only use IRT function if NOT 175 items
    values$name = input$name
    values$notes = input$notes
    values$notes_retest = input$notes_retest
    values$eskimo <- ifelse(values$new_test, input$eskimo, input$eskimo_retest)
    
    

    if(isTruthy(values$new_test)){
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$selected_test = input$numitems
      values$IRT = ifelse(grepl( "cat", input$numitems), TRUE, FALSE)
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl("walker", input$numitems), TRUE, FALSE)

      values$walker_form = ifelse(isTruthy(values$walker), input$walker, NA)
      if(isTruthy(values$walker)){
        values$item_difficulty = subset(values$item_difficulty, walker == input$walker)
      }
      
    } else {
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$selected_test = input$numitems_retest
      values$IRT = ifelse(grepl( "cat", input$numitems_retest), TRUE,
                          ifelse(grepl("SEM", input$numitems_retest), TRUE,
                                       FALSE))

      #print(values$IRT)
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl("walker", input$numitems_retest), TRUE, FALSE)

      values$walker_form = ifelse(isTruthy(values$walker), input$walker_retest, NA)
      if(isTruthy(values$walker)){
        values$item_difficulty = subset(values$item_difficulty, walker == input$walker_retest)
      }
      
    }
    
    shinyjs::show("start_over")
    #shinyjs::show("help")
    # go to practice slides

    updateNavbarPage(session, "mainpage",
                     selected = "Practice")
    
    # prints to the console for  troubleshooting
    cat(paste("Key app variables after selecting start practice:", "\n",
              "Selected test:", values$selected_test, "\n",
              "New test:", values$new_test, "\n",
              "Exclude previous:", values$exclude_previous, "\n",
              "IRT:", values$IRT, "\n",
              "Walker:", values$walker, "\n",
              "Walker form:", values$walker_form, "\n",
              "Excludes item 'eskimo':", values$eskimo, "\n", 
              "----------------------------------------", "\n"
              ))
  })
  
  
  
  ################################## START ASSESSMENT ############################
  # start button. sets the i value to 1 corresponding to the first slide
  # switches to the assessment tab
  # initialize values in here so that they reset whever someone hits start. 
  observeEvent(input$start, {
    
    # keeps track of button press 1 (error), 2 (correct)
    values$i = 1
    
    values$n = 
      # regular old CAT 
      if(isTruthy(values$IRT)){
      # samples one of four first possible items, unless used previously...
      # returns the first item number
      get_first_item(previous = values$previous,
                     exclude_previous = values$exclude_previous)
        
     } else if (isTruthy(values$walker)){ # walker first item
        values$item_difficulty[values$item_difficulty$walker_order == 1,]$slide_num 
     } else {
      14 #otherwise candle for standard PNT
     }
    # for testing:

    values$irt_out <- list(0, 0, 11) # reset saved data just in case. 
    # got to slides
    # reset keyval
    values$key_val = NULL # keeps track of button press 1 (error) or 2 (correct)
    updateNavbarPage(session, "mainpage", selected = "Assessment")
    
    # prints to the console for  troubleshooting
    cat(paste(
      "Key app variables after selecting start assessment:", "\n",
      "First item slide number:", values$n, "\n",
      "List for tracking CAT/IRT info:", paste(unlist(values$irt_out), collapse = " "), "\n",
      "----------------------------------------", "\n"
    ))
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
        values$i = ifelse(values$i<13, values$i + 1, values$i)
        # otherwise, (i.e. not a practice slide)
      } else if(is.null(values$key_val)){ 
        # require a key press
        showNotification("Enter a score", type = "error")
        # Remove tank from practice items
      } else if (values$i == 5){
        values$i = values$i + 2
        # as long as there's a response or it's an insturction slide...
      } else {
        values$i = ifelse(values$i<13, values$i + 1, values$i)
      }
      values$key_val = NULL
    } else {
      ########### main testing area###############
      # can you download data? yes - will calculate the data to go out. 
      values$downloadableData = T
      shinyjs::enable("downloadIncompleteData")
      
      another_item <- if(values$test_length == "SEM"){
        values$min_sem<values$irt_out[[3]]
      } else {
        values$i<=values$test_length
      }
      # require a key input response
      if(is.null(values$key_val)){ 
        showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
      } else if (another_item) {
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
                                      #test = input$numitems,
                                      exclude_eskimo = values$eskimo,
                                      walker = values$walker
        )
        # save info to the item_difficulty data_frame
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$order = values$i
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$key = values$key_val
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$resp = ifelse(values$key_val == incorrect_key_response,
                                                                                            "incorrect",
                                                                                            ifelse(values$key_val == correct_key_response,
                                                                                                   "correct", "NR")
        )
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$ability = round(values$irt_out[[1]],4)
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$sem = round(values$irt_out[[3]],4)

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
        #print(values$i)
      } 

      # decides whether to cut to the results page or not!
      # returns TRUE or FALSE
      go_to_results <- if(is.na(values$n)){
        TRUE
      } else if(values$test_length == "SEM"){
        values$min_sem>values$irt_out[[3]]
      } else {
        values$i>values$test_length
      }
      # go to results if indicated
      if (go_to_results){
        
        values$irt_final <- 
          get_final_numbers(out = values$irt_out,
                            previous = values$previous,
                            num_previous = values$num_previous)
        
        updateNavbarPage(session, "mainpage",
                         selected = "Results")
        shinyjs::show("download_report-report_download")
        shinyjs::show("download_results-results_download")
        
        cat(paste(
          "Key app variables after ending test:", "\n",
          "Number of items administered:", values$i-1, "\n",
          "----------------------------------------"
        ))
        
      }
      values$key_val = NULL

    }
    # don't run this on start up. 
  }, ignoreInit = T)
  
  ################################## EXPORT TEST DATA ############################
  # ------------------------------------------------------------------------------
  ################################################################################
  # get data into strings for exporting...test only
  observeEvent(input$mainpage=="Results",{
    if(!isTruthy(values$score_uploaded_test)){
    values$results_data_long = get_results_data_long(values)
    }
    #values$out_words <- pull_column(values$results_data_long, target)
    values$out_nums <- pull_column(values$results_data_long, response)
    values$out_ability <- pull_column(values$results_data_long, ability)
    values$out_sem <- pull_column(values$results_data_long,sem)
    values$item_dif <- pull_column(values$results_data_long, itemDifficulty)
    values$disc <- pull_column(values$results_data_long,discrimination)
    values$key <- pull_column(values$results_data_long, key)
    values$order <- pull_column(values$results_data_long, order)
    #values$item_number <- pull_column(values$results_data_long, item_number)
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
    req(values$irt_final)
    get_text_summary(ability = values$irt_final$ability,
                     sem = values$irt_final$sem,
                     last_ability = values$irt_final$last_ability,
                     last_sem = values$irt_final$last_sem,
                     num_previous = values$num_previous,
                     n_items = sum(!is.na(values$results_data_long$key)))
  })
  
  ################################## DOWNLOADS ###################################
  # ------------------------------------------------------------------------------
  ################################################################################

  # Data
  
  downloadResultsServer(id = "download_results",
                       values = values,
                       in_progress = input$mainpage, name = input$name)
  downloadResultsServer(id = "download_results_rescore",
                       values = values,
                       in_progress = input$mainpage, name = input$name)
  
   # REPORT 
  
  downloadReportServer(id = "download_report",
                       values = values,
                       name = input$name, notes = input$notes)
  downloadReportServer(id = "download_report_rescore",
                       values = values,
                       name = input$name, notes = input$notes)

  
  ################################## PLOT ######################################## 
  # ------------------------------------------------------------------------------
  ################################################################################
  # plot
  output$plot <- renderPlot({# Fergadiotis, 2019
      req(values$irt_final)
      get_plot(irt_final = values$irt_final)
  })
  
  output$plot_caption <- renderUI({
      req(values$irt_final)
      get_caption(repeat_admin = !values$new_test)
  })

  
  ################################## TABLE #######################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # outputs a table of the item level responses
  output$results_table <- DT::renderDT({
      req(values$results_data_long)
      cols = if(isTruthy(values$score_uploaded_test)){
        c("target", "resp", "key", "itemDifficulty")
      } else {
        c("order", "target", "resp", "key",
          "itemDifficulty", "ability", "sem")
      }
      out <- values$results_data_long[!is.na(values$results_data_long$key),
                                      cols]
      return(out)

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
  

  
  ################################## Continue paused test #################
  # ----------------------------------------------------------------------------
  ##############################################################################
  observeEvent(input$continue_test,{
      showModal(modalDialog(
        div(
          fileInput("file_incomplete", "Upload incomplete test csv"),#,
              shinyjs::hidden(
                tags$img(src = paste0("slides/Slide", 1, ".jpeg"), id = "instructions")
              )
        ),
        easyClose = TRUE,
        size = "m",
        footer = tagList(
          modalButton("Cancel"),
          shinyjs::disabled(actionButton("resume", "Continue Test"))
        )
      ))

    
  })
  
  # observer for uploading data
  observeEvent(input$file_incomplete,{
    file <- input$file_incomplete
    ext <- tools::file_ext(file$datapath)
    # check upload
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    # save upload
    
    incomplete_dat <- read.csv(file$datapath)# %>% dplyr::arrange(item_number)
    current_test = ifelse(values$new_test, input$numitems, input$numitems_retest)
    
    if (!all(c("key", "sem", "ability", "order", "test", "resp", "response",
              "item_number", "itemDifficulty", "discrimination") %in% colnames(incomplete_dat))) {
      
      showNotification("Error: Incompatible file uploaded; please upload another", type = "error")
      incomplete_dat <- NULL
      values$item_difficulty <- items
      shinyjs::reset("file_incomplete")

    } else if (incomplete_dat$test[1] != current_test) {
      
      showNotification("Error: Please select the same test to continue", type = "error")
      shinyjs::reset("file_incomplete")

    } else {
      
      shinyjs::enable("resume")
      shinyjs::show("instructions")
      values$item_difficulty <- incomplete_dat
      values$item_difficulty <- values$item_difficulty[order(values$item_difficulty$item_number), , drop = FALSE]
    }

  })
  
  
  
  observeEvent(input$resume,{
    
    ### start practice stuff  ############################################
    values$key_val = NULL # keeps track of button press 1 (error), 2 (correct)
    values$exclude_previous <- ifelse(values$new_test, F, input$exclude_previous) # only informs second tests
    # only use IRT function if NOT 175 items
    values$name = input$name
    values$notes = input$notes
    values$notes_retest = input$notes_retest
    values$eskimo <- ifelse(values$new_test, input$eskimo, input$eskimo_retest)
    if(isTruthy(values$new_test)){
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$selected_test = input$numitems
      values$IRT = ifelse(grepl( "cat", input$numitems), TRUE, FALSE)
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl("walker", input$numitems), TRUE, FALSE)
      values$exclude_previous = F
      values$walker_form = input$walker
      if(isTruthy(values$walker)){
        values$item_difficulty = values$item_difficulty[values$item_difficulty$walker == input$walker,]
        
      }
      
    } else {
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$selected_test = input$numitems_retest
      values$IRT = ifelse(grepl(input$numitems_retest, "cat"), TRUE, FALSE)
      #print(values$IRT)
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl(input$numitems_retest, "walker"), TRUE, FALSE)
      values$walker_form = input$walker_retest
      if(isTruthy(values$walker)){
        values$item_difficulty = values$item_difficulty[values$item_difficulty$walker == input$walker_retest,]
      }
      
    }

    shinyjs::show("start_over")
    #shinyjs::show("help")
    
    
    
    #### start stuff ############################################
    # how many items are done already?
    values$i = sum(!is.na(values$item_difficulty$response))+1
    #print(values$i)
    values$irt_out = irt_function(all_items = values$item_difficulty,
                                  IRT = values$IRT,
                                  exclude_previous = values$exclude_previous,
                                  previous = values$previous,
                                  #test = input$numitems,
                                  exclude_eskimo = values$eskimo,
                                  walker = values$walker
    )
    #print(values$irt_out)
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

    values$irt_out <- list(0, 0, 11) # reset saved data just in case. 

    values$key_val = NULL # keeps track of button press 1 (error) or 2 (correct)
    shiny::removeModal()
    updateNavbarPage(session, "mainpage", selected = "Assessment")
    
  })
  
  ################################## SCORE EXISTING TEST MODAL #################
  # ----------------------------------------------------------------------------
  ##############################################################################
  
  # 
  # downloading empty file
  output$downloadEmpty <- downloadHandler(
    filename = function() {
      "pnt-cat-blank.csv"
    },
    content = function(file) {
      write.csv(items[c("item_number", "target", "key")],
                file,
                row.names = FALSE)
    }
  )
  
  # observer for uploading data - including error messages
  observeEvent(input$file2,{

    uploadedData <- uploadData(input$file2, rescore = T)
    values$rescore <- uploadedData$dat

    # depending on the error message, allow progressing or show the error
    if(is.na(uploadedData$error)){
      shinyjs::enable("score_uploaded_data")
    } else {
      showNotification(uploadedData$error, type = "error")
      values$rescore <- NULL
      shinyjs::reset("file2")
      shinyjs::disable("score_uploaded_data")
    }
  })
  
  # scores the uploaded data, moves to the results page and shows the start over and 
  # download report buttons
  observeEvent(input$score_uploaded_data,{
    values$score_uploaded_test = T
    values$new_test = F
    values$rescore_list <- score_uploaded_data(values = values)
    values$results_data_long <- score_uploaded_data(values = values)$data
    values$irt_final <- score_uploaded_data(values = values)$irt_final
    updateNavbarPage(session, "mainpage",
                     selected = "Results")
    shinyjs::show("start_over")
    shinyjs::show("download_report-report_download")
    shinyjs::show("download_results-results_download")
    
  })
  
  
  ################################## END RESCORE MODULE ########################
  # ----------------------------------------------------------------------------
  ##############################################################################
  # end of app
}
