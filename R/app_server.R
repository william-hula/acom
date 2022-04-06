
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
  # this can be passed in its entirety to any function
  values = reactiveValues()
  # The following sets the initial values at app startup. 
  values$item_difficulty <- items #items...see observe #dataframe of potential values
  values$i = 0 # this is the counter to track the slide number
  values$test_length <- NULL # number of items to test
  values$irt_out <- list(0, 0, 11) # will be overwritten if IRT 
  values$min_sem <- NULL # sem precision
  values$exclude_previous <- NULL # exclude previous items 
  values$previous <- NULL # previous data if uploaded
  values$num_previous <- 0 # number of previous tests
  values$downloadableData = F # will the download data button appear? starts with no. yes after first response. 
  values$endTestEarly = F
  
  ################################################################################
  ########################## Dealing with user's time ############################
  # ------------------------------------------------------------------------------
  ################################################################################
  ################################################################################
  
  # runs a jsCode snippet from extendshinyJS (see app_ui)
  # this gets the users time
  # when it changes, it is saved in the reactive values
  shinyjs::js$gettime()
  observeEvent(input$jstime,{
    dt <- input$jstime # establishes datetime when app opens for saving
    values$datetime <- as.character(strptime(dt, format = '%a %b %d %Y %H:%M:%S GMT%z'))
    cat("app was opened on", values$datetime, "\n",
        "--------------------------------", "\n")
  }, once = T) # we only want this to happen once

  ################################################################################
  ################################## UPLOADS #####################################
  # ------------------------------------------------------------------------------
  ################################################################################ 
  ################################################################################
  
  # observer for uploading prior administration data
  observeEvent(input$file1,{
    
    uploadedData <- uploadData(file_input = input$file1)
    values$previous <- uploadedData$dat
    
    # sets upload conditions
    if(nrow(values$previous)>1){
        values$num_previous <- 1
        values$min_sem <- min(values$previous$sem, na.rm = T)
        shinyjs::enable("next_test")
      # triggered if uploadedData function returns an error
      # resets the upload function. 
    } else {
        showNotification(uploadedData$error, type = "error")
        values$previous <- NULL
        shinyjs::reset("file1")
    }
    
  })
  
  ##############################################################################
  ##############################################################################
  ################################ INTRO TAB NAV ###############################
  ##############################################################################
  ##############################################################################
  
  # This section creates the navigation for input$mainpage == "Home" otherwise
  # known as the introduction page. 
  
  # this function will change the intro page to the desired page. used below. 
  changeIntroPage <- function(go_to_page){
    updateTabsetPanel(session, "glide", go_to_page)
  }
  
  # TEST FLOW
  # If you press administer test, then change to the new_pnt page. 
  # reactive value holding new test set to TRUE

  observeEvent(input$administer_test,{
    changeIntroPage("new_pnt_page")
  })
  # go back to welcome page
  observeEvent(input$back_test,{
    changeIntroPage("welcome_page")
  })

  # If you press score offline test...
  observeEvent(input$score_test,{
    values$new_test = FALSE
    changeIntroPage("score_offline_page")
  })
  # go back to welcome page
  observeEvent(input$back_offline,{
    changeIntroPage("welcome_page")
  })
  # This is the next button which takes you to the 
  # instruction page. 
  observeEvent(input$next_test,{
    changeIntroPage("instructions_page")
  })

  # this goes back either to the test
  # or retest page depending on which you 
  # initially elevted. 
  observeEvent(input$back_to_test_or_retest,{
      changeIntroPage("new_pnt_page")
  })
  
  
  ##############################################################################
  ##############################################################################
  ################################ TEST OPTIONS ################################
  ##############################################################################
  ##############################################################################
  # controls available options for selecting a test or retest
  # the observe({ function means that the app is always monitoring these values})
  # in this case, its because we want to keep changing the possible options for 
  # the tests depending on which test is selected. 
  observeEvent(input$retest,{
    values$new_test = !input$retest
    if(isTruthy(input$retest)){
      shinyjs::disable("next_test")
      shinyjs::show("file1")
      updateRadioButtons(session, "numitems", 
                         label = NULL, #"Select PNT Test Administration",
                         choices = c(
                           "30-item Computer Adaptive PNT" = "30_cat",
                           #"175-item Computer Adaptive PNT" = "175_cat",
                           "Variable length Computer Adaptive PNT" = "SEM",
                           "30-item PNT Short form (Walker)" = "30_walker",
                           "175-item Standard PNT" = "175_standard"
                         ),
                         selected = "30_cat",
                         inline = F)
    } else {
      shinyjs::hide("file1")
      shinyjs::reset("file1")
      shinyjs::enable("next_test")
      updateRadioButtons(session, "numitems",
                         label = NULL, #"Select PNT Test Administration",
                         choices = c(
                           "30-item Computer Adaptive PNT" = "30_cat",
                           "175-item Computer Adaptive PNT" = "175_cat",
                           #"Variable length Computer Adaptive PNT" = "SEM",
                           "30-item PNT Short form (Walker)" = "30_walker",
                           "175-item Standard PNT" = "175_standard"
                         ),
                         selected = "30_cat",
                         inline = F)
    }
  })
  
  observe({
    # These options are available for new_test. 
    # They are shown on the new test page. 
    if(isTruthy(values$new_test)){
       if(input$numitems == "175_standard"){
          # full pnt standard administration
          values$test_length = ifelse(input$eskimo, 174, 175)
          shinyjs::show("eskimo")
          shinyjs::hide("walker")
        } else if(input$numitems == "175_cat"){
          # full pnt cat administration
          values$test_length = 174
          shinyjs::hide("eskimo")
          shinyjs::hide("walker")
        } else if(input$numitems == "30_cat"){
          # fixed length IRT
          values$test_length = 30
          shinyjs::hide("eskimo")
          shinyjs::hide("walker")
        } else { # this is the walker condition
          values$test_length = 30
          shinyjs::show("walker")
          shinyjs::hide("eskimo")
        }
    } else { # These options are for the rettest page. 
      # if the second test is a variable length. 
      if(input$numitems == "SEM"){
        # set values. 
        values$test_length <- "SEM"
        # by default the exclude previous option is set to TRUE
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(session, "exclude_previous", value = TRUE)
        # hide these options
        shinyjs::hide("walker")
        shinyjs::hide("eskimo")
        
      } else if(input$numitems == "30_cat"){
        
        # fixed length IRT of length 30
        values$test_length = 30
        shinyjs::enable("exclude_previous") # option is available to exclude previous. 
        updateCheckboxInput(session, "exclude_previous", value = TRUE)
        shinyjs::hide("walker")
        shinyjs::hide("eskimo")
        
      } else if(input$numitems == "175_cat"){
        
        values$test_length = 174
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(session, "exclude_previous", value = FALSE)
        shinyjs::hide("walker")
        shinyjs::hide("eskimo")
        
      } else if(input$numitems == "175_standard"){# full pnt
        values$test_length = ifelse(input$eskimo, 174, 175)
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(session, "exclude_previous", value = FALSE)
        shinyjs::hide("walker")
        shinyjs::show("eskimo")
        
      } else { #Final condition is for the walker test. No need to exclude previous
        
        values$test_length = 30
        shinyjs::disable("exclude_previous")
        updateCheckboxInput(session, "exclude_previous", value = FALSE)
        shinyjs::show("walker")
        shinyjs::hide("eskimo")
        
      }
    }
  })
  
  ##############################################################################
  ##############################################################################
  ################################ START OVER ##################################
  ##############################################################################
  ##############################################################################
  
  # Start over just refreshes the page/ app. This felt like the safest option
  # to ensure that nothing was carried over from the previous adminsitration. 
  observeEvent(input$start_over,{
    session$reload()
  })
  
  ##############################################################################
  ##############################################################################
  ################################ KEYS ########################################
  ##############################################################################
  ##############################################################################
  
  # Logic for how the key presses work. 
  
  # tracks the key inputs
  # When a key is selected (1 or 2) it logs the key. 
  observeEvent(input$keys, {
    values$key_val = input$keys
  })
  
  # the toggle key can also change the current selection.
  # it depends on what the current selection is. 
  observeEvent(input$toggle_key,{
    req(values$i)
    cat("Current values-i is", values$i, "\n")

      if(is.null(values$key_val)){
        values$key_val = "1"
        print("changed from null")
      } else if (values$key_val == "1"){
        print("changed from 1 to 2")
        values$key_val = "2"
      } else if (values$key_val == "2"){
        print("changed from 2 to 1")
        values$key_val = "1"
      } else { # this should never happen. 
        print("error: did not match conditions")
      }
    
  })
  
  # and the clear key can just remove the key_val input
  # you should not be able to progress to the next item after hitting clear
  observeEvent(input$clear_key, {
    values$key_val = NULL
  })
  
  # this is the green circle with the number in the top right on the practice slides
  output$key_feedback_practice <- renderUI({
    #req(values$key_val)
    if(!is.null(values$key_val)){
      val = values$key_val
    } else {
      val = "\u2013"
    }
    txt = paste0("Key: ", val)
    column(align = "right", width = 12,
           div(txt, class = "response"))
  })
  # and the green circle for the assessment slides. 
  output$key_feedback_slides <- renderUI({
    #req(values$key_val)
    if(!is.null(values$key_val)){
      val = values$key_val
    } else {
      val = "\u2013"
    }
    txt = paste0("Key: ", val)
    column(align = "right", width = 12,
           div(txt, class = "response"))
  })
  
  # and the green circle for the assessment slides. 
  output$item_number_slides <- renderUI({
    req(values$i)
    denom = strsplit(values$selected_test, split = "_")[[1]][1]
    denom = ifelse(denom == "SEM", "VL", denom)
    txt = paste0(values$i, "/", denom)
    column(align = "left", width = 12,
           div(txt, class = "response"))
  })
  
  
  #no key presses on home or results page. this turns off all key presses. 
  observe({
    if(isTruthy(input$mainpage=="Results" || input$mainpage=="Home")){
      keys::pauseKey()
    } else {
      keys::unpauseKey()
    }
  })

  ##############################################################################
  ##############################################################################
  ################################ START PRACTICE ##############################
  ##############################################################################
  ##############################################################################
  
  # This section holds logic for what happens when you hit the start practice button
  
  observeEvent(input$start_practice,{
    
    # gets the current USER time. 
    shinyjs::js$gettime()
      
    # save inputs as reactive values for easier use later
    #shinyjs::runjs(values$sound)
    values$i = 1 # reset values$i
    values$n = NULL # reset values$n - this is the slide number (e.g. slide1 if n =1)
    values$key_val = NULL # keeps track of button press 1 (error), 2 (correct)...make sure empty
    values$exclude_previous <- ifelse(values$new_test, F, input$exclude_previous) 
    values$notes = input$notes # holds notes
    values$eskimo <- input$eskimo# include eskimo?
    
    # These things need to happen for new tests
    # also saving values for the test item selections
    #if(isTruthy(values$new_test)){
      values$selected_test = input$numitems
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$IRT = ifelse(grepl( "cat", input$numitems), TRUE, FALSE)
      
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl("walker", input$numitems), TRUE, FALSE)
      values$walker_form = ifelse(isTruthy(values$walker), input$walker, NA)
      # If the input is walker, grab the right walker items
      if(isTruthy(values$walker)){
        values$item_difficulty = subset(values$item_difficulty, walker == input$walker)
      }
      
    #} else { # essentially retest
      # values$selected_test = input$numitems_retest
      # # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # # computer adaptive if the string cat is in the num items inputs
      # values$IRT = ifelse(grepl( "cat", input$numitems_retest), TRUE,
      #                     ifelse(grepl("SEM", input$numitems_retest), TRUE,
      #                                  FALSE))
      # 
      # # walker is true if the string walker is in the num items inputs
      # values$walker = ifelse(grepl("walker", input$numitems_retest), TRUE, FALSE)
      # values$walker_form = ifelse(isTruthy(values$walker), input$walker_retest, NA)
      # # walker again
      # if(isTruthy(values$walker)){
      #   values$item_difficulty = subset(values$item_difficulty, walker == input$walker_retest)
      # }
      
    #}
    # show the start over button now, which will refresh the app. 
    shinyjs::show("start_over")
    
    # go to practice slides
    updateNavbarPage(session, "mainpage",
                     selected = "Practice")
    
    # prints to the console for debugging
    cat(paste(
              "Key app variables after selecting start practice:", "\n",
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
  
  ##############################################################################
  ##############################################################################
  ################################## START ASSESSMENT ##########################
  ##############################################################################
  ##############################################################################
  
  # What happens when you press start assessment at the end of practice
  # Mostly, what is the first item to show?
  
  # start button. sets the i value to 1 corresponding to the first slide
  # switches to the assessment tab
  # initialize values in here so that they reset whever someone hits start. 
  observeEvent(input$start, {
    
    # the start time is initiated from the practice items before...
    # for some reason, the gettime and accessing the new input
    # can't be in the save observe event. 
    values$start_time = as.character(strptime(input$jstime,
                                              format = '%a %b %d %Y %H:%M:%S GMT%z'))
    cat("Testing started on", values$start_time, "\n")
    
    # keeps track of the number of items
    values$i = 1
    
    values$n = # slide number for first slide
      if(isTruthy(values$IRT)){ # if computer adaptive...
      # samples one of four first possible items, unless used previously...
      # returns the first item number
      get_first_item(previous = values$previous,
                     exclude_previous = values$exclude_previous)
        
     } else if (isTruthy(values$walker)){ # walker first item
        values$item_difficulty[values$item_difficulty$walker_order == 1,]$slide_num 
     } else {
      14 #otherwise candle for standard PNT
     }
    values$irt_out <- list(0, 0, 11) # reset saved data just in case. 
    # got to slides, reset keyval
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

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ################################ FANCY CAT STUFF #############################
  ##############################################################################
  ##############################################################################
  ##############################################################################

  # This is where the app will interact with the -CAT-IRT algorithm
  observeEvent(input$enter_key, {
    
  ##############################################################################
  # For situations when enter/space is hit on the practice slide page
  ##############################################################################
    
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
      
  ##############################################################################
  # What happens when enter is hit on the assessment page
  ##############################################################################
      
    } else {
      # can you download data? yes - will calculate the data to go out. 
      values$downloadableData = T
      shinyjs::enable("downloadIncompleteData")
      
      # require a key input response
      if(is.null(values$key_val)){ 
        showNotification("Enter a score", type = "error")
      } else { 
        
        ########################################################################
        # store key press in our dataframe of items,
        # 1 is incorrect (1) and 2 is correct (0); (IRT model reverses 1 and 0...)
        ########################################################################
        if(values$key_val == incorrect_key_response){
          values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response = 0
          # cat(paste0("logged key press: ", values$key_val, "\n"))
          # cat(paste0("incorrect_key_response: ", incorrect_key_response, "\n"))
          # cat("Incorrect response logged (1)")
        } else if(values$key_val == correct_key_response){
          values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response = 1
          # cat(paste0("logged key press: ", values$key_val, "\n"))
          # cat(paste0("correct_key_response: ", correct_key_response, "\n"))
          # cat("Correct response logged (0)")
        } else{
          values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response = "NR"
          cat("Warning: No response logged \n")
        }
        # values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <-
        #   ifelse(values$key_val == incorrect_key_response, 1,
        #          ifelse(values$key_val == correct_key_response, 0, "NR"))
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$order = values$i
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$key = values$key_val
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$resp = 
          ifelse(values$key_val == incorrect_key_response,"incorrect",
                 ifelse(values$key_val == correct_key_response,"correct", "NR")
          )
        
        ########################################################################
        # irt_function: current data, (values$item_difficulty) w/ most recent response 
        ########################################################################
        values$irt_out = irt_function(all_items = values$item_difficulty,
                                      IRT = values$IRT,
                                      exclude_previous = values$exclude_previous,
                                      previous = values$previous,
                                      exclude_eskimo = values$eskimo,
                                      walker = values$walker
        )
        
        ########################################################################
        # Append output of the CAT/IRT function....ability and SEM estimates
        ########################################################################
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$ability = round(values$irt_out[[1]],4)
        values$item_difficulty[values$item_difficulty$slide_num == values$n,]$sem = round(values$irt_out[[3]],4)

        ########################################################################
        # pick the next slide using the output of the irt
        ########################################################################
        values$n = # values$n refers to the slide number of next pnt item (e.g. the image file name)
          if(values$IRT){ # if its a computer adaptive test, use this element from the IRT out list
            if(!is.na(values$irt_out[[2]][[1]])){ # as long as its not na
              values$item_difficulty[values$item_difficulty$target == values$irt_out[[2]]$name,]$slide_num
            } else {
              190 # otherwise, just 190, which will end the test. 
            }
          } else { # if its not an IRT test, take this element of the list (walker, standard PNT)
            values$irt_out[[2]][[2]]
          } 
        # values$i = values$i + 1
       
      ########################################################################
      # Should the test end and go to the results page? and subsequent operations
      ########################################################################
      if(isTruthy(values$endTestEarly)){
        go_to_results = TRUE
      } else if (is.na(values$n)){
        go_to_results =  TRUE
      } else if(values$test_length == "SEM"){
        go_to_results = values$irt_out[[3]]<values$min_sem # is the new sem  less than the min of the previos test?
      } else {
        go_to_results = values$i==values$test_length # number of slides seen (+1) exceeds the test length # was < 
      }
      
      # go to results if indicated
      if (isTruthy(go_to_results)){
        shinyjs::js$gettime() # log time for end of test. 
        
        # if its the end of the test, calculate the final ability/sem values
        values$irt_final <- 
          get_final_numbers(out = values$irt_out,
                            previous = values$previous,
                            num_previous = values$num_previous)
        # go to the results page. 
        values$results_data_long = get_results_data_long(values)
        updateNavbarPage(session, "mainpage", selected = "Results")
        #req(input$mainpage=="Results")
        #values$current_page = input$mainpage
        # show the download buttons
        shinyjs::show("download_report-report_download")
        shinyjs::show("download_results-results_download")
        
        cat(paste(
          "Key app variables after ending test:", "\n",
          "Number of items administered (values$i):", values$i, "\n", 
          #"The current page is", values$current_page, "\n",
          #"Other way of calc all items:", sum(!is.na(values$results_data_long$key)), "\n",
          "----------------------------------------", "\n"
        ))
        
      }
      ############################################################################
      # final steps of the IRT function
      ############################################################################
      values$i = values$i + 1 # iterate the items
      values$key_val = NULL # reset the keys

    }}
    # don't run this on start up. 
  }, ignoreInit = T)
  
  ############################################################################
  ############################################################################
  ################################ END TEST EARLY ############################
  ############################################################################
  ############################################################################
  # Bring up the modal
  
  observeEvent(input$end_test,{ 
    if(values$i > 1 & input$mainpage == "Assessment"){
      showModal(modalDialog(
        get_endtest_div(),
        easyClose = TRUE,
        size = "m",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_end_test", "End test/Go to results")
        )
      ))
    }
  })

  # If end test has been confirmed in the modal. 
  observeEvent(input$confirm_end_test,{
    values$endTestEarly = T # marker indicating test was ended manually
    shinyjs::js$gettime() #end time
    if(!is.null(values$key_val)){
      shinyjs::runjs("Mousetrap.trigger('enter');") # hit enter if there is a response to log it
      cat("Logged last response before ending test early \n")
    } else {
      # if there's no current response, nothing to log.
      cat("Ended test early without logging last response \n")
      req(values$irt_out)
      # gets the final number. thisstuff would normally be done if the test
      # was going to end on its own, but have to do it manually here when
      # the test is ended by the user. 
      values$irt_final <-
        get_final_numbers(out = values$irt_out,
                          previous = values$previous,
                          num_previous = values$num_previous)
      shinyjs::show("download_report-report_download")
      shinyjs::show("download_results-results_download")
      values$results_data_long = get_results_data_long(values)
      updateNavbarPage(session, "mainpage", selected = "Results")
      values$current_page = input$mainpage
    }
    removeModal() # modal go bye bye
    
  })
  

  ################################## SUMMARY TEXT ################################
  # ------------------------------------------------------------------------------
  ################################################################################
  #  outputs a summary sentence
  output$results_summary <- renderUI({
    req(values$irt_final)
    req(values$results_data_long)
    div(
      get_text_summary(ability = values$irt_final$ability,
                     sem = values$irt_final$sem,
                     last_ability = values$irt_final$last_ability,
                     last_sem = values$irt_final$last_sem,
                     num_previous = values$num_previous),
      get_item_warning(values) # show a warning if not enugh items were given. 
    )
      
  })
  
  ################################## DOWNLOADS ###################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # Data
  # Code held in shiny modules download_report.R and download results.R
  downloadResultsServer(id = "download_results",
                       values = values,
                       in_progress = input$mainpage) 
  downloadResultsServer(id = "download_results_rescore",
                       values = values,
                       in_progress = input$mainpage)
   # REPORT 
  downloadReportServer(id = "download_report",
                       values = values,
                       notes = input$notes)
  downloadReportServer(id = "download_report_rescore",
                       values = values,
                       notes = input$notes)
  
  ################################## PLOT ######################################## 
  # ------------------------------------------------------------------------------
  ################################################################################
  output$plot <- renderPlot({
      req(values$irt_final)
      get_plot(irt_final = values$irt_final)
  })
  
  output$plot_caption <- renderUI({
      req(values$irt_final)
      tags$em(
        get_caption(repeat_admin = !values$new_test)
      )
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
        c("order", "target", "resp", "key", "itemDifficulty", "ability", "sem", "ci95_lower", "ci95_upper")
      }
      table_out = values$results_data_long[!is.na(values$results_data_long$key), cols]
      return(table_out)
  }, rownames = F,
  options = list(dom = "tp"))
  
  ################################## TAB UI ######################################
  ## this UI is on the server side so that it can be dynamic based. see tab_*.R ##
  ################################################################################
  
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

  ##############################################################################
  ##############################################################################
  ######################## Continue stopped test ###############################
  ##############################################################################
  # upload the file, check the file for any chagnes that will break the app
  # then start the test where it left off. 
  # this requires re-doing much of the above start functions because tehre
  # is already some data and that has to be specifically handled here. 
  
  # observer for uploading data
  observeEvent(input$file_incomplete,{
    file <- input$file_incomplete

    incomplete_dat <- read.csv(file$datapath)
    current_test = input$numitems
    
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
      values$item_difficulty <- incomplete_dat
      values$item_difficulty <- values$item_difficulty[order(values$item_difficulty$item_number), , drop = FALSE]
     # print(head(values$item_difficulty, 10))
    }

  })
  
  observeEvent(input$resume,{
    showModal(modalDialog(
      h4("Click to resume the test"),
      tags$img(src = paste0("slides/Slide", 1, ".jpeg"), id = "instructions"),
      easyClose = TRUE,
      size = "l",
    ))
    
    ### start practice stuff  ############################################
    values$key_val = NULL # keeps track of button press 1 (error), 2 (correct)
    values$exclude_previous <- ifelse(values$new_test, F, input$exclude_previous) # only informs second tests
    # only use IRT function if NOT 175 items
    #values$name = input$name
    values$notes = input$notes
    values$eskimo <- input$eskimo
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


    shinyjs::show("start_over")

    #### start stuff ############################################
    # how many items are done already?
    values$i = sum(!is.na(values$item_difficulty$response))+1
    values$irt_out = irt_function(all_items = values$item_difficulty,
                                  IRT = values$IRT,
                                  exclude_previous = values$exclude_previous,
                                  previous = values$previous,
                                  #test = input$numitems,
                                  exclude_eskimo = values$eskimo,
                                  walker = values$walker
    )
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
    updateNavbarPage(session, "mainpage", selected = "Assessment")
    
  })
  
  ##############################################################################
  ##############################################################################
  ########################## SCORE EXISTING TEST MODAL #########################
  ##############################################################################
  ##############################################################################
  # downloading empty file
  output$downloadEmpty <- downloadHandler(
    filename = function() {
      "pnt-cat-blank.csv"
    },
    content = function(file) {
      write.csv(download_df,
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
      values$selected_test <- values$rescore$test[1]
      cat("The uploaded test was", values$selected_test, "\n")
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
    values$results_data_long <- values$rescore_list$data
    values$i <- values$rescore_list$rescored_items
    values$irt_final <- values$rescore_list$irt_final
    updateNavbarPage(session, "mainpage", selected = "Results")
    shinyjs::show("start_over")
    shinyjs::show("download_report-report_download")
    shinyjs::show("download_results-results_download")
  })
  
  ################################################################################
  ################################################################################
  ################################## EXPORT TEST DATA ############################
  ################################################################################
  ################################################################################
  
  observeEvent(input$mainpage,{
    values$current_page = input$mainpage
    cat(paste("The page updated to", values$current_page, "\n"))
    if(values$current_page == "Results"){
      if(!isTruthy(values$score_uploaded_test)){
        values$end_time = as.character(strptime(input$jstime,
                                                format = '%a %b %d %Y %H:%M:%S GMT%z'))
        values$duration = as.POSIXlt(values$end_time)-as.POSIXlt(values$start_time)
        cat("Testing ended on", values$end_time, "\n",
            "Total testing time was", round(values$duration[[1]], 2),
                                          units(values$duration), "\n")
      }

      }
  })
  
  # This makes the above data available after running unit test.
  exportTestValues(irt_final = values$irt_final,
                   results = values$results_data_long,
                   current_page = values$current_page#,
                   #values = values
  )
  
  
  ################################################################################
  ################################################################################
  ################################## FEEDBACK MODAL ##############################
  ################################################################################
  ################################################################################
  
  observeEvent(input$feedback,{
    showModal(modalDialog(
      tags$iframe(src="https://docs.google.com/forms/d/e/1FAIpQLSeGOHvhNahDRcGwqQieJSQ_0Zo8LSVJb9dgKMd2YK3OmOeZKw/viewform?embedded=true",
                  style = "width:100%; height:60vh; frameborder:0;"),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  # ----------------------------------------------------------------------------
  ##############################################################################
  ##############################################################################
  # end of app -----------------------------------------------------------------
  ##############################################################################
  ##############################################################################
  # ----------------------------------------------------------------------------
}
