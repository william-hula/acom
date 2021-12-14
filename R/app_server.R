
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
  values$endTestEarly = F
  

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
  
  ##############################################################################
  ##############################################################################
  ################################ INTRO TAB NAV ###############################
  ##############################################################################
  ##############################################################################
  
  changeIntroPage <- function(go_to_page){
    updateTabsetPanel(session, "glide", go_to_page)
  }
  
  # INTRO FLOW
  # observeEvent(input$welcome_next,{
  #   values$new_test = T
  #   changeIntroPage("intro_page")
  # })
  
  # observeEvent(input$back_intro,{
  #   changeIntroPage("welcome_page")
  # })
  
  # TEST FLOW
  observeEvent(input$administer_test,{
    values$new_test = T
    changeIntroPage("new_pnt_page")
  })
  
  observeEvent(input$back_test,{
    changeIntroPage("welcome_page")
  })
  
  # RETEST FLOW
  observeEvent(input$administer_retest,{
    values$new_test = F
    changeIntroPage("retest_pnt_page")
  })
  
  observeEvent(input$back_retest,{
    changeIntroPage("welcome_page")
  })
  
  # OFFLINE FLOW
  observeEvent(input$score_test,{
    values$new_test = F
    changeIntroPage("score_offline_page")
  })
  
  observeEvent(input$back_offline,{
    changeIntroPage("welcome_page")
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
  
  
  ##############################################################################
  ##############################################################################
  ################################ TEST OPTIONS ################################
  ##############################################################################
  ##############################################################################
  # controls available options for selecting a test or retest
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
        
      } else if(input$numitems_retest == "175_standard"){# full pnt
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
  
  ##############################################################################
  ##############################################################################
  ################################ START OVER ##################################
  ##############################################################################
  ##############################################################################
  
  observeEvent(input$start_over,{
    session$reload()
  })
  
  ##############################################################################
  ##############################################################################
  ################################ KEYS ########################################
  ##############################################################################
  ##############################################################################
  
  # tracks the key inputs
  observeEvent(input$keys, {
    values$key_val = input$keys
  })
  
  observeEvent(input$toggle_key,{
    req(values$i)
    cat("Current values-i is", values$i, "\n")
    if(input$mainpage=="Practice" & values$i %in% c(1, 2)){
      shinyjs::runjs("Mousetrap.trigger('enter');")
      
    } else {
      if(is.null(values$key_val)){
        values$key_val = "1"
        print("changed from null")
      } else if (values$key_val == "1"){
        print("changed from 1 to 2")
        values$key_val = "2"
      } else if (values$key_val == "2"){
        print("changed from 2 to 1")
        values$key_val = "1"
      } else {
        print("did not match conditions")
      }
    }
    
  })
  
  
  output$key_feedback_practice <- renderUI({
    req(values$key_val)
    column(align = "right", width = 12,
           div(values$key_val, class = "response", onclick="Mousetrap.trigger('enter');"))
  })
  
  output$key_feedback_slides <- renderUI({
    req(values$key_val)
    column(align = "right", width = 12,
           div(values$key_val, class = "response", onclick="Mousetrap.trigger('enter');"))
  })
  
  
  #no key presses on home or results page
  observe({
    if(isTruthy(input$mainpage=="Results" || input$mainpage=="Home")){
      keys::pauseKey()
    } else {
      keys::unpauseKey()
    }
  })
  
  observeEvent(input$clear_key, {
    values$key_val = NULL
  })
  
  ##############################################################################
  ##############################################################################
  ################################ START PRACTICE ##############################
  ##############################################################################
  ##############################################################################
  
  observeEvent(input$start_practice,{
    
    # save inputs as reactive values for easier use later
    #shinyjs::runjs(values$sound)
    values$i = 1 # reset values$i
    values$n = NULL # reset 
    values$key_val = NULL # keeps track of button press 1 (error), 2 (correct)
    values$exclude_previous <- ifelse(values$new_test, F, input$exclude_previous) 
    #values$name = input$name
    values$notes = input$notes
    values$notes_retest = input$notes_retest
    values$eskimo <- ifelse(values$new_test, input$eskimo, input$eskimo_retest)
    values$start_time = Sys.time()
    if(isTruthy(values$new_test)){
      values$selected_test = input$numitems
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$IRT = ifelse(grepl( "cat", input$numitems), TRUE, FALSE)
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl("walker", input$numitems), TRUE, FALSE)
      values$walker_form = ifelse(isTruthy(values$walker), input$walker, NA)
      
      if(isTruthy(values$walker)){
        values$item_difficulty = subset(values$item_difficulty, walker == input$walker)
      }
      
    } else {
      values$selected_test = input$numitems_retest
      # IRT is poorly named - this should say CAT - aka not computer adaptive is CAT = F
      # computer adaptive if the string cat is in the num items inputs
      values$IRT = ifelse(grepl( "cat", input$numitems_retest), TRUE,
                          ifelse(grepl("SEM", input$numitems_retest), TRUE,
                                       FALSE))
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl("walker", input$numitems_retest), TRUE, FALSE)
      values$walker_form = ifelse(isTruthy(values$walker), input$walker_retest, NA)
      
      if(isTruthy(values$walker)){
        values$item_difficulty = subset(values$item_difficulty, walker == input$walker_retest)
      }
      
    }
    
    shinyjs::show("start_over")
    
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
  
  ##############################################################################
  ##############################################################################
  ################################## START ASSESSMENT ##########################
  ##############################################################################
  ##############################################################################
  
  # start button. sets the i value to 1 corresponding to the first slide
  # switches to the assessment tab
  # initialize values in here so that they reset whever someone hits start. 
  observeEvent(input$start, {
    
    # keeps track of button press 1 (error), 2 (correct)
    values$i = 1
    values$n = # slide number for current slide
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
      
      ##########################################################################
      # Should the test show another item? 
      ##########################################################################
      # another_item <- 
      #     if(values$test_length == "SEM"){
      #     values$min_sem<values$irt_out[[3]]
      #     } else {
      #     values$i<=values$test_length
      #     }
      
      # require a key input response
      if(is.null(values$key_val)){ 
        showNotification("Enter a score", type = "error")
      } else { #} if (another_item) {
        
        ########################################################################
        # store key press in our dataframe of items,
        # 1 is incorrect (1) and 2 is correct (0); (IRT model reverses 1 and 0...)
        ########################################################################
        values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <-
          ifelse(values$key_val == incorrect_key_response, 1,
                 ifelse(values$key_val == correct_key_response, 0, "NR"))
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
      # Should the test go to the results page? and subsequent operations
      ########################################################################
      go_to_results <- if(isTruthy(values$endTestEarly)){
        TRUE
      } else if (is.na(values$n)){
        TRUE
      } else if(values$test_length == "SEM"){
        values$irt_out[[3]]<values$min_sem # is the new sem  less than the min of the previos test?
      } else {
        values$i==values$test_length # number of slides seen (+1) exceeds the test length # was < 
      }
      
      # go to results if indicated
      if (isTruthy(go_to_results)){
        values$end_time = Sys.time()
        
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
    if(values$i > 1){
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
  # Download data before ending the test early. 
  # output$downloadIncompleteData <- downloadHandler(
  #   filename = function() {
  #     paste(gsub(" ", "-", input$name),
  #           as.character(Sys.Date()),
  #           "pnt.csv", sep = "_")
  #   },
  #   content = function(file) {
  #     write.csv(get_data_for_download(values = values,
  #                                     in_progress = input$mainpage
  #     ), file, row.names = FALSE)
  #   }
  # )
  # If end test has been confirmed in the modal. 
  observeEvent(input$confirm_end_test,{
    values$endTestEarly = T
    if(!is.null(values$key_val)){
      shinyjs::runjs("Mousetrap.trigger('enter');")
      cat("Logged last response before ending test early \n")
    } else {
      cat("Ended test early without logging last response \n")
      req(values$irt_out)
      values$irt_final <-
        get_final_numbers(out = values$irt_out,
                          previous = values$previous,
                          num_previous = values$num_previous)
      shinyjs::show("download_report-report_download")
      shinyjs::show("download_results-results_download")
      values$results_data_long = get_results_data_long(values)
      updateNavbarPage(session, "mainpage", selected = "Results")
      values$end_time = Sys.time()
      values$current_page = input$mainpage
    }
    removeModal()
    
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
      get_item_warning(values)
    )
      
  })
  
  ################################## DOWNLOADS ###################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # Data
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
  output$plot <- renderPlot({# Fergadiotis, 2019
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
  #########################UPDATE THIS SECTION WITH COMMENTED CODE#######################################!!!!!!!!!!
  ##############################################################################
  
  # observer for uploading data
  observeEvent(input$file_incomplete,{
    file <- input$file_incomplete

    incomplete_dat <- read.csv(file$datapath)
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
      values$item_difficulty <- incomplete_dat
      values$item_difficulty <- values$item_difficulty[order(values$item_difficulty$item_number), , drop = FALSE]
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
      # walker is true if the string walker is in the num items inputs
      values$walker = ifelse(grepl(input$numitems_retest, "walker"), TRUE, FALSE)
      values$walker_form = input$walker_retest
      if(isTruthy(values$walker)){
        values$item_difficulty = values$item_difficulty[values$item_difficulty$walker == input$walker_retest,]
      }
      
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
      tags$iframe(src="https://docs.google.com/forms/d/e/1FAIpQLSeGOHvhNahDRcGwqQieJSQ_0Zo8LSVJb9dgKMd2YK3OmOeZKw/viewform?embedded=true", style = "width:100%; height:60vh; frameborder:0;"),
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
