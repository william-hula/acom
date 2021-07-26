
################################## SERVER ######################################
# ------------------------------------------------------------------------------
################################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
########################## Initialize reactive values ##########################
# ------------------------------------------------------------------------------
################################################################################
  #establishes plot loading 
  w <- Waiter$new(id = "plot",
                  html = spin_loader(), 
                  color = "white")
  
  # reactiveValues is a list where elements of the list can change
  values = reactiveValues()
  values$item_difficulty <- items #dataframe of potential values
  values$i = 0 # this is the counter to track the slide number
  values$test_length <- NULL # number of items to test
  values$irt_out <- list(0, 0, 1) # will be overwritten if IRT 
  values$min_sem <- NULL # sem precision
  values$exclude_previous <- NULL
  values$previous <- NULL # previous data if uploaded
  values$num_previous <- 0 # number of previous tests
  values$datetime <- Sys.time() # establishes datetime when app opens for saving
  
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
      drop_na(response)
    # assign number of previous values
    values$num_previous <- length(unique(values$previous$date))

  })
  
################################## OBSERVERS ###################################
# ------------------------------------------------------------------------------
################################################################################
  
  ###########################Intro tab next and back############################
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
    
    ################################ END TEST ##################################
    
    observeEvent(input$end_test,{
      confirmSweetAlert(
        inputId = "confirm_end_test",
        session = session,
        title = "End test early?",
        text = "Only items with confirmed responses will be saved",
        type = "Warning",
      )
    })
    
    observeEvent(input$confirm_end_test,{
      if(isTruthy(input$confirm_end_test)){
        updateNavbarPage(session, "mainpage",
                         selected = "Results")
      }
    })
    
  ################################ START PRACTICE ##############################
    observeEvent(input$start_practice,{
      # runjs("document.getElementById('audio').play();") # play click
      runjs(values$sound)
      values$i = 1 # reset values$i
      values$keyval = NULL # keeps track of button press 1 (error), 2 (correct)
      values$exclude_previous <- input$exclude_previous
      # only use IRT function if NOT 175 items
      values$IRT = ifelse(input$numitems == "175", FALSE,
                          ifelse(input$numitems == "walker", FALSE,
                                 TRUE)
      )
      # go to practice slides
      updateNavbarPage(session, "mainpage",
                       selected = "Practice")
    })
  
################################## START ASSESSMENT ############################
  # start button. sets the i value to 1 corresponding to the first slide
  # switches to the assessment tab
  # initialize values in here so that they reset whever someone hits start. 
  observeEvent(input$start, {
    # dataframe of items, difficulty, discrimination; NA column for responses 
    values$item_difficulty <- items  
    values$i = 1
    
    
    if(isTruthy(input$numitems == "walker")){
      values$item_difficulty <- 
        values$item_difficulty %>%
          filter(walker == input$walker)
    }
    
    # randomly orders stuff if the random order box is checked. only affects 175
    if(isTruthy(input$random)){
      values$item_difficulty <-
        values$item_difficulty %>%
        mutate(pnt_order = sample(pnt_order)) %>%
        arrange(pnt_order)
    }
    
    values$n = if(isTruthy(values$IRT)){
      # samples one of four first possible items, unless used previously...
      get_first_item(all_items = values$item_difficulty,
                       previous = values$previous,
                       exclude_previous = values$exclude_previous)
      
    } else if (input$numitems == "walker"){
      values$item_difficulty[values$item_difficulty$walker_order == 1,]$slide_num 
      
    } else if (isTruthy(input$random)) {
      # if random, grab first row in values$item_difficulty,
      # which is already randomized in code above
      values$item_difficulty[values$item_difficulty$pnt_order == 1,]$slide_num 
    } else {
      14 #otherwise candle
    }
    # for testing:
    if (isTRUE(getOption("shiny.testmode"))) {
    reset("keys")
    }
    values$keyval = NULL # keeps track of button press 1 (error) or 2 (correct)
    values$irt_out <- list(0, 0, 1)
    #js$click_sound()
    runjs("document.getElementById('audio').play();")
    # got to slides
    updateNavbarPage(session, "mainpage", selected = "Assessment")
    
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
          } else if(input$numitems == "175"){
            # full pnt
            values$test_length <- as.numeric(input$numitems)
              shinyjs::show("random")
              shinyjs::hide("ci_95")
              shinyjs::hide("walker")
              shinyjs::hide("exclude_previous")
          } else {
            # fixed length IRT
            values$test_length <- as.numeric(input$numitems)
            shinyjs::hide("ci_95")
            shinyjs::hide("walker")
            shinyjs::hide("random")
            if(values$num_previous>0){
              shinyjs::show("exclude_previous")
            }
          }
        })
        
        # records the sem input
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
        pauseKey()
        shinyjs::show("footer_id")
      } else {
        unpauseKey()
        shinyjs::hide("footer_id")
        
      }
    })
#############################START OVER#########################################
    # if start over is hit, go to home page
    # start assessment button then resets everything
    observeEvent(input$start_over,{
      shinyjs::reset("intro_tab")
      values = reactiveValues()
      values$item_difficulty <- items #dataframe of potential values
      values$i = 0 # this is the counter to track the slide number
      values$test_length <- NULL # number of items to test
      values$irt_out <- list(0, 0, 1) # will be overwritten if IRT 
      values$min_sem <- NULL # sem precision
      values$previous <- NULL # previous data if uploaded
      values$exclude_previous <- NULL # should we exlcude the previoustest? 
      values$num_previous <- 0 # number of previous tests
      values$datetime <- Sys.time() # reestablishes datetime
      shinyjs::reset("file1")
      updateTabsetPanel(session, "glide", "glide1")
      updateNavbarPage(session, "mainpage",
                       selected = "Home")
      
      
    })
################ THIS IS WHRERE IRT STUFF GETS INCORPORATED ####################
    
# observe event will take an action if an input changes.
# here the next button or the enter key
  # This is where the app will interact with the IRT algorithm
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
        runjs("document.getElementById('audio').play();")
        #js$click_sound()
        values$i = ifelse(values$i<13, values$i + 1, values$i)
      # otherwise, (i.e. not a practice slide)
      } else if(is.null(values$key_val)){ 
              # require a key press
              showNotification("Enter a score", type = "error")
              # as long as there's a response or it's an insturction slide...
            } else {
              runjs("document.getElementById('audio').play();")
              #js$click_sound()
              values$i = ifelse(values$i<13, values$i + 1, values$i)
            }
      values$key_val = NULL
    } else {
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
            runjs("document.getElementById('audio').play();")
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
                                            test = input$numitems
                                            )
              # save info to the item_difficulty data_frame
              values$item_difficulty[values$item_difficulty$slide_num == values$n,][9:13] <-
                tibble(
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
        # prints to the console
        print(tail(values$item_difficulty %>% drop_na(response) %>%
                     arrange(order), 10))
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
    precision = if(input$numitems == "SEM"){
      paste0("95% CI: ", input$ci_95)
    } else {
      paste0(input$numitems, " items")
    }
    
    tmp = dplyr::bind_rows(values$item_difficulty) %>%
      mutate(ci_95 = sem*1.96,
             precision = precision,
             name = input$name,
             date = values$datetime,
             notes = NA
      ) %>%
      arrange(order)
    
    tmp$notes[[1]] = input$notes
    return(tmp)
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
    dplyr::bind_rows(values$item_difficulty) %>%
      # have to switch 0s and 1s because IRT is dumb. 
      drop_na() %>%
      mutate(response = as.numeric(ifelse(response == 0, 1, 0)),
             ci_95 = sem*1.96) %>%
      summarize(accuracy = mean(response)) %>%
      pull(accuracy)
  })
  
  # tracks final irt data.
  irt_final <- eventReactive(input$mainpage=="Results",{
    get_final_numbers(out = values$irt_out,
                      previous = values$previous,
                      num_previous = values$num_previous)
    
  })
################################## EXPORT TEST DATA ############################
# ------------------------------------------------------------------------------
################################################################################
  # get data into strings for exporting...test only
  observeEvent(input$mainpage=="Results",{
    values$out_words <- paste(results_data_long() %>% drop_na(response) %>%
                                 pull(target), collapse = "_")
    values$out_nums <- paste(results_data_long() %>% drop_na(response) %>%
                               pull(response), collapse = "_")
    values$out_ability <- paste(results_data_long() %>% drop_na(response) %>%
                                  pull(ability), collapse = "_")
    values$out_sem <- paste(results_data_long() %>% drop_na(response) %>%
                              pull(sem), collapse = "_")
    values$item_dif <- paste(results_data_long() %>% drop_na(response) %>%
                               pull(itemDifficulty), collapse = "_")
    values$disc <- paste(results_data_long() %>% drop_na(response) %>%
                           pull(discrimination), collapse = "_")
    values$key <- paste(results_data_long() %>% drop_na(response) %>%
                          pull(key), collapse = "_")
    values$order <- paste(results_data_long() %>% drop_na(response) %>%
                            pull(order), collapse = "_")
    values$item_number <- paste(results_data_long() %>% drop_na(response) %>%
                                  pull(item_number), collapse = "_")
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
        summary = get_text_summary(acc = results_data_summary(),
                               ability = irt_final()$ability,
                               ci_95 = irt_final()$ci_95,
                               last_ability = irt_final()$last_ability,
                               last_ci_95 = irt_final()$last_ci_95,
                               first_ability = irt_final()$first_ability,
                               first_ci_95 = irt_final()$first_ci_95,
                               num_previous = values$num_previous)
    })
################################## DOWNLOAD ####################################  
# ------------------------------------------------------------------------------
################################################################################
    # creates a data for downloading. added to accomodate previous data
    download_data <- eventReactive(input$mainpage=="Results",{
      if(!is.na(irt_final()$last_ability)){
        d1 = results_data_long() %>% mutate_all(as.character)
        d2 = values$previous %>% mutate_all(as.character)
        d3 = bind_rows(d1, d2)
      } else {
        d3 = results_data_long()
      }
      return(d3)
    })
    # downloading output
    output$downloadData <- downloadHandler(
        filename = function() {
          paste(gsub(" ", "-", input$name),
                as.character(Sys.Date()),
                "pnt.csv", sep = "_"
                )
        },
        content = function(file) {
          write.csv(download_data(), file, row.names = FALSE)
        }
    )
  
################################## FOOTER MODAL ################################
# ------------------------------------------------------------------------------
################################################################################
  # More information modal
  observeEvent(input$info, {
    showModal(modalDialog(
      tags$iframe(src="info.html", width = "100%",
                  height = "650px", frameBorder = "0"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  # readme modal. probabily will be deleted
  observeEvent(input$dev, {
    showModal(modalDialog(
      tags$iframe(src="README.html", width = "100%",
                  height = "650px", frameBorder = "0"),
      size = "l",
      easyClose = TRUE,
    ))
  })
  
################################## PLOT ######################################## 
# ------------------------------------------------------------------------------
################################################################################
  # plot
  output$plot <- renderPlot({# Fergadiotis, 2019
    w$show()
    req(irt_final())
    get_plot(values = values, irt_final = irt_final())
  })

################################## TABLE #######################################
# ------------------------------------------------------------------------------
################################################################################
  # outputs a table of the item level responses
  output$results_table <- renderDT({
    results_data_long() %>%
      drop_na(response) %>%
      select(order, target, resp, key, itemDifficulty, ability, sem)
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
    slides_tab_div(values = values, progbar = input$progbar)
  })
  
  outputOptions(output, "results_table", suspendWhenHidden = FALSE)

# end of app
}
)




