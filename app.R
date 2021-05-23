
###########################################################################################
###########################################################################################
################################# CAT PNT SHINY APP #######################################
###########################################################################################
###########################################################################################

library(shiny)
library(tibble)
library(tidyr)
library(dplyr)
library(keys)
library(DT)
library(shinyjs)

# These indicate errors (1) and correct responses (2)
incorrect_key_response = "1"
correct_key_response = "2"

response_keys <- c(
  incorrect_key_response, correct_key_response
)

# The next button
enter <- "enter"

# number of items to run:
#p = 10


################################## UI #####################################################
# -----------------------------------------------------------------------------------------
###########################################################################################  

ui <- fluidPage(
      
      ############################ SETUP #########################
      
                # css no clicky on tabs
                tags$head(tags$style(HTML('.navbar-nav a {cursor: default}'))),
                # imports javascript for hotkeys
                useKeys(),
                useShinyjs(),
                extendShinyjs(script = "click.js", functions = "click_sound"),
                keysInput("keys", response_keys),
                keysInput("enter_key", enter),
      
      ############################ layout starts here ######################### 
      
        navbarPage(title = pagetitle, id = "mainpage",
        # page 1 instructions
        
        ############################ Instructions ######################### 
        
         tabPanel(tabtitle0,
                  column(width = 2),
                  column(width = 8,
                         h4("Instructions:"),
                         tags$ol(
                           tags$li(instruction1),
                           tags$li(instruction2),
                           tags$li(instruction3)
                         ), br(),
                        div(align = "center",
                            textInput("name", nameinput),
                            textAreaInput("notes", otherinput),
                            
                            ### Use this to set how many items to run. 
                            radioButtons(inputId = "numitems",
                                         label = "Number of items (10 is for testing)",
                                         choices = c("10", "30", "60", "175", "SEM"),
                                         selected = "10",
                                         inline = T
                                         ),
                            
                            # sets SEM precision. disabled if SEM not selected in numitems radio buttons
                            sliderInput("sem", "Minimum acceptable SEM",
                                        min = 0.1,
                                        max = 0.5,
                                        step = 0.01,
                                        value = 0.3),
                            checkboxInput("progbar", "Show progress bar (fixed only)"), br(),
                            actionButton("start_practice", "Start Practice")
                            )
                  ),
                  column(width = 2)
         ),
        
        ############################ Practice #########################
        
        tabPanel(title = tabtitle_practice,
                 uiOutput("practice_tab")
                 ),
         
        ############################ Assessment #########################
        
         tabPanel(title = tabtitle1,
                  uiOutput("slides_tab")
                  ),
        
        ############################ Results #########################
        
         tabPanel(title = tabtitle2, 
                  uiOutput("results_tab")
                 )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
################################## Initialize reactive values #############################
# -----------------------------------------------------------------------------------------
###########################################################################################  
  
  # disable navigating by clicking on tabs
  shinyjs::disable(selector = '.navbar-nav a')
  
  # reactive list. 
  # reactiveValues is like a list where elements of the list can change based on user input
  values = reactiveValues()
  # default starting values
  values$i = 0 # this is the counter to track the slide number
  values$test_length <- NULL
  values$irt_out <- list(0, 0, 1)
  values$min_sem <- NULL
  
################################## OBSERVERS ##############################################    
# -----------------------------------------------------------------------------------------
###########################################################################################    
  
  observeEvent(input$start_practice,{
    # play click
    js$click_sound()
    # dataframe of items, difficulty, discrimination; NA column for responses to start. 
    values$item_difficulty <- items 
    values$i = 1
    values$keyval = NULL # keeps track of the button press 1 (error) or 2 (correct)
    updateNavbarPage(session, "mainpage",
                     selected = tabtitle_practice)
    values$IRT = ifelse(input$numitems == "175", FALSE, TRUE)
    
  })
  
  # start button. sets the i value to 1 corresponding to the first slide
  # switches to the assessment tab
  # updates the progress bar very slightly. 
  # initialize values in here so that they reset whever someone hits start. 
  observeEvent(input$start, {
    # dataframe of items, difficulty, discrimination; NA column for responses to start.
    values$item_difficulty <- items  
    values$i = 1
    values$n = ifelse(values$IRT, 130, 14) # this selects the picture. 130 = pumpkin
    values$keyval = NULL # keeps track of the button press 1 (error) or 2 (correct)
    values$irt_out <- list(0, 0, 1)
    updateNavbarPage(session, "mainpage",
                     selected = tabtitle1)
    if(input$numitems != "SEM"){
      updateProgressBar(session = session, id = "progress_bar", value = 0)
    }
    js$click_sound()
  })
  
  # enables or disables precision option if SEM is or isn't selected. 
  # also converts the numeric option to a number
  # saves either to values$test_length
  observeEvent(input$numitems,{
    if(input$numitems == "SEM"){
      values$test_length <- "SEM"
      shinyjs::enable("sem")
    } else {
      values$test_length <- as.numeric(input$numitems)
      shinyjs::disable("sem")
    }
  })
  
  # records the sem input
  observeEvent(input$sem,{
    values$min_sem <- input$sem
  })
  
  #no key presses on home or results page
  observe({
    if(input$mainpage==tabtitle2 || input$mainpage==tabtitle0){
      pauseKey()
    } else {
      unpauseKey()
    }
  })
  
  # tracks the key inputs
  observeEvent(input$keys, {
    values$key_val = input$keys
  })
  
  # if start over is hit, go to home page
  # start assessment button then resets everything
  observeEvent(input$start_over,{
    updateNavbarPage(session, "mainpage",
                     selected = tabtitle0)
  })
  
  
  ################################### THIS IS WHRERE IRT STUFF GETS INCORPORATED ########################
  # observe event will take an action if an input changes. here the next button or the enter key
  # This is where the app will interact with the IRT algorithm
  observeEvent(input$enter_key, {
    # should the app show another item?
    # if the stopping choice is SEM, check if the current sem is less than the desired precision
    # if its just a static number of items, then check if this number has already been shown
    # returns TRUE or FALSE
    if(input$mainpage==tabtitle_practice){
      
          if(is.null(values$key_val)){ 
            showNotification("Enter a score", type = "error")
            # as long as there's a response or it's an insturction slide...
          } else {
            js$click_sound()
            values$i = ifelse(values$i<13, values$i + 1, values$i)
            # updateNavbarPage(session, "mainpage",
            #                  selected = tabtitle1)
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
            
                js$click_sound()
                # If a keyu press was detected, store it in our dataframe of items, difficulty, discrimination etc...
                # 1 is incorrect (1) and 2 is correct (0). IRT model reverses 1 and 0...
                values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <- ifelse(
                  values$key_val == incorrect_key_response, 1,
                  ifelse(values$key_val == correct_key_response, 0, "NR"))
                
                # see R/next_slide for this script.
                # it takes in the current data, values$item_difficulty
                # which also includes the most recent response (See code immediately above)
                # returns a list of 3 elements
                # element[[1]] is the new ability estimate
                # element[[2]] is a list of information returned by catR::nextSlide(), 
                # including $name, the name of the next item
                # element[[3]] returns the sem after re-estimating the model
                values$irt_out = irt_function(values$item_difficulty, values$IRT)
                
                # save info to the item_difficulty data_frame
                values$item_difficulty[values$item_difficulty$slide_num == values$n,][7:11] = tibble(
                  
                  # what trial was the item presented
                  order = values$i,
                  # what picture did the item call
                  #slide_num = values$n,
                  # what was the key press
                  key = values$key_val,
                  # 1 is incorrect (1) and 2 is correct (0). IRT model reverses 1 and 0...
                  resp = ifelse(values$key_val == incorrect_key_response, "incorrect",
                                ifelse(values$key_val == correct_key_response, "correct", "NR")
                  ),
                  # NEW ability estimate after model restimation
                  ability = round(values$irt_out[[1]],3),
                  # NEW sem 
                  sem = round(values$irt_out[[3]], 3)
                  
                )
                
                # pick the next slide using the output of the irt
                # conditional fixes a bug for the last item if the test goes all the way to 175
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
          print(tail(values$item_difficulty %>% drop_na(response) %>% arrange(order), 10))
          
          
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
                                 selected = tabtitle2)
              }
          
          values$key_val = NULL
    }
    # don't run this on start up. 
  }, ignoreInit = T)
  ######################### END OF IRT OBSERVER ################################# 
  
  
################################## REACTIVE DATA ##########################################  
# -----------------------------------------------------------------------------------------
###########################################################################################  
  
  # holds the item-level responses. 
  results_data_long <- reactive({
    precision = if(input$numitems == "SEM"){
      paste0("SEM: ", input$sem)
    } else {
      paste0(input$numitems, " items")
    }
    
    tmp = dplyr::bind_rows(values$item_difficulty) %>%
      mutate(precision = precision,
             name = input$name,
             date = Sys.Date(),
             notes = NA
      ) %>%
      #drop_na(response) %>%
      dplyr::select(-slide_num) %>%
      arrange(order)
    
    tmp$notes[[1]] = input$notes
    return(tmp)
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
    dplyr::bind_rows(values$item_difficulty) %>%
      # have to switch 0s and 1s because IRT is dumb. 
      drop_na() %>%
      mutate(response = as.numeric(ifelse(response == 0, 1, 0))) %>%
      summarize(accuracy = mean(response)) %>%
      pull(accuracy)
  })
  
  # tracks final irt data.
  irt_final <- reactive({
    tibble(
      ability = values$irt_out[[1]],
      sem = values$irt_out[[3]]
    )
  })
  
  # this is for making data available for export during testing. 
  observeEvent(input$enter_key,{
  values$out_words <- paste(results_data_long() %>% drop_na(response) %>%pull(target), collapse = "_")
  values$out_nums <- paste(results_data_long() %>% drop_na(response) %>%pull(response), collapse = "_")
  values$out_ability <- paste(results_data_long() %>% drop_na(response) %>%pull(ability), collapse = "_")
  values$out_sem <- paste(results_data_long() %>% drop_na(response) %>%pull(sem), collapse = "_")
  values$item_dif <- paste(results_data_long() %>% drop_na(response) %>%pull(itemDifficulty), collapse = "_")
  values$disc <- paste(results_data_long() %>% drop_na(response) %>%pull(discrimination), collapse = "_")
  values$key <- paste(results_data_long() %>% drop_na(response) %>%pull(key), collapse = "_")
  values$order <- paste(results_data_long() %>% drop_na(response) %>%pull(order), collapse = "_")
  values$item_number <- paste(results_data_long() %>% drop_na(response) %>%pull(item_number), collapse = "_")
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
  
  
  ################################## OUTPUTS ##############################################    
  # ---------------------------------------------------------------------------------------
  #########################################################################################
  
  # this shows the slide for the i'th value
  output$slide <- renderUI({
      tmp = paste0("PNT/slide", values$n, ".jpeg")
      #print(tmp)
      tags$img(src = tmp)
  })
  
  # outputs a table of the item level responses
  output$results_long <- renderDT({
      results_data_long() %>%
        drop_na(response)
  }, rownames = F)
  
  #  outputs a summary sentence
  output$results_summary <- renderUI({
      h5(
        paste0("The total accuracy for this test was ",
               round(results_data_summary()*100, 1),
               "%.",
               " The final IRT ability estimate is ",
               round(irt_final()$ability, 3),
               " and the standard error of the mean is ",
               round(irt_final()$sem,3),
               ".")
      )
  })
  
  ################################## DOWNLOAD ##############################################    
  # ---------------------------------------------------------------------------------------
  #########################################################################################
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste(gsub(" ", "-", input$name), as.character(Sys.Date()), "pnt.csv", sep = "_")
      },
      content = function(file) {
        write.csv(results_data_long(), file, row.names = FALSE)
      }
  )
  
  ################################## TAB UI ##############################################    
  # ---------------------------------------------------------------------------------------
  #########################################################################################
  
  # this UI is on the server side so that it can be dynamic based on other conditions in the app. 
  output$practice_tab <-
    renderUI({
      column(width = 12, align = "center",
             # This is for that little icon that shows whether a key response has been logged...
             div(style = "float:right;",
                 if (isTruthy(values$key_val == incorrect_key_response | values$key_val == correct_key_response)){
                   icon("dot-circle", style = "color: grey;")
                 } else {
                   icon("circle", style = "color: grey;")
                 }),
            # the actual slides. 
            fluidRow(
              tags$img(src = paste0("PNT/slide", values$i, ".jpeg")),
              # start button, at the end of the practice slides
              if(values$i == 13){
                div(style = "float:center;", br(),
                    actionButton("start", inputstart)
                )
              }
              )
      )
    })
  
  # UI for slides with pictures.
  output$slides_tab <- renderUI({
        column(width = 12, align = "center",
               
               div(style = "float:right;",
                 if (isTruthy(values$key_val == incorrect_key_response | values$key_val == correct_key_response)){
                   icon("dot-circle", style = "color: grey;")
                 } else {
                   icon("circle", style = "color: grey;")
                 }),
               
               fluidRow(uiOutput("slide")),
               
               # note the progress bar and next/back buttons are not in the slide image. They
               # are their  own static area below the slides. 
               fluidRow(
                 div(align = "center", style = "width: 50%;",
                     
                     if (input$progbar){
                         progressBar(id = "progress_bar",
                                     value = values$i, display_pct = F,
                                     size = "xs",
                                     range_value = c(1,values$test_length+1))
                     },br()
                     
                     # This is solely for testing: always hidden
                     # shinyjs::hidden(
                     # radioButtons("keys", "for testing inputs",
                     #              choices = c(NA, incorrect_key_response, correct_key_response),
                     #              inline = T, selected = NULL),
                     # actionButton("enter_key", "enter")
                     # )
                 )
               )
        )
  })
  
  # UI for results page
  output$results_tab <- renderUI({
    
      div(
        h3("Example of data that is collected during testing"),
        uiOutput("results_summary"), br(),
        DTOutput("results_long"),
        tags$div(align = "center",
                 downloadButton("downloadData",
                                "Download results"),
                 actionButton("start_over",
                              "Start Over")
        )
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)







