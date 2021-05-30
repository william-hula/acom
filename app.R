
###########################################################################################
###########################################################################################
################################# CAT PNT SHINY APP #######################################
###########################################################################################
###########################################################################################
library(shiny)
################################## UI #####################################################
# -----------------------------------------------------------------------------------------
###########################################################################################  

ui <- tagList(
  
      ############################ SETUP ######################################
      
                # css no clicky on tabs
                #tags$head(tags$style(HTML('.navbar-nav a {cursor: default}'))),
                # imports javascript for hotkeys
                useKeys(),
                useShinyjs(),
                use_waiter(),
                #extendShinyjs(script = "click2.js", functions = "click_sound"),
                keysInput("keys", response_keys),
                keysInput("enter_key", enter),
                includeCSS("www/style.css"),
      ############################ layout starts here ######################### 
      
        navbarPage(title = pagetitle, id = "mainpage",
                   footer = tags$div(
                    id = "footer_id",
                    class = "footer",
                    footer_div
                   ),
                   theme= minimal_theme,
        # page 1 instructions
        
        ############################ Instructions ######################### 
        
         tabPanel(tabtitle0,
                  tags$audio(id = "audio",
                             src = "click.wav",
                             type = "audio/wav",
                             style = "display:none;"),
                  uiOutput("intro_tab")
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
    ),
    br(), br(), br(), br(), br(), # adjusting for footer. 
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
################################## Initialize reactive values #############################
# -----------------------------------------------------------------------------------------
###########################################################################################  
  w <- Waiter$new(id = "plot",
                  html = spin_loader(), 
                  color = "white")
  # reactive list. 
  # reactiveValues is like a list where elements of the list can change based on user input
  values = reactiveValues()
  values$item_difficulty <- items 
  # default starting values
  values$i = 0 # this is the counter to track the slide number
  values$test_length <- NULL
  values$irt_out <- list(0, 0, 1)
  values$min_sem <- NULL
  values$previous <- NULL
  values$num_previous <- NULL
  values$datetime <- Sys.time()
  
################################## PREVIOUS DATA ###########################################
# -----------------------------------------------------------------------------------------
###########################################################################################   
  
  observeEvent(input$file1,{
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    values$previous <- read.csv(file$datapath) %>%
      drop_na(response)
    
    values$num_previous <- length(unique(values$previous$date))
      
  })
  
################################## OBSERVERS ##############################################    
# -----------------------------------------------------------------------------------------
###########################################################################################    
  
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
  
  
  observeEvent(input$start_practice,{
    # play click
    #js$click_sound()
    runjs("document.getElementById('audio').play();")
    # dataframe of items, difficulty, discrimination; NA column for responses to start. 
    #values$item_difficulty <- items 
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
    
    # randomly orders stuff if the random order box is checked. only affects 175 selection
    if(isTruthy(input$random)){
      values$item_difficulty <-
        values$item_difficulty %>%
        mutate(pnt_order = sample(pnt_order)) %>%
        arrange(pnt_order)
    }
    
    values$n = if(isTruthy(values$IRT)){

      # samples one of four first possible items, unless used previously...
        get_first_item(values$previous)
        
      
    } else if (isTruthy(input$random)) {
      # if random, grab first row in values$item_difficulty, which is already randomized in code above
      values$item_difficulty[values$item_difficulty$pnt_order == 1,]$slide_num 
    } else {
      14 #otherwise candle
    }
    
    # for testing:
    if (isTRUE(getOption("shiny.testmode"))) {
    reset("keys")
    }
    values$keyval = NULL # keeps track of the button press 1 (error) or 2 (correct)
    values$irt_out <- list(0, 0, 1)
    
    # got to slides
    updateNavbarPage(session, "mainpage",
                     selected = tabtitle1)
    if(input$numitems != "SEM"){
      updateProgressBar(session = session, id = "progress_bar", value = 0)
    }
    #js$click_sound()
    runjs("document.getElementById('audio').play();")
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
      shinyjs::show("footer_id")
    } else {
      unpauseKey()
      shinyjs::hide("footer_id")
      
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
      
      # if slide 13, don't iterate, just show a message that says hit start...
      if(values$i == 13){
        showNotification("Press start to start testing", type = "message")
      }
      # essentially, if we're on the first two instruction slides, don't require a 1 or 2..
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
            
            runjs("document.getElementById('audio').play();")
                #js$click_sound()
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
                values$irt_out = irt_function(values$item_difficulty, IRT = values$IRT, previous = values$previous)
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
          #for testing::
          if (isTRUE(getOption("shiny.testmode"))) {
            reset("keys")
          }
    }
    # don't run this on start up. 
  }, ignoreInit = T)
  ######################### END OF IRT OBSERVER ################################# 
  
  
################################## REACTIVE DATA ##########################################  
# -----------------------------------------------------------------------------------------
###########################################################################################  
  
  # holds the item-level responses. 
  results_data_long <- reactive({
    req(input$numitems)
    precision = if(input$numitems == "SEM"){
      paste0("SEM: ", input$sem)
    } else {
      paste0(input$numitems, " items")
    }
    
    tmp = dplyr::bind_rows(values$item_difficulty) %>%
      mutate(precision = precision,
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
      mutate(response = as.numeric(ifelse(response == 0, 1, 0))) %>%
      summarize(accuracy = mean(response)) %>%
      pull(accuracy)
  })
  
  # tracks final irt data.
  irt_final <- eventReactive(input$mainpage=="tabtitle2",{
    tibble(
      ability = values$irt_out[[1]],
      sem = values$irt_out[[3]],
      last_ability = ifelse(is.null(values$previous),
                            NA,
                            values$previous[values$previous$sem==min(values$previous$sem),]$ability
      ),
      last_sem = ifelse(is.null(values$previous), NA, min(values$previous$sem))
    )
  })
  
  observeEvent(input$mainpage==tabtitle2,{
  
    # this is for making data available for export during testing.
    if (isTRUE(getOption("shiny.testmode"))) {  
        values$out_words <- paste(results_data_long() %>% drop_na(response) %>%pull(target), collapse = "_")
        values$out_nums <- paste(results_data_long() %>% drop_na(response) %>%pull(response), collapse = "_")
        values$out_ability <- paste(results_data_long() %>% drop_na(response) %>%pull(ability), collapse = "_")
        values$out_sem <- paste(results_data_long() %>% drop_na(response) %>%pull(sem), collapse = "_")
        values$item_dif <- paste(results_data_long() %>% drop_na(response) %>%pull(itemDifficulty), collapse = "_")
        values$disc <- paste(results_data_long() %>% drop_na(response) %>%pull(discrimination), collapse = "_")
        values$key <- paste(results_data_long() %>% drop_na(response) %>%pull(key), collapse = "_")
        values$order <- paste(results_data_long() %>% drop_na(response) %>%pull(order), collapse = "_")
        values$item_number <- paste(results_data_long() %>% drop_na(response) %>%pull(item_number), collapse = "_")
    }
          
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
  
  # outputs a table of the item level responses
  output$results_table <- renderDT({
      results_data_long() %>%
        drop_na(response) %>%
      select(order, target, resp, key, itemDifficulty, ability, sem)
  }, rownames = F,
     options = list(dom = "tp"))
  
  #  outputs a summary sentence
  output$results_summary <- renderUI({
      summary = 
        paste0(
                "The total accuracy for this test was ",
                 round(results_data_summary()*100, 1),
                 "%.",
                 " The final IRT ability estimate is ",
                 round(irt_final()$ability, 2),
                 " (blue) and the standard error of the mean is ",
                 round(irt_final()$sem,2),
                 "."
               )
      
      if(!is.null(values$num_previous)){
        summary = 
            paste0(
                    summary,
                    " Last assessment, the final IRT ability estimate was ",
                    round(irt_final()$last_ability,2),
                    " (red) and the standard error of the mean was ",
                    round(irt_final()$last_sem,2),
                    "."
                )
      }
      
      return(h5(summary))
  })
  
  ################################## DOWNLOAD ##############################################    
  # ---------------------------------------------------------------------------------------
  #########################################################################################
  
  # creates a data for downloading. added to accomodate previous data
  download_data <- eventReactive(input$mainpage=="tabtitle2",{
    
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
        paste(gsub(" ", "-", input$name), as.character(Sys.Date()), "pnt.csv", sep = "_")
      },
      content = function(file) {
        write.csv(download_data(), file, row.names = FALSE)
      }
  )
  
  ################################## FOOTER MODAL ##########################################
  # ---------------------------------------------------------------------------------------
  #########################################################################################
  
  # More information modal
  observeEvent(input$info, {
    showModal(modalDialog(
      # This is the content
              title = "This modal will contain important information about the app",
              "This is important",
              br(), br(), br(),br(), br(), br(),br(), br(), br(),br(), br(), 
              "Link to papers, contact info, more detailed scoring info etc...",
              br(), br(), br(),br(), br(), br(),br(), br(), br(),br(), br(), 
              "Dismiss it by clicking anywhere outside of it.",
      easyClose = TRUE,
      footer = NULL,
      size = "m"
    ))
  })
  
  observeEvent(input$dev, {
    showModal(modalDialog(
      tags$iframe(src="README.html", width = "100%", height = "650px", frameBorder = "0"),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ################################## PLOT #################################################    
  # ---------------------------------------------------------------------------------------
  #########################################################################################

  
  output$plot <- renderPlot({# Fergadiotis, 2019
    w$show()
    req(irt_final())
    
   dens = density(bayestestR::distribution_normal(1000, 0, 1.48))
   df <- tibble(
      x = dens$x,
      y = dens$y,
      lower = irt_final()$ability - irt_final()$sem,
      upper = irt_final()$ability + irt_final()$sem,
      last_lower = ifelse(is.na(irt_final()$last_ability), 
                          1000,
                          irt_final()$last_ability - irt_final()$last_sem),
      last_upper = ifelse(is.na(irt_final()$last_ability), 
                          1001,
                          irt_final()$last_ability + irt_final()$last_sem)) %>%
     rowwise() %>%
     mutate(fill1 = factor(ifelse(between(x, lower, upper), "current", NA)),
            fill2 = factor(ifelse(between(x, last_lower, last_upper), "last", NA)),
            fill3 = factor(ifelse(!between(x, lower, upper) & !between(x, last_lower, last_upper),
                                  "neither", NA))
     )
   
  p = df %>%
     ggplot2::ggplot(aes(x = x, y = y)) +
    geom_area(data = df %>% filter(fill3 == "neither"),
              aes(y = y),position = "identity", fill = "#F1F1F1", color = NA) +
    geom_area(data = df %>% filter(fill1 == "current"),
              aes(y = y),position = "identity", fill = "#0047ab", alpha = .25, color = NA) +
      
    geom_line(size = 2) +
    geom_vline(aes(xintercept = irt_final()$ability), color = "navy", alpha = .8, size = 1) +
    scale_x_continuous(breaks=seq(-5,5,1), limits = c(-5,5)) +
    scale_fill_manual(guide = "none", values = colors) +
    theme_minimal(base_size = 15) +
    xlab("PNT Ability Estimate") +
    ylab(NULL) +
    theme(axis.title.x = element_text(vjust=-1),
          plot.margin = unit(c(15, 5.5, 15, 5.5), "pt"),
          legend.position = "none",
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
  
  if (!is.null(values$num_previous)){
    p = p + 
      geom_area(data = df %>% filter(fill2 == "last"),
                aes(y = y),position = "identity", fill = "#FF0000", alpha = .25, color = NA) +
      geom_vline(aes(xintercept = irt_final()$last_ability), color = "darkred", alpha = .8, size = 1)
  } 
  
  return(p)

  })

  
  ################################## TAB UI ##############################################    
  # ---------------------------------------------------------------------------------------
  #########################################################################################
  
  # this UI is on the server side so that it can be dynamic based on other conditions in the app. 
  
  # intro tab. see glide.R
  output$intro_tab <-
    renderUI({
      column(width = 12,
        fluidRow(
          column(align = "center", width = 12,
                 div(
                   style = "width:50%;",
                   instruction_div
                 )
          )
        ),br(),
        fluidRow(
          column(width = 12,# offset = 4,
                 getting_started,
          )
        )
      )
    })
  outputOptions(output, "intro_tab", suspendWhenHidden = FALSE)
  
  # this shows the practice slides
  output$practice_tab <-
    renderUI({
      column(width = 12,
               fluidRow(
                 if(values$i %in% c(3:12)){
                   if (isTruthy(values$key_val == incorrect_key_response | values$key_val == correct_key_response)){
                     icon("dot-circle", style = "color: grey; position: absolute; right: 5px;")
                   } else {
                     icon("circle", style = "color: grey; position: absolute; right: 5px;")
                   }
                 }
               ),
            fluidRow(
              column(width = 8, offset = 2, align = "center",
                  tags$img(src = paste0("PNT/Slide", values$i, ".jpeg")),
                  # start button, at the end of the practice slides
                  if(values$i == 13){
                    div(br(),
                        actionButton("start", inputstart)
                    )
                  }
              ),
              column(width = 2)
            )
      )
    })
  
  # UI for assessment slides
  output$slides_tab <- 
    renderUI({
      column(width = 12,
          fluidRow(
            
                       if (isTruthy(values$key_val == incorrect_key_response | values$key_val == correct_key_response)){
                         icon("dot-circle", style = "color: grey; position: absolute; right: 10px;")
                       } else {
                         icon("circle", style = "color: grey; position: absolute; right: 10px;")
                       }
          ),
          fluidRow(
              column(width = 8, offset = 2, align = "center",
                     fluidRow(
                              tags$img(src = paste0("PNT/Slide", values$n, ".jpeg"))
                     ),
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
                           
                       )
                     )
              ),
              column(width = 2)
          )
      )
  })
  
  # UI for results page
  output$results_tab <- renderUI({
    
               fluidRow(
                 column(width = 8,offset = 2,
                        tabsetPanel(type = "pills",
                          tabPanel("Summary",br(),
                                   uiOutput("results_summary"), 
                                   plotOutput("plot")
                          ),
                          tabPanel("Data", 
                                   DTOutput("results_table"),
                          )
                      ), 
                      tags$div(align = "center",
                               downloadButton("downloadData",
                                              "Download results"),
                               actionButton("start_over",
                                            "Start Over",
                                            icon = icon("undo-alt")
                                            )
                      )
                  ),
                 column(width = 2)
              )
  })
  outputOptions(output, "results_table", suspendWhenHidden = FALSE)
  #bs_themer()
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)



# fluidRow(
#   column(width = 6,
#     h5("Input participant information"), br(),
#     textInput("name", nameinput),
#     textInput("notes", otherinput),
#     fileInput("file1", "Upload previous results", accept = ".csv")
#   ),
#   column(width = 6,
#     h5("Choose test options"), br(),
#     ### Use this to set how many items to run. 
#     radioButtons(inputId = "numitems",
#                  label = "Number of items (10 is for testing)",
#                  choices = c("10", "30", "60", "100", "175", "SEM"),
#                  selected = "10",
#                  inline = T
#                  ),
#     
#     # sets SEM precision. disabled if SEM not selected in numitems radio buttons
#     sliderInput("sem", "Minimum acceptable SEM",
#                 min = 0.1,
#                 max = 0.5,
#                 step = 0.01,
#                 value = 0.3),
#     # show the progress bar?
#     checkboxInput("progbar",
#                   "Show progress bar (fixed only)",
#                   value = F),
#     # randomize PNT order if doing the full 175 item test?
#     checkboxInput("random",
#                   "Random Order (175 only)",
#                   value = F),
#   )
# ),br(),
# div(align = "center",
#     
#     # start!
#     actionButton("start_practice",
#                  "Start Practice"),
#     # this is so that the app plays a click. probably could be moved elsewhere. 
#     tags$audio(id = "audio",
#                src = "click.wav",
#                type = "audio/wav",
#                style = "display:none;")
#     )


