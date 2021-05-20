# next:

# construct a dataframe that has the possible options for data entry to programmatically display an input

library(shiny)
library(tibble)
library(tidyr)
library(dplyr)
library(shinyWidgets)
library(keys)
library(DT)
library(shinyjs)
# Turns out if you put these in a folder called R you don't have to source them
# source(here('R','text.R'))
# source(here('R','next_slide.R'))

# These indicate errors (1) and correct responses (2)
response_keys <- c(
    "1","2"
)

# The next button
enter <- "enter"

# number of items to run:
#p = 10


# Define UI for the application. 
# Just the interface.
ui <- fluidPage(
                # imports javascript for hotkeys
                useKeys(),
                useShinyjs(),
                keysInput("keys", response_keys),
                keysInput("enter_key", enter),
        # layout starts here
        navbarPage(title = pagetitle, id = "mainpage",
        # page 1 instructions
         tabPanel(tabtitle0,
                  column(width = 4,
                         h2(welcome),
                  intro1,
                  intro2,
                  br(), br(),
                  textInput("name", nameinput),
                  textAreaInput("notes", otherinput),
                  airDatepickerInput(
                      inputId = "date",
                      label = dateinput,
                      multiple = FALSE,
                      value = Sys.time(),
                      timepicker = TRUE,
                      timepickerOpts = timepickerOptions(
                          dateTimeSeparator = " at ",
                          minutesStep = 10,
                          hoursStep = 1
                      )
                  ),
                  ### Use this to set how many items to run. 
                  radioButtons(inputId = "numitems",
                               label = "Number of items to test (10 is for testing)",
                               choices = c("10", "30", "40", "50", "SEM"),
                               selected = "10",
                               inline = T
                               ),
                  sliderInput("sem", "Minimum acceptable SEM", min = 0.1, max = 0.5, step = 0.01, value = 0.3, )
                  ),
                  column(width = 1),
                  column(width = 6,
                         h3("Instructions:"), br(),
                         instruction1,
                         br(),
                         br(),
                         instruction2,
                         br(),
                         br(),
                         instruction3,
                         br(), br(), br(),
                         div(align = "center",
                         actionButton("start", inputstart)
                         )
                  )

         ),
         # Page 2 contains the picture stimuli
         tabPanel(title = tabtitle1,
                  uiOutput("slides_tab")
         ),
        # page 3 contains the results
         tabPanel(title = tabtitle2, 
                  
                  uiOutput("results_tab")
            
         )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # reactiveValues is like a list where elements of the list can change based on user input
    values = reactiveValues()
    # default starting values
    values$i = 0 # this is the counter to track the slide number
    values$n = 130 # this selects the picture. 130 = pumpkin
    values$keyval = NULL # keeps track of the button press 1 (error) or 2 (correct)
    values$response = NULL # this list element holds 1-row tibbles of each response for each slide. (bind_rows to combine)
    values$item_difficulty <- items # dataframe of items, difficulty, discrimination; NA column for responses to start. 
    values$test_length <- NULL
    values$irt_out <- list(0, 0, 1)
    values$min_sem <- NULL
    
    observeEvent(input$numitems,{
        if(input$numitems == "SEM"){
          values$test_length <- "SEM"
          shinyjs::enable("sem")
        } else {
          values$test_length <- as.numeric(input$numitems)
          shinyjs::disable("sem")
        }
    })
    
    observeEvent(input$sem,{
      values$min_sem <- input$sem
    })
    
    observe({
      if(input$mainpage==tabtitle2){
        pauseKey()
      } else {
        unpauseKey()
      }
    })
  
    
    # start button. sets the i value to 1 corresponding to the first slide
    # switches to the assessment tab
    # updates the progress bar very slightly. 
    observeEvent(input$start, {
        values$i = 1
        updateNavbarPage(session, "mainpage",
                         selected = tabtitle1)
        if(input$numitems != "SEM"){
        updateProgressBar(session = session, id = "progress_bar", value = 0)
        }
    })
    
    # tracks the inputs
    observeEvent(input$keys, {
        values$key_val = input$keys
    })
    
    
    # observe event will take an action if an input changes. here the next button or the enter key
    observeEvent(input$enter_key, {
      
      another_item <- if(input$numitems == "SEM"){
        values$min_sem<values$irt_out[[3]]
      } else {
        values$i<=values$test_length
      }
      
        # if not an instructions slide, require a key input response
        if(is.null(values$key_val) && (values$i <values$test_length || values$min_sem<values$irt_out[[3]])){
            showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
        } else if (another_item) {
          
          
          # 1 is incorrect (1) and 2 is correct (0). IRT model reverses 1 and 0...
          values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <- ifelse(
                                                        values$key_val == "1", 1,
                                                              ifelse(values$key_val == "2", 0, "NR"))
          
          values$irt_out = irt_function(values$item_difficulty)
          
          values$response[[values$i]] = tibble(
            
                  order = values$i,
                  slide_num = values$n,
                  # 1 is incorrect (1) and 2 is correct (0). IRT model reverses 1 and 0...
                  key = values$key_val,
                  resp = ifelse(values$key_val == "1", "incorrect",
                                     ifelse(values$key_val == "2", "correct", "NR")
                                     
                                ),
                  resp_num = ifelse(values$key_val == "1", "1",
                                    ifelse(values$key_val == "2", "0", "NR")
                                    ),
                  ability = round(values$irt_out[[1]],3),
                  sem = round(values$irt_out[[3]], 3)
                  
          )
          
          # pick the next slide using the output of the irt
          # conditional fixes a bug for the last item if the test goes all the way to 175
          values$n = if(!is.na(values$irt_out[[2]][[1]])){
            values$item_difficulty[values$item_difficulty$target == values$irt_out[[2]]$name,]$slide_num
          } else {
            NA
          }
          # iterate the order
          values$i = values$i + 1
          
        } 
      
      # prints to the console
      print(tail(results_data_long(), 10))
      
      # decides whether to cut to the results page or not!
      go_to_results <- if(is.na(values$n)){
        TRUE
      } else if(input$numitems == "SEM"){
        values$min_sem>values$irt_out[[3]]
      } else {
        values$i>values$test_length
      }
      
      if (go_to_results){
        updateNavbarPage(session, "mainpage",
                         selected = tabtitle2)
      }
      
      values$key_val = NULL
        # don't run this on start up. 
    }, ignoreInit = T)
    
    
    
#### Outputs ##################################################################
    
    # this shows the slide for the i'th value
    output$slide <- renderUI({
        
        tmp = paste0("PNT/slide", values$n, ".jpeg")
        #print(tmp)
        tags$img(src = tmp)
 
    })
    
  # holds the item-level responses. 
  results_data_long <- reactive({
    precision = if(input$numitems == "SEM"){
      paste0("SEM: ", input$sem)
    } else {
      paste0(input$numitems, " items")
    }
    
     tmp = dplyr::bind_rows(values$response) %>%
          full_join(item_key, by = "slide_num") %>%
          mutate(precision = precision,
                 name = input$name,
                 date = as.Date(input$date),
                 notes = NA
                 ) %>%
          drop_na(resp_num) %>%
       dplyr::select(-slide_num)
     
     tmp$notes[[1]] = input$notes
     return(tmp)
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
      dplyr::bind_rows(values$response) %>%
      # have to switch 0s and 1s because IRT is dumb. 
          mutate(response = as.numeric(ifelse(resp_num == 0, 1, 0))) %>%
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
  
  
  # outputs a table of the item level responses
  output$results_long <- renderDT({
      results_data_long()
  }, rownames = F)
  
  #  outputs a summary sentence
  output$results_summary <- renderUI({
      h5(
        paste0("The total accuracy for this test was ", round(results_data_summary()*100, 1), "%.", 
             " The final IRT ability estimate is ",
             round(irt_final()$ability, 3), " and the standard error of the mean is ",
             round(irt_final()$sem,3), ".")
      )
  })
  
  ########### download function ##############3
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" ", "-", input$name), as.character(lubridate::date(input$date)), "pnt.csv", sep = "_")
    },
    content = function(file) {
      write.csv(results_data_long(), file, row.names = FALSE)
    }
  )
  
 ##### tab UI ###############################################################
  
  # this UI is on the server side so that it can be dynamic based on other conditions in the app. 
  
  # UI for slides with pictures.
  output$slides_tab <- renderUI({
      if(values$i == 0){
          "To start the test, hit 'Start Assessment' on the home page"
      } else {
      
              column(width = 12, align = "center",
                     fluidRow(uiOutput("slide")),
                     # note the progress bar and next/back buttons are not in the slide image. They
                     # are their  own static area below the slides. 
                     fluidRow(
                         div(align = "center", style = "width: 50%;",
                             #actionButton("back", backbutton),
                             if (input$numitems != "SEM"){
                             progressBar(id = "progress_bar",
                                         value = values$i, display_pct = F,
                                         size = "xs",
                                         range_value = c(1,values$test_length+1))
                               },
                             br(),
                             
                         )
                     )
              )
      }
      
  })
  
  # UI for results page
  output$results_tab <- renderUI({
      if(values$i < 2){
          "Hmmm....No results to show yet. "
      } else {
          div(
          h3("Example of data that is collected during testing"),
          uiOutput("results_summary"), br(),
          DTOutput("results_long"),
          tags$div(align = "center",
                   downloadButton("downloadData", "Download results")
          )
          )
      }
      
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)







