# next:

# construct a dataframe that has the possible options for data entry to programmatically display an input

library(shiny)
library(tibble)
library(dplyr)
library(shinyWidgets)
library(keys)
library(DT)

# This file holds the instructions. I've organized the text like this so that
# translating to another language is easy and finding text is easy.
source('www/english_structure.R')
item_names = read.csv('www/items.csv')

# These indicate errors (1) and correct responses (2)
response_keys <- c(
    "1","2"
)

# The next button
enter <- "enter"


# Define UI for application that draws a histogram
ui <- fluidPage(
                # imports javascript for hotkeys
                useKeys(),
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
                  textAreaInput("other", otherinput),
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
                  )
                  ),
                  column(width = 1),
                  column(width = 6,
                         h3("Instructions:"), br(),
                         instruction1,
                         br(), br(), br(),
                         div(align = "center",
                         actionButton("start", inputstart)
                         ),
                        br(), br(),
                        citation, a("Link to website", href = "https://mrri.org/philadelphia-naming-test/", target = "_blank")
                         
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
    values$i = 0 # this is the counter to track what picture to show (the "ith" slide)
    values$keyval = NULL # keeps track of the button press 1 or 2
    values$response = NULL # this list element holds 1-row tibbles of each response for each slide
    values$n = 190 # the number of slides for the progress bar
    
    
    # start button. sets the i value to 1 corresponding to the first slide
    # switches to the assessment tab
    # updates the progress bar very slightly. 
    observeEvent(input$start, {
        values$i = 1
        updateNavbarPage(session, "mainpage",
                         selected = tabtitle1)
        updateProgressBar(session = session, id = "progress_bar", value = 0, total = 100)
    })
    
    # tracks the inputs
    observeEvent(input$keys, {
        values$key_val = input$keys
    })
    
    
    # observe event will take an action if an input changes. here the next button or the enter key
    observeEvent(c(input$nxt,input$enter_key), {
        # if not an instructions slide, require a key input response
        if(is.null(values$key_val) && !(values$i %in% c(1,2,13,102, 190))){
            showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
        } else {
            
            # if an instruction slide, put an element in values$response for the instructions slide
            if(values$i %in% c(1,2,13, 102)){
                values$response[[values$i]] = tibble(
                    slide_num = values$i,
                    response = "NR",
                    slide_type = "INSTRUCTIONS"
                )
                # print the result to the console to check (this can be deleted if desired)
                print(dplyr::bind_rows(values$response))
                # add one to values$i to progress the slides
                values$i = values$i+1
                # update the progress bar
                updateProgressBar(session = session, id = "progress_bar", value = values$i, total = 190)
                
            # if not an instructions slide, but less than i = 13, must be a practice slide
            # fill out values$response accordingly. 
            } else if (values$i < 13){
                values$response[[values$i]] = tibble(
                    slide_num = values$i,
                    response = ifelse(values$key_val == "1", "0",
                                      ifelse(values$key_val == "2", "1", "NR")
                    ),
                    slide_type = "practice"
                )
                print(dplyr::bind_rows(values$response))
                values$i = values$i+1
                updateProgressBar(session = session, id = "progress_bar", value = values$i, total = 190)
                
            # otherwise, as long as its not the last slide, save key presses that we want
            # note, in this case, if the clinician changes their mind about a response, that's ok
            # it will save the most recent key press before the enter key or next button are pressed
                } else if (values$i < values$n){
                    
                    values$response[[values$i]] = tibble(
                        slide_num = values$i,
                        response = ifelse(values$key_val == "1", "0",
                                          ifelse(values$key_val == "2", "1",
                                                 "NR")
                        ),
                        slide_type = "test"
                    )
                    print(dplyr::bind_rows(values$response))
                    values$i = values$i+1
                    updateProgressBar(session = session, id = "progress_bar", value = values$i, total = 190)
                   
                # if its the last slide, then go to the results page automatically .
                } else {
                    
                    values$response[[values$i]] = tibble(
                        slide_num = values$i,
                        response = "NR",
                        slide_type = "END SLIDE"
                    )
                    print(dplyr::bind_rows(values$response))
                    updateNavbarPage(session, "mainpage",
                                     selected = tabtitle2)
                }
            # resets the key value AFTER saving the data. 
            values$key_val = NULL
        }
        # don't run this on start up. 
    }, ignoreInit = T)
    
    # Probably the back button will be disabled in production. 
    # note right now, if you hit the back button, you will have 
    # to re-enter a response. 
    observeEvent(input$back, {
        values$i = values$i-1
        if(values$i < 1){
            values$i = 1
            updateNavbarPage(session, "mainpage",
                             selected = tabtitle0)
        } else {
        }
    })
    
    
#### Outputs ##################################################################
    
    # this shows the slide for the i'th value
    output$slide <- renderUI({
        
        tmp = paste0("PNT/slide", values$i, ".jpeg")
        #print(tmp)
        tags$img(src = tmp)
 
    })
    
  # holds the item-level responses. 
  results_data_long <- reactive({
     dplyr::bind_rows(values$response) %>%
          full_join(item_names, by = "slide_num") %>%
          group_by(slide_type) %>%
          mutate(test_num = row_number(),
                 test_num = ifelse(slide_type == "INSTRUCTIONS", NA,
                                   ifelse(slide_type == "END SLIDE", NA,
                                          test_num)))
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
      dplyr::bind_rows(values$response) %>%
          filter(slide_type == "test") %>%
          mutate(response = as.numeric(response)) %>%
          summarize(accuracy = mean(response)) %>%
          pull(accuracy)
  })
  
  # outputs a table of the item level responses
  output$results_long <- renderDT({
      results_data_long()
  })
  
  #  outputs a summary sentence
  output$results_summary <- renderUI({
      paste0("The total accuracy for this test was ", round(results_data_summary()*100, 1), "%.")
  })
  
 ##### tab UI ###############################################################
  
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
                             actionButton("back", backbutton),
                             actionButton("nxt", nextbutton), br(), br(),
                             progressBar(id = "progress_bar", value = 0, display_pct = F, size = "xs"), br(),
                             
                         )
                     )
              )
      }
      
  })
  
  output$results_tab <- renderUI({
      if(values$i < 15){
          "Hmmm....No results to show yet. "
      } else {
          div(
          resultstext, br(), br(),
          uiOutput("results_summary"), br(),
          DTOutput("results_long")
          )
      }
      
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)







