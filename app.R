# next:

# construct a dataframe that has the possible options for data entry to programmatically display an input

library(shiny)
library(tibble)
library(tidyr)
library(dplyr)
library(shinyWidgets)
library(keys)
library(DT)
library(here)

# This file holds the instructions. I've organized the text like this so that
# translating to another language is easy and finding text is easy.
source('www/text.R')
source('www/next_slide.R')
#item_names = read.csv('www/items.csv')

# These indicate errors (1) and correct responses (2)
response_keys <- c(
    "1","2"
)

# The next button
enter <- "enter"

# number of items to run:
#p = 10


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
                  numericInput(inputId = "numitems", label = "Number of Items to Test", value = 10, min = 10, max = 100, step = 1)
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
    values$i = 0 # this is the counter to track what picture to show (the "ith" slide)
    values$n = 130 # order effects
    values$keyval = NULL # keeps track of the button press 1 or 2
    values$response = NULL # this list element holds 1-row tibbles of each response for each slide
    values$item_difficulty <- items
    values$current_item = 130
    
    observe({
    values$current_item <- items %>% filter(slide_num == values$n)
    })
    
    # start button. sets the i value to 1 corresponding to the first slide
    # switches to the assessment tab
    # updates the progress bar very slightly. 
    observeEvent(input$start, {
        values$i = 1
        updateNavbarPage(session, "mainpage",
                         selected = tabtitle1)
        updateProgressBar(session = session, id = "progress_bar", value = 0)
    })
    
    # tracks the inputs
    observeEvent(input$keys, {
        values$key_val = input$keys
    })
    
    
    # observe event will take an action if an input changes. here the next button or the enter key
    observeEvent(input$enter_key, {
        # if not an instructions slide, require a key input response
        if(is.null(values$key_val) && values$i <input$numitems){
            showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
        } else if (values$i<input$numitems) {
          
          values$response[[values$i]] = tibble(
            order = values$i,
            slide_num = values$n,
            response = ifelse(values$key_val == "1", "0",
                              ifelse(values$key_val == "2", "1", "NR")
            )
          )
          
          print(dplyr::bind_rows(values$response))
          values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <- ifelse(values$key_val == "1", 0,
                                                              ifelse(values$key_val == "2", 1, "NR"))
          values$irt_out = irt_function(values$item_difficulty)
          print(values$irt_out[[2]]$name)
          values$n = values$item_difficulty[values$item_difficulty$target == values$irt_out[[2]]$name,]$slide_num
          values$i = values$i + 1
          
          #print(paste0("The number of remaining available items is ", nrow(values$item_difficulty)))
          # resets the key value AFTER saving the data. 
          
          
        } else if (values$i == input$numitems && nrow(dplyr::bind_rows(values$response))<input$numitems) {
          values$response[[values$i]] = tibble(
            order = values$i,
            slide_num = values$n,
            response = ifelse(values$key_val == "1", "0",
                              ifelse(values$key_val == "2", "1", "NR")
            )
          )
          
          print(dplyr::bind_rows(values$response))
            updateNavbarPage(session, "mainpage",
                             selected = tabtitle2)
            # probably should add something to disable enter key here. 
        } else {
          print("no more responses - on the results page.")
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
     tmp = dplyr::bind_rows(values$response) %>%
          full_join(item_key, by = "slide_num") %>%
          mutate(date = input$date,
                 notes = NA) %>%
          drop_na(response)
     
     tmp$notes[[1]] = input$notes
     return(tmp)
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
      dplyr::bind_rows(values$response) %>%
          mutate(response = as.numeric(response)) %>%
          summarize(accuracy = mean(response)) %>%
          pull(accuracy)
  })
  
  # holds the mean accuracy
  output$results_irt <- renderPrint({
    print(as.character(values$irt_out))
  })
  
  # outputs a table of the item level responses
  output$results_long <- renderDT({
      results_data_long()
  })
  
  #  outputs a summary sentence
  output$results_summary <- renderUI({
      paste0("The total accuracy for this test was ", round(results_data_summary()*100, 1), "%.")
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
                             actionButton("nxt", nextbutton), br(), br(),
                             progressBar(id = "progress_bar", value = values$i, display_pct = F, size = "xs", range_value = c(1,input$numitems+1)), br(),
                             
                         )
                     )
              )
      }
      
  })
  
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
          ),
          uiOutput("results_irt")
          )
      }
      
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)







