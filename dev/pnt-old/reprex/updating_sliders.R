library(shiny)

# dataframe of value to send to the slider
slider_options <- data.frame(
  input_choice = c("ci95", "sem", "rel"),
  input_out = c("95% CI", "SEM", "Reliability"),
  min = c(0.5, 0.3, 0.7),
  max = c(1.5, 1.2, 1),
  value = c(0.8, 0.5, 0.85),
  step = c(0.05, 0.05, 0.05)
)

# this part is how the app looks
  ui = fluidPage(
    
        # br() just creates a line of space
        br(),
        
        # p for paragraph. just prints the text
        p("Choose a preferred entry scale"),
        
        # radio button input
        radioButtons(
                     #id of the inpuut
                     inputId = "select_input",
                     # what it says on the page
                     label = "Select desired input",
                     # choices. "Printed choice = what the app saves the value as"
                     choices = c("95% CI" = "ci95",
                                 "SEM" = "sem",
                                 "Marginal Reliability" = "rel"
                     ),
                     # what is selectd to begin with
                     selected = "ci95",
                     # choices in a horizontal line
                     inline = T
                    ),
        br(), 
        p("Choose the desired value"),
        # output of the slider. the slider is on the server side because it
        # changes in response to some input (the radio buttons)
        uiOutput("slider"),
        br(),
        p("Values change based on input. I just made up random functions"),
        # some text output. the text generator is on teh server side bcecause it
        # changes in response to some input
        uiOutput("text")
  )
  
# this part doe sthe calculations
server = function(input, output, session) {

    # list of values that can change
    v <- reactiveValues()
    
    # monitors the app for changes, and stores them as they happen in list items
    observe({
      # holds the desired value from the radio button
      v$input_val = input$desired_value
      # holds the row from the slider_options data frame that corresponds to the 
      # desired input - ci, sem, reliability
      v$options = slider_options[slider_options$input_choice==input$select_input,]
    })
    
    # observes if the value v$input_val changes, and if so, does some calculations
    observeEvent(v$input_val,{
      
      # hold these values
      val = v$input_val
      type = v$options$input_choice
      
      # conditional statement to calculate the other values, given what is selected
      
      # if the value selected is on teh confidence interval scale...
      if(type == "ci95"){
        
        # calculate the other two. 
        #  just making these up
        
        ############ LOOK HERE!!!!!!!!!! ########################################
        # IDEALLY, THERE WOULD BE FUNCTION(S) THAT CONVERT BETWEEN THE THREE THAT 
        # YOU WRITE AND GO HERE INSTEAD OF HARDCODED CALCULATIONS
        
        ci95 = val
        sem = ci95/1.96
        rel = ci95/3
        
      } else if(type == "sem"){
        
        #  just making these up
        sem = val
        ci95 = sem*1.96
        rel = ci95/3
        
      } else if(type == "rel"){
        
        #  just making these up
        rel = val
        ci95 = rel*2
        sem = ci95/1.96
        
      }
      
      # save the three values in the reactive values list v for later use
      v$ci95 = ci95
      v$sem = sem
      v$rel = rel
      
      
    })
    
    # dynamically outputs text
    output$text <- renderUI({
      
      # make sure that v$ci95 exists
      req(v$ci95)
      
      # div just creates a html element <div> to hold the output
      div(
        p(paste("The 95% CI is", round(v$ci95,2))),
        p(paste("The SEM is", round(v$sem, 2))),
        p(paste("The reliability is", round(v$rel, 2)))
      )
      
    })
    
    output$slider <- renderUI({
    
      # the slider input on the server side.
      sliderInput("desired_value", #id of input
                  "Desired Value", # what it actually says
                  min=v$options$min, # minimum possible value
                  max=v$options$max, # max
                  value=v$options$value, # starting value
                  step=v$options$step #increments
      )
    })
  }

shinyApp(ui = ui, server = server)
