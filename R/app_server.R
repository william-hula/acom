
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
  
  v = reactiveValues(
    i = 1,
    itnum = 1,
    results = d,
    num_enters = 0,
    counter = 1
  )
  
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
    v$datetime <- as.character(strptime(dt, format = '%a %b %d %Y %H:%M:%S GMT%z'))
    cat("app was opened on", v$datetime, "\n",
        "--------------------------------", "\n")
  }, once = T) # we only want this to happen once

  ################################################################################
  ################################## UPLOADS #####################################
  # ------------------------------------------------------------------------------
  ################################################################################ 
  ################################################################################
  
  # observer for uploading prior administration data
  # observeEvent(input$file1,{
  #   
  #   uploadedData <- uploadData(file_input = input$file1)
  #   values$previous <- uploadedData$dat
  #   
  #   # sets upload conditions
  #   if(nrow(values$previous)>1){
  #       values$num_previous <- 1
  #       values$min_sem <- min(values$previous$sem, na.rm = T)
  #       shinyjs::enable("next_test")
  #     # triggered if uploadedData function returns an error
  #     # resets the upload function. 
  #   } else {
  #       showNotification(uploadedData$error, type = "error")
  #       values$previous <- NULL
  #       shinyjs::reset("file1")
  #   }
  #   
  # })
  
  ##############################################################################
  ##############################################################################
  ################################ INTRO TAB NAV ###############################
  ##############################################################################
  ##############################################################################
  
  observeEvent(input$next1, {
    updateTabsetPanel(session, "intropage", selected = "intro2")
  })
  observeEvent(input$next2, {
    updateTabsetPanel(session, "intropage", selected = "intro3")
  })
  observeEvent(input$next3, {
    updateTabsetPanel(session, "intropage", selected = "intro4")
  })
  
  observeEvent(input$back2, {
    updateTabsetPanel(session, "intropage", selected = "intro1")
  })
  observeEvent(input$back3, {
    updateTabsetPanel(session, "intropage", selected = "intro2")
  })
  observeEvent(input$back4, {
    updateTabsetPanel(session, "intropage", selected = "intro3")
  })
  
  
  ##############################################################################
  ##############################################################################
  ################################ TEST OPTIONS ################################
  ##############################################################################
  ##############################################################################

  # observeEvent(input$test,{
  #   if(input$test=="custom"){
  #     shinyjs::show("custom")
  #   } else {
  #     shinyjs::hide("custom")
  #   }
  # })
  
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

  
  # and the green circle for the assessment slides. 
  # output$item_number_slides <- renderUI({
  #   req(v$test_length)
  #   txt = paste0(v$i, "/", v$test_length)
  #   column(align = "left", width = 12,
  #          div(txt, class = "response"))
  # })


  ##############################################################################
  ##############################################################################
  ################################## START ASSESSMENT ##########################
  ##############################################################################
  ##############################################################################
  
  observeEvent(input$start,{
    updateNavbarPage(session = session, "mainpage", selected = "acom")
    v$results$examiner = input$examiner
    v$results$participant = input$participant
    #v$test_length = ifelse(input$test=="custom", input$custom, input$test)
    v$test_length = input$test
    v$start_time = as.character(strptime(input$jstime,
                                              format = '%a %b %d %Y %H:%M:%S GMT%z'))
    cat("Testing started on", v$start_time, "\n")
  })
  
  

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ################################ FANCY CAT STUFF #############################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
  # observeEvent(input$enter,{
  #   v$counter = v$counter + 1
  #   cat("enter click")
  # })
  # 
  # observeEvent(input$enter_key,{
  #   v$counter = v$counter + 1
  #   cat("enter key")
  # })
  
  observeEvent(input$enter, {
    # check that a response was entered
    if (is.null(input$select)) {
      shiny::showNotification("Enter a response")
    }
    # don't gor further otherwise
    req(input$select)
    
    # If you select "Doesn't apply to me" open a modal...
    if (input$select == "Doesn't apply to me") {
      showModal(
        modalDialog(
          title = "Is this due to your communication difficulties or some other reason?",
          fluidRow(
            align = "center",
            shinyWidgets::radioGroupButtons(
              "clarify",
              label = NULL,
              choices = c(
                "No, due to some other reason" = "no",
                "Yes, due to my communication difficulties" =
                  "yes"
              ),
              selected = character(0)
              
            )
          ),
          footer = NULL,
          easyClose = FALSE
        )
      )
    } else {
      v$num_enters = v$num_enters + 1
    }
    
    
  })
  
  
  # close the modal when one of the buttons is pressed
  observeEvent(input$clarify, {
    req(input$clarify)
    v$num_enters = v$num_enters + 1
    removeModal()
  })
  
  observeEvent(v$num_enters, {
    # don't do this on start up
    req(v$num_enters > 0)
    
    
    # can't feed a null value to a function
    if (is.null(input$clarify)) {
      v$clarify = NA
    } else {
      v$clarify = input$clarify
    }
    
    responses = response_to_numeric(input$select, v$clarify, v$results$merge_cats[v$itnum])
    v$results$response_num[v$itnum] = responses$response_num
    v$results$response[v$itnum] = responses$response
    v$results$clarify[v$itnum] = responses$clarify
    v$results$response_merge[v$itnum] = responses$response_merge
    
    cat_data = goCAT(v)
    
    v$results$theta[v$itnum] = cat_data$theta
    v$results$sem[v$itnum] = cat_data$sem
    v$results$order[v$itnum] = v$i
    
    #print(head(v$results, 3))
    
    # if you've reached the max number of responses...go to results
    if (sum(!is.na(v$results$response_num)) == v$test_length) {
      updateNavbarPage(session = session, "mainpage", selected = "results")
      shinyjs::show("download_report-report_download")
      shinyjs::show("download_results-results_download")
    } else {
      # otherwise, iterate on the i
      shinyWidgets::updateRadioGroupButtons(session, "clarify", selected = character(0))
      updateRadioButtons(session, "select", selected = character(0))
      
      v$i = v$i + 1
      v$itnum = cat_data$next_item
      
    }
    
    
  })
  
  ############################################################################
  ############################################################################
  ################################ END TEST EARLY ############################
  ############################################################################
  ############################################################################
  # Bring up the modal
  
  # observeEvent(input$end_test,{ 
  #   if(values$i > 1 & input$mainpage == "Assessment"){
  #     showModal(modalDialog(
  #       get_endtest_div(),
  #       easyClose = TRUE,
  #       size = "m",
  #       footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("confirm_end_test", "End test/Go to results")
  #       )
  #     ))
  #   }
  # })
  # 
  # # If end test has been confirmed in the modal. 
  # observeEvent(input$confirm_end_test,{
  #   values$endTestEarly = T # marker indicating test was ended manually
  #   shinyjs::js$gettime() #end time
  #   if(!is.null(values$key_val)){
  #     shinyjs::runjs("Mousetrap.trigger('enter');") # hit enter if there is a response to log it
  #     cat("Logged last response before ending test early \n")
  #   } else {
  #     # if there's no current response, nothing to log.
  #     cat("Ended test early without logging last response \n")
  #     req(values$irt_out)
  #     # gets the final number. thisstuff would normally be done if the test
  #     # was going to end on its own, but have to do it manually here when
  #     # the test is ended by the user. 
  #     values$irt_final <-
  #       get_final_numbers(out = values$irt_out,
  #                         previous = values$previous,
  #                         num_previous = values$num_previous)
  #     shinyjs::show("download_report-report_download")
  #     shinyjs::show("download_results-results_download")
  #     values$results_data_long = get_results_data_long(values)
  #     updateNavbarPage(session, "mainpage", selected = "Results")
  #     values$current_page = input$mainpage
  #   }
  #   removeModal() # modal go bye bye
  #   
  # })
  
  
  ################################## OUTPUTS ################################
  # ------------------------------------------------------------------------------
  ################################################################################
  
  output$questionText <- renderUI({
    txt = getTxt(v = v)
    tags$div(glue::glue("How effectively do you {txt}?"),
           style = "font-size:1.3rem;margin-top:30px;")
  })
  
  
  output$test_text <- renderUI({
    
      txt = glue::glue("Administer a {input$test} item ACOM.")
    
    return(div(txt
               #,style = "width:300px;height:100px;"
               ))
    
  })
  

  ################################## SUMMARY TEXT ################################
  # ------------------------------------------------------------------------------
  ################################################################################
  #  outputs a summary sentence
  # output$results_summary <- renderUI({
  #   req(values$irt_final)
  #   req(values$results_data_long)
  #   div(
  #     get_text_summary(ability = values$irt_final$ability,
  #                    sem = values$irt_final$sem,
  #                    last_ability = values$irt_final$last_ability,
  #                    last_sem = values$irt_final$last_sem,
  #                    num_previous = values$num_previous),br(),
  #     get_item_warning(values) # show a warning if not enugh items were given. 
  #   )
  #     
  # })
  
  ################################## DOWNLOADS ###################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # Data
  # Code held in shiny modules download_report.R and download results.R
  downloadResultsServer(id = "download_results",
                       values = v$results) 
  # downloadResultsServer(id = "download_results_rescore",
  #                      values = v$results)
   # REPORT 
  downloadReportServer(id = "download_report",
                       v = v)
  # downloadReportServer(id = "download_report_rescore",
  #                      values = values,
  #                      notes = input$notes)
  
  ################################## PLOT ######################################## 
  # ------------------------------------------------------------------------------
  ################################################################################
  # output$plot <- renderPlot({
  #     req(values$irt_final)
  #     get_plot(irt_final = values$irt_final)
  # })
  # 
  # output$plot_caption <- renderUI({
  #     req(values$irt_final)
  #     tags$em(
  #       get_caption(repeat_admin = !values$new_test)
  #     )
  # })

  ################################## TABLE #######################################
  # ------------------------------------------------------------------------------
  ################################################################################
  # outputs a table of the item level responses
  
  output$responses <- DT::renderDataTable(server = FALSE, {
    req(input$mainpage == "results")
    df = v$results %>% dplyr::select(-discrim,-b1,-b2,-b3) %>%
      dplyr::mutate(theta = round(theta, 4), sem = round(sem, 4)) %>%
      dplyr::arrange(order)
    
    DT::datatable(df,
                  rownames = FALSE,
                  options = list(rowCallback = DT::JS(rowCallback),
                                 dom = 'frtip'),
                  filter = list(position = 'bottom', clear = FALSE)
    )
      
  })
  
  
  
  ################################################################################
  ################################################################################
  ################################## EXPORT TEST DATA ############################
  ################################################################################
  ################################################################################
  
  # observeEvent(input$mainpage,{
  #   values$current_page = input$mainpage
  #   cat(paste("The page updated to", values$current_page, "\n"))
  #   if(values$current_page == "Results"){
  #     if(!isTruthy(values$score_uploaded_test)){
  #       values$end_time = as.character(strptime(input$jstime,
  #                                               format = '%a %b %d %Y %H:%M:%S GMT%z'))
  #       values$duration = as.POSIXlt(values$end_time)-as.POSIXlt(values$start_time)
  #       cat("Testing ended on", values$end_time, "\n",
  #           "Total testing time was", round(values$duration[[1]], 2),
  #                                         units(values$duration), "\n")
  #     }}
  # })
  # 
  # # This makes the above data available after running unit test.
  # exportTestValues(irt_final = values$irt_final,
  #                  results = values$results_data_long,
  #                  current_page = values$current_page#,
  #                  #values = values
  # )
  
  
  
  # ----------------------------------------------------------------------------
  ##############################################################################
  ##############################################################################
  # end of app -----------------------------------------------------------------
  ##############################################################################
  ##############################################################################
  # ----------------------------------------------------------------------------
}
