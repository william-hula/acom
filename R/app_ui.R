

#' The application User-Interface
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
     
    # User Interface (UI)
    tagList(
      golem_add_external_resources(), # makes inst www folder available to app
      ################################### SETUP ######################################
      tags$head(
        # adds icon in browser tab
        favicon(ext="png"),
        # device width fix
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        # link the style sheet
        tags$link(rel = "stylesheet", type = "text/css", href = file.path("www","style.css"))
      ),
      # allows use of shinyjs() package
      shinyjs::useShinyjs(),
      # imports a js code snippet to get the users current time
      # sys.Time() gets the servers time, but not always the users time due
      # to timezone changes
      shinyjs::extendShinyjs(text = jsCode, functions = "gettime"),
      # allows us to use keys to enter responses
      keys::useKeys(),
      keys::keysInput("keys", response_keys), # 1 or 2. saved in sysdata.rda internal file
      keys::keysInput("enter_key", enter), # enter key and space bar used to progress response
      keys::keysInput("end_test", end_test_key), # esc is the end test key
      keys::keysInput("clear_key", "0"), # the 0 clears the current response
      keys::keysInput("toggle_key", "9"), # toggles between a 1 or 2 response
      
      ################################### layout starts here ######################### 
      
      navbarPage(title = "PNT-CAT (beta)", # App title
                 id = "mainpage", # id of page so you can access current page with input$mainpage
                 theme = minimal_theme(), # theme function from {bslib}. see theme.R

                 ############################ Instructions ############################## 
                 
                 tabPanelBody(value = "Home",
                              intro_tab_div()
                 ),
                 
                 ############################ Practice ##################################
                 
                 tabPanelBody(value = "Practice", 
                              uiOutput("practice_tab")
                 ),
                 
                 ############################ Assessment ################################
                 
                 tabPanelBody(value = "Assessment", 
                              uiOutput("slides_tab")
                 ),
                 
                 ############################ Results ###################################
                 
                 tabPanelBody(value = "Results", 
                              results_tab_div()
                 ),
                 # Adds information to the right of the navbar. navspacer moves it to
                 # the right. nav_items inserts each item
                 !!!list(bslib::nav_spacer(),
                         bslib::nav_item(pagetitle()),
                         bslib::nav_item(
                           tags$a(icon("readme"),
                                  href = "https://aphasia-apps.github.io/pnt",
                                  target = "_blank",
                                  style = "color:black;")
                         )
                 )
                 
                 ########################################################################
      )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )

  add_resource_path(
    'slides', app_sys('app/www/slides')
  )
 
  tags$head(
  favicon(ext="png"),
  bundle_resources(
    path = app_sys('app/www'),
    app_title = 'pnt'
  ))
}

