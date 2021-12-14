#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    tagList(
      
      ################################### SETUP ######################################
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = file.path("www","style.css"))
      ),
      keys::useKeys(),
      shinyjs::useShinyjs(),
      keys::keysInput("keys", response_keys),
      keys::keysInput("enter_key", enter),
      keys::keysInput("end_test", end_test_key),
      keys::keysInput("clear_key", "0"),
      keys::keysInput("toggle_key", "9"),
      # shiny.pwa::pwa("https://aphasia-apps.shinyapps.io/pnt-ipad/",
      #                output = "inst/app/www",
      #                title = "PNT-CAT",
      #                icon = "inst/app/www/cat.png"),
      
      ################################### layout starts here ######################### 
      
      navbarPage(title = "PNT-CAT", #pagetitle(),
                 id = "mainpage",
                 theme = minimal_theme(),
                 

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
                 !!!list(bslib::nav_spacer(), bslib::nav_item(pagetitle()), bslib::nav_item(
                   tags$a(icon("github"), href = "https://github.com/rstudio/shiny", target = "_blank", style = "color:black;")
                 ))
                 
                 ########################################################################
                 
                 # close navbar page
      ),
      
      # adjusting for footer. 
      # br(), br(), br(), br(), br(), 
      #end of UI   
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

