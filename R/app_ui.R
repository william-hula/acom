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
      waiter::use_waiter(),
      waiter::waiter_preloader(html = waiter::spin_dots(), color = "white"),
      keys::keysInput("keys", response_keys),
      keys::keysInput("enter_key", enter),
      keys::keysInput("end_test", end_test_key),
      
      ################################### layout starts here ######################### 
      
      navbarPage(title = pagetitle(),
                 id = "mainpage",
                 # footer = tags$div(
                 #   id = "footer_id",
                 #   class = "footer",
                 #   footer_div()
                 # ),
                 theme = minimal_theme(),
                 
                 ############################ Instructions ############################## 
                 
                 tabPanelBody(value = "Home",
                              # shiny.pwa::pwa("https://rb-cavanaugh.shinyapps.io/pnt-cat/",
                              #                output = "inst",
                              #                title = "shinyCAT",
                              #                icon = "www/cat.png"),
                              tags$audio(id = "audio",
                                         src = system.file("app", "www", "click.wav",package = "PNT.CAT"),
                                         type = "audio/wav",
                                         style = "display:none;"),
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
                 
                 ############################ Rescored Results ###########################
                 
                 tabPanelBody(value = "Results2", 
                              results_tab_div2()
                 )
                 
                 ########################################################################
                 
                 # close navbar page
      ),
      
      # adjusting for footer. 
      br(), br(), br(), br(), br(), 
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
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'PNT.CAT'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

