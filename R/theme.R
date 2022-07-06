

#' Theming function for the app
#' 
#' Returns a {bslib} theme as long as the app is not in test mode
#'
#' @export
minimal_theme <- function(){

 if (isTruthy(getOption("shiny.testmode"))) {
   versionNumber = 4
    cat("Test mode")
  } else {
    versionNumber = 5
  }
  
    theme_out =  bslib::bs_theme(bootswatch = "default",
                     base_font = bslib::font_google("Open Sans"),
                     heading_font = bslib::font_google("Open Sans"),
                     version = versionNumber,
                     `enable-rounded` = FALSE,
                     `enable-transitions` = F,
                     font_scale = 0.9,
                     primary = "#1665AC"
    )
    
    return(theme_out)
  
}
