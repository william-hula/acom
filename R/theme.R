
#' theme function
#'
#' @export
minimal_theme <- function(){
  
    if (isTRUE(getOption("shiny.testmode"))) {
     
    } else {
      bslib::bs_theme(bootswatch = "default",
                     base_font = bslib::font_google("Open Sans"),
                     heading_font = bslib::font_google("Open Sans"),
                     version = "4",
                     `enable-rounded` = FALSE,
                     `enable-transitions` = F,
                     primary = "#1665AC"
    )
    }
  
}