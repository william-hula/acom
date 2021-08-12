
#' Run PNT
#' @description Function to locally run the main concept app
#' @export
#' @import shiny 
#' @examples
#' \dontrun{
#' runPNT()
#' }
runPNT <- function(...) {
  ui <- app_ui
  server <- app_server
  shinyApp(ui, server, ...)
}
