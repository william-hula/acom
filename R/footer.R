#' footer
#'
#' @export
footer_div <- function(){
  p(
    column(10, offset = 1, align = "center",
         p(
           actionButton(
             inputId='source',
             label="Source Code",
             icon = icon("github"),
             onclick ="window.open('https://github.com/rbcavanaugh/pnt', '_blank')",
             style = "background:transparent; border:none;"
             
           ),
           actionButton(
             inputId = "info",
             label = "About this app",
             icon = icon("info-circle"),
             style = "background:transparent; border:none;"
           ),
           # actionButton(
           #   inputId = "dev",
           #   label = "about",
           #   icon = icon("code-branch"),
           #   style = "background:transparent; border:none;"
           # ),
           uiOutput("test_inputs"),
         )
      )
    )
}