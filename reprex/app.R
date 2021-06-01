#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
# Define UI for application that draws a histogram
ui <- tagList(
    useShinyjs(),
    navbarPage(title = "testApp", id = "main_page",
        tabPanel("page1",
                 tabsetPanel(id = "nested1", type = "hidden",
                             tabPanel("tab1",
                                      actionButton("gotopage2", "Page 2")),
                             tabPanel("tab2"),
                             tabPanel("tab3")
                 )
        ),
        tabPanel("page2",
                 tabsetPanel(id = "nested2", type = "pill",
                             tabPanel("tab4"),
                             tabPanel("tab5")
                             )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$gotopage2,{
        updateTabsetPanel(session, "main_page", "page2")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
