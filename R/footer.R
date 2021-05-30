footer_div <- p(
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
             label = "More Information",
             icon = icon("info-circle"),
             style = "background:transparent; border:none;"
           ),
           actionButton(
             inputId = "dev",
             label = "Development Status",
             icon = icon("code-branch"),
             style = "background:transparent; border:none;"
           )                            
         )
  )
)