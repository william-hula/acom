minimal_theme = if (isTRUE(getOption("shiny.testmode"))) {
 
} else {
  bs_theme(bootswatch = "default",
                 base_font = font_google("Open Sans"),
                 heading_font = font_google("Open Sans"),
                 version = "4",
                 `enable-rounded` = FALSE,
                 `enable-transitions` = F,
                 primary = "#1665AC"
)
}