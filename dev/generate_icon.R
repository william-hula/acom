

library(tidyverse)
library(showtext)
library(hexSticker)
library(here)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto Condensed", "font_name")
## Automatically use showtext to render text for future devices
showtext_auto()
plot(sticker(subplot = ggplot() + theme_void(),
             package = "PNT-CAT",
             p_family = "font_name",
             p_size =30,
             p_fontface = "bold",
             p_y= 1,
             h_color = "#FFB81C",
             h_fill = "#003594",
             filename = here("inst", "app", "www", "icon.png")
             )
)

golem::use_favicon(here("inst", "app", "www", "icon.png"))
