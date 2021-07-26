
################## PACKAGES ###################

#!!!!!!!!!!!! Make sure each of these is listed in the description file!!!
library(shiny)
library(here)
library(waiter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(keys)
library(DT)
library(shinyjs)
library(htmltools)
library(shinyWidgets)
library(bslib)
library(shiny.pwa)

library(catR)
library(shinyWidgets)
######## DEFINE GLOBAL VARIABLES ########

# These indicate errors (1) and correct responses (2)
incorrect_key_response = "1"
correct_key_response = "2"

response_keys <- c(
  incorrect_key_response, correct_key_response
)

# The next button
enter <- c("enter", "space")
end_test_key = "esc"

`%!in%` <- Negate(`%in%`)

items = read.csv(here("data", "item_difficulty.csv")) %>% 
  dplyr::select(target, itemDifficulty = Item.Difficulty, discrimination = Discrimination, slide_num, walker, walker_order) %>%
  arrange(slide_num)%>%
  mutate(response = NA,
         item_number = row_number(),
         order = NA,
         key = NA,
         resp = NA,
         ability = NA,
         sem = NA,
         pnt_order = row_number()
  )

item_key = read.csv(here("data", "item_difficulty.csv")) %>% 
  dplyr::select(target, slide_num, itemDifficulty = Item.Difficulty, walker, walker_order)

starting_items <- c(130, 25, 39, 154) # are 0.02 or -0.02. closest to 0.

