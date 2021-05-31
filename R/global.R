
################## PACKAGES ###################

#!!!!!!!!!!!! Make sure each of these is listed in the description file!!!

library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(tibble)
library(keys)
library(DT)
library(shinyjs)
library(htmltools)
library(shinyWidgets)
library(bslib)
library(bayestestR)
library(waiter)

######## DEFINE GLOBAL VARIABLES ########

# These indicate errors (1) and correct responses (2)
incorrect_key_response = "1"
correct_key_response = "2"

response_keys <- c(
  incorrect_key_response, correct_key_response
)

# The next button
enter <- "enter"

`%!in%` <- Negate(`%in%`)

