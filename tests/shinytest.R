# This script calls the testing script. 

library(shinytest)
shinytest::testApp(here::here())

